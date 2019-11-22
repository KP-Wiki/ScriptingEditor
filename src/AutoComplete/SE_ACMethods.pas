unit SE_ACMethods;

interface
uses
  Generics.Collections, Classes,
  VerySimpleXML,
  SE_ACParams;

type
  TSEMethodType = (ftFunction, ftProcedure);

const
  MethodTypeName: array[TSEMethodType] of string = (
    'function', 'procedure'
  );

type
  TSEMethod = class(TObject)
  public
    MethodType:     TSEMethodType;
    Params:         TSEParamList;
    MethodName,
    ResultType:     string;
    class function MethodTypeToStr(aType: TSEMethodType): string;
    class function StrToMethodType(aValue: string): TSEMethodType;
    class function ParseMethodStr(aValue: string): TSEMethod;
    constructor Create;
    destructor Destroy; override;
    procedure AppendToXML(const aParent: TXmlNode);
    procedure FromXML(const aNode: TXmlNode);
  end;

  TSEMethodList = class(TObjectList<TSEMethod>)
  private
    fPrefix: string;
  public
    IsEventList: Boolean;
    constructor Create(aPrefix: string = ''; aOwnsObjects: Boolean = True); overload;
    function NewItem: TSEMethod;
    function IndexByName(aMethodName: string): Integer; virtual;
    procedure SaveToFile(aFileName: string);
    procedure LoadFromFile(aFileName: string);
    function GenerateMethodInsertNames: TStringList;
    function GenerateMethodItemList: TStringList;
    function GenerateParameterInsertList: TStringList;
    function GenerateParameterLookupList: TStringList;
    procedure SortItems;
  end;

implementation
uses
  SysUtils, Generics.Defaults;

{ TSEMethod }
class function TSEMethod.MethodTypeToStr(aType: TSEMethodType): string;
begin
  case aType of
    ftFunction:  Result := 'function';
    ftProcedure: Result := 'procedure';
    else         raise Exception.Create('Unknown value for TFuncType');
  end;
end;

class function TSEMethod.StrToMethodType(aValue: string): TSEMethodType;
var
  I: TSEMethodType;
begin
  Result := ftProcedure;

  for I := Low(TSEMethodType) to High(TSEMethodType) do
    if MethodTypeName[I] = aValue then
      Exit(I);
end;

class function TSEMethod.ParseMethodStr(aValue: string): TSEMethod;
var
  param:        TSEParam;
  s,
  paramType,
  defaultValue,
  params:       string;
  paramFlag:    TSEParamFlag;
  paramList,
  parseList:    TStringList;
  I, J,
  splitPos:     Integer;
begin
  s         := aValue;
  paramList := TStringList.Create;
  parseList := TStringList.Create;
  Result    := TSEMethod.Create;

  {
    Case:
    We need to parse the following string to a TSEMethod with it's params:
    'function Example(aId, aAmount: Integer; var aDest: TObjList; aType: TObjType = otSimple): Boolean;'
  }
  // Grab the method type; Expected output: ftFunction
  splitPos := Pos(' ', s);
  Result.MethodType := StrToMethodType(Copy(s, 1, splitPos - 1));
  Delete(s, 1, splitPos);

  if (Pos('(', s) > 0) then
  begin
    // Grab the method name; Expected output: Example
    splitPos := Pos('(', s);
    Result.MethodName := Copy(s, 1, splitPos - 1);
    Delete(s, 1, splitPos);
    // Grab the params; Expected output: aId, aAmount: Integer; var aDest: TObjList; aType: TObjType = otSimple
    splitPos := Pos(')', s);
    params := Copy(s, 1, splitPos - 1);
    Delete(s, 1, splitPos);

    if Result.MethodType = ftFunction then
    begin
      // Grab the result type; Expected output: Boolean
      Delete(s, 1, 1); // Delete ':'
      Result.ResultType := Trim(Copy(s, 1, Pos(';', s) - 1));
    end;

    // Split the parameters into a list
    while params <> '' do
    begin
      splitPos := Pos(';', params);
      s        := Copy(params, 1, splitPos - 1);
      Delete(params, 1, splitPos);
      params := Trim(params);

      if s = '' then
      begin
        s      := params;
        params := '';
      end;

      paramList.Add(s);
    end;

    for I := 0 to paramList.Count - 1 do
    begin
      parseList.Clear;
      paramFlag    := pfNone;
      defaultValue := '';
      paramType    := '';
      s            := Trim(paramList[I]);

      // Grab the param flags; Expected output (In order): pfNone, pfVar, pfOptional
      splitPos := Pos('=', s);

      if splitPos > 0 then
      begin
        // Grab the default values; Expected output: otSimple
        defaultValue := Trim(Copy(s, splitPos + 1, Length(s) - splitPos));
        paramFlag    := pfOptional;
        Delete(s, splitPos, Length(s) - splitPos + 1);
      end;

      if LowerCase(s).StartsWith('var ', True) then
      begin
        paramFlag := pfVar;
        Delete(s, 1, 4);
      end else if LowerCase(s).StartsWith('const ', True) then
      begin
        paramFlag := pfConst;
        Delete(s, 1, 6);
      end;

      // Grab the parameter types; Expected output (In order): Integer, TObjList, TObjType
      splitPos  := Pos(':', s);
      paramType := Trim(Copy(s, splitPos + 1, Length(s) - splitPos));
      Delete(s, splitPos, Length(s) - splitPos + 1);

      // Grab the parameter names; Expected output (In order): [aId, aAmount], [aDest], [aType]
      while s <> '' do
      begin
        s         := Trim(s);
        splitPos  := Pos(',', s);

        if splitPos > 0 then
        begin
          parseList.Add(Trim(Copy(s, 1, splitPos - 1)));
          Delete(s, 1, splitPos);
        end else
        begin
          parseList.Add(s);
          s := '';
        end;
      end;

      // Create param objects and add them to the result
      for J := 0 to parseList.Count - 1 do
      begin
        param              := Result.Params.NewItem;
        param.ParamName    := parseList[J];
        param.ParamType    := paramType;
        param.DefaultValue := defaultValue;
        param.Flag         := paramFlag;
      end;
    end;
  end else
  begin
    // Grab the method name; Expected output: Example
    if Result.MethodType = ftFunction then
    begin
      // Grab the result type; Expected output: Boolean
      splitPos := Pos(':', s);
      Result.MethodName := Copy(s, 1, splitPos - 1);
      Delete(s, 1, splitPos);
      Result.ResultType := Trim(Copy(s, 1, Pos(';', s) - 1));
    end else
    begin
      splitPos := Pos(';', s);
      Result.MethodName := Copy(s, 1, splitPos - 1);
      Delete(s, 1, splitPos);
    end;
  end;

  FreeAndNil(paramList);
  FreeAndNil(parseList);
end;

constructor TSEMethod.Create;
begin
  inherited;
  Params := TSEParamList.Create;
end;

destructor TSEMethod.Destroy;
begin
  FreeAndNil(Params);
  inherited;
end;

procedure TSEMethod.AppendToXML(const aParent: TXmlNode);
var
  item,
  paramParent,
  param:       TXmlNode;
  I:           Integer;
begin
  item := aParent.AddChild('Definition');
  item.SetAttribute('Type', MethodTypeToStr(MethodType));
  item.SetAttribute('Name', MethodName);
  item.SetAttribute('ResultType', ResultType);
  paramParent := item.AddChild('ParameterList');

  for I := 0 to Params.Count - 1 do
  begin
    param := paramParent.AddChild('Parameter');
    param.SetAttribute('Name', Params[I].ParamName);
    param.SetAttribute('Type', Params[I].ParamType);

    if Params[I].DefaultValue <> '' then
      param.SetAttribute('DefaultValue', Params[I].ParamName);

    if Params[I].Flag <> pfNone then
      param.SetAttribute('Flag', TSEParam.ParamFlagToStr(Params[I].Flag));
  end;
end;

procedure TSEMethod.FromXML(const aNode: TXmlNode);
var
  paramParent,
  param:       TXmlNode;
  newItem:     TSEParam;
  I:           Integer;
begin
  MethodType  := StrToMethodType(aNode.Attribute['Type']);
  MethodName  := aNode.Attribute['Name'];
  ResultType  := aNode.Attribute['ResultType'];
  paramParent := aNode.Find('ParameterList');

  for I := 0 to paramParent.ChildNodes.Count - 1 do
  begin
    param                := paramParent.ChildNodes[I];
    newItem              := Params.NewItem;
    newItem.ParamName    := param.Attribute['Name'];
    newItem.ParamType    := param.Attribute['Type'];
    newItem.DefaultValue := param.Attribute['DefaultValue'];
    newItem.Flag         := TSEParam.StrToParamFlag(param.Attribute['Flag']);
  end;
end;

{ TSEMethodList }
constructor TSEMethodList.Create(aPrefix: string = ''; aOwnsObjects: Boolean = True);
begin
  Inherited Create(aOwnsObjects);
  IsEventList := False;
  fPrefix     := aPrefix;
end;

function TSEMethodList.NewItem: TSEMethod;
begin
  Result := TSEMethod.Create;
  Add(Result);
end;

function TSEMethodList.IndexByName(aMethodName: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
    if Items[I].MethodName = aMethodName then
      Exit(I);
end;

procedure TSEMethodList.SaveToFile(aFileName: string);
var
  xml: TXmlVerySimple;
  I:   integer;
begin
  SortItems;
  xml := TXmlVerySimple.Create;
  xml.Root.NodeName := 'SEMethodDict';

  for I := 0 to Count - 1 do
    Items[I].AppendToXML(xml.Root);

  xml.SaveToFile(aFileName);
end;

procedure TSEMethodList.LoadFromFile(aFileName: string);
var
  xml:  TXmlVerySimple;
  item: TSEMethod;
  I:    Integer;
begin
  if not FileExists(aFileName) then
    Exit;

  xml := TXmlVerySimple.Create;
  xml.LoadFromFile(aFileName);

  for I := 0 to xml.Root.ChildNodes.Count - 1 do
  begin
    item := NewItem;
    item.FromXML(xml.Root.ChildNodes[I]);
  end;

  SortItems;
end;

function TSEMethodList.GenerateMethodInsertNames: TStringList;
const
  EOL              = #13#10;
  DEOL             = #13#10#13#10;
  BEGIN_END_CLAUSE = EOL + 'begin' + DEOL + 'end;'  + EOL;
var
  I, J: Integer;
  s, p: string;
begin
  Result := TStringList.Create;

  for I := 0 to Count - 1 do
  begin
    if IsEventList then
    begin
      s := TSEMethod.MethodTypeToStr(Items[I].MethodType) + ' ' + Items[I].MethodName;

      if Items[I].Params.Count > 0 then
      begin
        p := '';
        s := s + '(';

        for J := 0 to Items[I].Params.Count - 1 do
        begin
          case Items[I].Params.Items[J].Flag of
            pfVar, pfConst: p := p    + TSEParam.ParamFlagToStr(Items[I].Params.Items[J].Flag) +
                                 ' '  + Items[I].Params.Items[J].ParamName +
                                 ': ' + Items[I].Params.Items[J].ParamType;
            pfOptional:     p := p     + TSEParam.ParamFlagToStr(Items[I].Params.Items[J].Flag) +
                                 ' '   + Items[I].Params.Items[J].ParamName +
                                 ': '  + Items[I].Params.Items[J].ParamType +
                                 ' = ' + Items[I].Params.Items[J].DefaultValue;
            pfNone:         p := p     + Items[I].Params.Items[J].ParamName +
                                 ': '  + Items[I].Params.Items[J].ParamType;
          end;

          if J < Items[I].Params.Count - 1 then
            p := p + '; ';
        end;

        s := s + p + ')';
      end;

      Result.Add(s + ';' + BEGIN_END_CLAUSE);
    end else
    begin
      if Items[I].Params.Count > 0 then
        Result.Add(fPrefix + Items[I].MethodName + '(')
      else
        Result.Add(fPrefix + Items[I].MethodName + ';');
    end;
  end;
end;

function TSEMethodList.GenerateMethodItemList: TStringList;
var
  I, J: Integer;
  s, p: string;
begin
  Result := TStringList.Create;

  for I := 0 to Count - 1 do
  begin
    p := '';
    s := TSEMethod.MethodTypeToStr(Items[I].MethodType) + ' \column{}' +
         fPrefix + Items[I].MethodName;

    if Items[I].Params.Count > 0 then
    begin
      s := s + '(';

      for J := 0 to Items[I].Params.Count - 1 do
      begin
        case Items[I].Params.Items[J].Flag of
          pfVar, pfConst: p := p    + TSEParam.ParamFlagToStr(Items[I].Params.Items[J].Flag) +
                               ' '  + Items[I].Params.Items[J].ParamName +
                               ': ' + Items[I].Params.Items[J].ParamType;
          pfOptional:     p := p     + TSEParam.ParamFlagToStr(Items[I].Params.Items[J].Flag) +
                               ' '   + Items[I].Params.Items[J].ParamName +
                               ': '  + Items[I].Params.Items[J].ParamType +
                               ' = ' + Items[I].Params.Items[J].DefaultValue;
          pfNone:         p := p     + Items[I].Params.Items[J].ParamName +
                               ': '  + Items[I].Params.Items[J].ParamType;
        end;

        if J < Items[I].Params.Count - 1 then
          p := p + '; ';
      end;

      s := s + p + ')';
    end;

    case Items[I].MethodType of
      ftFunction:  s := s + ': ' + Items[I].ResultType + ';';
      ftProcedure: s := s + ';';
    end;

    Result.Add(s);
  end;
end;

function TSEMethodList.GenerateParameterInsertList: TStringList;
var
  I, J: Integer;
  s:    string;
begin
  Result := TStringList.Create;

  for I := 0 to Count - 1 do
  begin
    s := '';

    for J := 0 to Items[I].Params.Count - 1 do
    begin
      s := s + '"' + Items[I].Params[J].ParamName + ': ' + Items[I].Params[J].ParamType + '"';

      if J <> Items[I].Params.Count - 1 then
        s := s + ', ';
    end;

    Result.Add(s);
  end;
end;

function TSEMethodList.GenerateParameterLookupList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;

  for I := 0 to Count - 1 do
    Result.Add(UpperCase(Items[i].MethodName));
end;

procedure TSEMethodList.SortItems;
begin
  Sort(TComparer<TSEMethod>.Construct(
    function (const aLeft, aRight: TSEMethod): Integer
    begin
      Result := CompareText(aLeft.MethodName, aRight.MethodName);
    end
  ));
end;

end.
