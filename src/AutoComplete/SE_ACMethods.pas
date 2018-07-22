unit SE_ACMethods;
{$M+}

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
  TSEMethod = class
  public
    MethodType:     TSEMethodType;
    Params:         TSEParamList;
    ExternalMethod: Boolean;
    MethodName,
    ResultType,
    PluginName:     string;
    constructor Create;
    destructor Destroy; override;
    procedure SaveAsXML(aParent: TXmlNode);
    procedure LoadFromXML(aParent: TXmlNode);
  published
    class function MethodTypeToStr(aType: TSEMethodType): string;
    class function StrToMethodType(aValue: string): TSEMethodType;
    class function ParseMethodStr(aValue: string): TSEMethod;
  end;

  TSEMethodList = class(TObjectList<TSEMethod>)
  public
    function NewItem: TSEMethod;
    function IndexByName(aFuncName: string): Integer; virtual;
    procedure SaveAsXML(aParent: TXmlNode);
    procedure LoadFromXML(aParent: TXmlNode);
    procedure LoadFromString(aXMLString: String);
    function GenerateFunctionInsertNames: TStringList;
    function GenerateFunctionItemList: TStringList;
    function GenerateParameterInsertList: TStringList;
    function GenerateParameterLookupList: TStringList;
  end;

implementation
uses
  SysUtils;

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
  paramList,
  parseList:    TStringList;
  I, J,
  splitPos:     Integer;
  paramFlag:    TSEParamFlag;
begin
  s         := aValue;
  paramList := TStringList.Create;
  parseList := TStringList.Create;
  Result    := TSEMethod.Create;

  {
    Case:
    We need to parse the following string to a TSEMethod with it's params:
    'function Example(aId, aAmount: Integer; var aDest: TObjList; aType: TObjType = otSimple): Boolean;'

    Note:
    Also need to make this compatible with parameterless methods
  }
  // Grab the method type; Expected output: ftFunction
  Result.MethodType := StrToMethodType(Copy(s, 1, Pos(' ', s) - 1));
  Delete(s, 1, Pos(' ', s));
  // Grab the method name; Expected output: Example
  Result.MethodName := Copy(s, 1, Pos('(', s) - 1);
  Delete(s, 1, Pos('(', s));
  // Grab the params; Expected output: aId, aAmount: Integer; var aDest: TObjList; aType: TObjType = otSimple
  params := Copy(s, 1, Pos(')', s) - 1);
  Delete(s, 1, Pos(')', s));

  if Result.MethodType = ftFunction then
  begin
    // Grab the result type; Expected output: Boolean
    Delete(s, 1, 1); // Delete ':'
    Result.ResultType := Trim(Copy(s, 1, Pos(';', s) - 1));
  end;

  // Split the parameters into a list
  while params <> '' do
  begin

    s := Copy(params, 1, Pos(';', params) - 1);
    Delete(params, 1, Pos(';', params));
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
    splitPos  := Pos('=', s);

    if splitPos > 0 then
    begin
      // Grab the default values; Expected output: otSimple
      defaultValue := Trim(Copy(s, splitPos + 1, Length(s) - (splitPos + 1)));
      paramFlag    := pfOptional;
      Delete(s, splitPos, Length(s) - splitPos);
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
    paramType := Trim(Copy(s, splitPos + 1, Length(s) - (splitPos + 1)));
    Delete(s, splitPos, Length(s) - splitPos);

    // Grab the parameter names; Expected output (In order): [aId, aAmount], [aDest], [aType]
    while s <> '' do
    begin
      s := Trim(s);

      if Pos(',', s) <> 0 then
      begin
        parseList.Add(Trim(Copy(s, 1, Pos(',', s))));
        Delete(s, 1, Pos(',', s));
      end else
      begin
        ParseList.Add(Trim(s));
        s := '';
      end;
    end;

    // Create param objects and add them to the result
    for J := 0 to paramList.Count - 1 do
    begin
      param              := Result.Params.NewItem;
      param.ParamName    := paramList[J];
      param.ParamType    := paramType;
      param.DefaultValue := defaultValue;
      param.Flag         := paramFlag;
    end;
  end;

  FreeAndNil(paramList);
  FreeAndNil(parseList);
end;

constructor TSEMethod.Create;
begin
  //
end;

destructor TSEMethod.Destroy;
begin
  //
end;

procedure TSEMethod.SaveAsXML(aParent: TXmlNode);
begin
  //
end;

procedure TSEMethod.LoadFromXML(aParent: TXmlNode);
begin
  //
end;

{ TSEMethodList }
function TSEMethodList.NewItem: TSEMethod;
begin
  Result := TSEMethod.Create;
  Add(Result);
end;

function TSEMethodList.IndexByName(aFuncName: string): Integer;
begin
  //
end;

procedure TSEMethodList.SaveAsXML(aParent: TXmlNode);
begin
  //
end;

procedure TSEMethodList.LoadFromXML(aParent: TXmlNode);
begin
  //
end;

procedure TSEMethodList.LoadFromString(aXMLString: String);
begin
  //
end;

function TSEMethodList.GenerateFunctionInsertNames: TStringList;
begin
  //
end;

function TSEMethodList.GenerateFunctionItemList: TStringList;
begin
  //
end;

function TSEMethodList.GenerateParameterInsertList: TStringList;
begin
  //
end;

function TSEMethodList.GenerateParameterLookupList: TStringList;
begin
  //
end;

end.
