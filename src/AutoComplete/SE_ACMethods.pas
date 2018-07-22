unit SE_ACMethods;

interface
uses
  Contnrs, Classes,
  VerySimpleXML,
  SE_ACParams;

type
  TSEMethodType = (ftFunction, ftProcedure);

  TSEMethod = class
  strict private
    fMethodType:     TSEMethodType;
    fParams:         TSEParamList;
    fExternalMethod: Boolean;
    fFuncName,
    fResultType,
    fPluginName:     string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveAsXML(aParent: TXmlNode);
    procedure LoadFromXML(aParent: TXmlNode);
    property MethodType:     TSEMethodType read fMethodType     write fMethodType;
    property Params:         TSEParamList  read fParams         write fParams;
    property ExternalMethod: Boolean       read fExternalMethod write fExternalMethod;
    property MethodName:     string        read fFuncName       write fFuncName;
    property ResultType:     string        read fResultType     write fResultType;
    property PluginName:     string        read fPluginName     write fPluginName;
  published
    class function MethodTypeToStr(aType: TSEMethodType): string;
    class function StrToMethodType(aValue: string): TSEMethodType;
    class function ParseMethodStr(aValue: string): TSEMethod;
  end;

  TSEMethodList = class(TObjectList)
  protected
    function GetItem(aIndex: Integer): TSEMethod; virtual;
    procedure SetItem(aIndex: Integer; aItem: TSEMethod); virtual;
  public
    function NewItem: TSEMethod;
    function Add(aItem: TSEMethod): Integer; virtual;
    function Remove(aItem: TSEMethod): Integer; virtual;
    function IndexOf(aItem: TSEMethod): Integer; virtual;
    procedure Insert(aIndex: Integer; aItem: TSEMethod); virtual;
    function First: TSEMethod; virtual;
    function Last: TSEMethod; virtual;
    function IndexByName(aFuncName: string): Integer; virtual;
    procedure SaveAsXML(aParent: TXmlNode);
    procedure LoadFromXML(aParent: TXmlNode);
    procedure LoadFromString(aXMLString: String);
    function GenerateFunctionInsertNames: TStringList;
    function GenerateFunctionItemList: TStringList;
    function GenerateParameterInsertList: TStringList;
    function GenerateParameterLookupList: TStringList;
    property Items[aIndex: Integer]: TSEMethod Read GetItem Write SetItem; default;
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
begin
  Result := ftProcedure;

  if LowerCase(aValue) = 'function' then
    Result := ftFunction;
  if LowerCase(aValue) = 'procedure' then
    Result := ftProcedure;
end;

class function TSEMethod.ParseMethodStr(aValue: string): TSEMethod;
var
  method:       TSEMethod;
  param:        TSEParam;
  s,
  paramType,
  defaultValue,
  params:       string;
  paramList,
  parseList:    TStringList;
  I,
  splitPos:     Integer;
  paramFlag:    TSEParamFlag;
begin
  s         := aValue;
  paramList := TStringList.Create;
  parseList := TStringList.Create;
  method    := TSEMethod.Create;

  method.MethodType := StrToMethodType(Copy(s, 1, Pos(' ', s) - 1));
  Delete(s, 1, Pos(' ', s));

  method.MethodName := Copy(s, 1, Pos('(', s) - 1);
  Delete(s, 1, Pos('(', s));

  params := Copy(s, 1, Pos(')', s) - 1);
  Delete(s, 1, Pos(')', s));

  if method.MethodType = ftFunction then
  begin
    Delete(s, 1, 1); // Delete ':'
    method.ResultType := Trim(Copy(s, 1, Pos(';', s) - 1));
  end;

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
    paramFlag := pfNone;
    s         := Trim(paramList[I]);
    splitPos  := Pos('=', s);

    if splitPos > 0 then
    begin
      defaultValue := Trim(Copy(s, splitPos + 1, Length(s) - (splitPos + 1)));
      paramFlag    := pfOptional;
      Delete(s, splitPos, Length(s) - splitPos);
    end;

    splitPos  := Pos(':', s);
    paramType := Trim(Copy(s, splitPos + 1, Length(s) - (splitPos + 1)));
    Delete(s, splitPos, Length(s) - splitPos);

    if LowerCase(s).StartsWith('var ', True) then
    begin
      paramFlag := pfVar;
      Delete(s, 1, 4);
    end else if LowerCase(s).StartsWith('const ', True) then
    begin
      paramFlag := pfConst;
      Delete(s, 1, 6);
    end;

    while s <> '' do
    begin // Paramter einzeln parsen
      s := Trim(s);

      if Pos(' ', s) <> 0 then
      begin
        parseList.Add(Trim(Copy(s, 1, Pos(' ', s))));
        Delete(s, 1, Pos(' ', s));
      end else
      begin
        ParseList.Add(s);
        s := '';
      end;
    end;
  end;
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

end.
