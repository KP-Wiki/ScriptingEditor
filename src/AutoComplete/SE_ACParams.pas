unit SE_ACParams;

interface
uses
  Contnrs;

type
  TSEParamFlag = (pfVar, pfConst, pfOptional, pfNone);

  TSEParam = class
  strict private
    fParamName,
    fParamType,
    fDefaultValue: string;
    fFlag:        TSEParamFlag;
  public
    constructor Create;
    property ParamName:    string       read fParamName    write fParamName;
    property ParamType:    string       read fParamType    write fParamType;
    property DefaultValue: string       read fDefaultValue write fDefaultValue;
    property Flag:         TSEParamFlag read fFlag         write fFlag;
  end;

  TSEParamList = class(TObjectList)
  protected
    function GetItem(aIndex: Integer): TSEParam; virtual;
    procedure SetItem(aIndex: Integer; aItem: TSEParam); virtual;
  public
    function NewItem: TSEParam;
    function Add(aItem: TSEParam): Integer; virtual;
    function Remove(aItem: TSEParam): Integer; virtual;
    function IndexOf(aItem: TSEParam): Integer; virtual;
    procedure Insert(aIndex: Integer; aItem: TSEParam); virtual;
    function First: TSEParam; virtual;
    function Last:  TSEParam; virtual;
    function IndexByName(aParamName: string): Integer; virtual;
    property Items[aIndex: Integer]: TSEParam Read GetItem Write SetItem; default;
  published
    class function ParamFlagToStr(aFlags: TSEParamFlag): string;
    class function StrToParamFlag(aValue: string): TSEParamFlag;
  end;

implementation
uses
  SysUtils;

{ TSEParam }
constructor TSEParam.Create;
begin
  inherited;
  fParamName    := 'NewParam';
  fParamType    := 'NewParamType';
  fFlag         := pfNone;
  fDefaultValue := '';
end;

{ TSEParamList }
class function TSEParamList.ParamFlagToStr(aFlags: TSEParamFlag): string;
begin
  case aFlags of
    pfVar:      Result := 'var';
    pfConst:    Result := 'const';
    pfOptional: Result := 'Optional';
    else        Result := 'None';
  end;
end;

class function TSEParamList.StrToParamFlag(aValue: string): TSEParamFlag;
begin
  Result := pfNone;

  if LowerCase(aValue) = 'var' then
    Result := pfVar;
  if LowerCase(aValue) = 'const' then
    Result := pfConst;
  if LowerCase(aValue) = 'Optional' then
    Result := pfOptional;
end;

function TSEParamList.GetItem(aIndex: Integer): TSEParam;
begin
  Result := TSEParam(inherited Items[aIndex]);
end;

procedure TSEParamList.SetItem(aIndex: Integer; aItem: TSEParam);
begin
  inherited Items[aIndex] := aItem;
end;

function TSEParamList.NewItem: TSEParam;
begin
  Result := TSEParam.Create;
  Add(Result);
end;

function TSEParamList.Add(aItem: TSEParam): Integer;
begin
  Result := inherited Add(aItem);
end;

function TSEParamList.Remove(aItem: TSEParam): Integer;
begin
  Result := inherited Remove(aItem);
end;

function TSEParamList.IndexOf(aItem: TSEParam): Integer;
begin
  Result := inherited IndexOf(aItem);
end;

procedure TSEParamList.Insert(aIndex: Integer; aItem: TSEParam);
begin
  inherited Insert(aIndex, aItem);
end;

function TSEParamList.First: TSEParam;
begin
  Result := TSEParam(inherited  First);
end;

function TSEParamList.Last: TSEParam;
begin
  Result := TSEParam(inherited Last);
end;

function TSEParamList.IndexByName(aParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
    if GetItem(I).ParamName = aParamName then
    begin
      Result := I;
      Exit;
    end;
end;

end.
