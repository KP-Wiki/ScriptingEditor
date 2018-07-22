unit SE_ACParams;

interface
uses
  Generics.Collections;

type
  TSEParamFlag = (pfVar, pfConst, pfOptional, pfNone);

  TSEParam = class
  strict private
  public
    ParamName,
    ParamType,
    DefaultValue: string;
    Flag:        TSEParamFlag;
    constructor Create;
  end;

  TSEParamList = class(TObjectList<TSEParam>)
  public
    function IndexByName(aParamName: string): Integer; virtual;
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
  ParamName    := 'NewParam';
  ParamType    := 'NewParamType';
  Flag         := pfNone;
  DefaultValue := '';
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

function TSEParamList.IndexByName(aParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
    if Items[I].ParamName = aParamName then
      Exit(I);
end;

end.
