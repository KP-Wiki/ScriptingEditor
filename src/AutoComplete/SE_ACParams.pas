unit SE_ACParams;

interface
uses
  Generics.Collections;

type
  TSEParamFlag = (pfVar, pfConst, pfOptional, pfNone);

const
  ParamFlagName: array[TSEParamFlag] of string = (
    'var', 'const', 'optional', 'none'
  );

type
  TSEParam = class(TObject)
  public
    ParamName,
    ParamType,
    DefaultValue: string;
    Flag:        TSEParamFlag;
    class function ParamFlagToStr(aFlags: TSEParamFlag): string;
    class function StrToParamFlag(aValue: string): TSEParamFlag;
    constructor Create;
  end;

  TSEParamList = class(TObjectList<TSEParam>)
  public
    function NewItem: TSEParam;
    function IndexByName(aParamName: string): Integer; virtual;
  end;

implementation
uses
  SysUtils;

{ TSEParam }
class function TSEParam.ParamFlagToStr(aFlags: TSEParamFlag): string;
begin
  case aFlags of
    pfVar:      Result := 'var';
    pfConst:    Result := 'const';
    pfOptional: Result := 'optional';
    else        Result := 'none';
  end;
end;

class function TSEParam.StrToParamFlag(aValue: string): TSEParamFlag;
var
  I: TSEParamFlag;
begin
  Result := pfNone;

  for I := Low(TSEParamFlag) to High(TSEParamFlag) do
    if ParamFlagName[I] = aValue then
      Exit(I);
end;

constructor TSEParam.Create;
begin
  inherited;
  ParamName    := 'NewParam';
  ParamType    := 'NewParamType';
  Flag         := pfNone;
  DefaultValue := '';
end;

{ TSEParamList }
function TSEParamList.NewItem: TSEParam;
begin
  Result := TSEParam.Create;
  Add(Result);
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
