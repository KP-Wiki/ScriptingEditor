unit SE_ACParams;
{$M+}

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
    pfOptional: Result := 'optional';
    else        Result := 'none';
  end;
end;

class function TSEParamList.StrToParamFlag(aValue: string): TSEParamFlag;
var
  I: TSEParamFlag;
begin
  Result := pfNone;

  for I := Low(TSEParamFlag) to High(TSEParamFlag) do
    if ParamFlagName[I] = aValue then
      Exit(I);
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
