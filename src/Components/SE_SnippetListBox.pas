unit SE_SnippetListBox;

interface
uses
  StdCtrls, Classes;

type
  TSESnippet = record
    DisplayText,
    SnippetText: string;
  end;
  TSESnippetArray = array of TSESnippet;

  TSESnippetListBox = class(TListBox)
  strict private
    fSnippets: TSESnippetArray;
    function GetSnippets: TSESnippetArray;
    procedure SetSnippets(aSnippetArray: TSESnippetArray);
    function SanitizeDisplayText(aDisplayText: string): string;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddSnippet(aSnippet: TSESnippet); overload;
    procedure AddSnippet(aDisplayText, aSnippetText: string); overload;
    function GetSnippet(aDisplayText: string): string; overload;
    function GetSnippet(aIndex: Integer): string; overload;
    procedure AppendSnippets(aSnippetArray: TSESnippetArray); overload;
    procedure AppendSnippets(var aDisplayTextDict: TStringList;
                             var aSnippetTextDict: TStringList;
                             indAppendBeginEnd: Boolean = False); overload;
  published
    property Snippets: TSESnippetArray read GetSnippets write SetSnippets;
  end;

implementation
uses
  SysUtils, Math;

destructor TSESnippetListBox.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSESnippetListBox.Clear;
begin
  SetLength(fSnippets, 0);
  inherited;
end;

procedure TSESnippetListBox.AddSnippet(aSnippet: TSESnippet);
var
  len: Integer;
begin
  aSnippet.DisplayText := SanitizeDisplayText(aSnippet.DisplayText);
  len                  := Length(fSnippets);
  SetLength(fSnippets, len + 1);
  fSnippets[len] := aSnippet;
  Items.Add(aSnippet.DisplayText);
end;

procedure TSESnippetListBox.AddSnippet(aDisplayText, aSnippetText: string);
var
  len:     Integer;
  snippet: TSESnippet;
begin
  snippet.DisplayText := SanitizeDisplayText(aDisplayText);
  snippet.SnippetText := aSnippetText;
  len                 := Length(fSnippets);
  SetLength(fSnippets, len + 1);
  fSnippets[len] := snippet;
  Items.Add(aDisplayText);
end;

function TSESnippetListBox.GetSnippet(aDisplayText: string): string;
var
  I: Integer;
begin
  Result := '';

  if Length(fSnippets) = 0 then
    Exit;

  for I := 0 to Length(fSnippets) - 1 do
    if fSnippets[I].DisplayText = aDisplayText then
    begin
      Result := fSnippets[I].SnippetText;
      Break;
    end;
end;

function TSESnippetListBox.GetSnippet(aIndex: Integer): string;
begin
  Result := '';

  if (aIndex < 0) or (Length(fSnippets) = 0) then
    Exit;

  if Items[aIndex] = fSnippets[aIndex].DisplayText then
    Result := fSnippets[aIndex].SnippetText
  else
    Result := GetSnippet(Items[aIndex]);
end;

procedure TSESnippetListBox.AppendSnippets(aSnippetArray: TSESnippetArray);
var
  item: TSESnippet;
begin
  for item in aSnippetArray do
    AddSnippet(item);
end;

procedure TSESnippetListBox.AppendSnippets(var aDisplayTextDict: TStringList;
                                           var aSnippetTextDict: TStringList;
                                           indAppendBeginEnd: Boolean = False);
const
  EOL              = #13#10;
  DEOL             = #13#10#13#10;
  BEGIN_END_CLAUSE = EOL + 'begin' + DEOL + 'end;'  + EOL;
var
  item: TSESnippet;
  I,
  len:  Integer;
begin
  len := Min(aDisplayTextDict.Count, aSnippetTextDict.Count);

  for I := 0 to len - 1 do
  begin
    item.DisplayText := aDisplayTextDict[I];
    item.SnippetText := aSnippetTextDict[I];

    if indAppendBeginEnd then
      item.SnippetText := item.SnippetText + BEGIN_END_CLAUSE;

    AddSnippet(item);
  end;
end;

procedure TSESnippetListBox.SetSnippets(aSnippetArray: TSESnippetArray);
begin
  Clear;
  AppendSnippets(aSnippetArray);
end;

function TSESnippetListBox.GetSnippets: TSESnippetArray;
begin
  Result := fSnippets;
end;

function TSESnippetListBox.SanitizeDisplayText(aDisplayText: string): string;
begin
  Result := StringReplace(aDisplayText, '\column{}',  '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,       '\style{+B}', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,       '\style{-B}', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,       'Utils.',     '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,       'States.',    '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result,       'Actions.',   '', [rfReplaceAll, rfIgnoreCase]);
end;

end.
