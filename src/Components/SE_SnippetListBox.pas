unit SE_SnippetListBox;

interface
uses
  Controls, StdCtrls, Classes, Types;

type
  PImageList = ^TImageList;

  TSESnippet = record
    DisplayText,
    SnippetText: string;
  end;
  TSESnippetArray = array of TSESnippet;

  TSESnippetListBox = class(TListBox)
  strict private
    fSnippets: TSESnippetArray;
    fImgList:  PImageList;
    procedure SetSnippets(aSnippetArray: TSESnippetArray);
    function SanitizeDisplayText(aDisplayText: string): string;
    procedure DoDrawItem(aControl: TWinControl; aIndex: Integer; aRect: TRect;
                         aState: TOwnerDrawState);
  public
    constructor Create(aOwner: TComponent; aImgList: PImageList); overload;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddSnippet(aSnippet: TSESnippet); overload;
    procedure AddSnippet(aDisplayText, aSnippetText: string); overload;
    function GetSnippet(aDisplayText: string): string; overload;
    function GetSnippet(aIndex: Integer): string; overload;
    procedure AppendSnippets(aSnippetArray: TSESnippetArray); overload;
    procedure AppendSnippets(const aDisplayTextDict: TStringList;
                             const aSnippetTextDict: TStringList); overload;
  published
    property Snippets: TSESnippetArray read fSnippets write SetSnippets;
  end;

implementation
uses
  SysUtils, Math;

constructor TSESnippetListBox.Create(aOwner: TComponent; aImgList: PImageList);
begin
  inherited Create(aOwner);
  fImgList   := aImgList;
  OnDrawItem := DoDrawItem;
  Style      := lbOwnerDrawFixed;
end;

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

procedure TSESnippetListBox.AppendSnippets(const aDisplayTextDict: TStringList;
                                           const aSnippetTextDict: TStringList);
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
    AddSnippet(item);
  end;
end;

procedure TSESnippetListBox.SetSnippets(aSnippetArray: TSESnippetArray);
begin
  Clear;
  AppendSnippets(aSnippetArray);
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

procedure TSESnippetListBox.DoDrawItem(aControl: TWinControl; aIndex: Integer;
                                       aRect: TRect; aState: TOwnerDrawState);
var
  s:          string;
  txtTopPos,
  txtLeftPos,
  txtHeight:  Integer;
const
  IMG_TXT_PAD    = 5;
  IMG_IDX_FUNC_A = 0;
  IMG_IDX_FUNC_B = 1;
  IMG_IDX_PROC_A = 2;
  IMG_IDX_PROC_B = 3;
begin
  s := Items[aIndex];
  Canvas.FillRect(aRect);

  if s.StartsWith('procedure') then
  begin
    s := StringReplace(s, 'procedure ', '', [rfReplaceAll, rfIgnoreCase]);
    fImgList.Draw(Canvas, aRect.Left, aRect.Top, IMG_IDX_PROC_B);
  end else if s.StartsWith('function') then
  begin
    s := StringReplace(s, 'function ', '', [rfReplaceAll, rfIgnoreCase]);
    fImgList.Draw(Canvas, aRect.Left, aRect.Top, IMG_IDX_FUNC_B);
  end;

  txtHeight  := Canvas.TextHeight(s);
  txtLeftPos := aRect.Left + fImgList.Width + IMG_TXT_PAD;
  txtTopPos  := aRect.Top + (aRect.Height div 2) - (txtHeight div 2);
  Canvas.TextOut(txtLeftPos, txtTopPos, s);
end;

end.
