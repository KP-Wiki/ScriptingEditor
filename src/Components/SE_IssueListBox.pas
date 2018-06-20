unit SE_IssueListBox;

interface
uses
  StdCtrls, ScriptValidatorResult;

type
  TSEIssueListBox = class(TListBox)
  strict private
    fIssues: TScriptValidatorIssueArray;
    function GetIssues: TScriptValidatorIssueArray;
    procedure SetIssues(aIssueArray: TScriptValidatorIssueArray);
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddIssue(aIssue: TScriptValidatorIssue);
    function GetIssue(aText: string): TScriptValidatorIssue; overload;
    function GetIssue(aIndex: Integer): TScriptValidatorIssue; overload;
    procedure AppendIssues(aIssueArray: TScriptValidatorIssueArray);
  published
    property Issues: TScriptValidatorIssueArray read GetIssues write SetIssues;
  end;

implementation

destructor TSEIssueListBox.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSEIssueListBox.Clear;
begin
  SetLength(fIssues, 0);
  inherited;
end;

procedure TSEIssueListBox.AddIssue(aIssue: TScriptValidatorIssue);
var
  len: Integer;
begin
  len := Length(fIssues);
  SetLength(fIssues, len + 1);
  fIssues[len] := aIssue;
  Items.Add(aIssue.Msg);
end;

function TSEIssueListBox.GetIssue(aText: string): TScriptValidatorIssue;
var
  I: Integer;
begin
  Result.Line := -2;

  if Length(fIssues) = 0 then
    Exit;

  for I := 0 to Length(fIssues) - 1 do
    if fIssues[I].Msg = aText then
    begin
      Result := fIssues[I];
      Break;
    end;
end;

function TSEIssueListBox.GetIssue(aIndex: Integer): TScriptValidatorIssue;
begin
  Result.Line := -2;

  if (Length(fIssues) = 0) or (aIndex < 0) then
    Exit;

  if Items[aIndex] = fIssues[aIndex].Msg then
    Result := fIssues[aIndex]
  else
    Result := GetIssue(Items[aIndex]);
end;

procedure TSEIssueListBox.AppendIssues(aIssueArray: TScriptValidatorIssueArray);
var
  item: TScriptValidatorIssue;
begin
  for item in aIssueArray do
    AddIssue(item);
end;

procedure TSEIssueListBox.SetIssues(aIssueArray: TScriptValidatorIssueArray);
begin
  Clear;
  AppendIssues(aIssueArray);
end;

function TSEIssueListBox.GetIssues: TScriptValidatorIssueArray;
begin
  Result := fIssues;
end;

end.
