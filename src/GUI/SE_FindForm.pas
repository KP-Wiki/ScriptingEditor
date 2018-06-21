unit SE_FindForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TSEFindForm = class(TForm)
    Label1: TLabel;
    cbFindText: TComboBox;
    btnCancel: TButton;
    btnFind: TButton;
    GroupBox1: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWords: TCheckBox;
    cbSelectedOnly: TCheckBox;
    cbRegEx: TCheckBox;
    cbBackwards: TCheckBox;
    cbFromCursor: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetBackwards: Boolean;
    function GetCaseSensitive: Boolean;
    function GetFromCursor: Boolean;
    function GetInSelection: Boolean;
    function GetText: string;
    function GetTextHistory: string;
    function GetWholeWords: Boolean;
    function GetRegEx: Boolean;
    procedure SetBackwards(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetFromCursor(const Value: Boolean);
    procedure SetInSelection(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetTextHistory(const Value: string);
    procedure SetWholeWords(const Value: Boolean);
    procedure SetRegEx(const Value: Boolean);
  public
    property Backwards:         Boolean read GetBackwards     write SetBackwards;
    property CaseSensitive:     Boolean read GetCaseSensitive write SetCaseSensitive;
    property FromCursor:        Boolean read GetFromCursor    write SetFromCursor;
    property InSelection:       Boolean read GetInSelection   write SetInSelection;
    property SearchText:        string  read GetText          write SetText;
    property SearchTextHistory: string  read GetTextHistory   write SetTextHistory;
    property WholeWords:        Boolean read GetWholeWords    write SetWholeWords;
    property RegEx:             Boolean read GetRegEx         write SetRegEx;
  end;

implementation

{$R *.dfm}

procedure TSEFindForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  str: string;
  I:   Integer;
begin
  if ModalResult = mrOK then
  begin
    str := cbFindText.Text;

    if str <> '' then
    begin
      I := cbFindText.Items.IndexOf(str);

      if I > -1 then
      begin
        cbFindText.Items.Delete(I);
        cbFindText.Items.Insert(0, str);
        cbFindText.Text := str;
      end else
        cbFindText.Items.Insert(0, str);
    end;
  end;
end;

function TSEFindForm.GetBackwards: Boolean;
begin
  Result := cbBackwards.Checked;
end;

function TSEFindForm.GetCaseSensitive: Boolean;
begin
  Result := cbCaseSensitive.Checked;
end;

function TSEFindForm.GetFromCursor: Boolean;
begin
  Result := cbFromCursor.Checked;
end;

function TSEFindForm.GetInSelection: Boolean;
begin
  Result := cbSelectedOnly.Checked;
end;

function TSEFindForm.GetText: string;
begin
  Result := cbFindText.Text;
end;

function TSEFindForm.GetTextHistory: string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to cbFindText.Items.Count - 1 do begin
    if I >= 10 then
      break;

    if I > 0 then
      Result := Result + #13#10;

    Result := Result + cbFindText.Items[I];
  end;
end;

function TSEFindForm.GetWholeWords: Boolean;
begin
  Result := cbWholeWords.Checked;
end;

function TSEFindForm.GetRegEx: Boolean;
begin
  Result := cbRegEx.Checked;
end;

procedure TSEFindForm.SetBackwards(const Value: Boolean);
begin
  cbBackwards.Checked := Value;
end;

procedure TSEFindForm.SetCaseSensitive(const Value: Boolean);
begin
  cbCaseSensitive.Checked := Value;
end;

procedure TSEFindForm.SetFromCursor(const Value: Boolean);
begin
  cbFromCursor.Checked := Value;
end;

procedure TSEFindForm.SetInSelection(const Value: Boolean);
begin
  cbSelectedOnly.Checked := Value;
end;

procedure TSEFindForm.SetText(const Value: string);
begin
  cbfindText.Text := Value;
end;

procedure TSEFindForm.SetTextHistory(const Value: string);
begin
  cbfindText.Items.Text := Value;
end;

procedure TSEFindForm.SetWholeWords(const Value: Boolean);
begin
  cbWholeWords.Checked := Value;
end;

procedure TSEFindForm.SetRegEx(const Value: Boolean);
begin
  cbRegEx.Checked := Value;
end;

end.
