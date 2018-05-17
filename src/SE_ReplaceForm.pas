unit SE_ReplaceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, SE_FindForm;

type
  TSEReplaceForm = class(TSEFindForm)
    Label2: TLabel;
    cbReplaceText: TComboBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
  public
    property ReplaceText:        string read GetReplaceText        write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory write SetReplaceTextHistory;
  end;

implementation

{$R *.dfm}

procedure TSEReplaceForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

function TSEReplaceForm.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

function TSEReplaceForm.GetReplaceTextHistory: string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to cbReplaceText.Items.Count - 1 do begin
    if I >= 10 then
      Break;

    if I > 0 then
      Result := Result + #13#10;

    Result := Result + cbReplaceText.Items[I];
  end;
end;

procedure TSEReplaceForm.SetReplaceText(Value: string);
begin
  cbReplaceText.Text := Value;
end;

procedure TSEReplaceForm.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

end.
