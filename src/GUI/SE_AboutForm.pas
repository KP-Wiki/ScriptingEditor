unit SE_AboutForm;

interface

uses
  Windows, ShellAPI, UITypes, Classes, Controls, Dialogs, Forms, StdCtrls,
  ExtCtrls;

type
  TSEAboutForm = class(TForm)
    Label1: TLabel;
    Label7: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    stSynEdit: TStaticText;
    stVerySimpleXML: TStaticText;
    procedure StaticTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

implementation
{$R *.dfm}
uses
  SE_Globals;

procedure TSEAboutForm.FormCreate(Sender: TObject);
begin
  StaticText2.Caption := 'Dynamic Script editor for KaM Remake and Knights Province' +
                         sLineBreak + sLineBreak + 'Version: ' + FULL_VERSION;
end;

procedure TSEAboutForm.StaticTextClick(Sender: TObject);
begin
  if Sender = stSynEdit then
  begin
    if ShellExecute(Application.Handle, 'open', PChar(SYNEDIT_URL), nil, nil, SW_SHOWNORMAL) <= 32 then
      MessageDlg('Unable to open the SynEdit URL.', mtError, [mbOK], 0);
  end else if Sender = stVerySimpleXML then
  begin
    if ShellExecute(Application.Handle, 'open', PChar(VSXML_URL), nil, nil, SW_SHOWNORMAL) <= 32 then
      MessageDlg('Unable to open the VSXML URL.', mtError, [mbOK], 0);
  end;
end;

end.
