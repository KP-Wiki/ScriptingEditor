unit SE_AboutForm;

interface

uses
  Windows, ShellAPI, UITypes, Classes,
  Controls, Dialogs, Forms, StdCtrls, ExtCtrls;

type
  TSEAboutForm = class(TForm)
    Label1: TLabel;
    Label7: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    stSynEdit: TStaticText;
    stVerySimpleXML: TStaticText;
    procedure StaticTextClick(Sender: TObject);
  end;

const
  SYNEDIT_URL = 'https://github.com/SynEdit/SynEdit';
  VSXML_URL   = 'http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/';

implementation

{$R *.dfm}

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
