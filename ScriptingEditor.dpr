program ScriptingEditor;

uses
  Vcl.Forms,
  SE_MainForm in 'src\SE_MainForm.pas' {SEMainForm},
  SE_FindForm in 'src\SE_FindForm.pas' {SEFindForm},
  SE_GoToLineForm in 'src\SE_GoToLineForm.pas' {SEGoToLineForm},
  SE_ConfirmReplaceForm in 'src\SE_ConfirmReplaceForm.pas' {SEConfirmReplaceForm},
  SE_ReplaceForm in 'src\SE_ReplaceForm.pas' {SEReplaceForm},
  SE_SaveModifiedForm in 'src\SE_SaveModifiedForm.pas' {SESaveModifiedForm},
  SE_AboutForm in 'src\SE_AboutForm.pas' {SEAboutForm},
  ScriptValidatorResult in 'src\Common\ScriptValidatorResult.pas',
  VerySimpleXML in 'src\Common\VerySimpleXML.pas',
  SE_IssueListBox in 'src\Components\SE_IssueListBox.pas',
  SE_ValidationPlugin in 'src\Plugins\SE_ValidationPlugin.pas',
  SE_SnippetListBox in 'src\Components\SE_SnippetListBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSEMainForm, SEMainForm);
  Application.Run;
end.
