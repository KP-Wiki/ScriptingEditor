program ScriptingEditor;

uses
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  VerySimpleXML in 'src\Common\VerySimpleXML.pas',
  ScriptValidatorResult in 'src\Common\ScriptValidatorResult.pas',
  SE_MainForm in 'src\GUI\SE_MainForm.pas' {SEMainForm},
  SE_EditorForm in 'src\GUI\SE_EditorForm.pas' {SEEditorForm},
  SE_GoToLineForm in 'src\GUI\SE_GoToLineForm.pas' {SEGoToLineForm},
  SE_FindForm in 'src\GUI\SE_FindForm.pas' {SEFindForm},
  SE_ReplaceForm in 'src\GUI\SE_ReplaceForm.pas' {SEReplaceForm},
  SE_ConfirmReplaceForm in 'src\GUI\SE_ConfirmReplaceForm.pas' {SEConfirmReplaceForm},
  SE_SaveModifiedForm in 'src\GUI\SE_SaveModifiedForm.pas' {SESaveModifiedForm},
  SE_OptionsForm in 'src\GUI\SE_OptionsForm.pas' {SEOptionsForm},
  SE_AboutForm in 'src\GUI\SE_AboutForm.pas' {SEAboutForm},
  SE_WelcomeTab in 'src\GUI\SE_WelcomeTab.pas' {SEWelcomeTab},
  SE_CommandsDataModule in 'src\GUI\SE_CommandsDataModule.pas' {SECommandsDataModule: TDataModule},
  SE_IssueListBox in 'src\Components\SE_IssueListBox.pas',
  SE_SnippetListBox in 'src\Components\SE_SnippetListBox.pas',
  SE_ValidationPlugin in 'src\Plugins\SE_ValidationPlugin.pas',
  SE_Globals in 'src\SE_Globals.pas',
  SE_Interfaces in 'src\SE_Interfaces.pas',
  SE_EditorFactory in 'src\SE_EditorFactory.pas',
  SE_Exceptions in 'src\SE_Exceptions.pas',
  SE_Log in 'src\SE_Log.pas',
  SE_ACMethods in 'src\AutoComplete\SE_ACMethods.pas',
  SE_ACParams in 'src\AutoComplete\SE_ACParams.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Slate Classico');
  Application.CreateForm(TSEMainForm, gMainForm);
  Application.Run;
end.
