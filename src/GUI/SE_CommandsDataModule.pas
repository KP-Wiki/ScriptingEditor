unit SE_CommandsDataModule;

interface
uses
  SysUtils, Classes, ImageList, ImgList, Controls,
  Actions, ActnList, Dialogs;

type
  TSECommandsDataModule = class(TDataModule)
    alMain: TActionList;
    ActNewFile: TAction;
    ActOpenFile: TAction;
    ActUndo: TAction;
    ActFind: TAction;
    ActValidate: TAction;
    ActKPDocWiki: TAction;
    ActSaveFile: TAction;
    ActSaveFileAs: TAction;
    ActOptions: TAction;
    ActExit: TAction;
    ActRedo: TAction;
    ActCut: TAction;
    ActCopy: TAction;
    ActPaste: TAction;
    ActDelete: TAction;
    ActSelectAll: TAction;
    ActFindNext: TAction;
    ActFindPrevious: TAction;
    ActReplace: TAction;
    ActGoToLine: TAction;
    ActAboutSE: TAction;
    ilActions16x16: TImageList;
    ilActions22x22: TImageList;
    dlgFileOpen: TOpenDialog;
    dlgFileSave: TSaveDialog;
    ActCloseFile: TAction;
    ActCloseAllFiles: TAction;
    ActShowWelcome: TAction;
    actIssueGoTo: TAction;
    actIssueCopy: TAction;
    ActKMRDocWiki: TAction;
    ActThemeLight: TAction;
    ActThemeClassic: TAction;
    ActThemeOcean: TAction;
    ActThemeVisualStudio: TAction;
    ActThemeTwilight: TAction;
    ActThemeDark: TAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure ActCutUpdate(Sender: TObject);
    procedure ActCopyUpdate(Sender: TObject);
    procedure ActPasteUpdate(Sender: TObject);
    procedure ActDeleteUpdate(Sender: TObject);
    procedure ActUndoUpdate(Sender: TObject);
    procedure ActRedoUpdate(Sender: TObject);
    procedure ActSelectAllUpdate(Sender: TObject);
    procedure ActGoToLineUpdate(Sender: TObject);
    procedure ActFindUpdate(Sender: TObject);
    procedure ActFindNextUpdate(Sender: TObject);
    procedure ActFindPreviousUpdate(Sender: TObject);
    procedure ActReplaceUpdate(Sender: TObject);
    procedure ActValidateUpdate(Sender: TObject);
    procedure ActNewFileUpdate(Sender: TObject);
    procedure ActOpenFileUpdate(Sender: TObject);
    procedure ActSaveFileUpdate(Sender: TObject);
    procedure ActSaveFileAsUpdate(Sender: TObject);
    procedure ActCloseFileUpdate(Sender: TObject);
    procedure ActCloseAllFilesUpdate(Sender: TObject);
    procedure ActShowWelcomeTabUpdate(Sender: TObject);

    procedure ActCutExecute(Sender: TObject);
    procedure ActCopyExecute(Sender: TObject);
    procedure ActPasteExecute(Sender: TObject);
    procedure ActDeleteExecute(Sender: TObject);
    procedure ActUndoExecute(Sender: TObject);
    procedure ActRedoExecute(Sender: TObject);
    procedure ActSelectAllExecute(Sender: TObject);
    procedure ActGoToLineExecute(Sender: TObject);
    procedure ActFindExecute(Sender: TObject);
    procedure ActFindNextExecute(Sender: TObject);
    procedure ActFindPreviousExecute(Sender: TObject);
    procedure ActReplaceExecute(Sender: TObject);
    procedure ActValidateExecute(Sender: TObject);
    procedure ActNewFileExecute(Sender: TObject);
    procedure ActOpenFileExecute(Sender: TObject);
    procedure ActSaveFileExecute(Sender: TObject);
    procedure ActSaveFileAsExecute(Sender: TObject);
    procedure ActCloseFileExecute(Sender: TObject);
    procedure ActCloseAllFilesExecute(Sender: TObject);
    procedure ActOptionsExecute(Sender: TObject);
    procedure ActExitExecute(Sender: TObject);
    procedure ActKMRDocWikiExecute(Sender: TObject);
    procedure ActKPDocWikiExecute(Sender: TObject);
    procedure ActAboutSEExecute(Sender: TObject);
    procedure ActShowWelcomeTabExecute(Sender: TObject);
    procedure ActIssueGoToExecute(Sender: TObject);
    procedure ActIssueCopyExecute(Sender: TObject);
    procedure ActThemeLightExecute(Sender: TObject);
    procedure ActThemeClassicExecute(Sender: TObject);
    procedure ActThemeOceanExecute(Sender: TObject);
    procedure ActThemeVisualStudioExecute(Sender: TObject);
    procedure ActThemeTwilightExecute(Sender: TObject);
    procedure ActThemeDarkExecute(Sender: TObject);
  private
    fMRIFiles:        TStringList;
    fUntitledNumbers: TBits;
  public
    procedure AddMRIEntry(aFileName: string);
    function GetMRIEntries: Integer;
    function GetMRIEntry(aIndex: Integer): string;
    procedure RemoveMRIEntry(aFileName: string);
    function GetSaveFileName(var aNewName: string): Boolean;
    function GetUntitledNumber: Integer;
    function GetWelcomePageIndex: Integer;
    procedure ReleaseUntitledNumber(aNumber: Integer);
  end;

var
  gCommandsDataModule: TSECommandsDataModule;

implementation
{$R *.dfm}

uses
  Windows, ShellAPI, Forms, UITypes, ClipBrd, ComCtrls,
  SE_Globals, SE_AboutForm, SE_WelcomeTab, SE_OptionsForm;

procedure TSECommandsDataModule.DataModuleCreate(Sender: TObject);
begin
  fMRIFiles              := TStringList.Create;
  dlgFileOpen.InitialDir := gExeDir;
  dlgFileSave.InitialDir := gExeDir;
end;

procedure TSECommandsDataModule.DataModuleDestroy(Sender: TObject);
begin
  fMRIFiles.Free;
  fUntitledNumbers.Free;
  gCommandsDataModule := nil;
end;

procedure TSECommandsDataModule.ActCutUpdate(Sender: TObject);
begin
  ActCut.Enabled := (gEditCmds <> nil) and gEditCmds.CanCut;
end;

procedure TSECommandsDataModule.ActCopyUpdate(Sender: TObject);
begin
  ActCopy.Enabled := (gEditCmds <> nil) and gEditCmds.CanCopy;
end;

procedure TSECommandsDataModule.ActPasteUpdate(Sender: TObject);
begin
  ActPaste.Enabled := (gEditCmds <> nil) and gEditCmds.CanPaste and Clipboard.HasFormat(CF_TEXT);
end;

procedure TSECommandsDataModule.ActDeleteUpdate(Sender: TObject);
begin
  ActDelete.Enabled := (gEditCmds <> nil) and gEditCmds.CanDelete;
end;

procedure TSECommandsDataModule.ActUndoUpdate(Sender: TObject);
begin
  ActUndo.Enabled := (gEditCmds <> nil) and gEditCmds.CanUndo;
end;

procedure TSECommandsDataModule.ActRedoUpdate(Sender: TObject);
begin
  ActRedo.Enabled := (gEditCmds <> nil) and gEditCmds.CanRedo;
end;

procedure TSECommandsDataModule.ActSelectAllUpdate(Sender: TObject);
begin
  ActSelectAll.Enabled := (gEditCmds <> nil) and gEditCmds.CanSelectAll;
end;

procedure TSECommandsDataModule.ActGoToLineUpdate(Sender: TObject);
begin
  ActGoToLine.Enabled := (gSearchCmds <> nil) and gSearchCmds.CanGoTo;
end;

procedure TSECommandsDataModule.ActFindUpdate(Sender: TObject);
begin
  ActFind.Enabled := (gSearchCmds <> nil) and gSearchCmds.CanFind;
end;

procedure TSECommandsDataModule.ActFindNextUpdate(Sender: TObject);
begin
  ActFindNext.Enabled := (gSearchCmds <> nil) and gSearchCmds.CanFindNext;
end;

procedure TSECommandsDataModule.ActFindPreviousUpdate(Sender: TObject);
begin
  ActFindPrevious.Enabled := (gSearchCmds <> nil) and gSearchCmds.CanFindPrev;
end;

procedure TSECommandsDataModule.ActReplaceUpdate(Sender: TObject);
begin
  ActReplace.Enabled := (gSearchCmds <> nil) and gSearchCmds.CanFind;
end;

procedure TSECommandsDataModule.ActValidateUpdate(Sender: TObject);
begin
  ActValidate.Enabled := (gEditCmds <> nil) and gEditCmds.CanValidate;
end;

procedure TSECommandsDataModule.ActNewFileUpdate(Sender: TObject);
begin
  ActNewFile.Enabled := gEditorFactory <> nil;
end;

procedure TSECommandsDataModule.ActOpenFileUpdate(Sender: TObject);
begin
  ActOpenFile.Enabled := gEditorFactory <> nil;
end;

procedure TSECommandsDataModule.ActSaveFileUpdate(Sender: TObject);
begin
  ActSaveFile.Enabled := (gFileCmds <> nil) and gFileCmds.CanSave;
end;

procedure TSECommandsDataModule.ActSaveFileAsUpdate(Sender: TObject);
begin
  ActSaveFileAs.Enabled := (gFileCmds <> nil) and gFileCmds.CanSaveAs;
end;

procedure TSECommandsDataModule.ActCloseFileUpdate(Sender: TObject);
begin
  ActCloseFile.Enabled := //(gFileCmds <> nil) and gFileCmds.CanClose;
                          gMainForm.pcEditors.PageCount >= 1;
end;

procedure TSECommandsDataModule.ActCloseAllFilesUpdate(Sender: TObject);
begin
  ActCloseAllFiles.Enabled := //(gEditCmds <> nil) and gEditCmds.CanValidate;
                              gMainForm.pcEditors.PageCount >= 2;
end;

procedure TSECommandsDataModule.ActShowWelcomeTabUpdate(Sender: TObject);
begin
  ActShowWelcome.Enabled := GetWelcomePageIndex = -1;
end;

procedure TSECommandsDataModule.ActThemeLightExecute(Sender: TObject);
begin
  gOptions.Theme               := tkLight;
  ActThemeLight.Checked        := True;
  ActThemeClassic.Checked      := False;
  ActThemeOcean.Checked        := False;
  ActThemeVisualStudio.Checked := False;
  ActThemeTwilight.Checked     := False;
  ActThemeDark.Checked         := False;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActThemeClassicExecute(Sender: TObject);
begin
  gOptions.Theme               := tkClassic;
  ActThemeLight.Checked        := False;
  ActThemeClassic.Checked      := True;
  ActThemeOcean.Checked        := False;
  ActThemeVisualStudio.Checked := False;
  ActThemeTwilight.Checked     := False;
  ActThemeDark.Checked         := False;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActThemeOceanExecute(Sender: TObject);
begin
  gOptions.Theme               := tkOcean;
  ActThemeLight.Checked        := False;
  ActThemeClassic.Checked      := False;
  ActThemeOcean.Checked        := True;
  ActThemeVisualStudio.Checked := False;
  ActThemeTwilight.Checked     := False;
  ActThemeDark.Checked         := False;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActThemeVisualStudioExecute(Sender: TObject);
begin
  gOptions.Theme               := tkVisualStudio;
  ActThemeLight.Checked        := False;
  ActThemeClassic.Checked      := False;
  ActThemeOcean.Checked        := False;
  ActThemeVisualStudio.Checked := True;
  ActThemeTwilight.Checked     := False;
  ActThemeDark.Checked         := False;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActThemeTwilightExecute(Sender: TObject);
begin
  gOptions.Theme               := tkTwilight;
  ActThemeLight.Checked        := False;
  ActThemeClassic.Checked      := False;
  ActThemeOcean.Checked        := False;
  ActThemeVisualStudio.Checked := False;
  ActThemeTwilight.Checked     := True;
  ActThemeDark.Checked         := False;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActThemeDarkExecute(Sender: TObject);
begin
  gOptions.Theme               := tkDark;
  ActThemeLight.Checked        := False;
  ActThemeClassic.Checked      := False;
  ActThemeOcean.Checked        := False;
  ActThemeVisualStudio.Checked := False;
  ActThemeTwilight.Checked     := False;
  ActThemeDark.Checked         := True;

  if gEditorFactory <> nil then
    gEditorFactory.ReloadTheme;
end;

procedure TSECommandsDataModule.ActCutExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecCut;
end;

procedure TSECommandsDataModule.ActCopyExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecCopy;
end;

procedure TSECommandsDataModule.ActPasteExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecPaste;
end;

procedure TSECommandsDataModule.ActDeleteExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecDelete;
end;

procedure TSECommandsDataModule.ActUndoExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecUndo;
end;

procedure TSECommandsDataModule.ActRedoExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecRedo;
end;

procedure TSECommandsDataModule.ActSelectAllExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecSelectAll;
end;

procedure TSECommandsDataModule.ActGoToLineExecute(Sender: TObject);
begin
  if gSearchCmds <> nil then
    gSearchCmds.ExecGoTo;
end;

procedure TSECommandsDataModule.ActFindExecute(Sender: TObject);
begin
  if gSearchCmds <> nil then
    gSearchCmds.ExecFind;
end;

procedure TSECommandsDataModule.ActFindNextExecute(Sender: TObject);
begin
  if gSearchCmds <> nil then
    gSearchCmds.ExecFindNext;
end;

procedure TSECommandsDataModule.ActFindPreviousExecute(Sender: TObject);
begin
  if gSearchCmds <> nil then
    gSearchCmds.ExecFindPrev;
end;

procedure TSECommandsDataModule.ActReplaceExecute(Sender: TObject);
begin
  if gSearchCmds <> nil then
    gSearchCmds.ExecReplace;
end;

procedure TSECommandsDataModule.ActValidateExecute(Sender: TObject);
begin
  if gEditCmds <> nil then
    gEditCmds.ExecValidate;
end;

procedure TSECommandsDataModule.ActNewFileExecute(Sender: TObject);
begin
  if gEditorFactory <> nil then
    gEditorFactory.ExecNewFile;
end;

procedure TSECommandsDataModule.ActOpenFileExecute(Sender: TObject);
begin
  if gEditorFactory <> nil then
    gEditorFactory.ExecOpenFile;
end;

procedure TSECommandsDataModule.ActSaveFileExecute(Sender: TObject);
begin
  if gFileCmds <> nil then
    gFileCmds.ExecSave;
end;

procedure TSECommandsDataModule.ActSaveFileAsExecute(Sender: TObject);
begin
  if gFileCmds <> nil then
    gFileCmds.ExecSaveAs;
end;

procedure TSECommandsDataModule.ActCloseFileExecute(Sender: TObject);
begin
  CloseTab(gMainForm.pcEditors.ActivePageIndex);
end;

procedure TSECommandsDataModule.ActCloseAllFilesExecute(Sender: TObject);
begin
  if gEditorFactory <> nil then
    gEditorFactory.CloseAll;
end;

procedure TSECommandsDataModule.ActOptionsExecute(Sender: TObject);
var
  dlg: TSEOptionsForm;
begin
  dlg := TSEOptionsForm.Create(gMainForm);

  with dlg do
    try
      if ShowModal = mrOK then
      begin
        dlg.ApplySettings;

        if gActiveEditor <> nil then
          gActiveEditor.ReloadSettings;
      end;
    finally
      FreeAndNil(dlg);
    end;
end;

procedure TSECommandsDataModule.ActExitExecute(Sender: TObject);
begin
  gMainForm.Close;
  Application.Terminate;
end;

procedure TSECommandsDataModule.ActKMRDocWikiExecute(Sender: TObject);
const
  DOC_WIKI_URL = 'https://github.com/reyandme/kam_remake/wiki';
begin
  if ShellExecute(Application.Handle, 'open', PChar(DOC_WIKI_URL), nil, nil, SW_SHOWNORMAL) <= 32 then
    MessageDlg('Unable to open the documentation wiki.', mtError, [mbOK], 0);
end;

procedure TSECommandsDataModule.ActKPDocWikiExecute(Sender: TObject);
const
  DOC_WIKI_URL = 'https://github.com/Kromster80/knights_province/wiki';
begin
  if ShellExecute(Application.Handle, 'open', PChar(DOC_WIKI_URL), nil, nil, SW_SHOWNORMAL) <= 32 then
    MessageDlg('Unable to open the documentation wiki.', mtError, [mbOK], 0);
end;

procedure TSECommandsDataModule.ActAboutSEExecute(Sender: TObject);
var
  dlg: TSEAboutForm;
begin
  dlg := TSEAboutForm.Create(Self);
  dlg.ShowModal;
  FreeAndNil(dlg);
end;

procedure TSECommandsDataModule.ActShowWelcomeTabExecute(Sender: TObject);
var
  Sheet:      TTabSheet;
  NewForm:    TSEWelcomeTab;
  ManualShow: Boolean;
begin
  ManualShow := gMainForm.pcEditors.PageCount = 0;
  Sheet      := TSEWelcomeTabSheet.Create(gMainForm.pcEditors);

  try
    Sheet.Caption     := 'Welcome Page';
    Sheet.PageControl := gMainForm.pcEditors;
    NewForm           := TSEWelcomeTab.Create(Sheet);
    NewForm.Parent    := Sheet;
    NewForm.Visible   := True;
    Sheet.OnShow      := NewForm.ParentTabShow;
    gMainForm.pcEditors.ActivePage := Sheet;

    NewForm.SetFocus;
    NewForm.Realign;

    if ManualShow then
      NewForm.ParentTabShow(Self);

    gMainForm.SetListboxesVisible(False);
  except
    Sheet.Free;
  end;
end;

procedure TSECommandsDataModule.ActIssueGoToExecute(Sender: TObject);
var
  cursorPos: TPoint;
  index:     Integer;
begin
  cursorPos := gMainForm.LbIssues.ScreenToClient(gMainForm.pmIssues.PopupPoint);
  index     := gMainForm.LbIssues.ItemAtPos(cursorPos, True);
  gMainForm.GoToIssue(index);
end;

procedure TSECommandsDataModule.ActIssueCopyExecute(Sender: TObject);
var
  cursorPos: TPoint;
  index:     Integer;
begin
  cursorPos := gMainForm.LbIssues.ScreenToClient(gMainForm.pmIssues.PopupPoint);
  index     := gMainForm.LbIssues.ItemAtPos(cursorPos, True);
  gMainForm.CopyIssue(index);
end;

procedure TSECommandsDataModule.AddMRIEntry(aFileName: string);
begin
  if aFileName <> '' then
  begin
    RemoveMRIEntry(aFileName);
    fMRIFiles.Insert(0, aFileName);

    while fMRIFiles.Count > MAX_MRI_FILES do
      fMRIFiles.Delete(fMRIFiles.Count - 1);
  end;
end;

function TSECommandsDataModule.GetMRIEntries: Integer;
begin
  Result := fMRIFiles.Count;
end;

function TSECommandsDataModule.GetMRIEntry(aIndex: Integer): string;
begin
  if (aIndex >= 0) and (aIndex < fMRIFiles.Count) then
    Result := fMRIFiles[aIndex]
  else
    Result := '';
end;

procedure TSECommandsDataModule.RemoveMRIEntry(aFileName: string);
var
  I: Integer;
begin
  for I := fMRIFiles.Count - 1 downto 0 do
  begin
    if CompareText(aFileName, fMRIFiles[I]) = 0 then
      fMRIFiles.Delete(I);
  end;
end;

function TSECommandsDataModule.GetSaveFileName(var aNewName: string): Boolean;
begin
  with dlgFileSave do
  begin
    if aNewName <> '' then
    begin
      InitialDir := ExtractFileDir(aNewName);
      FileName   := ExtractFileName(aNewName);
    end else
    begin
      InitialDir := '';
      FileName   := '';
    end;

    if Execute then
    begin
      aNewName := FileName;
      Result   := True;
    end else
      Result := False;
  end;
end;

function TSECommandsDataModule.GetUntitledNumber: Integer;
begin
  if fUntitledNumbers = nil then
    fUntitledNumbers := TBits.Create;

  Result := fUntitledNumbers.OpenBit;

  if Result = fUntitledNumbers.Size then
    fUntitledNumbers.Size := fUntitledNumbers.Size + 32;

  fUntitledNumbers[Result] := True;
  Inc(Result);
end;

function TSECommandsDataModule.GetWelcomePageIndex: Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to gMainForm.pcEditors.PageCount - 1 do
    if gMainForm.pcEditors.Pages[I] is TSEWelcomeTabSheet then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TSECommandsDataModule.ReleaseUntitledNumber(aNumber: Integer);
begin
  Dec(aNumber);

  if (fUntitledNumbers <> nil) and (aNumber >= 0) and
     (aNumber < fUntitledNumbers.Size) then
    fUntitledNumbers[aNumber] := False;
end;

end.
