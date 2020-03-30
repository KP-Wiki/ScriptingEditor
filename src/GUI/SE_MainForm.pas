unit SE_MainForm;

interface

uses
  Windows, Messages, Classes, ToolWin, ExtCtrls, Controls, Forms, ComCtrls,
  Menus, StdCtrls, ImageList, ImgList,
  ScriptValidatorResult,
  SE_IssueListBox, SE_SnippetListBox, SE_CommandsDataModule;

type
  { TScriptingEditorForm }
  TSEMainForm = class(TForm)
    MainMenu1:             TMainMenu;
    StatusBar1:            TStatusBar;
    ToolBar1:              TToolBar;
    pcLeft,
    pcBottom,
    pcEditors:             TPageControl; // Page controls
    tsIssues,
    tsRawSVOutput,
    tsEvents,
    tsStates,
    tsActions,
    tsUtils:               TTabSheet; // Tab sheets
    edtRawSVOutput:        TMemo; // Memos
    pmIssues:              TPopupMenu; // Popups
    Edit1,
    File1,
    Help1,
    Run1,
    Search1,
    Theme1:                TMenuItem; // Main menu categories
    N1,
    N2,
    N3,
    N4,
    N5,
    N6,
    N7,
    N8:                    TMenuItem; // Main menu seperators
    mri1,
    mri2,
    mri3,
    mri4,
    mri5,
    mri6,
    mri7,
    mri8,
    mri9,
    mri10:                 TMenuItem; // Main menu reopen items
    MenuNew,
    MenuOpen,
    MenuSave,
    MenuSaveAs,
    MenuClose,
    MenuCloseAll,
    MenuReopen,
    MenuExit,
    MenuUndo,
    MenuRedo,
    MenuCut,
    MenuCopy,
    MenuPaste,
    MenuDelete,
    MenuSelectAll,
    MenuFind,
    MenuReplace,
    MenuFindForward,
    MenuFindBackward,
    MenuGoToLine,
    MenuValidate,
    MenuOptions,
    MenuKMRDocWiki,
    MenuKPDocWiki,
    MenuAboutSE,
    MenuShowWelcomeTab,
    MenuThemeLight,
    MenuThemeClassic,
    MenuThemeOcean,
    MenuthemeVisualStudio,
    MenuthemeTwilight,
    MenuthemeDark:         TMenuItem; // Main menu items
    miIssueGoTo,
    miIssueCopy:           TMenuItem; // fLbIssues popup items
    tbSep1,
    tbSep2,
    tbSep3,
    tbSep4:                TToolButton; // Toolbar button seperators
    tbNewFile,
    tbOpen,
    tbSaveFile,
    tbSaveFileAs,
    tbValidate,
    tbCut,
    tbCopy,
    tbPaste,
    tbDelete,
    tbUndo,
    tbRedo,
    tbFind,
    tbReplace,
    tbGoToLine:            TToolButton; // Toolbar buttons
    Splitter1,
    Splitter2:             TSplitter; // Splitters
    ilMethodTypes:         TImageList;
    Panel1:                TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pcEditorsMouseUp(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    procedure MenuReopenClick(Sender: TObject);
    procedure MriClick(Sender: TObject);
    procedure pcEditorsChange(Sender: TObject);
  strict private
    fLbEvents,
    fLBStates,
    fLBActions,
    fLBUtils:   TSESnippetListBox;
    fLbIssues:  TSEIssueListBox;
    fHintIndex: Integer;
    fMRIItems:  array[0..9] of TMenuItem;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FLbIssuesDblClick(Sender: TObject);
    procedure FLbSnippetsDblClick(Sender: TObject);
    procedure FLbIssuesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FLbIssuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FLbSnippetsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ReadSettings;
    procedure WriteSettings;
  public
    procedure GoToIssue(aIndex: Integer);
    procedure CopyIssue(aIndex: Integer);
    function ExecuteScriptValidator(const aFileName: string): string;
    procedure ReloadDictionaries;
    procedure SetListboxesVisible(aState: Boolean);
    property LbIssues: TSEIssueListBox read fLbIssues;
  end;

implementation
{$R *.dfm}
uses
  IOUtils, SysUtils, ClipBrd, ShellAPI, UITypes, IniFiles,
  SE_Globals, SE_Log, SE_Exceptions, SE_WelcomeTab, SE_ACMethods;

{ TScriptingEditorForm }
procedure TSEMainForm.FormCreate(Sender: TObject);
begin
  Caption     := 'Scripting Editor - ' + FULL_VERSION;
  gExeDir     := ExtractFilePath(ParamStr(0));
  fExceptions := TSEExceptions.Create;
  CreateDir(gExeDir + DATA_DIR_LOGS);
  gLog := TSELog.Create(gExeDir + DATA_DIR_LOGS + 'SE_' +
                        FormatDateTime('yyyy-mm-dd_hh-nn-ss-zzz', Now) + '.log'); //First thing - create a log
  gLog.DeleteOldLogs;

  fMRIItems[0] := mri1;
  fMRIItems[1] := mri2;
  fMRIItems[2] := mri3;
  fMRIItems[3] := mri4;
  fMRIItems[4] := mri5;
  fMRIItems[5] := mri6;
  fMRIItems[6] := mri7;
  fMRIItems[7] := mri8;
  fMRIItems[8] := mri9;
  fMRIItems[9] := mri10;

  fLbEvents  := TSESnippetListBox.Create(tsEvents, @ilMethodTypes);
  fLBStates  := TSESnippetListBox.Create(tsStates, @ilMethodTypes);
  fLBActions := TSESnippetListBox.Create(tsActions, @ilMethodTypes);
  fLBUtils   := TSESnippetListBox.Create(tsUtils, @ilMethodTypes);
  fLbIssues  := TSEIssueListBox.Create(tsIssues);

  gAllACItems                   := TStringList.Create;
  gAllACInserts                 := TStringList.Create;
  gActionsMethodList            := TSEMethodList.Create('Actions.');
  gEventsMethodList             := TSEMethodList.Create;
  gEventsMethodList.IsEventList := True;
  gStatesMethodList             := TSEMethodList.Create('States.');
  gUtilsMethodList              := TSEMethodList.Create;
  gPasScriptMethodList          := TSEMethodList.Create;

  fLbIssues.Parent       := tsIssues;
  fLbIssues.Align        := alClient;
  fLbIssues.AutoComplete := False;
  fLbIssues.Sorted       := False;
  fLbIssues.ShowHint     := True;
  fLbIssues.PopupMenu    := pmIssues;
  fLbIssues.OnDblClick   := FLbIssuesDblClick;
  fLbIssues.OnMouseUp    := FLbIssuesMouseUp;
  fLbIssues.OnMouseMove  := FLbIssuesMouseMove;

  fLbEvents.Parent       := tsEvents;
  fLbEvents.Align        := alClient;
  fLbEvents.AutoComplete := False;
  fLbEvents.Sorted       := False;
  fLbEvents.ShowHint     := True;
  fLbEvents.OnDblClick   := FLbSnippetsDblClick;
  fLbEvents.OnMouseMove  := FLbSnippetsMouseMove;

  fLBStates.Parent       := tsStates;
  fLBStates.Align        := alClient;
  fLBStates.AutoComplete := False;
  fLBStates.Sorted       := False;
  fLBStates.ShowHint     := True;
  fLBStates.OnDblClick   := FLbSnippetsDblClick;
  fLBStates.OnMouseMove  := FLbSnippetsMouseMove;

  fLBActions.Parent       := tsActions;
  fLBActions.Align        := alClient;
  fLBActions.AutoComplete := False;
  fLBActions.Sorted       := False;
  fLBActions.ShowHint     := True;
  fLBActions.OnDblClick   := FLbSnippetsDblClick;
  fLBActions.OnMouseMove  := FLbSnippetsMouseMove;

  fLBUtils.Parent       := tsUtils;
  fLBUtils.Align        := alClient;
  fLBUtils.AutoComplete := False;
  fLBUtils.Sorted       := False;
  fLBUtils.ShowHint     := True;
  fLBUtils.OnDblClick   := FLbSnippetsDblClick;
  fLBUtils.OnMouseMove  := FLbSnippetsMouseMove;

  SetListboxesVisible(False);
  gCommandsDataModule := TSECommandsDataModule.Create(Self);
  ReadSettings;
  gLog.AddTime('Done loading');
end;

procedure TSEMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);

  if gEditorFactory <> nil then
    gEditorFactory.CloseAll;

  WriteSettings;
  FreeAndNil(gCommandsDataModule);
  FreeAndNil(fLbEvents);
  FreeAndNil(fLBStates);
  FreeAndNil(fLBActions);
  FreeAndNil(fLBUtils);
  FreeAndNil(fLbIssues);
  FreeAndNil(gAllACItems);
  FreeAndNil(gAllACInserts);
  FreeAndNil(gActionsMethodList);
  FreeAndNil(gEventsMethodList);
  FreeAndNil(gStatesMethodList);
  FreeAndNil(gUtilsMethodList);
  FreeAndNil(gPasScriptMethodList);
end;

procedure TSEMainForm.FormShow(Sender: TObject);
begin
  ReloadDictionaries;
  DragAcceptFiles(Handle, True);
  gCommandsDataModule.ActShowWelcomeTabExecute(Self);
end;

procedure TSEMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if gEditorFactory <> nil then
    CanClose := gEditorFactory.CanCloseAll;

  inherited;
end;

procedure TSEMainForm.pcEditorsChange(Sender: TObject);
begin
  SetListboxesVisible(not (pcEditors.ActivePage is TSEWelcomeTabSheet));
end;

procedure TSEMainForm.pcEditorsMouseUp(Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    CloseTab(pcEditors.IndexOfTabAt(X, Y));
end;

procedure TSEMainForm.MenuReopenClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := 0 to Length(fMRIItems) - 1 do
  begin
    if fMRIItems[I] <> nil then
    begin
      S                    := gCommandsDataModule.GetMRIEntry(I);
      fMRIItems[I].Visible := S <> '';
      fMRIItems[I].Caption := S;
    end;
  end;
end;

procedure TSEMainForm.MriClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := 0 to Length(fMRIItems) - 1 do
    if Sender = fMRIItems[I] then
    begin
      S := gCommandsDataModule.GetMRIEntry(I);

      if S <> '' then
        gEditorFactory.OpenFile(S);
    end;
end;

procedure TSEMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  Filename: array[0..MAX_PATH] of Char;
begin
  DragQueryFile(Msg.Drop, 0, Filename, MAX_PATH);
  gEditorFactory.OpenFile(Filename);
  DragFinish(Msg.Drop);
  gLog.AddTime('File dragged in: ' + Filename);
end;

procedure TSEMainForm.FLbIssuesDblClick(Sender: TObject);
var
  cursorPos: TPoint;
begin
  cursorPos := fLbIssues.ScreenToClient(Mouse.CursorPos);
  GoToIssue(fLbIssues.ItemAtPos(cursorPos, True));
end;

procedure TSEMainForm.FLbSnippetsDblClick(Sender: TObject);
var
  cursorPos: TPoint;
  listBox:   TSESnippetListBox;
  temp,
  snippet:   string;
begin
  listBox   := TSESnippetListBox(Sender);
  cursorPos := listBox.ScreenToClient(Mouse.CursorPos);
  snippet   := listBox.GetSnippet(listBox.ItemAtPos(cursorPos, True));

  if snippet = '' then
    Exit; // Non-existing item

  temp             := Clipboard.AsText;
  Clipboard.AsText := snippet;
  gEditCmds.ExecPaste;
  Clipboard.AsText := temp;
end;

procedure TSEMainForm.FLbIssuesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  popPos,
  cursorPos: TPoint;
begin
  if Button = mbRight then
  begin
    cursorPos.X := X;
    cursorPos.Y := Y;
    popPos      := fLbIssues.ClientToScreen(cursorPos);

    if fLbIssues.ItemAtPos(cursorPos, True) <> -1 then
      pmIssues.Popup(popPos.X, popPos.Y);
  end;
end;

procedure TSEMainForm.FLbIssuesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  cursorPos: TPoint;
  index:     Integer;
  issue:     TScriptValidatorIssue;
begin
  cursorPos := fLbIssues.ScreenToClient(Mouse.CursorPos);
  index     := fLbIssues.ItemAtPos(cursorPos, True);
  issue     := fLbIssues.GetIssue(index);

  if issue.Line = NON_EXISTING_ITEM then
  begin
    Application.CancelHint;
    fLbIssues.Hint := '';
    fHintIndex     := -1;
    Exit;
  end;

  if fHintIndex <> index then
  begin
    Application.CancelHint;
    fLbIssues.Hint := '';
    fHintIndex     := index;
  end;

  fLbIssues.Hint := Format('[%d:%d] %s', [issue.Line, issue.Column, issue.Msg]);
end;

procedure TSEMainForm.FLbSnippetsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  listBox:   TSESnippetListBox;
  cursorPos: TPoint;
  index:     Integer;
  snippet:   string;
begin
  listBox   := TSESnippetListBox(Sender);
  cursorPos := listBox.ScreenToClient(Mouse.CursorPos);
  index     := listBox.ItemAtPos(cursorPos, True);
  snippet   := listBox.GetSnippet(index);

  if snippet = '' then
  begin
    Application.CancelHint;
    listBox.Hint := '';
    fHintIndex   := -1;
    Exit; // Non-existing item
  end;

  if fHintIndex <> index then
  begin
    Application.CancelHint;
    listBox.Hint := '';
    fHintIndex   := index;
  end;

  listBox.Hint := snippet;
end;

procedure TSEMainForm.GoToIssue(aIndex: Integer);
var
  issue: TScriptValidatorIssue;
begin
  issue := fLbIssues.GetIssue(aIndex);

  if issue.Line = NON_EXISTING_ITEM then
    Exit;

  if issue.Line < 1 then
    issue.Line := 1;

  if issue.Column < 1 then
    issue.Column := 1;

  gActiveEditor.SetCaret(issue.Column, issue.Line);
end;

procedure TSEMainForm.CopyIssue(aIndex: Integer);
var
  issue: TScriptValidatorIssue;
begin
  issue := fLbIssues.GetIssue(aIndex);

  if issue.Line = NON_EXISTING_ITEM then
    Exit;

  Clipboard.AsText := Format('[%d:%d] <Module: %s | Param: %s> %s',
                             [issue.Line, issue.Column, issue.Module,
                              issue.Param, issue.Msg]);
end;

function TSEMainForm.ExecuteScriptValidator(const aFileName: string): string;
var
  SA:              TSecurityAttributes;
  SI:              TStartupInfo;
  PI:              TProcessInformation;
  StdOutPipeRead,
  StdOutPipeWrite: THandle;
  WasOK:           Boolean;
  Buffer:          array[0..255] of AnsiChar;
  BytesRead:       Cardinal;
  Command:         string;
const
  CMD_EXE = 'C:\Windows\System32\cmd.exe';
  SV_EXE_FORMAT = '%s /c ""%sScriptValidator.exe" -x "%s""';
begin
  gLog.AddTime('Starting script validator');
  Result  := '';
  Command := Format(SV_EXE_FORMAT, [CMD_EXE, ExtractFilePath(ParamStr(0)) + DATA_DIR, aFileName]);
  gLog.AddTime('Call cmd ''' + Command + '''');

  SA.nLength              := SizeOf(SA);
  SA.bInheritHandle       := True;
  SA.lpSecurityDescriptor := nil;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);

  try
    ZeroMemory(@SI, SizeOf(SI));
    SI.cb          := SizeOf(SI);
    SI.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput   := GetStdHandle(STD_INPUT_HANDLE); // Don't redirect stdin
    SI.hStdOutput  := StdOutPipeWrite;
    SI.hStdError   := StdOutPipeWrite;

    if CreateProcess(
      PChar(CMD_EXE), PChar(Command), nil, nil, True,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, SI, PI
    ) then
      try
        CloseHandle(StdOutPipeWrite);
        WaitForSingleObject(PI.hProcess, INFINITE);

        repeat
          ZeroMemory(@Buffer, SizeOf(Buffer));
          WasOK := ReadFile(StdOutPipeRead, Buffer, 256, BytesRead, nil);

          if BytesRead > 0 then
            Result := Result + string(Buffer);
        until not WasOK or (BytesRead = 0);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
        gLog.AddTime('Successfully executed script validator');
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

procedure TSEMainForm.ReloadDictionaries;
begin
  gAllACItems.Clear;
  gAllACInserts.Clear;
  gActionsMethodList.Clear;
  gEventsMethodList.Clear;
  gStatesMethodList.Clear;
  gUtilsMethodList.Clear;
  gPasScriptMethodList.Clear;
  gActionsMethodList.LoadFromFile(DATA_DIR + DICT_FILE_ACTIONS);
  gEventsMethodList.LoadFromFile(DATA_DIR + DICT_FILE_EVENTS);
  gStatesMethodList.LoadFromFile(DATA_DIR + DICT_FILE_STATES);
  gUtilsMethodList.LoadFromFile(DATA_DIR + DICT_FILE_UTILS);
  gPasScriptMethodList.LoadFromFile(DATA_DIR + DICT_FILE_PASSCRIPT);
  gLog.AddTime('Dictionaries loaded');

  fLBActions.Clear;
  fLbEvents.Clear;
  fLBStates.Clear;
  fLBUtils.Clear;

  fLBActions.AppendSnippets(gActionsMethodList.GenerateMethodItemList,
                            gActionsMethodList.GenerateMethodInsertNames);
  fLbEvents.AppendSnippets(gEventsMethodList.GenerateMethodItemList,
                           gEventsMethodList.GenerateMethodInsertNames);
  fLBStates.AppendSnippets(gStatesMethodList.GenerateMethodItemList,
                           gStatesMethodList.GenerateMethodInsertNames);
  fLBUtils.AppendSnippets(gUtilsMethodList.GenerateMethodItemList,
                          gUtilsMethodList.GenerateMethodInsertNames);

  SetListboxScrollWidth(fLBActions);
  SetListboxScrollWidth(fLbEvents);
  SetListboxScrollWidth(fLBStates);
  SetListboxScrollWidth(fLBUtils);

  gAllACItems.AddStrings(gActionsMethodList.GenerateParameterLookupList);
  gAllACInserts.AddStrings(gActionsMethodList.GenerateParameterInsertList);
  gAllACItems.AddStrings(gStatesMethodList.GenerateParameterLookupList);
  gAllACInserts.AddStrings(gStatesMethodList.GenerateParameterInsertList);
  gAllACItems.AddStrings(gUtilsMethodList.GenerateParameterLookupList);
  gAllACInserts.AddStrings(gUtilsMethodList.GenerateParameterInsertList);
  gAllACItems.AddStrings(gPasScriptMethodList.GenerateParameterLookupList);
  gAllACInserts.AddStrings(gPasScriptMethodList.GenerateParameterInsertList);
end;

procedure TSEMainForm.SetListboxesVisible(aState: Boolean);
begin
  fLbIssues.Visible  := aState;
  fLbEvents.Visible  := aState;
  fLBStates.Visible  := aState;
  fLBActions.Visible := aState;
  fLBUtils.Visible   := aState;
end;

procedure TSEMainForm.ReadSettings;
var
  IniFile:      TIniFile;
  PcLeftWidth,
  PcBottomHeight,
  I,
  X,
  Y,
  W,
  H:            Integer;
  S:            string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

  try
    X              := IniFile.ReadInteger('Application', 'Left',         0);
    Y              := IniFile.ReadInteger('Application', 'Top',          0);
    W              := IniFile.ReadInteger('Application', 'Width',        1024);
    H              := IniFile.ReadInteger('Application', 'Height',       600);
    PcLeftWidth    := IniFile.ReadInteger('Application', 'LeftWidth',    270);
    PcBottomHeight := IniFile.ReadInteger('Application', 'BottomHeight', 150);

    gOptions.Theme     := TSEThemeKind(IniFile.ReadInteger('Options', 'Theme',    Ord(tkDark)));
    gOptions.Font.Name := IniFile.ReadString('Options',               'FontName', 'Courier New');
    gOptions.Font.Size := IniFile.ReadInteger('Options',              'FontSize', 10);

    if (W > 0) and (H > 0) then
      SetBounds(X, Y, W, H);

    if IniFile.ReadBool('Application', 'Maximized', False) then
      WindowState := wsMaximized;

    gCommandsDataModule.ActThemeLight.Checked        := gOptions.Theme = tkLight;
    gCommandsDataModule.ActThemeClassic.Checked      := gOptions.Theme = tkClassic;
    gCommandsDataModule.ActThemeOcean.Checked        := gOptions.Theme = tkOcean;
    gCommandsDataModule.ActThemeVisualStudio.Checked := gOptions.Theme = tkVisualStudio;
    gCommandsDataModule.ActThemeTwilight.Checked     := gOptions.Theme = tkTwilight;
    gCommandsDataModule.ActThemeDark.Checked         := gOptions.Theme = tkDark;
    pcLeft.Width                                     := PcLeftWidth;
    pcBottom.Height                                  := PcBottomHeight;

    gSearchBackwards     := IniFile.ReadBool('SearchReplace',   'Backwards',      False);
    gSearchCaseSensitive := IniFile.ReadBool('SearchReplace',   'CaseSensitive',  False);
    gSearchSelection     := IniFile.ReadBool('SearchReplace',   'SelectionOnly',  False);
    gSearchWholeWords    := IniFile.ReadBool('SearchReplace',   'WholeWordsOnly', False);
    gSearchRegex         := IniFile.ReadBool('SearchReplace',   'Regex',          False);
    gSearchTextHistory   := IniFile.ReadString('SearchReplace', 'SearchHistory',  '');
    gReplaceTextHistory  := IniFile.ReadString('SearchReplace', 'ReplaceHistory', '');

    for I := 9 downto 0 do
    begin
      S := IniFile.ReadString('MRIFiles', Format('MRIFile%d', [I]), '');

      if S <> '' then
        gCommandsDataModule.AddMRIEntry(S);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TSEMainForm.WriteSettings;
var
  IniFile: TIniFile;
  wp:      TWindowPlacement;
  I:       Integer;
  S:       string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

  try
    wp.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @wp);

    // form properties
    with wp.rcNormalPosition do
    begin
      IniFile.WriteInteger('Application', 'Left',   Left);
      IniFile.WriteInteger('Application', 'Top',    Top);
      IniFile.WriteInteger('Application', 'Width',  Right - Left);
      IniFile.WriteInteger('Application', 'Height', Bottom - Top);
    end;

    IniFile.WriteInteger('Application', 'LeftWidth',    pcLeft.Width);
    IniFile.WriteInteger('Application', 'BottomHeight', pcBottom.Height);
    IniFile.WriteBool('Application',    'Maximized',    (WindowState = wsMaximized));

    IniFile.WriteInteger('Options', 'Theme',    Ord(gOptions.Theme));
    IniFile.WriteString('Options',  'FontName', gOptions.Font.Name);
    IniFile.WriteInteger('Options', 'FontSize', gOptions.Font.Size);

    IniFile.WriteBool('SearchReplace',   'Backwards',      gSearchBackwards);
    IniFile.WriteBool('SearchReplace',   'CaseSensitive',  gSearchCaseSensitive);
    IniFile.WriteBool('SearchReplace',   'SelectionOnly',  gSearchSelection);
    IniFile.WriteBool('SearchReplace',   'WholeWordsOnly', gSearchWholeWords);
    IniFile.WriteBool('SearchReplace',   'Regex',          gSearchRegex);
    IniFile.WriteString('SearchReplace', 'SearchHistory',  gSearchTextHistory);
    IniFile.WriteString('SearchReplace', 'ReplaceHistory', gReplaceTextHistory);

    for I := 0 to 9 do begin
      S := gCommandsDataModule.GetMRIEntry(I);

      if S <> '' then
        IniFile.WriteString('MRIFiles', Format('MRIFile%d', [I]), S)
      else
        IniFile.DeleteKey('MRIFiles', Format('MRIFile%d', [I]));
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
