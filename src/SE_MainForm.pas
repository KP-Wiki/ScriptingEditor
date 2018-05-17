unit SE_MainForm;

interface

uses
  Windows, Messages, Classes, ImageList, Actions, Graphics,
  Controls, Forms, ComCtrls, Menus, ImgList, ActnList, StdActns, Dialogs,
  ToolWin, ExtCtrls,
  SynEdit, SynCompletionProposal, SynHighlighterPas,
  ScriptValidatorResult, SE_ConfirmReplaceForm, SE_IssueListBox, SE_SnippetListBox;

type
  { TScriptingEditorForm }
  TSEMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    N1: TMenuItem;
    MenuExit: TMenuItem;
    Edit1: TMenuItem;
    MenuUndo: TMenuItem;
    MenuRedo: TMenuItem;
    N2: TMenuItem;
    MenuValidate: TMenuItem;
    N3: TMenuItem;
    Run1: TMenuItem;
    Search1: TMenuItem;
    N4: TMenuItem;
    MenuOptions: TMenuItem;
    StatusBar1: TStatusBar;
    Help1: TMenuItem;
    MenuDocWiki: TMenuItem;
    N5: TMenuItem;
    MenuAboutSE: TMenuItem;
    MenuFind: TMenuItem;
    MenuReplace: TMenuItem;
    MenuFindForward: TMenuItem;
    MenuFindBackward: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    MenuGoToLine: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    MenuDelete: TMenuItem;
    MenuSelectAll: TMenuItem;
    ilActions22x22: TImageList;
    ActionList1: TActionList;
    ActOpenFile: TAction;
    ActUndo: TAction;
    ActFind: TAction;
    ActValidate: TAction;
    ActDocWiki: TAction;
    ActNewFile: TAction;
    MenuNew: TMenuItem;
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
    ActFindForward: TAction;
    ActFindBackward: TAction;
    ActReplace: TAction;
    ActGoToLine: TAction;
    ActAboutSE: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbSep1: TToolButton;
    ToolButton6: TToolButton;
    tbSep2: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    tbSep3: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    tbSep4: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton19: TToolButton;
    ilActions16x16: TImageList;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pcLeft: TPageControl;
    tsEvents: TTabSheet;
    pcRight: TPageControl;
    tsIssues: TTabSheet;
    tsStates: TTabSheet;
    tsActions: TTabSheet;
    tsUtils: TTabSheet;
    procedure ActExitExecute(Sender: TObject);
    procedure ActSaveFileExecute(Sender: TObject);
    procedure ActNewFileExecute(Sender: TObject);
    procedure ActOpenFileExecute(Sender: TObject);
    procedure ActSaveFileAsExecute(Sender: TObject);
    procedure ActOptionsExecute(Sender: TObject);
    procedure ActDocWikiExecute(Sender: TObject);
    procedure ActAboutSEExecute(Sender: TObject);
    procedure ActValidateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActUndoExecute(Sender: TObject);
    procedure ActRedoExecute(Sender: TObject);
    procedure ActCutExecute(Sender: TObject);
    procedure ActCopyExecute(Sender: TObject);
    procedure ActPasteExecute(Sender: TObject);
    procedure ActDeleteExecute(Sender: TObject);
    procedure ActSelectAllExecute(Sender: TObject);
    procedure ActFindExecute(Sender: TObject);
    procedure ActFindForwardExecute(Sender: TObject);
    procedure ActFindBackwardExecute(Sender: TObject);
    procedure ActReplaceExecute(Sender: TObject);
    procedure ActGoToLineExecute(Sender: TObject);
    procedure ActCopyUpdate(Sender: TObject);
    procedure ActCutUpdate(Sender: TObject);
    procedure ActDeleteUpdate(Sender: TObject);
    procedure ActFindUpdate(Sender: TObject);
    procedure ActGoToLineUpdate(Sender: TObject);
    procedure ActPasteUpdate(Sender: TObject);
    procedure ActRedoUpdate(Sender: TObject);
    procedure ActReplaceUpdate(Sender: TObject);
    procedure ActSelectAllUpdate(Sender: TObject);
    procedure ActUndoUpdate(Sender: TObject);
    procedure ActValidateUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
    fLbEvents,
    fLBStates,
    fLBActions,
    fLBUtils:              TSESnippetListBox;
    fLbIssues:             TSEIssueListBox;
    fIssues:               TScriptValidatorResult;
    fSynEdit:              TSynEdit;
    fSynPasSyn:            TSynPasSyn;
    fSynCompletion:        TSynCompletionProposal;
    fConfirmReplaceDialog: TSEConfirmReplaceForm;
    fReplaceAll,
    fSearchBackwards,
    fSearchCaseSensitive,
    fSearchSelection,
    fSearchWholeWords,
    fSearchRegex:          Boolean;
    fExeDir,
    fFileName,
    fSearchText,
    fSearchTextHistory,
    fReplaceText,
    fReplaceTextHistory:   string;
    fEventsDict,
    fEventsInsDict,
    fStatesDict,
    fStatesInsDict,
    fActionsDict,
    fActionsInsDict,
    fUtilsDict,
    fUtilsInsDict,
    fPasScriptDict,
    fPasScriptInsDict: TStringList;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FLbIssuesDblClick(Sender: TObject);
    procedure FLbSnippetsDblClick(Sender: TObject);
    procedure SynEditorGutterClick(Sender: TObject; Button: TMouseButton;
                                   X, Y, Line: Integer; Mark: TSynEditMark);
    procedure SynEditorChange(Sender: TObject);
    procedure SynEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure PerformSearchReplace(aReplace: Boolean; aBackwards: Boolean);
    procedure ShowSearchReplaceDialog(aReplace: Boolean);
    procedure SynEditorReplaceText(Sender: TObject;
                                   const aSearch, aReplace: UnicodeString;
                                   aLine, aColumn: Integer;
                                   var aAction: TSynReplaceAction);
    procedure PaintGutterGlyphs(aCanvas: TCanvas; const aClip: TRect;
                                aFirstLine, aLastLine: Integer);
    procedure LinesInserted(aFirstLine, aCount: Integer);
    procedure LinesDeleted(aFirstLine, aCount: Integer);
    procedure LinesChange(aFirstLine, aCount: Integer; indDeleted: Boolean);
    procedure SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                   var CurrentInput: String; var x, y: Integer;
                                   var CanExecute: Boolean);
    function ShowSaveModified: Boolean;
    function ShowSaveAs: Boolean;
    function ExecuteScriptValidator(const aFileName: string): string;
    procedure RunValidate(const aFileName: string);
    procedure ReadSettings;
    procedure WriteSettings;
  end;

var
  SEMainForm: TSEMainForm;

implementation
uses
  IOUtils, SysUtils, ClipBrd, ShellAPI, UITypes, Math, IniFiles, StrUtils,
  SynEditHighlighter, SynEditTypes, SynEditRegexSearch, SynEditSearch,
  SynEditMiscClasses, SynUnicode, SynEditMiscProcs, SynEditTextBuffer,
  SE_GoToLineForm, SE_FindForm, SE_ReplaceForm, SE_SaveModifiedForm,
  SE_AboutForm, SE_ValidationPlugin;

{$R *.dfm}

{ TScriptingEditorForm }
procedure TSEMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;

  if fSynEdit.Modified then
    if not ShowSaveModified then
      CanClose := False;
end;

procedure TSEMainForm.FormCreate(Sender: TObject);
var
  Settings: TStringList;
begin
  fExeDir           := ExtractFilePath(ParamStr(0));
  fLbEvents         := TSESnippetListBox.Create(tsEvents);
  fLBStates         := TSESnippetListBox.Create(tsStates);
  fLBActions        := TSESnippetListBox.Create(tsActions);
  fLBUtils          := TSESnippetListBox.Create(tsUtils);
  fLbIssues         := TSEIssueListBox.Create(tsIssues);
  fSynEdit          := TSynEdit.Create(Self);
  fSynPasSyn        := TSynPasSyn.Create(Self);
  fSynCompletion    := TSynCompletionProposal.Create(fSynEdit);
  Settings          := TStringList.Create;
  fEventsDict       := TStringList.Create;
  fEventsInsDict    := TStringList.Create;
  fStatesDict       := TStringList.Create;
  fStatesInsDict    := TStringList.Create;
  fActionsDict      := TStringList.Create;
  fActionsInsDict   := TStringList.Create;
  fUtilsDict        := TStringList.Create;
  fUtilsInsDict     := TStringList.Create;
  fPasScriptDict    := TStringList.Create;
  fPasScriptInsDict := TStringList.Create;

  try
    fSynPasSyn.EnumUserSettings(Settings);

    if Settings.Count > 0 then
      fSynPasSyn.UseUserSettings(Settings.Count - 1);
  finally
    FreeAndNil(Settings);
  end;

  with fLbIssues do
  begin
    Parent       := tsIssues;
    Align        := alClient;
    AutoComplete := False;
    Sorted       := False;
    OnDblClick   := FLbIssuesDblClick;
  end;

  with fSynEdit do
  begin
    Parent                        := Self;
    Align                         := alClient;
    Highlighter                   := fSynPasSyn;
    TabOrder                      := 1;
    TabWidth                      := 2;
    WantTabs                      := True;
    UseCodeFolding                := True;
    CodeFolding.ShowCollapsedLine := True;
    CodeFolding.IndentGuides      := True;
    Gutter.ShowLineNumbers        := True;
    Gutter.ShowModification       := True;
    Gutter.AutoSize               := True;
    OnChange                      := SynEditorChange;
    OnGutterClick                 := SynEditorGutterClick;
    OnStatusChange                := SynEditorStatusChange;
    OnReplaceText                 := SynEditorReplaceText;
    Options                       := [
      eoAutoIndent, eoDragDropEditing, eoScrollPastEol, eoShowScrollHint,
      eoSmartTabs, eoSmartTabDelete, eoTabsToSpaces, eoTabIndent,
      eoTrimTrailingSpaces, eoKeepCaretX, eoEnhanceEndKey, eoGroupUndo
    ];
  end;

  with fSynCompletion do
  begin
    Editor            := fSynEdit;
    OnExecute         := SynCompletionExecute;
    Width             := 750;
    NbLinesInWindow   := 15;
    Title             := 'Suggested completions';
    ClTitleBackground := clInfoBk;
    Options           := [
      scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText,
      scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab,
      scoCompleteWithEnter, scoLimitToMatchedTextAnywhere
    ];
    Columns.ClearAndResetID;

    with Columns.Add do
      ColumnWidth := 96;
  end;

  with TSEValidationPlugin.Create(fSynEdit) do
  begin
    OnGutterPaint   := PaintGutterGlyphs;
    OnLinesInserted := LinesInserted;
    OnLinesDeleted  := LinesDeleted;
  end;

  fEventsDict.LoadFromFile(fExeDir       + 'Data\Events.dict');
  fEventsInsDict.LoadFromFile(fExeDir    + 'Data\Events.ins.dict');
  fStatesDict.LoadFromFile(fExeDir       + 'Data\States.dict');
  fStatesInsDict.LoadFromFile(fExeDir    + 'Data\States.ins.dict');
  fActionsDict.LoadFromFile(fExeDir      + 'Data\Actions.dict');
  fActionsInsDict.LoadFromFile(fExeDir   + 'Data\Actions.ins.dict');
  fUtilsDict.LoadFromFile(fExeDir        + 'Data\Utils.dict');
  fUtilsInsDict.LoadFromFile(fExeDir     + 'Data\Utils.ins.dict');
  fPasScriptDict.LoadFromFile(fExeDir    + 'Data\PasScript.dict');
  fPasScriptInsDict.LoadFromFile(fExeDir + 'Data\PasScript.ins.dict');

  with fLbEvents do
  begin
    Parent       := tsEvents;
    Align        := alClient;
    AutoComplete := False;
    Sorted       := False;
    OnDblClick   := FLbSnippetsDblClick;
    Clear;
    AppendSnippets(fEventsDict, fEventsInsDict, True);
  end;

  with fLBStates do
  begin
    Parent       := tsStates;
    Align        := alClient;
    AutoComplete := False;
    Sorted       := False;
    OnDblClick   := FLbSnippetsDblClick;
    Clear;
    AppendSnippets(fStatesDict, fStatesInsDict);
  end;

  with fLBActions do
  begin
    Parent       := tsActions;
    Align        := alClient;
    AutoComplete := False;
    Sorted       := False;
    OnDblClick   := FLbSnippetsDblClick;
    Clear;
    AppendSnippets(fActionsDict, fActionsInsDict);
  end;

  with fLBUtils do
  begin
    Parent       := tsUtils;
    Align        := alClient;
    AutoComplete := False;
    Sorted       := False;
    OnDblClick   := FLbSnippetsDblClick;
    Clear;
    AppendSnippets(fUtilsDict, fUtilsInsDict);
  end;

  ReadSettings;
  DragAcceptFiles(Handle, True);
end;

procedure TSEMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
  WriteSettings;
  FreeAndNil(fLbEvents);
  FreeAndNil(fLBStates);
  FreeAndNil(fLBActions);
  FreeAndNil(fLBUtils);
  FreeAndNil(fLbIssues);
  FreeAndNil(fSynCompletion);
  FreeAndNil(fSynEdit);
  FreeAndNil(fSynPasSyn);
  FreeAndNil(fEventsDict);
  FreeAndNil(fEventsInsDict);
  FreeAndNil(fStatesDict);
  FreeAndNil(fStatesInsDict);
  FreeAndNil(fActionsDict);
  FreeAndNil(fActionsInsDict);
  FreeAndNil(fUtilsDict);
  FreeAndNil(fUtilsInsDict);
  FreeAndNil(fPasScriptDict);
  FreeAndNil(fPasScriptInsDict);
end;

procedure TSEMainForm.FormShow(Sender: TObject);
begin
  SynEditorStatusChange(Self, [scAll]);
end;

procedure TSEMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  Filename: array[0..MAX_PATH] of Char;
begin
  DragQueryFile(Msg.Drop, 0, Filename, MAX_PATH);
  fFileName := Filename;
  fSynEdit.Lines.Clear;
  fSynEdit.Lines.LoadFromFile(fFileName);
  DragFinish(Msg.Drop);
end;

procedure TSEMainForm.FLbIssuesDblClick(Sender: TObject);
var
  cursorPos: TPoint;
  caretPos:  TBufferCoord;
  issue:     TScriptValidatorIssue;
begin
  cursorPos := fLbIssues.ScreenToClient(Mouse.CursorPos);
  caretPos  := fSynEdit.CaretXY;
  issue     := fLbIssues.GetIssue(fLbIssues.ItemAtPos(cursorPos, True));

  if issue.Line = -2 then
    Exit; // Non-existing item

  if issue.Line < 1 then
    issue.Line := 1;

  if issue.Column < 1 then
    issue.Column := 1;

  caretPos.Line    := issue.Line;
  caretPos.Char    := issue.Column;
  fSynEdit.CaretXY := caretPos;
  fSynEdit.SetFocus;
end;


procedure TSEMainForm.FLbSnippetsDblClick(Sender: TObject);
var
  cursorPos: TPoint;
  temp,
  snippet:   string;
  listBox:   TSESnippetListBox;
begin
  listBox   := TSESnippetListBox(Sender);
  cursorPos := listBox.ScreenToClient(Mouse.CursorPos);
  snippet   := listBox.GetSnippet(listBox.ItemAtPos(cursorPos, True));

  if snippet = '' then
    Exit; // Non-existing item

  temp             := Clipboard.AsText;
  Clipboard.AsText := snippet;
  fSynEdit.SetFocus;
  fSynEdit.PasteFromClipboard;
  Clipboard.AsText := temp;
end;


procedure TSEMainForm.SynEditorGutterClick(Sender: TObject; Button: TMouseButton;
                                           X, Y, Line: Integer; Mark: TSynEditMark);
begin
  fSynEdit.CaretY := Line;
end;

procedure TSEMainForm.SynEditorChange(Sender: TObject);
begin
  //
end;

procedure TSEMainForm.SynEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
const
  ModifiedStrs:   array[boolean] of string = ('',          'Modified');
  InsertModeStrs: array[boolean] of string = ('Overwrite', 'Insert');
var
  CaretPos: TBufferCoord;
begin
  // Caret position has changed
  if Changes * [scAll, scCaretX, scCaretY] <> [] then
  begin
    CaretPos                  := fSynEdit.CaretXY;
    Statusbar1.Panels[0].Text := Format('%6d:%3d', [CaretPos.Line, CaretPos.Char]);
  end;

  // InsertMode property has changed
  if Changes * [scAll, scInsertMode, scReadOnly] <> [] then
  begin
    if fSynEdit.ReadOnly then
      Statusbar1.Panels[2].Text := 'ReadOnly'
    else
      Statusbar1.Panels[2].Text := InsertModeStrs[fSynEdit.InsertMode];
  end;

  // Modified property has changed
  if Changes * [scAll, scModified] <> [] then
    Statusbar1.Panels[1].Text := ModifiedStrs[fSynEdit.Modified];
end;

procedure TSEMainForm.PerformSearchReplace(aReplace: Boolean; aBackwards: Boolean);
var
  Options:     TSynSearchOptions;
  CaretPos,
  CaretPosOld: TBufferCoord;
begin
  Statusbar1.Panels[3].Text := '';

  if aReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];

  if aBackwards then
    Include(Options, ssoBackwards);

  if fSearchCaseSensitive then
    Include(Options, ssoMatchCase);

  if fSearchSelection then
    Include(Options, ssoSelectedOnly);

  if fSearchWholeWords then
    Include(Options, ssoWholeWord);

  if fSearchRegex then
    fSynEdit.SearchEngine := TSynEditRegexSearch.Create(fSynEdit)
  else
    fSynEdit.SearchEngine := TSynEditSearch.Create(fSynEdit);

  if fSynEdit.SearchReplace(fSearchText, fReplaceText, Options) = 0 then
  begin
    CaretPosOld := fSynEdit.CaretXY;

    if ssoBackwards in Options then
    begin
      CaretPos.Char := Length(fSynEdit.Lines[fSynEdit.Lines.Count - 1]);
      CaretPos.Line := fSynEdit.Lines.Count;
    end else
    begin
      CaretPos.Char := 1;
      CaretPos.Line := 1;
    end;

    fSynEdit.CaretXY := CaretPos;

    if fSynEdit.SearchReplace(fSearchText, fReplaceText, Options) = 0 then
    begin
      MessageBeep(MB_ICONASTERISK);
      Statusbar1.Panels[3].Text := 'Text not found';
      fSynEdit.CaretXY          := CaretPosOld;
    end;
  end;

  if fReplaceAll then
  begin
    Exclude(Options, ssoPrompt);
    CaretPosOld := fSynEdit.CaretXY;

    if ssoBackwards in Options then
    begin
      CaretPos.Char := Length(fSynEdit.Lines[fSynEdit.Lines.Count - 1]);
      CaretPos.Line := fSynEdit.Lines.Count;
    end else
    begin
      CaretPos.Char := 1;
      CaretPos.Line := 1;
    end;

    fSynEdit.CaretXY := CaretPos;

    if fSynEdit.SearchReplace(fSearchText, fReplaceText, Options) = 0 then
      fSynEdit.CaretXY := CaretPosOld;
  end;

  if fConfirmReplaceDialog <> nil then
    FreeAndNil(fConfirmReplaceDialog);
end;

procedure TSEMainForm.ShowSearchReplaceDialog(aReplace: Boolean);
var
  dlg: TSEFindForm;
begin
  Statusbar1.Panels[3].Text := '';

  if aReplace then
    dlg := TSEReplaceForm.Create(Self)
  else
    dlg := TSEFindForm.Create(Self);

  with dlg do
    try
      // Assign search options
      Backwards         := fSearchBackwards;
      CaseSensitive     := fSearchCaseSensitive;
      InSelection       := fSearchSelection;
      WholeWords        := fSearchWholeWords;
      SearchText        := fSearchText; // Start with last search text
      SearchTextHistory := fSearchTextHistory;
      RegEx             := fSearchRegex;

      // If something is selected search for that text
      if fSynEdit.SelAvail and (fSynEdit.BlockBegin.Line = fSynEdit.BlockEnd.Line) then
        SearchText := fSynEdit.SelText
      else
        SearchText := fSynEdit.GetWordAtRowCol(fSynEdit.CaretXY);

      if aReplace then
        with dlg as TSEReplaceForm do
        begin
          ReplaceText        := fReplaceText;
          ReplaceTextHistory := fReplaceTextHistory;
        end;

      if ShowModal = mrOK then
      begin
        fSearchBackwards        := Backwards;
        fSearchCaseSensitive    := CaseSensitive;
        fSearchSelection        := InSelection;
        fSearchWholeWords       := WholeWords;
        fSearchText             := SearchText;
        fSearchTextHistory      := SearchTextHistory;
        fSearchRegex            := RegEx;
        ActFindForward.Enabled  := True;
        ActFindBackward.Enabled := True;

        if aReplace then
          with dlg as TSEReplaceForm do
          begin
            fReplaceText        := ReplaceText;
            fReplaceTextHistory := ReplaceTextHistory;
          end;

        if fSearchText <> '' then
          PerformSearchReplace(aReplace, fSearchBackwards);
      end;
    finally
      FreeAndNil(dlg);
    end;
end;

procedure TSEMainForm.SynEditorReplaceText(Sender: TObject;
                                           const aSearch, aReplace: UnicodeString;
                                           aLine, aColumn: Integer;
                                           var aAction: TSynReplaceAction);
var
  APos:     TPoint;
  EditRect: TRect;
begin
  if aSearch = aReplace then
    aAction := raSkip
  else
  begin
    EditRect             := ClientRect;
    EditRect.TopLeft     := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);
    APos                 := fSynEdit.ClientToScreen(
                              fSynEdit.RowColumnToPixels(
                                fSynEdit.BufferToDisplayPos(
                                  BufferCoord(aColumn, aLine)
                                )
                              )
                            );

    if fConfirmReplaceDialog = nil then
      fConfirmReplaceDialog := TSEConfirmReplaceForm.Create(Self);

    fConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
                                      APos.Y + fSynEdit.LineHeight, ASearch);

    case fConfirmReplaceDialog.ShowModal of
      mrYesToAll:
      begin
        fReplaceAll := True;
        aAction     := raReplaceAll;
      end;
      mrYes: aAction := raReplace;
      mrNo:  aAction := raSkip;
      else   aAction := raCancel;
    end;
  end;
end;

procedure TSEMainForm.PaintGutterGlyphs(aCanvas: TCanvas; const aClip: TRect;
                                        aFirstLine, aLastLine: Integer);
var
  LH,
  X,
  Y:    Integer;
  item: TScriptValidatorIssue;
const
  ERROR_IMG = 0;
  WARN_IMG  = 20;
  HINT_IMG  = 21;
begin
  if fIssues = nil then
    Exit;

  X  := 2;
  LH := fSynEdit.LineHeight;

  for item in fIssues.Errors do
  begin
    Y := (LH - ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    ilActions16x16.Draw(aCanvas, X, Y, ERROR_IMG);
  end;

  for item in fIssues.Warnings do
  begin
    Y := (LH - ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    ilActions16x16.Draw(aCanvas, X, Y, WARN_IMG);
  end;

  for item in fIssues.Hints do
  begin
    Y := (LH - ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    ilActions16x16.Draw(aCanvas, X, Y, HINT_IMG);
  end;
end;

procedure TSEMainForm.LinesInserted(aFirstLine, aCount: Integer);
begin
  LinesChange(aFirstLine, aCount, False);
end;

procedure TSEMainForm.LinesDeleted(aFirstLine, aCount: Integer);
begin
  LinesChange(aFirstLine, aCount, True);
end;

procedure TSEMainForm.LinesChange(aFirstLine, aCount: Integer; indDeleted: Boolean);
  function ProcessIssues(aIssues: TScriptValidatorIssueArray): TScriptValidatorIssueArray;
  var
    I:     Integer;
    issue: TScriptValidatorIssue;
  begin
    for I := 0 to Length(aIssues) - 1 do
    begin
      issue := aIssues[I];

      if indDeleted then
      begin
        if aFirstLine = issue.Line then
          Continue
        else if aFirstLine < issue.Line then
          issue.Line := issue.Line - aCount;
      end else if aFirstLine <= issue.Line then
        issue.Line := issue.Line + aCount;

      SetLength(Result, Length(Result) + 1);
      Result[I] := issue;
    end;

    fLbIssues.AppendIssues(Result);
  end;
begin
  if fIssues = nil then
    Exit;

  fLbIssues.Clear;

  if Length(fIssues.Hints) > 0 then
    fIssues.Hints    := ProcessIssues(fIssues.Hints);
  if Length(fIssues.Warnings) > 0 then
    fIssues.Warnings := ProcessIssues(fIssues.Warnings);
  if Length(fIssues.Errors) > 0 then
    fIssues.Errors   := ProcessIssues(fIssues.Errors);
end;

procedure TSEMainForm.SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                           var CurrentInput: String; var x, y: Integer;
                                           var CanExecute: Boolean);
const
  LEN_UTILS   = 5;
  LEN_STATES  = 6;
  LEN_ACTIONS = 7;
var
  caretPos: TBufferCoord;
begin
  {case Kind of
    ctCode   : CanExecute := True;
    ctParams : CanExecute := True;
    ctHint   : CanExecute := True;
    else CanExecute := False;
  end;}
  caretPos := fSynEdit.CaretXY;
  fSynCompletion.ItemList.Clear;
  fSynCompletion.InsertList.Clear;

  if fSynEdit.Lines.Text <> '' then
  begin
    if fSynEdit.Lines[caretPos.Line - 1][caretPos.Char - 1] = '.' then
    begin
      if LowerCase(Copy(fSynEdit.Lines[caretPos.Line - 1],
                        caretPos.Char - (LEN_UTILS + 1), LEN_UTILS)) = 'utils' then
      begin
        fSynCompletion.ItemList.AddStrings(fUtilsDict);
        fSynCompletion.InsertList.AddStrings(fUtilsInsDict);
      end else if LowerCase(Copy(fSynEdit.Lines[caretPos.Line - 1],
                        caretPos.Char - (LEN_STATES + 1), LEN_STATES)) = 'states' then
      begin
        fSynCompletion.ItemList.AddStrings(fStatesDict);
        fSynCompletion.InsertList.AddStrings(fStatesInsDict);
      end else if LowerCase(Copy(fSynEdit.Lines[caretPos.Line - 1],
                        caretPos.Char - (LEN_ACTIONS + 1), LEN_ACTIONS)) = 'actions' then
      begin
        fSynCompletion.ItemList.AddStrings(fActionsDict);
        fSynCompletion.InsertList.AddStrings(fActionsInsDict);
      end
    end else
    begin
      fSynCompletion.ItemList.AddStrings(fPasScriptDict);
      fSynCompletion.InsertList.AddStrings(fPasScriptInsDict);
      fSynCompletion.ItemList.AddStrings(fUtilsDict);
      fSynCompletion.InsertList.AddStrings(fUtilsInsDict);
      fSynCompletion.ItemList.AddStrings(fEventsDict);
      fSynCompletion.InsertList.AddStrings(fEventsInsDict);
      fSynCompletion.ItemList.AddStrings(fStatesDict);
      fSynCompletion.InsertList.AddStrings(fStatesInsDict);
      fSynCompletion.ItemList.AddStrings(fActionsDict);
      fSynCompletion.InsertList.AddStrings(fActionsInsDict);
    end;
  end else
  begin
    fSynCompletion.ItemList.AddStrings(fPasScriptDict);
    fSynCompletion.InsertList.AddStrings(fPasScriptInsDict);
    fSynCompletion.ItemList.AddStrings(fEventsDict);
    fSynCompletion.InsertList.AddStrings(fEventsInsDict);
  end;
end;

function TSEMainForm.ShowSaveModified: Boolean;
var
  dlg: TSESaveModifiedForm;
begin
  dlg := TSESaveModifiedForm.Create(Self);

  case dlg.ShowModal of
    mrYes:
    begin
      if fFileName <> '' then
      begin
        fSynEdit.Lines.SaveToFile(fFileName);
        Result := True;
      end else
        Result := ShowSaveAs;
    end;
    mrAll: Result := ShowSaveAs; // Used as Save As
    mrNo:  Result := True;
    else   Result := False;
  end;
end;

function TSEMainForm.ShowSaveAs: Boolean;
begin
  if fFileName <> '' then
  begin
    SaveDialog1.FileName   := ExtractFileName(fFileName);
    SaveDialog1.InitialDir := ExtractFilePath(fFileName);

    if SaveDialog1.Execute then
    begin
      fFileName := SaveDialog1.FileName;
      fSynEdit.Lines.SaveToFile(fFileName);
      fSynEdit.Modified := False;
      Result            := True;
      fSynEdit.ResetModificationIndicator;
    end else
      Result := False;
  end else
  begin
    SaveDialog1.FileName   := '';
    SaveDialog1.InitialDir := fExeDir;

    if SaveDialog1.Execute then
    begin
      fFileName := SaveDialog1.FileName;
      fSynEdit.Lines.SaveToFile(fFileName);
      fSynEdit.Modified := False;
      Result            := True;
      fSynEdit.ResetModificationIndicator;
    end else
      Result := False;
  end;
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
  SV_EXE_FORMAT = 'cmd.exe /C "%sScriptValidator.exe -x ^"%s^""';
begin
  Result  := '';
  Command := Format(SV_EXE_FORMAT, [fExeDir, aFileName]);

  with SA do begin
    nLength              := SizeOf(SA);
    bInheritHandle       := True;
    lpSecurityDescriptor := nil;
  end;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);

  try
    ZeroMemory(@SI, SizeOf(SI));

    with SI do
    begin
      cb          := SizeOf(SI);
      dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput   := GetStdHandle(STD_INPUT_HANDLE); // Don't redirect stdin
      hStdOutput  := StdOutPipeWrite;
      hStdError   := StdOutPipeWrite;
    end;

    if CreateProcess(nil, PChar(Command), nil, nil, True, 0, nil, PChar(fExeDir), SI, PI) then
      try
        CloseHandle(StdOutPipeWrite);
        WaitForSingleObject(PI.hProcess, INFINITE);

        repeat
          ZeroMemory(@Buffer, SizeOf(Buffer));
          WasOK := ReadFile(StdOutPipeRead, Buffer, 256, BytesRead, nil);

          if BytesRead > 0 then
            Result := Result + Buffer;
        until not WasOK or (BytesRead = 0);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

procedure TSEMainForm.RunValidate(const aFileName: string);
var
  resultStr: string;
begin
  if fIssues <> nil then
    FreeAndNil(fIssues);

  fLbIssues.Clear;
  resultStr := ExecuteScriptValidator(aFileName);
  fIssues   := TScriptValidatorResult.Create;
  fIssues.FromXML(resultStr);

  if Length(fIssues.Hints) > 0 then
    fLbIssues.AppendIssues(fIssues.Hints);

  if Length(fIssues.Warnings) > 0 then
    fLbIssues.AppendIssues(fIssues.Warnings);

  if Length(fIssues.Errors) > 0 then
    fLbIssues.AppendIssues(fIssues.Errors);

  fSynEdit.Invalidate;
end;

procedure TSEMainForm.ReadSettings;
var
  iniFile:      TIniFile;
  pcLeftWidth,
  pcRightWidth,
  x,
  y,
  w,
  h:            Integer;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

  try
    x            := iniFile.ReadInteger('Application', 'Left',       0);
    y            := iniFile.ReadInteger('Application', 'Top',        0);
    w            := iniFile.ReadInteger('Application', 'Width',      1024);
    h            := iniFile.ReadInteger('Application', 'Height',     600);
    pcLeftWidth  := iniFile.ReadInteger('Application', 'LeftWidth',  250);
    pcRightWidth := iniFile.ReadInteger('Application', 'RightWidth', 250);

    if (w > 0) and (h > 0) then
      SetBounds(x, y, w, h);

    if iniFile.ReadBool('Application', 'Maximized', False) then
      WindowState := wsMaximized;

    pcLeft.Width  := pcLeftWidth;
    pcRight.Width := pcRightWidth;

    fSearchBackwards     := iniFile.ReadBool('SearchReplace',   'Backwards',      False);
    fSearchCaseSensitive := iniFile.ReadBool('SearchReplace',   'CaseSensitive',  False);
    fSearchSelection     := iniFile.ReadBool('SearchReplace',   'SelectionOnly',  False);
    fSearchWholeWords    := iniFile.ReadBool('SearchReplace',   'WholeWordsOnly', False);
    fSearchRegex         := iniFile.ReadBool('SearchReplace',   'Regex',          False);
    fSearchTextHistory   := iniFile.ReadString('SearchReplace', 'SearchHistory',  '');
    fReplaceTextHistory  := iniFile.ReadString('SearchReplace', 'ReplaceHistory', '');
  finally
    FreeAndNil(iniFile);
  end;
end;

procedure TSEMainForm.WriteSettings;
var
  iniFile: TIniFile;
  wp:      TWindowPlacement;
begin
  iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

  try
    wp.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @wp);

    // form properties
    with wp.rcNormalPosition do
    begin
      iniFile.WriteInteger('Application', 'Left',   Left);
      iniFile.WriteInteger('Application', 'Top',    Top);
      iniFile.WriteInteger('Application', 'Width',  Right - Left);
      iniFile.WriteInteger('Application', 'Height', Bottom - Top);
    end;

    iniFile.WriteBool('Application',     'Maximized',      (WindowState = wsMaximized));
    iniFile.WriteInteger('Application',  'LeftWidth',      pcLeft.Width);
    iniFile.WriteInteger('Application',  'RightWidth',     pcRight.Width);
    iniFile.WriteBool('SearchReplace',   'Backwards',      fSearchBackwards);
    iniFile.WriteBool('SearchReplace',   'CaseSensitive',  fSearchCaseSensitive);
    iniFile.WriteBool('SearchReplace',   'SelectionOnly',  fSearchSelection);
    iniFile.WriteBool('SearchReplace',   'WholeWordsOnly', fSearchWholeWords);
    iniFile.WriteBool('SearchReplace',   'Regex',          fSearchRegex);
    iniFile.WriteString('SearchReplace', 'SearchHistory',  fSearchTextHistory);
    iniFile.WriteString('SearchReplace', 'ReplaceHistory', fReplaceTextHistory);
  finally
    iniFile.Free;
  end;
end;

procedure TSEMainForm.ActAboutSEExecute(Sender: TObject);
var
  dlg: TSEAboutForm;
begin
  dlg := TSEAboutForm.Create(Self);
  dlg.ShowModal;
  FreeAndNil(dlg);
end;

procedure TSEMainForm.ActDocWikiExecute(Sender: TObject);
const
  DOC_WIKI_URL = 'https://github.com/Kromster80/knights_province/wiki';
begin
  if ShellExecute(Application.Handle, 'open', PChar(DOC_WIKI_URL), nil, nil, SW_SHOWNORMAL) <= 32 then
    MessageDlg('Unable to open the documentation wiki.', mtError, [mbOK], 0);
end;

procedure TSEMainForm.ActOptionsExecute(Sender: TObject);
begin
  { TODO 2 -oThimo -cOptions : Add Options dialog }
end;

procedure TSEMainForm.ActValidateExecute(Sender: TObject);
var
  tempFileName: string;
begin
  if fSynEdit.Modified then
  begin
    tempFileName := TPath.GetTempPath + TPath.GetRandomFileName + '.script';
    fSynEdit.Lines.SaveToFile(tempFileName);
    RunValidate(tempFileName);
    DeleteFile(PWideChar(tempFileName));
  end else if fFileName <> '' then
    RunValidate(fFileName);
end;

procedure TSEMainForm.ActValidateUpdate(Sender: TObject);
begin
  ActValidate.Enabled := fSynEdit.Lines.Text <> '';
end;

procedure TSEMainForm.ActRedoExecute(Sender: TObject);
begin
  fSynEdit.Redo;
end;

procedure TSEMainForm.ActRedoUpdate(Sender: TObject);
begin
  ActRedo.Enabled := fSynEdit.CanRedo;
end;

procedure TSEMainForm.ActUndoExecute(Sender: TObject);
begin
  fSynEdit.Undo;
end;

procedure TSEMainForm.ActUndoUpdate(Sender: TObject);
begin
  ActUndo.Enabled := fSynEdit.CanUndo;
end;

procedure TSEMainForm.ActSelectAllExecute(Sender: TObject);
begin
  fSynEdit.SelectAll;
end;

procedure TSEMainForm.ActSelectAllUpdate(Sender: TObject);
begin
  ActSelectAll.Enabled := fSynEdit.Lines.Text <> '';
end;

procedure TSEMainForm.ActCutExecute(Sender: TObject);
begin
  fSynEdit.CutToClipboard;
end;

procedure TSEMainForm.ActCutUpdate(Sender: TObject);
begin
  ActCut.Enabled := fSynEdit.SelAvail;
end;

procedure TSEMainForm.ActCopyExecute(Sender: TObject);
begin
  fSynEdit.CopyToClipboard;
end;

procedure TSEMainForm.ActCopyUpdate(Sender: TObject);
begin
  ActCopy.Enabled := fSynEdit.SelAvail;
end;

procedure TSEMainForm.ActDeleteExecute(Sender: TObject);
begin
  fSynEdit.SelText := '';
end;

procedure TSEMainForm.ActDeleteUpdate(Sender: TObject);
begin
  ActDelete.Enabled := fSynEdit.SelAvail;
end;

procedure TSEMainForm.ActPasteExecute(Sender: TObject);
begin
  fSynEdit.PasteFromClipboard;
end;

procedure TSEMainForm.ActPasteUpdate(Sender: TObject);
begin
  ActPaste.Enabled := fSynEdit.CanPaste and Clipboard.HasFormat(CF_TEXT);
end;

procedure TSEMainForm.ActExitExecute(Sender: TObject);
begin
  SEMainForm.Close;
  Application.Terminate;
end;

procedure TSEMainForm.ActGoToLineExecute(Sender: TObject);
var
  dlg:      TSEGoToLineForm;
  caretPos: TBufferCoord;
begin
  dlg := TSEGoToLineForm.Create(Self);

  with dlg do
    try
      caretPos := fSynEdit.CaretXY;
      Line     := caretPos.Line;
      LineMax  := fSynEdit.Lines.Count;

      if ShowModal = mrOK then
      begin
        caretPos.Char    := 1;
        caretPos.Line    := Line;
        fSynEdit.CaretXY := caretPos;
      end;
    finally
      FreeAndNil(dlg);
    end;
end;

procedure TSEMainForm.ActGoToLineUpdate(Sender: TObject);
begin
  ActGoToLine.Enabled := fSynEdit.Lines.Count > 1;
end;

procedure TSEMainForm.ActFindExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(False);
end;

procedure TSEMainForm.ActFindForwardExecute(Sender: TObject);
begin
  PerformSearchReplace(False, False);
end;

procedure TSEMainForm.ActFindUpdate(Sender: TObject);
begin
  ActFind.Enabled := fSynEdit.Lines.Text <> '';
end;

procedure TSEMainForm.ActFindBackwardExecute(Sender: TObject);
begin
  PerformSearchReplace(False, True);
end;

procedure TSEMainForm.ActReplaceExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(True);
end;

procedure TSEMainForm.ActReplaceUpdate(Sender: TObject);
begin
  ActReplace.Enabled := fSynEdit.Lines.Text <> '';
end;

procedure TSEMainForm.ActNewFileExecute(Sender: TObject);
begin
  if fSynEdit.Modified then
    if not ShowSaveModified then
      Exit;

  fFileName := '';
  fLbIssues.Clear;
  fSynEdit.ClearAll;
  SynEditorStatusChange(Self, [scAll]);
end;

procedure TSEMainForm.ActOpenFileExecute(Sender: TObject);
begin
  if fSynEdit.Modified then
    if not ShowSaveModified then
      Exit;

  if OpenDialog1.Execute then
  begin
    fFileName            := OpenDialog1.FileName;
    OpenDialog1.FileName := '';
    fLbIssues.Clear;
    fSynEdit.ClearAll;
    fSynEdit.Lines.LoadFromFile(fFileName);
  end;

  SynEditorStatusChange(Self, [scAll]);
end;

procedure TSEMainForm.ActSaveFileExecute(Sender: TObject);
begin
  if not fSynEdit.Modified then
    Exit;

  if fFileName <> '' then
  begin
    fSynEdit.Lines.SaveToFile(fFileName);
    fSynEdit.ResetModificationIndicator;
    fSynEdit.Modified := False;
  end else
    ShowSaveAs;

  SynEditorStatusChange(Self, [scAll]);
end;

procedure TSEMainForm.ActSaveFileAsExecute(Sender: TObject);
begin
  ShowSaveAs;
  SynEditorStatusChange(Self, [scAll]);
end;

end.
