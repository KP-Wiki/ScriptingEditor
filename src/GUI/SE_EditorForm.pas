unit SE_EditorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls,
  SynEdit, SynEditTypes, SynEditMiscProcs, SynEditMiscClasses, SynUnicode,
  SynCompletionProposal, SynHighlighterPas,
  ScriptValidatorResult,
  SE_Interfaces, SE_ConfirmReplaceForm;

type
  TSEEditor = class;

  TSEEditorForm = class(TForm)
    pmEditor: TPopupMenu;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    procedure FormShow(aSender: TObject);
    procedure FormClose(aSender: TObject; var aAction: TCloseAction);
    procedure FormCloseQuery(aSender: TObject; var aCanClose: Boolean);
    procedure FormCreate(aSender: TObject);
    procedure FormDestroy(aSender: TObject);
  strict private
    fEditor:               TSEEditor;
    fSynEdit:              TSynEdit;
    fSynPasSyn:            TSynPasSyn;
    fSynCompletion,
    fSynParamCompletion:   TSynCompletionProposal;
    fConfirmReplaceDialog: TSEConfirmReplaceForm;
    fIssues:               TScriptValidatorResult;
    fRawIssues:            string;
    procedure SynCompletionAfterCodeCompletion(Sender: TObject; const Value: string;
                                               Shift: TShiftState; Index: Integer;
                                               EndToken: Char);
    procedure SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                   var CurrentInput: string; var x, y: Integer;
                                   var CanExecute: Boolean);
    procedure SynParamCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                        var CurrentInput: string; var x, y: Integer;
                                        var CanExecute: Boolean);
    procedure SynEditorReplaceText(aSender: TObject; const aSearch, aReplace: UnicodeString;
                                   aLine, aColumn: Integer; var aAction: TSynReplaceAction);
    procedure SynEditorGutterClick(aSender: TObject; aButton: TMouseButton;
                                   aX, aY, aLine: Integer; aMark: TSynEditMark);
    procedure PaintGutterGlyphs(aCanvas: TCanvas; const aClip: TRect;
                                aFirstLine, aLastLine: Integer);
    procedure LinesInserted(aFirstLine, aCount: Integer);
    procedure LinesDeleted(aFirstLine, aCount: Integer);
    procedure LinesChange(aFirstLine, aCount: Integer; indDeleted: Boolean);
    procedure RunValidate(const aFileName: string);
  private
    procedure SynEditorChange(aSender: TObject);
    procedure SynEditorStatusChange(aSender: TObject; aChanges: TSynStatusChanges);
    procedure DoActivate;
    procedure DoValidate;
    procedure DoUpdateCaption;
    function DoAskSaveChanges: Boolean;
    function DoSave: Boolean;
    function DoSaveFile: Boolean;
    function DoSaveAs: Boolean;
    procedure DoSearchReplaceText(aReplace, aBackwards: Boolean);
  public
    procedure ParentTabShow(Sender: TObject);
    procedure ParentTabHide(Sender: TObject);
    procedure DoAssignInterfacePointer(aActive: Boolean);
    procedure ShowSearchReplaceDialog(aReplace: Boolean);
    procedure ReloadTheme;
    property Editor: TSEEditor read fEditor write fEditor;
    property SynEditor: TSynEdit read fSynEdit write fSynEdit;
  end;

  TSEEditor = class(TInterfacedObject, ISEEditor, ISEEditCommands, ISEFileCommands,
                    ISESearchCommands)
  strict private
    fFileName:       string;
    fForm:           TSEEditorForm;
    fHasSelection,
    fIsEmpty,
    fIsReadOnly,
    fModified:       Boolean;
    fUntitledNumber: Integer;
    procedure DoSetFileName(aFileName: string);
  public
    constructor Create(aForm: TSEEditorForm);
    // ISEEditor implementation
    procedure Activate;
    function AskSaveChanges: Boolean;
    procedure OpenFile(aFileName: string);
    procedure Close;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetModified: Boolean;
    procedure SetCaret(aX, aY: Integer);
    procedure ReloadSettings;
    procedure ReloadTheme;
    // ISEEditCommands implementation
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function ISEEditCommands.CanDelete = CanCut;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function CanSelectAll: Boolean;
    function CanValidate: Boolean;
    procedure ExecCut;
    procedure ExecCopy;
    procedure ExecPaste;
    procedure ExecDelete;
    procedure ExecUndo;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecValidate;
    // ISEFileCommands implementation
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure ExecSave;
    procedure ExecSaveAs;
    // ISESearchCommands implementation
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function ISESearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: Boolean;
    function CanGoTo: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    procedure ExecGoTo;
    property FileName:       string        read fFileName       write DoSetFileName;
    property Form:           TSEEditorForm read fForm           write fForm;
    property HasSelection:   Boolean       read fHasSelection   write fHasSelection;
    property IsEmpty:        Boolean       read fIsEmpty        write fIsEmpty;
    property IsReadOnly:     Boolean       read fIsReadOnly     write fIsReadOnly;
    property Modified:       Boolean       read fModified       write fModified;
    property UntitledNumber: Integer       read fUntitledNumber write fUntitledNumber;
  end;

implementation
{$R *.dfm}
uses
  IOUtils, Types,
  SynEditRegexSearch, SynEditSearch,
  SE_Globals, SE_ValidationPlugin, SE_CommandsDataModule, SE_GoToLineForm,
  SE_FindForm, SE_ReplaceForm, SE_SaveModifiedForm, SE_EditorFactory;

type
  TUTF8NoBOMEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

{ TUTF8NoBOMEncoding }

function TUTF8NoBOMEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TSEEditorForm }

procedure TSEEditorForm.FormShow(aSender: TObject);
begin
  DoUpdateCaption;
end;

procedure TSEEditorForm.FormClose(aSender: TObject; var aAction: TCloseAction);
begin
  PostMessage(Parent.Handle, WM_DELETETHIS, 0, 0);
  aAction := caNone;
end;

procedure TSEEditorForm.FormCloseQuery(aSender: TObject; var aCanClose: Boolean);
begin
  // need to prevent this from happening more than once
  if not (csDestroying in ComponentState) then
    aCanClose := DoAskSaveChanges;
end;

procedure TSEEditorForm.FormCreate(aSender: TObject);
begin
  fSynEdit            := TSynEdit.Create(Self);
  fSynPasSyn          := TSynPasSyn.Create(Self);
  fSynCompletion      := TSynCompletionProposal.Create(fSynEdit);
  fSynParamCompletion := TSynCompletionProposal.Create(fSynEdit);

  fSynEdit.Parent                        := Self;
  fSynEdit.Align                         := alClient;
  fSynEdit.Highlighter                   := fSynPasSyn;
  fSynEdit.TabOrder                      := 1;
  fSynEdit.TabWidth                      := 2;
  fSynEdit.WantTabs                      := True;
  fSynEdit.UseCodeFolding                := True;
  fSynEdit.CodeFolding.ShowCollapsedLine := True;
  fSynEdit.CodeFolding.IndentGuides      := True;
  fSynEdit.Gutter.ShowLineNumbers        := True;
  fSynEdit.Gutter.ShowModification       := True;
  fSynEdit.Gutter.AutoSize               := True;
  fSynEdit.OnChange                      := SynEditorChange;
  fSynEdit.OnReplaceText                 := SynEditorReplaceText;
  fSynEdit.OnStatusChange                := SynEditorStatusChange;
  fSynEdit.OnGutterClick                 := SynEditorGutterClick;
  fSynEdit.PopupMenu                     := pmEditor;
  fSynEdit.Options                       := [
    eoAltSetsColumnMode,     eoAutoIndent,         eoAutoSizeMaxScrollWidth,
    eoDisableScrollArrows,   eoDragDropEditing,    eoEnhanceHomeKey,
    eoEnhanceEndKey,         eoGroupUndo,          eoKeepCaretX,
    eoRightMouseMovesCursor, eoScrollPastEol,      eoTabIndent,
    eoTabsToSpaces,          eoTrimTrailingSpaces
    (*
      // Not used:
      eoDropFiles, eoHalfPageScroll, eoHideShowScrollbars, eoNoCaret,
      eoNoSelection, eoScrollByOneLess, eoScrollHintFollows, eoScrollPastEof,
      eoShowScrollHint, eoShowSpecialChars, eoSmartTabDelete, eoSmartTabs,
      eoSpecialLineDefaultFg
    *)
  ];

  fSynCompletion.Editor                := fSynEdit;
  fSynCompletion.Options               := [
    scoLimitToMatchedText, scoUseInsertList,     scoUsePrettyText,
    scoUseBuiltInTimer,    scoEndCharCompletion, scoCompleteWithTab,
    scoCompleteWithEnter
    (*
      // Not used:
      scoCaseSensitive, scoTitleIsCentered, scoConsiderWordBreakChars,
      scoLimitToMatchedTextAnywhere
    *)
  ];
  fSynCompletion.Width                 := 750;
  fSynCompletion.NbLinesInWindow       := 15;
  fSynCompletion.EndOfTokenChr         := '()[] ;';
  fSynCompletion.TriggerChars          := '.';
  fSynCompletion.Title                 := 'Suggested completions';
  fSynCompletion.ClTitleBackground     := clInfoBk;
  fSynCompletion.TimerInterval         := 500;
  fSynCompletion.OnExecute             := SynCompletionExecute;
  fSynCompletion.OnAfterCodeCompletion := SynCompletionAfterCodeCompletion;
  fSynCompletion.ShortCut              := scCtrl + VK_SPACE; //Ctrl+Space
  fSynCompletion.Resizeable            := True;
  fSynCompletion.Columns.Clear;

  with fSynCompletion.Columns.Add do
    ColumnWidth := 96;

  fSynParamCompletion.Editor          := fSynEdit;
  fSynParamCompletion.DefaultType     := ctParams;
  fSynParamCompletion.Options         := [
    scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer
  ];
  fSynParamCompletion.Width           := 262;
  fSynParamCompletion.EndOfTokenChr   := '()[]. ;';
  fSynParamCompletion.TriggerChars    := '(';
  fSynParamCompletion.ClBackground    := clInfoBk;
  fSynParamCompletion.TimerInterval   := 500;
  fSynParamCompletion.OnExecute       := SynParamCompletionExecute;
  fSynParamCompletion.ShortCut        := scShift + scCtrl + VK_SPACE; //Shift+Ctrl+Space
  fSynParamCompletion.Columns.Clear;

  with TSEValidationPlugin.Create(fSynEdit) do
  begin
    OnGutterPaint   := PaintGutterGlyphs;
    OnLinesInserted := LinesInserted;
    OnLinesDeleted  := LinesDeleted;
  end;
end;

procedure TSEEditorForm.FormDestroy(aSender: TObject);
var
  Editor: ISEEditor;
begin
  FreeAndNil(fSynCompletion);
  FreeAndNil(fSynParamCompletion);
  FreeAndNil(fSynEdit);
  FreeAndNil(fSynPasSyn);
  Assert(fEditor <> nil);
  Editor       := fEditor;
  fEditor.Form := nil;
  Assert(gEditorFactory <> nil);
  gEditorFactory.RemoveEditor(Editor);
end;

procedure TSEEditorForm.ParentTabShow(Sender: TObject);
begin
  gMainForm.LbIssues.Clear;
  gMainForm.edtRawSVOutput.Lines.Clear;
  DoAssignInterfacePointer(True);

  if fIssues <> nil then
  begin
    gMainForm.LbIssues.AppendIssues(fIssues.Hints);
    gMainForm.LbIssues.AppendIssues(fIssues.Warnings);
    gMainForm.LbIssues.AppendIssues(fIssues.Errors);
    gMainForm.edtRawSVOutput.Lines.Text := fRawIssues;
  end;

  fEditor.ReloadSettings;
  fSynEdit.Invalidate;
end;

procedure TSEEditorForm.ParentTabHide(Sender: TObject);
begin
  if (Parent.Parent as TPageControl).PageCount <= 1 then
    gMainForm.LbIssues.Clear;

  DoAssignInterfacePointer(False);
end;

procedure TSEEditorForm.SynCompletionAfterCodeCompletion(Sender: TObject; const Value: string;
                                                         Shift: TShiftState; Index: Integer;
                                                         EndToken: Char);
begin
  fSynParamCompletion.ActivateCompletion;
end;

procedure TSEEditorForm.SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                             var CurrentInput: string; var x, y: Integer;
                                             var CanExecute: Boolean);
begin
  CanExecute := True;

  // XE8 has no SetStrings method, hence it's necessary to do the Clear+Add
  fSynCompletion.ItemList.Clear;
  fSynCompletion.InsertList.Clear;
  fSynCompletion.ItemList.AddStrings(gActionsMethodList.GenerateMethodItemList);
  fSynCompletion.InsertList.AddStrings(gActionsMethodList.GenerateMethodInsertNames);
  fSynCompletion.ItemList.AddStrings(gStatesMethodList.GenerateMethodItemList);
  fSynCompletion.InsertList.AddStrings(gStatesMethodList.GenerateMethodInsertNames);
  fSynCompletion.ItemList.AddStrings(gUtilsMethodList.GenerateMethodItemList);
  fSynCompletion.InsertList.AddStrings(gUtilsMethodList.GenerateMethodInsertNames);
  fSynCompletion.ItemList.AddStrings(gPasScriptMethodList.GenerateMethodItemList);
  fSynCompletion.InsertList.AddStrings(gPasScriptMethodList.GenerateMethodInsertNames);
  fSynCompletion.ItemList.AddStrings(gEventsMethodList.GenerateMethodItemList);
  fSynCompletion.InsertList.AddStrings(gEventsMethodList.GenerateMethodInsertNames);
end;

procedure TSEEditorForm.SynParamCompletionExecute(Kind: SynCompletionType; Sender: TObject;
                                                  var CurrentInput: string; var x, y: Integer;
                                                  var CanExecute: Boolean);
var
  lookup,
  curLineStr: string;
  tmpX,
  oldPos,
  startX,
  counter,
  tmpLoc:     Integer;
  found:      Boolean;
begin
  curLineStr := fSynEdit.LineText;
  tmpX       := fSynEdit.CaretX;
  found      := False;
  tmpLoc     := 0;

  // Go back from caret to find the first open bracket
  if tmpX > Length(curLineStr) then
    tmpX := Length(curLineStr)
  else
    Dec(tmpX);

  while (tmpX > 0) and not found do
  begin
    if curLineStr[tmpX] = ',' then
    begin
      Inc(tmpLoc);
      Dec(tmpX);
    end else if curLineStr[tmpX] = ')' then
    begin
      // Found a close, go till a new open is found
      counter := 1;
      Dec(tmpX);

      while (tmpX > 0) and (counter > 0) do
      begin
        if curLineStr[tmpX] = ')' then
          Inc(counter)
        else if curLineStr[tmpX] = '(' then
          Dec(counter);

        Dec(tmpX);
      end;

      if tmpX > 0 then
        Dec(tmpX);
    end else if curLineStr[tmpX] = '(' then
    begin
      // Found a valid open, check what's infront of it
      startX := tmpX;

      while (tmpX > 0) and not CharInSet(curLineStr[tmpX], TSynValidStringChars) do
        Dec(tmpX);

      if tmpX > 0 then
      begin
        oldPos := tmpX;

        while (tmpX > 0) and CharInSet(curLineStr[tmpX], TSynValidStringChars) do
          Dec(tmpX);

        Inc(tmpX);
        lookup := UpperCase(Copy(curLineStr, tmpX, oldPos - tmpX + 1));
        found  := gAllACItems.IndexOf(Lookup) > -1;

        if not found then
        begin
          tmpX := startX;
          Dec(tmpX);
        end;
      end;
    end else
      Dec(tmpX);
  end;

  CanExecute := found;

  if found then
  begin
    fSynParamCompletion.Form.CurrentIndex := tmpLoc;

    if lookup <> fSynParamCompletion.PreviousToken then
    begin
      fSynParamCompletion.ItemList.Clear;
      fSynParamCompletion.ItemList.Add(
        gAllACInserts[gAllACItems.IndexOf(lookup)]
      );
    end else
      fSynParamCompletion.ItemList.Clear;
  end;
end;

procedure TSEEditorForm.SynEditorChange(aSender: TObject);
var
  Empty: Boolean;
begin
  Assert(fEditor <> nil);
  Empty := Trim(fSynEdit.Text) = '';
  fEditor.IsEmpty := Empty;
end;

procedure TSEEditorForm.SynEditorReplaceText(aSender: TObject; const aSearch, aReplace: UnicodeString;
                                             aLine, aColumn: Integer; var aAction: TSynReplaceAction);
var
  aPos:     TPoint;
  EditRect: TRect;
begin
  if aSearch = aReplace then
    aAction := raSkip
  else
  begin
    aPos := fSynEdit.ClientToScreen(
              fSynEdit.RowColumnToPixels(
                fSynEdit.BufferToDisplayPos(BufferCoord(aColumn, aLine))
              )
            );
    EditRect             := ClientRect;
    EditRect.TopLeft     := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if fConfirmReplaceDialog = nil then
      fConfirmReplaceDialog := TSEConfirmReplaceForm.Create(Self);

    fConfirmReplaceDialog.PrepareShow(EditRect, aPos.X, aPos.Y,
                                      aPos.Y + fSynEdit.LineHeight, aSearch);

    case fConfirmReplaceDialog.ShowModal of
      mrYesToAll:
      begin
        gReplaceAll := True;
        aAction     := raReplaceAll;
      end;
      mrYes: aAction := raReplace;
      mrNo:  aAction := raSkip;
      else   aAction := raCancel;
    end;
  end;
end;

procedure TSEEditorForm.SynEditorStatusChange(aSender: TObject; aChanges: TSynStatusChanges);
const
  ModifiedStrs:   array[Boolean] of string = ('',          'Modified');
  InsertModeStrs: array[Boolean] of string = ('Overwrite', 'Insert');
var
  CaretPos: TBufferCoord;
  TabText:  string;
begin
  Assert(fEditor <> nil);

  if aChanges * [scAll, scSelection] <> [] then
    fEditor.HasSelection := fSynEdit.SelAvail;

  if aChanges * [scAll, scModified] <> [] then
  begin
    fEditor.Modified                    := fSynEdit.Modified;
    gMainForm.Statusbar1.Panels[1].Text := ModifiedStrs[fSynEdit.Modified];
    TabText                             := (Parent as TSEEditorTabSheet).Caption;

    if fSynEdit.Modified and not TabText.StartsWith('* ') then
      TabText := '* ' + TabText
    else if TabText.StartsWith('* ') then
      TabText := TabText.Replace('* ', '', []);

    (Parent as TSEEditorTabSheet).Caption := TabText;
  end;

  // Caret position has changed
  if aChanges * [scAll, scCaretX, scCaretY] <> [] then
  begin
    CaretPos                            := fSynEdit.CaretXY;
    gMainForm.Statusbar1.Panels[0].Text := Format('%6d:%3d', [CaretPos.Line, CaretPos.Char]);
  end;

  // InsertMode property has changed
  if aChanges * [scAll, scInsertMode, scReadOnly] <> [] then
  begin
    if fSynEdit.ReadOnly then
    begin
      gMainForm.Statusbar1.Panels[2].Text := 'ReadOnly';
      fEditor.IsReadOnly                  := fSynEdit.ReadOnly;
    end
    else
      gMainForm.Statusbar1.Panels[2].Text := InsertModeStrs[fSynEdit.InsertMode];
  end;
end;

procedure TSEEditorForm.SynEditorGutterClick(aSender: TObject; aButton: TMouseButton;
                                             aX, aY, aLine: Integer; aMark: TSynEditMark);
begin
  fSynEdit.CaretY := aLine;
end;

procedure TSEEditorForm.DoActivate;
var
  Sheet: TTabSheet;
  PCtrl: TPageControl;
begin
  Sheet := TTabSheet(Parent);
  PCtrl := Sheet.PageControl;

  if PCtrl <> nil then
    PCtrl.ActivePage := Sheet;

  gMainForm.SetListboxesVisible(True);
end;

procedure TSEEditorForm.DoAssignInterfacePointer(aActive: boolean);
begin
  if aActive then
  begin
    gActiveEditor := fEditor;
    gEditCmds     := fEditor;
    gFileCmds     := fEditor;
    gSearchCmds   := fEditor;
  end else
  begin
    if gActiveEditor = ISEEditor(fEditor) then
      gActiveEditor := nil;

    if gEditCmds = ISEEditCommands(fEditor) then
      gEditCmds := nil;

    if gFileCmds = ISEFileCommands(fEditor) then
      gFileCmds := nil;

    if gSearchCmds = ISESearchCommands(fEditor) then
      gSearchCmds := nil;
  end;
end;

procedure TSEEditorForm.DoUpdateCaption;
begin
  Assert(fEditor <> nil);
  (Parent as TTabSheet).Caption := fEditor.GetFileTitle;
end;

function TSEEditorForm.DoAskSaveChanges: boolean;
const
  MBType = MB_YESNOCANCEL or MB_ICONQUESTION;
var
  s: string;
begin
  // this is necessary to prevent second confirmation when closing MDI childs
  if fSynEdit.Modified then
  begin
    DoActivate;
    MessageBeep(MB_ICONQUESTION);
    Assert(fEditor <> nil);
    s := Format('The text in the "%s" file has changed.'#13#10#13#10 +
                'Do you want to save the modifications?', [ExtractFileName(fEditor.GetFileTitle)]);

    case Application.MessageBox(PChar(s), PChar(Application.Title), MBType) of
      IDYes: Result := DoSave;
      IDNo:  Result := True;
    else
      Result := False;
    end;
  end else
    Result := True;
end;

function TSEEditorForm.DoSave: boolean;
begin
  Assert(fEditor <> nil);

  if fEditor.FileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TSEEditorForm.DoSaveFile: Boolean;
var
  enc: TEncoding;
begin
  Assert(fEditor <> nil);

  try
    try
      enc := TUTF8NoBOMEncoding.Create;
      fSynEdit.Lines.SaveToFile(fEditor.FileName, enc);
      fSynEdit.Modified := False;
      SynEditorStatusChange(Self, [scAll]);
      fSynEdit.ResetModificationIndicator;
      Result := True;
    except
      Application.HandleException(Self);
      Result := False;
    end;
  finally
    FreeAndNil(enc);
  end;
end;

function TSEEditorForm.DoSaveAs: Boolean;
var
  NewName: string;
begin
  Assert(fEditor <> nil);
  NewName := fEditor.FileName;

  if gCommandsDataModule.GetSaveFileName(NewName) then
  begin
    fEditor.FileName := NewName;
    DoUpdateCaption;
    Result := DoSaveFile;
  end else
    Result := False;
end;

procedure TSEEditorForm.DoSearchReplaceText(aReplace: boolean; aBackwards: boolean);
var
  Options:     TSynSearchOptions;
  CaretPos,
  CaretPosOld: TBufferCoord;
begin
  if aReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];

  if aBackwards then
    Include(Options, ssoBackwards);

  if gSearchCaseSensitive then
    Include(Options, ssoMatchCase);

  if not gSearchFromCaret then
    Include(Options, ssoEntireScope);

  if gSearchSelection then
    Include(Options, ssoSelectedOnly);

  if gSearchWholeWords then
    Include(Options, ssoWholeWord);

  if gSearchRegex then
    fSynEdit.SearchEngine := TSynEditRegexSearch.Create(fSynEdit)
  else
    fSynEdit.SearchEngine := TSynEditSearch.Create(fSynEdit);

  if fSynEdit.SearchReplace(gSearchText, gReplaceText, Options) = 0 then
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

    if fSynEdit.SearchReplace(gSearchText, gReplaceText, Options) = 0 then
    begin
      MessageBeep(MB_ICONASTERISK);
      gMainForm.Statusbar1.Panels[3].Text := 'Text not found';
      fSynEdit.CaretXY          := CaretPosOld;
    end;
  end;

  if gReplaceAll then
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

    if fSynEdit.SearchReplace(gSearchText, gReplaceText, Options) = 0 then
      fSynEdit.CaretXY := CaretPosOld;
  end;

  if fConfirmReplaceDialog <> nil then
    FreeAndNil(fConfirmReplaceDialog);
end;

procedure TSEEditorForm.DoValidate;
var
  tempFileName: string;
  enc:          TEncoding;
begin
  if fSynEdit.Modified then
  begin
    tempFileName := TPath.GetTempPath + TPath.GetRandomFileName + '.script';
    enc          := TUTF8NoBOMEncoding.Create;
    fSynEdit.Lines.SaveToFile(tempFileName, enc);
    RunValidate(tempFileName);
    DeleteFile(PWideChar(tempFileName));
    FreeAndNil(enc);
  end else if fEditor.FileName <> '' then
    RunValidate(fEditor.FileName);
end;

procedure TSEEditorForm.ShowSearchReplaceDialog(aReplace: Boolean);
var
  dlg: TSEFindForm;
begin
  gMainForm.Statusbar1.Panels[3].Text := '';

  if aReplace then
    dlg := TSEReplaceForm.Create(Self)
  else
    dlg := TSEFindForm.Create(Self);

  with dlg do
    try
      // Assign search options
      Backwards         := gSearchBackwards;
      CaseSensitive     := gSearchCaseSensitive;
      FromCursor        := gSearchFromCaret;
      InSelection       := gSearchSelection;
      WholeWords        := gSearchWholeWords;
      SearchText        := gSearchText; // Start with last search text
      SearchTextHistory := gSearchTextHistory;
      RegEx             := gSearchRegex;

      // If something is selected search for that text
      if fSynEdit.SelAvail and (fSynEdit.BlockBegin.Line = fSynEdit.BlockEnd.Line) then
        SearchText := fSynEdit.SelText
      else
        SearchText := fSynEdit.GetWordAtRowCol(fSynEdit.CaretXY);

      if aReplace then
        with dlg as TSEReplaceForm do
        begin
          ReplaceText        := gReplaceText;
          ReplaceTextHistory := gReplaceTextHistory;
        end;

      if ShowModal = mrOK then
      begin
        gSearchBackwards     := Backwards;
        gSearchCaseSensitive := CaseSensitive;
        gSearchFromCaret     := FromCursor;
        gSearchSelection     := InSelection;
        gSearchWholeWords    := WholeWords;
        gSearchText          := SearchText;
        gSearchTextHistory   := SearchTextHistory;
        gSearchRegex         := RegEx;

        if aReplace then
          with dlg as TSEReplaceForm do
          begin
            gReplaceText        := ReplaceText;
            gReplaceTextHistory := ReplaceTextHistory;
          end;

        if gSearchText <> '' then
        begin
          DoSearchReplaceText(aReplace, gSearchBackwards);
          gSearchFromCaret := True;
        end;
      end;
    finally
      FreeAndNil(dlg);
    end;
end;

procedure TSEEditorForm.ReloadTheme;
begin
  case gOptions.Theme of
    tkLight: begin
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.AsmAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.CharAttri.Foreground := StringToColor('clPurple');
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.CommentAttri.Foreground := StringToColor('clGreen');
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.FloatAttri.Foreground := StringToColor('clBlue');
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.HexAttri.Foreground := StringToColor('clBlue');
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.IdentifierAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.NumberAttri.Foreground := StringToColor('clBlue');
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [];
      fSynPasSyn.DirectiveAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.DirectiveAttri.Foreground := StringToColor('clTeal');
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [fsBold];
      fSynPasSyn.KeyAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.KeyAttri.Foreground := StringToColor('clNavy');
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.StringAttri.Foreground := StringToColor('clBlue');
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.SymbolAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.SpaceAttri.Foreground := StringToColor('clWindowText'); // clBlack
    end;
    tkClassic: begin
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := StringToColor('clNavy');
      fSynPasSyn.AsmAttri.Foreground := StringToColor('clLime');
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := StringToColor('clNavy');
      fSynPasSyn.CharAttri.Foreground := StringToColor('clAqua');
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := StringToColor('clNavy');
      fSynPasSyn.CommentAttri.Foreground := StringToColor('clSilver');
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := StringToColor('clNavy');
      fSynPasSyn.FloatAttri.Foreground := StringToColor('clFuchsia');
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := StringToColor('clNavy');
      fSynPasSyn.HexAttri.Foreground := StringToColor('clFuchsia');
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := StringToColor('clNavy');
      fSynPasSyn.IdentifierAttri.Foreground := StringToColor('clYellow');
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := StringToColor('clNavy');
      fSynPasSyn.NumberAttri.Foreground := StringToColor('clFuchsia');
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [];
      fSynPasSyn.DirectiveAttri.Background := StringToColor('clNavy');
      fSynPasSyn.DirectiveAttri.Foreground := StringToColor('clSilver');
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [];
      fSynPasSyn.KeyAttri.Background := StringToColor('clNavy');
      fSynPasSyn.KeyAttri.Foreground := StringToColor('clWhite');
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := StringToColor('clNavy');
      fSynPasSyn.StringAttri.Foreground := StringToColor('clAqua');
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := StringToColor('clNavy');
      fSynPasSyn.SymbolAttri.Foreground := StringToColor('clLime');
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := StringToColor('clNavy');
      fSynPasSyn.SpaceAttri.Foreground := StringToColor('clYellow');
    end;
    tkOcean: begin
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := StringToColor('clAqua');
      fSynPasSyn.AsmAttri.Foreground := StringToColor('clBlue');
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := StringToColor('clAqua');
      fSynPasSyn.CharAttri.Foreground := StringToColor('clPurple');
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := StringToColor('clAqua');
      fSynPasSyn.CommentAttri.Foreground := StringToColor('clTeal');
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := StringToColor('clAqua');
      fSynPasSyn.FloatAttri.Foreground := StringToColor('clOlive');
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := StringToColor('clAqua');
      fSynPasSyn.HexAttri.Foreground := StringToColor('clOlive');
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := StringToColor('clAqua');
      fSynPasSyn.IdentifierAttri.Foreground := StringToColor('clBlue');
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := StringToColor('clAqua');
      fSynPasSyn.NumberAttri.Foreground := StringToColor('clOlive');
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [fsItalic];
      fSynPasSyn.DirectiveAttri.Background := StringToColor('clAqua');
      fSynPasSyn.DirectiveAttri.Foreground := StringToColor('clBlack');
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [];
      fSynPasSyn.KeyAttri.Background := StringToColor('clAqua');
      fSynPasSyn.KeyAttri.Foreground := StringToColor('clBlack');
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := StringToColor('clAqua');
      fSynPasSyn.StringAttri.Foreground := StringToColor('clPurple');
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := StringToColor('clAqua');
      fSynPasSyn.SymbolAttri.Foreground := StringToColor('clBlack');
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := StringToColor('clAqua');
      fSynPasSyn.SpaceAttri.Foreground := StringToColor('clBlack');
    end;
    tkVisualStudio: begin
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.AsmAttri.Foreground := StringToColor('clBlack');
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.CharAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.CommentAttri.Foreground := StringToColor('clGreen');
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.FloatAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.HexAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.IdentifierAttri.Foreground := StringToColor('clBlack');
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.NumberAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [];
      fSynPasSyn.DirectiveAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.DirectiveAttri.Foreground := StringToColor('clBlue');
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [];
      fSynPasSyn.KeyAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.KeyAttri.Foreground := StringToColor('clBlue');
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.StringAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.SymbolAttri.Foreground := StringToColor('clWindowText'); // clBlack
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := StringToColor('clWindow'); // clWhite
      fSynPasSyn.SpaceAttri.Foreground := StringToColor('clWindowText'); // clBlack
    end;
    tkTwilight: begin
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := StringToColor('clBlack');
      fSynPasSyn.AsmAttri.Foreground := StringToColor('clLime');
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := StringToColor('clBlack');
      fSynPasSyn.CharAttri.Foreground := StringToColor('clYellow');
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := StringToColor('clBlack');
      fSynPasSyn.CommentAttri.Foreground := StringToColor('clLime');
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := StringToColor('clBlack');
      fSynPasSyn.FloatAttri.Foreground := StringToColor('clFuchsia');
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := StringToColor('clBlack');
      fSynPasSyn.HexAttri.Foreground := StringToColor('clFuchsia');
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := StringToColor('clBlack');
      fSynPasSyn.IdentifierAttri.Foreground := StringToColor('clWhite');
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := StringToColor('clBlack');
      fSynPasSyn.NumberAttri.Foreground := StringToColor('clFuchsia');
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [fsItalic];
      fSynPasSyn.DirectiveAttri.Background := StringToColor('clWhite');
      fSynPasSyn.DirectiveAttri.Foreground := StringToColor('clGreen');
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [];
      fSynPasSyn.KeyAttri.Background := StringToColor('clBlack');
      fSynPasSyn.KeyAttri.Foreground := StringToColor('clAqua');
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := StringToColor('clBlack');
      fSynPasSyn.StringAttri.Foreground := StringToColor('clYellow');
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := StringToColor('clBlack');
      fSynPasSyn.SymbolAttri.Foreground := StringToColor('clSilver');
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := StringToColor('clBlack');
      fSynPasSyn.SpaceAttri.Foreground := StringToColor('clWhite');
    end;
    tkDark: begin
      {$IFDEF Hy} { Test } {$ENDIF}
      // Assembler
      fSynPasSyn.AsmAttri.Style      := [];
      fSynPasSyn.AsmAttri.Background := $00322F2D;
      fSynPasSyn.AsmAttri.Foreground := $00D0E3F0;
      // Char
      fSynPasSyn.CharAttri.Style      := [];
      fSynPasSyn.CharAttri.Background := $00322F2D;
      fSynPasSyn.CharAttri.Foreground := $00AA7FFF;
      // Comment
      fSynPasSyn.CommentAttri.Style      := [fsItalic];
      fSynPasSyn.CommentAttri.Background := $00322F2D;
      fSynPasSyn.CommentAttri.Foreground := $0084E7BC;
      // Float
      fSynPasSyn.FloatAttri.Style      := [];
      fSynPasSyn.FloatAttri.Background := $00322F2D;
      fSynPasSyn.FloatAttri.Foreground := $00AA7FFF;
      // Hex
      fSynPasSyn.HexAttri.Style      := [];
      fSynPasSyn.HexAttri.Background := $00322F2D;
      fSynPasSyn.HexAttri.Foreground := $00AA7FFF;
      // Identifier
      fSynPasSyn.IdentifierAttri.Style      := [];
      fSynPasSyn.IdentifierAttri.Background := $00322F2D;
      fSynPasSyn.IdentifierAttri.Foreground := $00F0F9FF;
      // Number
      fSynPasSyn.NumberAttri.Style      := [];
      fSynPasSyn.NumberAttri.Background := $00322F2D;
      fSynPasSyn.NumberAttri.Foreground := $00AA7FFF;
      // Preprocessor
      fSynPasSyn.DirectiveAttri.Style      := [];
      fSynPasSyn.DirectiveAttri.Background := $00322F2D;
      fSynPasSyn.DirectiveAttri.Foreground := $00FFCD66;
      // Reserved word
      fSynPasSyn.KeyAttri.Style      := [fsBold];
      fSynPasSyn.KeyAttri.Background := $00322F2D;
      fSynPasSyn.KeyAttri.Foreground := $00BCE0FF;
      // String
      fSynPasSyn.StringAttri.Style      := [];
      fSynPasSyn.StringAttri.Background := $00322F2D;
      fSynPasSyn.StringAttri.Foreground := $00FFAA7F;
      // Symbol
      fSynPasSyn.SymbolAttri.Style      := [];
      fSynPasSyn.SymbolAttri.Background := $00322F2D;
      fSynPasSyn.SymbolAttri.Foreground := $00F0F9FF;
      // Whitespace
      fSynPasSyn.SpaceAttri.Style      := [];
      fSynPasSyn.SpaceAttri.Background := $00322F2D;
      fSynPasSyn.SpaceAttri.Foreground := $00FFFEFA;
    end;
  end;
end;

procedure TSEEditorForm.RunValidate(const aFileName: string);
var
  noIssues: TScriptValidatorIssue;
begin
  if fIssues <> nil then
    FreeAndNil(fIssues);

  gMainForm.LbIssues.Clear;
  gMainForm.edtRawSVOutput.Lines.Clear;
  fRawIssues                          := gMainForm.ExecuteScriptValidator(aFileName).Trim;
  gMainForm.edtRawSVOutput.Lines.Text := fRawIssues;
  fIssues                             := TScriptValidatorResult.Create;
  fIssues.FromXML(fRawIssues);

  if Length(fIssues.Hints) > 0 then
    gMainForm.LbIssues.AppendIssues(fIssues.Hints);

  if Length(fIssues.Warnings) > 0 then
    gMainForm.LbIssues.AppendIssues(fIssues.Warnings);

  if Length(fIssues.Errors) > 0 then
    gMainForm.LbIssues.AppendIssues(fIssues.Errors);

  if (gMainForm.LbIssues.Items.Count = 0) then
  begin
    noIssues.Line   := 0;
    noIssues.Column := 0;
    noIssues.Msg    := 'No issues found. :)';
    gMainForm.LbIssues.AddIssue(noIssues);
  end;

  fSynEdit.Invalidate;
end;

procedure TSEEditorForm.PaintGutterGlyphs(aCanvas: TCanvas; const aClip: TRect;
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
    Y := (LH - gCommandsDataModule.ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    gCommandsDataModule.ilActions16x16.Draw(aCanvas, X, Y, ERROR_IMG);
  end;

  for item in fIssues.Warnings do
  begin
    Y := (LH - gCommandsDataModule.ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    gCommandsDataModule.ilActions16x16.Draw(aCanvas, X, Y, WARN_IMG);
  end;

  for item in fIssues.Hints do
  begin
    Y := (LH - gCommandsDataModule.ilActions16x16.Height) div 2 +
         LH * (fSynEdit.LineToRow(item.Line) - fSynEdit.TopLine);
    gCommandsDataModule.ilActions16x16.Draw(aCanvas, X, Y, HINT_IMG);
  end;
end;

procedure TSEEditorForm.LinesInserted(aFirstLine, aCount: Integer);
begin
  LinesChange(aFirstLine, aCount, False);
end;

procedure TSEEditorForm.LinesDeleted(aFirstLine, aCount: Integer);
begin
  LinesChange(aFirstLine, aCount, True);
end;

procedure TSEEditorForm.LinesChange(aFirstLine, aCount: Integer; indDeleted: Boolean);
  function ProcessIssues(aIssues: TScriptValidatorIssueArray): TScriptValidatorIssueArray;
  var
    I,
    ResultLen: Integer;
    Issue:     TScriptValidatorIssue;
  begin
    for I := 0 to Length(aIssues) - 1 do
    begin
      Issue := aIssues[I];

      if indDeleted then
      begin
        if aFirstLine = Issue.Line then
          Continue
        else if aFirstLine < Issue.Line then
          Issue.Line := Issue.Line - aCount;
      end else if aFirstLine <= Issue.Line then
        Issue.Line := Issue.Line + aCount;

      ResultLen := Length(Result);
      SetLength(Result, ResultLen + 1);
      Result[ResultLen] := Issue;
    end;

    if Length(Result) > 0 then
      gMainForm.LbIssues.AppendIssues(Result);
  end;
begin
  if fIssues = nil then
    Exit;

  gMainForm.LbIssues.Clear;

  if Length(fIssues.Hints) > 0 then
    fIssues.Hints := ProcessIssues(fIssues.Hints);

  if Length(fIssues.Warnings) > 0 then
    fIssues.Warnings := ProcessIssues(fIssues.Warnings);

  if Length(fIssues.Errors) > 0 then
    fIssues.Errors := ProcessIssues(fIssues.Errors);
end;

{ TSEEditor }

constructor TSEEditor.Create(aForm: TSEEditorForm);
begin
  Assert(aForm <> nil);
  inherited Create;
  fForm := aForm;
  fUntitledNumber := -1;
  ReloadTheme;
end;

procedure TSEEditor.DoSetFileName(aFileName: string);
begin
  if aFileName <> fFileName then
  begin
    if IsRelativePath(aFileName) then
      fFileName := aFileName
    else
      fFileName := ExtractRelativePath(ParamStr(0), aFileName);

    if fUntitledNumber <> -1 then
    begin
      gCommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);
      fUntitledNumber := -1;
    end;
  end;
end;

// ISEEditor implementation
procedure TSEEditor.Activate;
begin
  if fForm <> nil then
    fForm.DoActivate;
end;

function TSEEditor.AskSaveChanges: Boolean;
begin
  if fForm <> nil then
    Result := fForm.DoAskSaveChanges
  else
    Result := True;
end;

procedure TSEEditor.OpenFile(aFileName: string);
begin
  DoSetFileName(aFileName);

  if fForm <> nil then
  begin
    fForm.SynEditor.BeginUpdate;
    fForm.SynEditor.Lines.Clear;

    if (fFileName <> '') and FileExists(fFileName) then
      fForm.SynEditor.Lines.LoadFromFile(fFileName)
    else
      fForm.SynEditor.Lines.Clear;

    Application.ProcessMessages;
    fForm.DoUpdateCaption;
    fForm.SynEditorChange(self);
    fForm.SynEditorStatusChange(Self, [scAll]);
    fForm.SynEditor.EndUpdate;
    gMainForm.SetListboxesVisible(True);
  end;
end;

procedure TSEEditor.Close;
begin
  if (fFileName <> '') and (gCommandsDataModule <> nil) then
    gCommandsDataModule.AddMRIEntry(fFileName);

  if fUntitledNumber <> -1 then
    gCommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);

  if fForm <> nil then
    fForm.Close;
end;

function TSEEditor.GetCaretPos: TPoint;
begin
  if fForm <> nil then
    Result := TPoint(fForm.SynEditor.CaretXY)
  else
    Result := Point(-1, -1);
end;

function TSEEditor.GetEditorState: string;
begin
  if fForm <> nil then
  begin
    if fForm.SynEditor.ReadOnly then
      Result := 'Read Only'
    else if fForm.SynEditor.InsertMode then
      Result := 'Insert'
    else
      Result := 'Overwrite';
  end else
    Result := '';
end;

function TSEEditor.GetFileName: string;
begin
  Result := fFileName;
end;

function TSEEditor.GetFileTitle: string;
begin
  if fFileName <> '' then
    Result := ExtractFileName(fFileName)
  else
  begin
    if fUntitledNumber = -1 then
      fUntitledNumber := gCommandsDataModule.GetUntitledNumber;

    Result := 'Untitled' + IntToStr(fUntitledNumber);
  end;
end;

function TSEEditor.GetModified: Boolean;
begin
  if fForm <> nil then
    Result := fForm.SynEditor.Modified
  else
    Result := False;
end;

procedure TSEEditor.SetCaret(aX, aY: Integer);
var
  caretPos: TBufferCoord;
begin
  caretPos.Char           := aX;
  caretPos.Line           := aY;
  fForm.SynEditor.CaretXY := caretPos;
  fForm.SynEditor.SetFocus;
end;

procedure TSEEditor.ReloadSettings;
var
  EditFont: TFont;
begin
  EditFont             := fForm.SynEditor.Font;
  EditFont.Name        := gOptions.Font.Name;
  EditFont.Size        := gOptions.Font.Size;
  fForm.SynEditor.Font := EditFont;
end;

procedure TSEEditor.ReloadTheme;
begin
  fForm.ReloadTheme;
end;

// ISEEditCommands implementation
function TSEEditor.CanCut: Boolean;
begin
  Result := (fForm <> nil) and fHasSelection and not fIsReadOnly;
end;

function TSEEditor.CanCopy: Boolean;
begin
  Result := (fForm <> nil) and fHasSelection;
end;

function TSEEditor.CanPaste: Boolean;
begin
  Result := (fForm <> nil) and fForm.SynEditor.CanPaste;
end;

function TSEEditor.CanUndo: Boolean;
begin
  Result := (fForm <> nil) and fForm.SynEditor.CanUndo;
end;

function TSEEditor.CanRedo: Boolean;
begin
  Result := (fForm <> nil) and fForm.SynEditor.CanRedo;
end;

function TSEEditor.CanSelectAll: Boolean;
begin
  Result := fForm <> nil;
end;

function TSEEditor.CanValidate: Boolean;
begin
  Result := (fForm <> nil) and not fIsEmpty;
end;

procedure TSEEditor.ExecCut;
begin
  if fForm <> nil then
    fForm.SynEditor.CutToClipboard;
end;

procedure TSEEditor.ExecCopy;
begin
  if fForm <> nil then
    fForm.SynEditor.CopyToClipboard;
end;

procedure TSEEditor.ExecPaste;
begin
  if fForm <> nil then
  begin
    fForm.SynEditor.SetFocus;
    fForm.SynEditor.PasteFromClipboard;
  end;
end;

procedure TSEEditor.ExecDelete;
begin
  if fForm <> nil then
    fForm.SynEditor.SelText := '';
end;

procedure TSEEditor.ExecUndo;
begin
  if fForm <> nil then
    fForm.SynEditor.Undo;
end;

procedure TSEEditor.ExecRedo;
begin
  if fForm <> nil then
    fForm.SynEditor.Redo;
end;

procedure TSEEditor.ExecSelectAll;
begin
  if fForm <> nil then
    fForm.SynEditor.SelectAll;
end;

procedure TSEEditor.ExecValidate;
begin
  if fForm <> nil then
    fForm.DoValidate;
end;

// ISEFileCommands implementation
function TSEEditor.CanSave: Boolean;
begin
  Result := (fForm <> nil) and (fModified or (fFileName = ''));
end;

function TSEEditor.CanSaveAs: Boolean;
begin
  Result := fForm <> nil;
end;

procedure TSEEditor.ExecSave;
begin
  if fForm <> nil then
  begin
    if fFileName <> '' then
      fForm.DoSave
    else
      fForm.DoSaveAs
  end;
end;

procedure TSEEditor.ExecSaveAs;
begin
  if fForm <> nil then
    fForm.DoSaveAs;
end;

// ISESearchCommands implementation
function TSEEditor.CanFind: boolean;
begin
  Result := (fForm <> nil) and not fIsEmpty;
end;

function TSEEditor.CanFindNext: boolean;
begin
  Result := (fForm <> nil) and not fIsEmpty and (gSearchText <> '');
end;

function TSEEditor.CanReplace: boolean;
begin
  Result := (fForm <> nil) and not fIsReadOnly and not fIsEmpty;
end;

function TSEEditor.CanGoTo: boolean;
begin
  Result := (fForm <> nil) and not fIsEmpty;
end;

procedure TSEEditor.ExecFind;
begin
  if fForm <> nil then
    fForm.ShowSearchReplaceDialog(False);
end;

procedure TSEEditor.ExecFindNext;
begin
  if fForm <> nil then
    fForm.DoSearchReplaceText(False, False);
end;

procedure TSEEditor.ExecFindPrev;
begin
  if fForm <> nil then
    fForm.DoSearchReplaceText(False, True);
end;

procedure TSEEditor.ExecReplace;
begin
  if fForm <> nil then
    fForm.ShowSearchReplaceDialog(True);
end;

procedure TSEEditor.ExecGoTo;
var
  dlg:       TSEGoToLineForm;
  caretPos:  TBufferCoord;
  cursorPos: TPoint;
begin
  dlg       := TSEGoToLineForm.Create(fForm.SynEditor);
  cursorPos := Mouse.CursorPos;

  with dlg do
    try
      caretPos := fForm.SynEditor.CaretXY;
      Line     := caretPos.Line;
      LineMax  := fForm.SynEditor.Lines.Count;
      Left     := cursorPos.X;
      Top      := cursorPos.Y;

      if ShowModal = mrOK then
      begin
        caretPos.Char           := 1;
        caretPos.Line           := Line;
        fForm.SynEditor.CaretXY := caretPos;
      end;
    finally
      FreeAndNil(dlg);
    end;
end;

end.
