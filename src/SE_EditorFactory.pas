unit SE_EditorFactory;

interface
uses
  Messages, Classes, ComCtrls,
  SE_Globals, SE_Interfaces;

type
  TSEEditorTabSheet = class(TTabSheet)
  private
    procedure WMDeleteThis(var aMsg: TMessage); message WM_DELETETHIS;
  end;

  TSEEditorFactory = class(TInterfacedObject, ISEEditorFactory)
  strict private
    fEditors: TInterfaceList;
    procedure DoOpenFile(aFileName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecNewFile;
    procedure ExecOpenFile;
    procedure OpenFile(aFileName: string);
    function CanCloseAll: boolean;
    procedure CloseAll;
    function CreateEditor: ISEEditor;
    function GetEditorCount: Integer;
    function GetEditor(aIndex: Integer): ISEEditor;
    procedure RemoveEditor(aEditor: ISEEditor);
  end;

implementation
uses
  Controls, SysUtils,
  SE_EditorForm, SE_CommandsDataModule;

{ TSEEditorTabSheet }

procedure TSEEditorTabSheet.WMDeleteThis(var aMsg: TMessage);
begin
  Free;
end;

{ TSEEditorFactory }

constructor TSEEditorFactory.Create;
begin
  inherited Create;
  fEditors := TInterfaceList.Create;
end;

destructor TSEEditorFactory.Destroy;
begin
  fEditors.Free;
  inherited Destroy;
end;

procedure TSEEditorFactory.ExecNewFile;
begin
  DoOpenFile('');
end;

procedure TSEEditorFactory.ExecOpenFile;
begin
  with gCommandsDataModule.dlgFileOpen do
  begin
    if Execute then
      DoOpenFile(FileName);
  end;
end;

procedure TSEEditorFactory.OpenFile(aFileName: string);
begin
  DoOpenFile(aFileName);
end;

function TSEEditorFactory.CanCloseAll: boolean;
var
  I:      Integer;
  Editor: ISEEditor;
begin
  for I := fEditors.Count - 1 downto 0 do
  begin
    Editor := ISEEditor(fEditors[I]);

    if not Editor.AskSaveChanges then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := TRUE;
end;

procedure TSEEditorFactory.CloseAll;
var
  I: Integer;
begin
  for I := fEditors.Count - 1 downto 0 do
    ISEEditor(fEditors[I]).Close;
end;

function TSEEditorFactory.CreateEditor: ISEEditor;
var
  Sheet:   TTabSheet;
  newForm: TSEEditorForm;
begin
  Sheet := TSEEditorTabSheet.Create(gMainForm.pcEditors);

  try
    Sheet.PageControl := gMainForm.pcEditors;
    newForm           := TSEEditorForm.Create(Sheet);
    Sheet.OnShow      := newForm.ParentTabShow;
    Sheet.OnHide      := newForm.ParentTabHide;

    with newForm do
    begin
      Editor  := TSEEditor.Create(newForm);
      Result  := Editor;
      Parent  := Sheet;
      Visible := True;
    end;

    gMainForm.pcEditors.ActivePage := Sheet;
    newForm.SetFocus;
    newForm.Realign;

    if Result <> nil then
    begin
      if fEditors.Count < 1 then
        newForm.DoAssignInterfacePointer(True);

      fEditors.Add(Result);
    end;
  except
    Sheet.Free;
  end;
end;

function TSEEditorFactory.GetEditorCount: integer;
begin
  Result := fEditors.Count;
end;

function TSEEditorFactory.GetEditor(aIndex: Integer): ISEEditor;
begin
  Result := ISEEditor(fEditors[aIndex]);
end;

procedure TSEEditorFactory.RemoveEditor(aEditor: ISEEditor);
var
  I: Integer;
begin
  I := fEditors.IndexOf(aEditor);

  if I > -1 then
    fEditors.Delete(I);
end;

procedure TSEEditorFactory.DoOpenFile(aFileName: string);
var
  I:       Integer;
  LEditor: ISEEditor;
begin
  AFileName := ExpandFileName(aFileName);

  if aFileName <> '' then
  begin
    gCommandsDataModule.RemoveMRIEntry(aFileName);

    // activate the editor if already open
    for I := fEditors.Count - 1 downto 0 do
    begin
      LEditor := ISEEditor(fEditors[I]);

      if CompareText(LEditor.GetFileName, aFileName) = 0 then
      begin
        LEditor.Activate;
        Exit;
      end;
    end;
  end;

  // create a new editor, add it to the editor list, open the file
  LEditor := CreateEditor;

  if LEditor <> nil then
    LEditor.OpenFile(aFileName);
end;

initialization
  gEditorFactory := TSEEditorFactory.Create;
finalization
  gEditorFactory := nil;

end.
