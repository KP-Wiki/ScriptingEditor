unit SE_WelcomeTab;
interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TSEWelcomeTabSheet = class(TTabSheet)
  end;

  TSEWelcomeTab = class(TForm)
    GridPanel1: TGridPanel;
    Label1: TLabel;
    Panel1: TPanel;
    btnNewScript: TSpeedButton;
    btnOpenScript: TSpeedButton;
    Label2: TLabel;
    Panel2: TPanel;
    lbRecentfiles: TListBox;
    Label3: TLabel;
    Panel3: TPanel;
    lbExamples: TListBox;
    Label4: TLabel;
    procedure btnNewScriptClick(Sender: TObject);
    procedure btnOpenScriptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbRecentfilesDblClick(Sender: TObject);
  public
    procedure ParentTabShow(Sender: TObject);
  end;

implementation
uses
  SE_Globals, SE_CommandsDataModule;

{$R *.dfm}

procedure TSEWelcomeTab.ParentTabShow(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  lbRecentfiles.Clear;

  for I := 0 to MAX_MRI_FILES - 1 do
  begin
    S := gCommandsDataModule.GetMRIEntry(I);

    if S <> '' then
      lbRecentfiles.Items.Add(ExtractRelativePath(gExeDir, S));
  end;

  SetListboxScrollWidth(lbRecentfiles);
end;

procedure TSEWelcomeTab.btnNewScriptClick(Sender: TObject);
begin
  gCommandsDataModule.ActNewFileExecute(Self);
end;

procedure TSEWelcomeTab.btnOpenScriptClick(Sender: TObject);
begin
  gCommandsDataModule.ActOpenFileExecute(Self);
end;

procedure TSEWelcomeTab.FormCreate(Sender: TObject);
var
  ExampleDir: string;
  SearchRec: TSearchRec;
begin
  ExampleDir := gExeDir + DATA_DIR_EXAMPLES;

  if not DirectoryExists(ExampleDir) then
    Exit;

  if FindFirst(ExampleDir + '*.script', faAnyFile - faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        lbExamples.Items.Add(ExtractRelativePath(gExeDir, DATA_DIR_EXAMPLES + SearchRec.Name));
      until (FindNext(SearchRec) <> 0);
    finally
      FindClose(SearchRec);
    end;
  end;

  SetListboxScrollWidth(lbExamples);
end;

procedure TSEWelcomeTab.lbRecentfilesDblClick(Sender: TObject);
var
  cursorPos: TPoint;
  Index:     Integer;
begin
  cursorPos := (Sender as TListBox).ScreenToClient(Mouse.CursorPos);
  Index     := (Sender as TListBox).ItemAtPos(cursorPos, True);

  if Index < 0 then
    Exit;

  gEditorFactory.OpenFile((Sender as TListBox).Items[Index]);
end;

end.
