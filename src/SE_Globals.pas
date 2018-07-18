unit SE_Globals;

interface
uses
  Messages, SysUtils, StdCtrls,
  SE_Interfaces, SE_MainForm, SE_SnippetListBox, SE_IssueListBox;

type
  TFontOptions = record
    Name: string;
    Size: Integer;
  end;
  TSEOptions = record
    KPMode:  Boolean;
    Font:    TFontOptions;
  end;

const
  WM_DELETETHIS =  WM_USER + 42;

  MAX_MRI_FILES = 10;

  VERSION_PREFIX = 'Alpha ';
  VERSION        = '1.6.3';
  VERSION_SUFFIX = ' DEV ONLY!';
  FULL_VERSION   = VERSION_PREFIX + VERSION + VERSION_SUFFIX;

  SYNEDIT_URL = 'https://github.com/SynEdit/SynEdit';
  VSXML_URL   = 'http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/';

  DATA_DIR          = 'SE_Data' + PathDelim;
  DATA_DIR_KMR      = DATA_DIR  + 'KMR'      + PathDelim;
  DATA_DIR_KP       = DATA_DIR  + 'KP'       + PathDelim;
  DATA_DIR_LOGS     = DATA_DIR  + 'Logs'     + PathDelim;
  DATA_DIR_EXAMPLES = DATA_DIR  + 'Examples' + PathDelim;

var
  gExeDir:  string;
  gOptions: TSEOptions;

  gSearchFromCaret,
  gSearchBackwards,
  gSearchCaseSensitive,
  gSearchSelection,
  gSearchWholeWords,
  gSearchRegex:         Boolean;
  gSearchText,
  gSearchTextHistory:   string;

  gReplaceAll:         Boolean;
  gReplaceText,
  gReplaceTextHistory: string;

  gMainForm:      TSEMainForm;
  gEditorFactory: ISEEditorFactory;
  gActiveEditor:  ISEEditor;
  gEditCmds:      ISEEditCommands;
  gFileCmds:      ISEFileCommands;
  gSearchCmds:    ISESearchCommands;

procedure SetListboxScrollWidth(aListbox: TListbox);
procedure CloseTab(aIndex: Integer);

implementation

uses
  ComCtrls, Forms,
  SE_WelcomeTab, SE_CommandsDataModule;

procedure SetListboxScrollWidth(aListbox: TListbox);
var
  I, Width, Len: Integer;
begin
  Width := 0; // Reset just to be sure

  for I := 0 to aListbox.Count - 1 do
  begin
    Len := (Length(aListbox.Items[I]) + 1) * 8;

    if Len > Width then
      Width := Len;
  end;

  aListbox.ScrollWidth := Width;
end;

procedure CloseTab(aIndex: Integer);
var
  WelcomeIndex: Integer;
  Tab,
  NewATab:      TTabSheet;
begin
  if aIndex < 0 then
    Exit;

  Tab     := gMainForm.pcEditors.Pages[aIndex];
  NewATab := nil;

  if gMainForm.pcEditors.PageCount > 0 then
  begin
    if aIndex > 0 then
      NewATab := gMainForm.pcEditors.Pages[aIndex - 1]
    else
      NewATab := gMainForm.pcEditors.Pages[aIndex];
  end;

  if Tab is TSEWelcomeTabSheet then
      Tab.Free
  else
  begin
    WelcomeIndex := gCommandsDataModule.GetWelcomePageIndex;

    if (WelcomeIndex <> -1) and (WelcomeIndex < aIndex) then
      gEditorFactory.GetEditor(aIndex - 1).Close
    else
      gEditorFactory.GetEditor(aIndex).Close;
  end;

  Application.ProcessMessages;

  if NewATab <> nil then
  begin
    if NewATab is TSEWelcomeTabSheet then
      gMainForm.SetListboxesVisible(False)
    else
      gMainForm.SetListboxesVisible(True);

    gMainForm.pcEditors.ActivePage := NewATab
  end else
    gMainForm.SetListboxesVisible(False);
end;

end.
