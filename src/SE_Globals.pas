unit SE_Globals;

interface
uses
  Messages, SysUtils, StdCtrls, Classes,
  SE_Interfaces, SE_MainForm, SE_SnippetListBox, SE_IssueListBox, SE_ACMethods;

type
  TSEThemeKind = (
    tkLight, tkClassic, tkOcean, tkVisualStudio, tkTwilight, tkDark
  );

  TSEFontOptions = record
    Name: string;
    Size: Integer;
  end;

  TSEOptions = record
    Theme: TSEThemeKind;
    Font:  TSEFontOptions;
  end;

const
  WM_DELETETHIS = WM_USER + 42;

  MAX_MRI_FILES = 10;

  NON_EXISTING_ITEM = -1;

  VERSION_PREFIX = 'Alpha ';
  VERSION        = '2.1.4';
  VERSION_SUFFIX = '';
  FULL_VERSION   = VERSION_PREFIX + VERSION + VERSION_SUFFIX;

  SYNEDIT_URL = 'https://github.com/SynEdit/SynEdit';
  VSXML_URL   = 'http://blog.spreendigital.de/2011/11/10/verysimplexml-a-lightweight-delphi-xml-reader-and-writer/';

  DATA_DIR          = 'SE_Data' + PathDelim;
  DATA_DIR_LOGS     = DATA_DIR  + 'Logs'     + PathDelim;
  DATA_DIR_EXAMPLES = DATA_DIR  + 'Examples' + PathDelim;

  DICT_FILE_ACTIONS   = 'Actions.xml';
  DICT_FILE_EVENTS    = 'Events.xml';
  DICT_FILE_STATES    = 'States.xml';
  DICT_FILE_UTILS     = 'Utils.xml';
  DICT_FILE_PASSCRIPT = 'PasScript.xml';

  TSynSpecialChars     = [#128..#255]; // MG: special chars. Meaning depends on system encoding/codepage.
  TSynValidStringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z'] + TSynSpecialChars;

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

  gAllACInserts,
  gAllACItems:          TStringList;
  gActionsMethodList,
  gEventsMethodList,
  gStatesMethodList,
  gUtilsMethodList,
  gPasScriptMethodList: TSEMethodList;

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
  Math, ComCtrls, Forms,
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
  welcomeIdx: Integer;
  tab,
  newATab:    TTabSheet;
begin
  if aIndex < 0 then // No tab exists at clicking pos
    Exit;

  tab        := gMainForm.pcEditors.Pages[aIndex];
  welcomeIdx := gCommandsDataModule.GetWelcomePageIndex;

  if gMainForm.pcEditors.PageCount > 1 then
  begin
    newATab    := gMainForm.pcEditors.Pages[
      IfThen(aIndex > 0, aIndex - 1, aIndex + 1)
    ];

    if aIndex = welcomeIdx then
      tab.Free
    else
      gEditorFactory.GetEditor(
        IfThen((welcomeIdx <> -1) and (aIndex > welcomeIdx), aIndex - 1, aIndex)
      ).Close;

    Application.ProcessMessages;
    gMainForm.pcEditors.ActivePage := newATab;
    gMainForm.SetListboxesVisible(not (newATab is TSEWelcomeTabSheet));
  end else
  begin
    if aIndex = welcomeIdx then
      tab.Free
    else
      gEditorFactory.GetEditor(aIndex).Close;

    gMainForm.pcEditors.ActivePage := nil;
    gMainForm.SetListboxesVisible(False);
  end;
end;

end.
