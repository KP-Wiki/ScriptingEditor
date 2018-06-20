unit SE_Globals;

interface
uses
  Messages, SysUtils, StdCtrls,
  SE_Interfaces, SE_MainForm, SE_SnippetListBox, SE_IssueListBox;

const
  WM_DELETETHIS =  WM_USER + 42;

  MAX_MRI_FILES = 10;

  VERSION_PREFIX = 'Alpha ';
  VERSION        = '1.5.2';
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
  gExeDir: string;

  gKPMode: Boolean;

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

implementation

procedure SetListboxScrollWidth(aListbox: TListbox);
var
  I, Width: Integer;
begin
  Width := 0; // Reset just to be sure

  for I := 0 to aListbox.Count - 1 do
    if aListbox.Canvas.TextWidth(aListbox.Items[I] + 'W') > Width then
      Width := aListbox.Canvas.TextWidth(aListbox.Items[I] + 'W');

  aListbox.ScrollWidth := Width;
end;

end.
