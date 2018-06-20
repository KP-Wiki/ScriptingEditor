unit SE_Interfaces;

interface
uses
  Windows, Forms, ComCtrls;

type
  ISEEditor = interface
    procedure Activate;
    function AskSaveChanges: Boolean;
    procedure OpenFile(aFileName: string);
    function CanClose: Boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetModified: Boolean;
    procedure SetCaret(aX, aY: Integer);
  end;

  ISEEditorFactory = interface
    procedure ExecNewFile;
    procedure ExecOpenFile;
    procedure OpenFile(aFileName: string);
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function CreateEditor: ISEEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): ISEEditor;
    procedure RemoveEditor(aEditor: ISEEditor);
    property Editor[Index: Integer]: ISEEditor read GetEditor;
  end;

  ISEEditCommands = interface
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanDelete: Boolean;
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
  end;

  ISEFileCommands = interface
    function CanClose: Boolean;
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure ExecClose;
    procedure ExecSave;
    procedure ExecSaveAs;
  end;

  ISESearchCommands = interface
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function CanFindPrev: Boolean;
    function CanReplace: Boolean;
    function CanGoTo: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    procedure ExecGoTo;
  end;

implementation

end.
