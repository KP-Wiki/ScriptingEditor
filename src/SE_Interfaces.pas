unit SE_Interfaces;

interface
uses
  Windows, Forms, ComCtrls, Graphics;

type
  ISEEditor = interface
    ['{BA1C2652-D994-4D25-B357-A8F6976CF885}']
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
  end;

  ISEEditorFactory = interface
    ['{BE0D9FD3-1721-4D1A-885E-A6B77C4C7939}']
    procedure ExecNewFile;
    procedure ExecOpenFile;
    procedure OpenFile(aFileName: string);
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function CreateEditor: ISEEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): ISEEditor;
    procedure RemoveEditor(aEditor: ISEEditor);
    procedure ReloadTheme;
    property Editor[Index: Integer]: ISEEditor read GetEditor;
  end;

  ISEEditCommands = interface
    ['{4F031C0B-611A-4FF5-B3EC-E6A52543E7D7}']
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
    ['{F1E7B985-973B-4A47-8CB6-A2E799F91763}']
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure ExecSave;
    procedure ExecSaveAs;
  end;

  ISESearchCommands = interface
    ['{EB9716E2-4238-4135-8648-4A5E7E207B8B}']
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
