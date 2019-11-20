unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, Grids, DBGrids, ValEdit, ExtCtrls,
  SE_ACMethods;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    btnAddNew: TButton;
    btnDeleteSelected: TButton;
    btnSave: TButton;
    btnReset: TButton;
    cbDict: TComboBox;
    Label1: TLabel;
    btnA1Import: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure btnAddNewClick(Sender: TObject);
    procedure btnDeleteSelectedClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure cbDictChange(Sender: TObject);
    procedure rgGameClick(Sender: TObject);
    procedure btnA1ImportClick(Sender: TObject);
  strict private
    fLbDrawOffset,
    fNbItemsMax:   Integer;
    fActiveDict:   string;
    procedure ListBoxEdit(aItem: Integer);
    procedure AddDictToListBox(const aMethodList: TStrings);
    function GetCurrentDict: string;
    procedure LoadDict(indSave: Boolean = False);
    procedure SaveDict;
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}
uses
  UITypes,
  SE_Globals;

procedure TForm1.FormShow(Sender: TObject);
Var
  R: TRect;
begin
  gExeDir         := ExtractFilePath(ParamStr(0));
  R               := ListBox1.ItemRect(0);
  fLbDrawOffset   := ListBox1.Width - R.Right;
  fNbItemsMax     := (ListBox1.Height - fLbDrawOffset) Div ListBox1.ItemHeight;
  // make the listbox the exact height of NbItemsMax elements
  ListBox1.Height := fNbItemsMax * ListBox1.ItemHeight + fLbDrawOffset;
  fLbDrawOffset   := fLbDrawOffset Div 2;
  LoadDict;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveDict;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Edit1.Visible := False;
    Key           := #0;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ListBox1.Items[Edit1.Tag] := Edit1.Text;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  Edit1.Visible := False;
end;

procedure TForm1.btnA1ImportClick(Sender: TObject);
const
  DICT_OLD_FILE_ACTIONS   = 'OLD' + PathDelim + 'Actions.dict';
  DICT_OLD_FILE_EVENTS    = 'OLD' + PathDelim + 'Events.dict';
  DICT_OLD_FILE_STATES    = 'OLD' + PathDelim + 'States.dict';
  DICT_OLD_FILE_UTILS     = 'OLD' + PathDelim + 'Utils.dict';
  DICT_OLD_FILE_PASSCRIPT = 'OLD' + PathDelim + 'PasScript.dict';
var
  I:            Integer;
  s,
  impFile:      string;
  impStrings:   TStringList;
  parsedMethod: TSEMethod;
  methodList:   TSEMethodList;
begin
  impFile := gExeDir + DATA_DIR;

  case cbDict.ItemIndex of
    0:   impFile := impFile + DICT_OLD_FILE_ACTIONS;
    1:   impFile := impFile + DICT_OLD_FILE_EVENTS;
    2:   impFile := impFile + DICT_OLD_FILE_STATES;
    3:   impFile := impFile + DICT_OLD_FILE_UTILS;
    4:   impFile := impFile + DICT_OLD_FILE_PASSCRIPT;
    else impFile := impFile + DICT_OLD_FILE_ACTIONS;
  end;

  if not FileExists(impFile) then
  begin
    MessageDlg('Unable to import: File does not exist.' + sLineBreak + impFile,
               mtError, [mbOK], 0);
    Exit;
  end;

  impStrings := TStringList.Create;
  methodList := TSEMethodList.Create;
  methodList.LoadFromFile(fActiveDict);

  try
    impStrings.LoadFromFile(impFile);

    for I := 0 to impStrings.Count - 1 do
    begin
      s := StringReplace(impStrings[I], '\column{}', '', [rfReplaceAll, rfIgnoreCase]);
      s := StringReplace(s, '\style{+B}', '', [rfReplaceAll, rfIgnoreCase]);
      s := StringReplace(s, '\style{-B}', '', [rfReplaceAll, rfIgnoreCase]);

      if (s <> '') and (LowerCase(s).StartsWith('procedure') or
                        LowerCase(s).StartsWith('function')) then
      begin
        parsedMethod := TSEMethod.ParseMethodStr(s);

        if methodList.IndexByName(parsedMethod.MethodName) = -1 then
          methodList.Add(parsedMethod);
      end;
    end;

    AddDictToListBox(methodList.GenerateMethodItemList);
  finally
    FreeAndNil(impStrings);
    FreeAndNil(methodList);
  end;

  MessageDlg('Successfully imported A1 dict file.' + sLineBreak + impFile,
             mtInformation, [mbOK], 0);
end;

procedure TForm1.btnAddNewClick(Sender: TObject);
begin
  ListBoxEdit(ListBox1.Items.Add('New'));
end;

procedure TForm1.btnDeleteSelectedClick(Sender: TObject);
begin
  ListBox1.DeleteSelected;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  SaveDict;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  LoadDict;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  cursorPos: TPoint;
begin
  cursorPos := ListBox1.ScreenToClient(Mouse.CursorPos);
  ListBoxEdit(ListBox1.ItemAtPos(cursorPos, True));
end;

procedure TForm1.cbDictChange(Sender: TObject);
begin
  LoadDict(True);
end;

procedure TForm1.rgGameClick(Sender: TObject);
begin
  LoadDict(True);
end;

function TForm1.GetCurrentDict: string;
begin
  Result := gExeDir + DATA_DIR;

  case cbDict.ItemIndex of
    0:   Result := Result + DICT_FILE_ACTIONS;
    1:   Result := Result + DICT_FILE_EVENTS;
    2:   Result := Result + DICT_FILE_STATES;
    3:   Result := Result + DICT_FILE_UTILS;
    4:   Result := Result + DICT_FILE_PASSCRIPT;
    else Result := Result + DICT_FILE_ACTIONS;
  end;
end;

procedure TForm1.ListBoxEdit(aItem: Integer);
Var
  R: TRect;
begin
  if (aItem < 0) or (ListBox1.Items.Count = 0) then
    Exit;

  if aItem >= fNbItemsMax then
    ListBox1.TopIndex := aItem - fNbItemsMax + 1;

  ListBox1.ItemIndex := aItem;
  R                  := ListBox1.ItemRect(aItem);
  Edit1.Tag          := aItem; // Set Edit.Tag to the currently edited index
  Edit1.Top          := ListBox1.Top + R.Top + fLbDrawOffset;
  // add 1 to make both lb text and edit text at the same place
  Edit1.Left         := ListBox1.Left + R.Left + fLbDrawOffset + 2; 
  Edit1.Width        := R.Right - R.Left - 2;
  Edit1.Height       := R.Bottom - R.Top;
  Edit1.Text         := ListBox1.Items[aItem];
  Edit1.Visible      := True;
  Edit1.SetFocus;
end;

procedure TForm1.AddDictToListBox(const aMethodList: TStrings);
var
  I: Integer;
  s: string;
begin
  ListBox1.Items.Clear;

  for I := 0 to aMethodList.Count - 1 do
  begin
    s := StringReplace(aMethodList[I], '\column{}', '', [rfReplaceAll, rfIgnoreCase]);

    if s <> '' then
      ListBox1.Items.Add(s);
  end;
end;

procedure TForm1.LoadDict(indSave: Boolean = False);
var
  methodList: TSEMethodList;
begin
  if indSave then
    SaveDict;

  fActiveDict := GetCurrentDict;
  methodList  := TSEMethodList.Create;

  try
    methodList.LoadFromFile(fActiveDict);
    AddDictToListBox(methodList.GenerateMethodItemList);
  finally
    FreeAndNil(methodList);
  end;
end;

procedure TForm1.SaveDict;
var
  methodList: TSEMethodList;
  I:          Integer;
  item:       string;
begin
  methodList := TSEMethodList.Create;

  for I := 0 to ListBox1.Items.Count - 1 do
  begin
    item := ListBox1.Items[I];

    if (item <> '') and (LowerCase(item) <> 'new') then
      methodList.Add(TSEMethod.ParseMethodStr(ListBox1.Items[I]));
  end;

  methodList.SaveToFile(fActiveDict);
  FreeAndNil(methodList);
end;

end.
