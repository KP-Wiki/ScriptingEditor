unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, Grids, DBGrids,
  SE_ACMethods, Vcl.ValEdit, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  strict private
    fLbDrawOffset,
    fNbItemsMax:   Integer;
    procedure ListBoxEdit(aItem: Integer);
    procedure AddDictToListBox(const aMethodList: TStrings);
    procedure ReloadDict;
    function GetCurrentDict: string;
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}
uses
  SE_Globals;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ListBoxEdit(ListBox1.Items.Add('New'));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ListBox1.DeleteSelected;
end;

procedure TForm1.Button3Click(Sender: TObject);
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
    
  methodList.SaveToFile(GetCurrentDict);
  FreeAndNil(methodList);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ReloadDict;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  ReloadDict;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ListBox1.Items[Edit1.Tag] := Edit1.Text;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  Edit1.Visible := False;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
  begin
    Edit1.Visible := False;
    Key           := #0;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
Var
  R:          TRect;
  methodList: TSEMethodList;
begin
  gExeDir         := ExtractFilePath(ParamStr(0));
  R               := ListBox1.ItemRect(0);
  fLbDrawOffset   := ListBox1.Width - R.Right;
  fNbItemsMax     := (ListBox1.Height - fLbDrawOffset) Div ListBox1.ItemHeight;
  // make the listbox the exact height of NbItemsMax elements
  ListBox1.Height := fNbItemsMax * ListBox1.ItemHeight + fLbDrawOffset;
  fLbDrawOffset   := fLbDrawOffset Div 2;
  ReloadDict;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  cursorPos: TPoint;
begin
  cursorPos := ListBox1.ScreenToClient(Mouse.CursorPos);
  ListBoxEdit(ListBox1.ItemAtPos(cursorPos, True));
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

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  ReloadDict;
end;

procedure TForm1.ReloadDict;
var
  methodList: TSEMethodList;
begin
  methodList := TSEMethodList.Create;
  
  try
    methodList.LoadFromFile(GetCurrentDict);
    AddDictToListBox(methodList.GenerateMethodItemList);
  finally
    FreeAndNil(methodList);
  end;
end;

function TForm1.GetCurrentDict: string;
begin
  Result := gExeDir;

  case RadioGroup1.ItemIndex of
    0:   Result := Result + DATA_DIR_KMR;
    1:   Result := Result + DATA_DIR_KP;
    else Result := Result + DATA_DIR_KMR;
  end;

  case ComboBox1.ItemIndex of
    0:   Result := Result + DICT_FILE_ACTIONS;
    1:   Result := Result + DICT_FILE_EVENTS;
    2:   Result := Result + DICT_FILE_States;
    3:   Result := Result + DICT_FILE_Utils;
    4:   Result := Result + DICT_FILE_PASSCRIPT;
    else Result := Result + DICT_FILE_ACTIONS;
  end;
end;

end.
