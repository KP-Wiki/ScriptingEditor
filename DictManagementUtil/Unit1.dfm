object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Dict Management Util'
  ClientHeight = 515
  ClientWidth = 1107
  Color = clBtnFace
  Constraints.MaxHeight = 554
  Constraints.MaxWidth = 1123
  Constraints.MinHeight = 554
  Constraints.MinWidth = 1123
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    1107
    515)
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 441
    Width = 77
    Height = 15
    Caption = 'Dictionary:'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 1091
    Height = 427
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 15
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Edit1: TEdit
    Left = 144
    Top = 47
    Width = 121
    Height = 13
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    OnChange = Edit1Change
    OnExit = Edit1Exit
    OnKeyPress = Edit1KeyPress
  end
  object btnAddNew: TButton
    Left = 544
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Add New Item'
    TabOrder = 2
    OnClick = btnAddNewClick
  end
  object btnDeleteSelected: TButton
    Left = 544
    Top = 482
    Width = 161
    Height = 25
    Caption = 'Delete Selected Item'
    TabOrder = 3
    OnClick = btnDeleteSelectedClick
  end
  object btnSave: TButton
    Left = 928
    Top = 482
    Width = 161
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnReset: TButton
    Left = 928
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Reset'
    TabOrder = 5
    OnClick = btnResetClick
  end
  object cbDict: TComboBox
    Left = 8
    Top = 462
    Width = 155
    Height = 23
    AutoComplete = False
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 6
    Text = 'Actions'
    OnChange = cbDictChange
    Items.Strings = (
      'Actions'
      'Events'
      'States'
      'Utils'
      'PasScript')
  end
  object btnA1Import: TButton
    Left = 744
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Import SE A1 Dict'
    TabOrder = 7
    OnClick = btnA1ImportClick
  end
end
