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
  OnShow = FormShow
  DesignSize = (
    1107
    515)
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 199
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
  object Button1: TButton
    Left = 728
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Add New Item'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 728
    Top = 482
    Width = 161
    Height = 25
    Caption = 'Delete Selected Item'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 928
    Top = 482
    Width = 161
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 928
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Reset'
    TabOrder = 5
    OnClick = Button4Click
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 441
    Width = 161
    Height = 66
    Caption = 'Game'
    ItemIndex = 0
    Items.Strings = (
      'KaM Remake'
      'Knights Province')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
  object ComboBox1: TComboBox
    Left = 199
    Top = 462
    Width = 155
    Height = 23
    AutoComplete = False
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 7
    Text = 'Actions'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Actions'
      'Events'
      'States'
      'Utils'
      'PasScript')
  end
end
