object SEAboutForm: TSEAboutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About Scripting Editor'
  ClientHeight = 209
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 104
    Width = 193
    Height = 20
    AutoSize = False
    Caption = 'Created by:'
    Layout = tlCenter
  end
  object Label7: TLabel
    Left = 16
    Top = 144
    Width = 193
    Height = 20
    AutoSize = False
    Caption = 'Third-party components:'
  end
  object StaticText1: TStaticText
    Left = 224
    Top = 104
    Width = 273
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Thimo ( Thibmo)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 16
    Top = 16
    Width = 481
    Height = 65
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkSoft
    Caption = 
      'Dynamic Script editor for KaM Remake and Knights Province'#13#10#13#10'Ver' +
      'sion x.x.x'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object stSynEdit: TStaticText
    Left = 224
    Top = 144
    Width = 273
    Height = 20
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkTile
    BevelOuter = bvSpace
    Caption = 'SynEdit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = StaticTextClick
  end
  object stVerySimpleXML: TStaticText
    Left = 224
    Top = 176
    Width = 273
    Height = 20
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkTile
    BevelOuter = bvSpace
    Caption = 'VerySimpleXML'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = StaticTextClick
  end
end
