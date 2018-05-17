object SEAboutForm: TSEAboutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About Scripting Editor'
  ClientHeight = 161
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 129
    Height = 17
    AutoSize = False
    Caption = 'Created by:'
    Layout = tlCenter
  end
  object Label7: TLabel
    Left = 16
    Top = 96
    Width = 129
    Height = 17
    AutoSize = False
    Caption = 'Third-party components:'
  end
  object StaticText1: TStaticText
    Left = 152
    Top = 56
    Width = 273
    Height = 17
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Thimo ( Thibmo)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 16
    Top = 16
    Width = 409
    Height = 17
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkSoft
    Caption = 'Dynamic Script editor for KaM Remake and Knights Province'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object stSynEdit: TStaticText
    Left = 152
    Top = 96
    Width = 273
    Height = 17
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkTile
    BevelOuter = bvSpace
    Caption = 'SynEdit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = StaticTextClick
  end
  object stVerySimpleXML: TStaticText
    Left = 152
    Top = 128
    Width = 273
    Height = 17
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    BevelKind = bkTile
    BevelOuter = bvSpace
    Caption = 'VerySimpleXML'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = StaticTextClick
  end
end
