object SEFindForm: TSEFindForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 241
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    497
    241)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 49
    Height = 24
    AutoSize = False
    Caption = 'Find:'
    Layout = tlCenter
  end
  object cbFindText: TComboBox
    Left = 71
    Top = 16
    Width = 410
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 475
  end
  object btnCancel: TButton
    Left = 318
    Top = 200
    Width = 163
    Height = 28
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnFind: TButton
    Left = 16
    Top = 200
    Width = 163
    Height = 28
    Caption = 'Find'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 51
    Width = 465
    Height = 134
    Caption = 'Options'
    TabOrder = 1
    object cbCaseSensitive: TCheckBox
      Left = 16
      Top = 24
      Width = 217
      Height = 17
      Caption = '&Match case'
      TabOrder = 0
    end
    object cbWholeWords: TCheckBox
      Left = 16
      Top = 47
      Width = 217
      Height = 17
      Caption = 'Match &whole words only'
      TabOrder = 1
    end
    object cbSelectedOnly: TCheckBox
      Left = 262
      Top = 24
      Width = 179
      Height = 17
      Caption = '&Selected text only'
      TabOrder = 2
    end
    object cbRegEx: TCheckBox
      Left = 262
      Top = 47
      Width = 179
      Height = 17
      Caption = '&Regular expression'
      TabOrder = 3
    end
    object cbBackwards: TCheckBox
      Left = 16
      Top = 103
      Width = 217
      Height = 17
      Caption = '&Backwards direction'
      TabOrder = 4
    end
    object cbFromCursor: TCheckBox
      Left = 16
      Top = 70
      Width = 217
      Height = 17
      Caption = 'Search from &caret'
      TabOrder = 5
    end
  end
end
