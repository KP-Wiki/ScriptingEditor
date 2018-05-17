object SEFindForm: TSEFindForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 209
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    377
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 23
    Height = 21
    AutoSize = False
    Caption = 'Find:'
    Layout = tlCenter
  end
  object cbFindText: TComboBox
    Left = 56
    Top = 16
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 198
    Top = 173
    Width = 163
    Height = 28
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnFind: TButton
    Left = 16
    Top = 173
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
    Width = 345
    Height = 110
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      345
      110)
    object cbCaseSensitive: TCheckBox
      Left = 16
      Top = 24
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Match &case'
      TabOrder = 0
    end
    object cbWholeWords: TCheckBox
      Left = 16
      Top = 47
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Match &whole words only'
      TabOrder = 1
    end
    object cbSelectedOnly: TCheckBox
      Left = 182
      Top = 24
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Selected text only'
      TabOrder = 2
    end
    object cbRegEx: TCheckBox
      Left = 182
      Top = 47
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Regular expression'
      TabOrder = 3
    end
    object cbBackwards: TCheckBox
      Left = 16
      Top = 79
      Width = 147
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Backwards direction'
      TabOrder = 4
    end
  end
end
