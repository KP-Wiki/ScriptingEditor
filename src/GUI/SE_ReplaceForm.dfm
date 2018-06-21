inherited SEReplaceForm: TSEReplaceForm
  Caption = 'Replace'
  ClientHeight = 281
  OldCreateOrder = True
  ExplicitHeight = 310
  DesignSize = (
    377
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel [1]
    Left = 16
    Top = 56
    Width = 65
    Height = 21
    AutoSize = False
    Caption = 'Replace with:'
    Layout = tlCenter
  end
  inherited cbFindText: TComboBox
    Left = 96
    Width = 265
    ExplicitLeft = 96
    ExplicitWidth = 265
  end
  inherited btnCancel: TButton
    Top = 239
    Height = 27
    TabOrder = 4
    ExplicitTop = 239
    ExplicitHeight = 27
  end
  inherited btnFind: TButton
    Top = 239
    Height = 27
    Caption = 'Replace'
    TabOrder = 3
    ExplicitTop = 239
    ExplicitHeight = 27
  end
  inherited GroupBox1: TGroupBox
    Top = 91
    TabOrder = 2
    ExplicitTop = 91
  end
  object cbReplaceText: TComboBox
    Left = 96
    Top = 56
    Width = 265
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
