inherited SEReplaceForm: TSEReplaceForm
  Caption = 'Replace'
  ClientHeight = 281
  OldCreateOrder = True
  ExplicitHeight = 310
  DesignSize = (
    497
    281)
  PixelsPerInch = 96
  TextHeight = 16
  inherited Label1: TLabel
    Width = 113
    ExplicitWidth = 113
  end
  object Label2: TLabel [1]
    Left = 16
    Top = 56
    Width = 113
    Height = 24
    AutoSize = False
    Caption = 'Replace with:'
    Layout = tlCenter
  end
  inherited cbFindText: TComboBox
    Left = 135
    Width = 346
    ExplicitLeft = 135
    ExplicitWidth = 346
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
    inherited cbCaseSensitive: TCheckBox
      Width = 225
      ExplicitWidth = 225
    end
    inherited cbWholeWords: TCheckBox
      Width = 225
      ExplicitWidth = 225
    end
    inherited cbSelectedOnly: TCheckBox
      Width = 200
      ExplicitWidth = 200
    end
    inherited cbRegEx: TCheckBox
      Width = 200
      ExplicitWidth = 200
    end
    inherited cbBackwards: TCheckBox
      Width = 225
      ExplicitWidth = 225
    end
    inherited cbFromCursor: TCheckBox
      Width = 225
      ExplicitWidth = 225
    end
  end
  object cbReplaceText: TComboBox
    Left = 135
    Top = 56
    Width = 346
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
