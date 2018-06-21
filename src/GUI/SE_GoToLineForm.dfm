object SEGoToLineForm: TSEGoToLineForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Go To Line'
  ClientHeight = 89
  ClientWidth = 217
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
    Top = 16
    Width = 23
    Height = 22
    AutoSize = False
    Caption = 'Line:'
    Layout = tlCenter
  end
  object btnCancel: TButton
    Left = 120
    Top = 54
    Width = 81
    Height = 27
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnGo: TButton
    Left = 16
    Top = 54
    Width = 81
    Height = 27
    Caption = 'Go'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object seLine: TSpinEdit
    Left = 56
    Top = 16
    Width = 145
    Height = 22
    EditorEnabled = False
    MaxValue = 1
    MinValue = 1
    TabOrder = 0
    Value = 0
  end
end
