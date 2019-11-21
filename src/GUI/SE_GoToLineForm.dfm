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
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 50
    Height = 24
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Raavi'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
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
    Left = 72
    Top = 16
    Width = 129
    Height = 26
    MaxValue = 1
    MinValue = 1
    TabOrder = 0
    Value = 0
  end
end
