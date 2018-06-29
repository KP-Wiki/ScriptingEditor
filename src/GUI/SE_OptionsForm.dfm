object SEOptionsForm: TSEOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 321
  ClientWidth = 645
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
  object PageControl1: TPageControl
    Left = 16
    Top = 8
    Width = 609
    Height = 265
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Font'
      ExplicitTop = 26
      ExplicitHeight = 235
      object Label2: TLabel
        Left = 13
        Top = 18
        Width = 92
        Height = 24
        AutoSize = False
        Caption = 'Font name:'
        Layout = tlCenter
      end
      object Label1: TLabel
        Left = 13
        Top = 63
        Width = 92
        Height = 26
        AutoSize = False
        Caption = 'Font size:'
        Layout = tlCenter
      end
      object cbFontName: TComboBox
        Left = 111
        Top = 18
        Width = 193
        Height = 24
        AutoDropDown = True
        TabOrder = 0
      end
      object seFontSize: TSpinEdit
        Left = 111
        Top = 63
        Width = 65
        Height = 26
        MaxValue = 30
        MinValue = 1
        TabOrder = 1
        Value = 1
      end
    end
  end
  object btnCancel: TButton
    Left = 544
    Top = 288
    Width = 81
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnSave: TButton
    Left = 16
    Top = 288
    Width = 81
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 2
  end
end
