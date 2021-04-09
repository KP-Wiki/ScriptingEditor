object SEEditorForm: TSEEditorForm
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'SEEditorForm'
  ClientHeight = 338
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object pmEditor: TPopupMenu
    Images = SECommandsDataModule.ilActions16x16
    MenuAnimation = [maLeftToRight, maTopToBottom]
    Left = 24
    Top = 24
    object Undo1: TMenuItem
      Action = SECommandsDataModule.ActUndo
    end
    object Redo1: TMenuItem
      Action = SECommandsDataModule.ActRedo
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Action = SECommandsDataModule.ActCut
    end
    object Copy1: TMenuItem
      Action = SECommandsDataModule.ActCopy
    end
    object Paste1: TMenuItem
      Action = SECommandsDataModule.ActPaste
    end
    object Delete1: TMenuItem
      Action = SECommandsDataModule.ActDelete
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Action = SECommandsDataModule.ActSelectAll
    end
  end
end
