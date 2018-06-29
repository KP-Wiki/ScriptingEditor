object SEMainForm: TSEMainForm
  Left = 0
  Top = 0
  Caption = 'Scripting Editor'
  ClientHeight = 642
  ClientWidth = 1484
  Color = clBtnFace
  Constraints.MinHeight = 700
  Constraints.MinWidth = 1450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 1231
    Top = 24
    Height = 599
    Align = alRight
    ExplicitLeft = 895
    ExplicitTop = 35
    ExplicitHeight = 587
  end
  object Splitter2: TSplitter
    Left = 250
    Top = 24
    Height = 599
    ExplicitLeft = 244
    ExplicitTop = 35
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 623
    Width = 1484
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 64
      end
      item
        Alignment = taCenter
        Width = 60
      end
      item
        Alignment = taCenter
        Width = 64
      end
      item
        Width = 250
      end
      item
        Bevel = pbNone
        Width = 50
      end>
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1484
    Height = 24
    AutoSize = True
    ButtonWidth = 24
    DoubleBuffered = True
    EdgeBorders = [ebLeft, ebTop]
    Images = SECommandsDataModule.ilActions16x16
    List = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    AllowTextButtons = True
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    object tbNewFile: TToolButton
      Left = 0
      Top = 0
      Action = SECommandsDataModule.ActNewFile
    end
    object tbOpen: TToolButton
      Left = 24
      Top = 0
      Action = SECommandsDataModule.ActOpenFile
    end
    object tbSaveFile: TToolButton
      Left = 48
      Top = 0
      Action = SECommandsDataModule.ActSaveFile
    end
    object tbSaveFileAs: TToolButton
      Left = 72
      Top = 0
      Action = SECommandsDataModule.ActSaveFileAs
    end
    object tbSep1: TToolButton
      Left = 96
      Top = 0
      Width = 5
      Caption = 'tbSep1'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object tbCut: TToolButton
      Left = 101
      Top = 0
      Action = SECommandsDataModule.ActCut
    end
    object tbCopy: TToolButton
      Left = 125
      Top = 0
      Action = SECommandsDataModule.ActCopy
    end
    object tbPaste: TToolButton
      Left = 149
      Top = 0
      Action = SECommandsDataModule.ActPaste
    end
    object tbDelete: TToolButton
      Left = 173
      Top = 0
      Action = SECommandsDataModule.ActDelete
    end
    object tbSep2: TToolButton
      Left = 197
      Top = 0
      Width = 5
      Caption = 'tbSep2'
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbUndo: TToolButton
      Left = 202
      Top = 0
      Action = SECommandsDataModule.ActUndo
    end
    object tbRedo: TToolButton
      Left = 226
      Top = 0
      Action = SECommandsDataModule.ActRedo
    end
    object tbSep3: TToolButton
      Left = 250
      Top = 0
      Width = 5
      Caption = 'tbSep3'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tbFind: TToolButton
      Left = 255
      Top = 0
      Action = SECommandsDataModule.ActFind
    end
    object tbReplace: TToolButton
      Left = 279
      Top = 0
      Action = SECommandsDataModule.ActReplace
    end
    object tbGoToLine: TToolButton
      Left = 303
      Top = 0
      Action = SECommandsDataModule.ActGoToLine
    end
    object tbSep4: TToolButton
      Left = 327
      Top = 0
      Width = 5
      Caption = 'tbSep4'
      Down = True
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbValidate: TToolButton
      Left = 332
      Top = 0
      Action = SECommandsDataModule.ActValidate
    end
  end
  object pcLeft: TPageControl
    Left = 0
    Top = 24
    Width = 250
    Height = 599
    ActivePage = tsEvents
    Align = alLeft
    TabOrder = 2
    TabPosition = tpBottom
    object tsEvents: TTabSheet
      Caption = 'Events'
    end
    object tsStates: TTabSheet
      Caption = 'States'
      ImageIndex = 1
    end
    object tsActions: TTabSheet
      Caption = 'Actions'
      ImageIndex = 2
    end
    object tsUtils: TTabSheet
      Caption = 'Utils'
      ImageIndex = 3
    end
  end
  object pcRight: TPageControl
    Left = 1234
    Top = 24
    Width = 250
    Height = 599
    ActivePage = tsIssues
    Align = alRight
    MultiLine = True
    TabOrder = 3
    TabPosition = tpBottom
    object tsIssues: TTabSheet
      Caption = 'Issues'
    end
    object tsRawSVOutput: TTabSheet
      Caption = 'Raw Validator Output'
      ImageIndex = 1
      object edtRawSVOutput: TMemo
        Left = 0
        Top = 0
        Width = 242
        Height = 570
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object pcEditors: TPageControl
    Left = 253
    Top = 24
    Width = 978
    Height = 599
    Align = alClient
    HotTrack = True
    TabOrder = 4
    OnChange = pcEditorsChange
    OnMouseUp = pcEditorsMouseUp
  end
  object MainMenu1: TMainMenu
    Images = SECommandsDataModule.ilActions22x22
    Left = 32
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object MenuNew: TMenuItem
        Action = SECommandsDataModule.ActNewFile
      end
      object MenuOpen: TMenuItem
        Action = SECommandsDataModule.ActOpenFile
      end
      object MenuReopen: TMenuItem
        Caption = 'Reopen'
        OnClick = MenuReopenClick
        object mri1: TMenuItem
          Caption = 'mri1'
          OnClick = MriClick
        end
        object mri2: TMenuItem
          Caption = 'mri2'
          OnClick = MriClick
        end
        object mri3: TMenuItem
          Caption = 'mri3'
          OnClick = MriClick
        end
        object mri4: TMenuItem
          Caption = 'mri4'
          OnClick = MriClick
        end
        object mri5: TMenuItem
          Caption = 'mri5'
          OnClick = MriClick
        end
        object mri6: TMenuItem
          Caption = 'mri6'
          OnClick = MriClick
        end
        object mri7: TMenuItem
          Caption = 'mri7'
          OnClick = MriClick
        end
        object mri8: TMenuItem
          Caption = 'mri8'
          OnClick = MriClick
        end
        object mri9: TMenuItem
          Caption = 'mri9'
          OnClick = MriClick
        end
        object mri10: TMenuItem
          Caption = 'mri10'
          OnClick = MriClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuSave: TMenuItem
        Action = SECommandsDataModule.ActSaveFile
      end
      object MenuSaveAs: TMenuItem
        Action = SECommandsDataModule.ActSaveFileAs
      end
      object Close1: TMenuItem
        Action = SECommandsDataModule.ActCloseFile
      end
      object CloseAll1: TMenuItem
        Action = SECommandsDataModule.ActCloseAllFiles
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuOptions: TMenuItem
        Action = SECommandsDataModule.ActOptions
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Action = SECommandsDataModule.ActExit
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object MenuUndo: TMenuItem
        Action = SECommandsDataModule.ActUndo
      end
      object MenuRedo: TMenuItem
        Action = SECommandsDataModule.ActRedo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuCut: TMenuItem
        Action = SECommandsDataModule.ActCut
      end
      object MenuCopy: TMenuItem
        Action = SECommandsDataModule.ActCopy
      end
      object MenuPaste: TMenuItem
        Action = SECommandsDataModule.ActPaste
      end
      object MenuDelete: TMenuItem
        Action = SECommandsDataModule.ActDelete
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object MenuSelectAll: TMenuItem
        Action = SECommandsDataModule.ActSelectAll
      end
    end
    object Search1: TMenuItem
      Caption = 'Search'
      object MenuFind: TMenuItem
        Action = SECommandsDataModule.ActFind
      end
      object MenuFindForward: TMenuItem
        Action = SECommandsDataModule.ActFindNext
      end
      object MenuFindBackward: TMenuItem
        Action = SECommandsDataModule.ActFindPrevious
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MenuReplace: TMenuItem
        Action = SECommandsDataModule.ActReplace
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MenuGoToLine: TMenuItem
        Action = SECommandsDataModule.ActGoToLine
      end
    end
    object Run1: TMenuItem
      Caption = 'Run'
      object MenuValidate: TMenuItem
        Action = SECommandsDataModule.ActValidate
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object MenuDocWiki: TMenuItem
        Action = SECommandsDataModule.ActDocWiki
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuAboutSE: TMenuItem
        Action = SECommandsDataModule.ActAboutSE
      end
      object ShowWelcomeTab1: TMenuItem
        Action = SECommandsDataModule.ActShowWelcome
      end
    end
    object Mode1: TMenuItem
      Caption = 'Game Mode'
      object MenuKMR: TMenuItem
        Action = SECommandsDataModule.ActModeKMR
        RadioItem = True
      end
      object MenuKP: TMenuItem
        Action = SECommandsDataModule.ActModeKP
        RadioItem = True
      end
    end
  end
  object pmIssues: TPopupMenu
    AutoPopup = False
    MenuAnimation = [maTopToBottom]
    Left = 104
    Top = 40
    object miIssueGoTo: TMenuItem
      Action = SECommandsDataModule.actIssueGoTo
    end
    object miIssueCopy: TMenuItem
      Action = SECommandsDataModule.actIssueCopy
    end
  end
end
