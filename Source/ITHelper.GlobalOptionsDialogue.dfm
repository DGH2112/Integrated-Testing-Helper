object frmITHGlobalOptionsDialogue: TfrmITHGlobalOptionsDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Global Options'
  ClientHeight = 396
  ClientWidth = 519
  Color = clBtnFace
  Constraints.MinHeight = 421
  Constraints.MinWidth = 525
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    519
    396)
  PixelsPerInch = 96
  TextHeight = 16
  object lblClearMessagesAfter: TLabel
    Left = 8
    Top = 134
    Width = 270
    Height = 16
    Caption = 'Clear Messages After Period of Time (seconds)'
  end
  object lblZIPEXE: TLabel
    Left = 8
    Top = 57
    Width = 82
    Height = 16
    Caption = '&Zip Executable'
  end
  object lblZipParams: TLabel
    Left = 8
    Top = 84
    Width = 87
    Height = 16
    Caption = '&Zip Parameters'
  end
  object lblShortcuts: TLabel
    Left = 8
    Top = 153
    Width = 54
    Height = 16
    Caption = 'Short&cuts'
    FocusControl = lvShortcuts
  end
  object edtClearMessages: TEdit
    Left = 426
    Top = 131
    Width = 63
    Height = 24
    ReadOnly = True
    TabOrder = 6
    Text = '0'
  end
  object udClearMessages: TUpDown
    Left = 495
    Top = 131
    Width = 16
    Height = 21
    Associate = edtClearMessages
    Max = 3600
    Increment = 10
    TabOrder = 7
  end
  object btnBrowseZipEXE: TButton
    Left = 477
    Top = 52
    Width = 34
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseZipEXEClick
  end
  object chkAutoScrollMessages: TCheckBox
    Left = 8
    Top = 31
    Width = 503
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Auto Scroll Messages.'
    TabOrder = 1
  end
  object chkGroupMessages: TCheckBox
    Left = 8
    Top = 8
    Width = 503
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Group Messages into their own tab.'
    TabOrder = 0
  end
  object chkSwitchToMessages: TCheckBox
    Left = 8
    Top = 108
    Width = 503
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Switch To Messages after a Successful compile'
    TabOrder = 5
  end
  object edtZipEXE: TEdit
    Left = 103
    Top = 54
    Width = 368
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edtZipParams: TEdit
    Left = 103
    Top = 81
    Width = 408
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object btnOK: TBitBtn
    Left = 355
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 12
  end
  object btnCancel: TBitBtn
    Left = 436
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 13
  end
  object hkShortcut: THotKey
    Left = 8
    Top = 332
    Width = 422
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    Modifiers = []
    TabOrder = 9
  end
  object btnAssign: TBitBtn
    Left = 436
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Assign'
    TabOrder = 10
    OnClick = btnAssignClick
  end
  object lvShortcuts: TListView
    Left = 8
    Top = 172
    Width = 503
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Action Name'
        Width = 150
      end
      item
        Caption = 'Shortcut'
        Width = 300
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 8
    ViewStyle = vsReport
    OnSelectItem = lvShortcutsSelectItem
  end
  object btnHelp: TBitBtn
    Left = 274
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 11
    OnClick = btnHelpClick
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 381
    Top = 6
  end
end
