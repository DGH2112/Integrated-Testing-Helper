object frameGlobalOptions: TframeGlobalOptions
  Left = 0
  Top = 0
  Width = 529
  Height = 405
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    529
    405)
  object lblShortcuts: TLabel
    Left = 8
    Top = 153
    Width = 54
    Height = 16
    Caption = 'Short&cuts'
    FocusControl = lvShortcuts
  end
  object lblZipParams: TLabel
    Left = 8
    Top = 84
    Width = 87
    Height = 16
    Caption = '&Zip Parameters'
  end
  object lblZIPEXE: TLabel
    Left = 8
    Top = 57
    Width = 82
    Height = 16
    Caption = '&Zip Executable'
  end
  object lblClearMessagesAfter: TLabel
    Left = 8
    Top = 134
    Width = 270
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Clear Messages After Period of Time (seconds)'
  end
  object btnAssign: TBitBtn
    Left = 451
    Top = 377
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Assign'
    Enabled = False
    TabOrder = 10
    OnClick = btnAssignClick
  end
  object lvShortcuts: TListView
    Left = 8
    Top = 172
    Width = 518
    Height = 202
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
  object hkShortcut: THotKey
    Left = 8
    Top = 380
    Width = 437
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    Modifiers = []
    TabOrder = 9
  end
  object edtZipParams: TEdit
    Left = 103
    Top = 81
    Width = 423
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object edtZipEXE: TEdit
    Left = 103
    Top = 54
    Width = 383
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object chkSwitchToMessages: TCheckBox
    Left = 8
    Top = 108
    Width = 518
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Switch To Messages after a Successful compile'
    TabOrder = 5
  end
  object chkGroupMessages: TCheckBox
    Left = 8
    Top = 8
    Width = 518
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Group Messages into their own tab.'
    TabOrder = 0
  end
  object chkAutoScrollMessages: TCheckBox
    Left = 8
    Top = 31
    Width = 518
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Auto Scroll Messages.'
    TabOrder = 1
  end
  object btnBrowseZipEXE: TButton
    Left = 492
    Top = 50
    Width = 34
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseZipEXEClick
  end
  object udClearMessages: TUpDown
    Left = 504
    Top = 131
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtClearMessages
    Max = 3600
    Increment = 10
    TabOrder = 7
  end
  object edtClearMessages: TEdit
    Left = 441
    Top = 131
    Width = 63
    Height = 24
    Anchors = [akTop, akRight]
    ReadOnly = True
    TabOrder = 6
    Text = '0'
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 381
    Top = 6
  end
end
