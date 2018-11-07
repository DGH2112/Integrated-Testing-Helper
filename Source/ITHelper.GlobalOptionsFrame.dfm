object frameGlobalOptions: TframeGlobalOptions
  Left = 0
  Top = 0
  Width = 456
  Height = 426
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlFudgePanel: TPanel
    Left = 0
    Top = 0
    Width = 456
    Height = 426
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 3
    ExplicitTop = 320
    ExplicitWidth = 523
    ExplicitHeight = 481
    DesignSize = (
      456
      426)
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
    object lblShortcuts: TLabel
      Left = 8
      Top = 153
      Width = 54
      Height = 16
      Caption = 'Short&cuts'
      FocusControl = lvShortcuts
    end
    object lblClearMessagesAfter: TLabel
      Left = 8
      Top = 134
      Width = 355
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Clear Messages After Period of Time (seconds)'
    end
    object chkGroupMessages: TCheckBox
      Left = 5
      Top = 8
      Width = 445
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Group Messages into their own tab.'
      TabOrder = 0
      ExplicitWidth = 532
    end
    object lvShortcuts: TListView
      Left = 5
      Top = 172
      Width = 445
      Height = 214
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
      ExplicitWidth = 532
      ExplicitHeight = 659
    end
    object hkShortcut: THotKey
      Left = 8
      Top = 392
      Width = 361
      Height = 22
      Anchors = [akLeft, akRight, akBottom]
      Modifiers = []
      TabOrder = 10
      ExplicitTop = 837
      ExplicitWidth = 448
    end
    object edtZipParams: TEdit
      Left = 103
      Top = 81
      Width = 347
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      ExplicitWidth = 434
    end
    object edtZipEXE: TEdit
      Left = 103
      Top = 54
      Width = 307
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      ExplicitWidth = 394
    end
    object edtClearMessages: TEdit
      Left = 369
      Top = 131
      Width = 63
      Height = 24
      Anchors = [akTop, akRight]
      ReadOnly = True
      TabOrder = 6
      Text = '0'
      ExplicitLeft = 448
    end
    object chkSwitchToMessages: TCheckBox
      Left = 5
      Top = 108
      Width = 445
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Switch To Messages after a Successful compile'
      TabOrder = 5
      ExplicitWidth = 524
    end
    object chkAutoScrollMessages: TCheckBox
      Left = 5
      Top = 31
      Width = 445
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Auto Scroll Messages.'
      TabOrder = 1
      ExplicitWidth = 532
    end
    object btnBrowseZipEXE: TButton
      Left = 416
      Top = 54
      Width = 34
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseZipEXEClick
      ExplicitLeft = 503
    end
    object btnAssign: TBitBtn
      Left = 375
      Top = 389
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Assign'
      Enabled = False
      TabOrder = 9
      OnClick = btnAssignClick
      ExplicitLeft = 462
      ExplicitTop = 834
    end
    object udClearMessages: TUpDown
      Left = 434
      Top = 131
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtClearMessages
      Max = 3600
      Increment = 10
      TabOrder = 7
      ExplicitLeft = 513
    end
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 61
    Top = 278
  end
end
