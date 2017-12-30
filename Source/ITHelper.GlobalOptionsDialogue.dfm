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
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    519
    396)
  PixelsPerInch = 96
  TextHeight = 13
  object lblClearMessagesAfter: TLabel
    Left = 8
    Top = 134
    Width = 224
    Height = 13
    Caption = 'Clear Messages After Period of Time (seconds)'
  end
  object lblZIPEXE: TLabel
    Left = 8
    Top = 57
    Width = 70
    Height = 13
    Caption = '&Zip Executable'
  end
  object lblZipParams: TLabel
    Left = 8
    Top = 84
    Width = 72
    Height = 13
    Caption = '&Zip Parameters'
  end
  object lblShortcuts: TLabel
    Left = 8
    Top = 153
    Width = 46
    Height = 13
    Caption = 'Short&cuts'
    FocusControl = lvShortcuts
  end
  object edtClearMessages: TEdit
    Left = 244
    Top = 131
    Width = 63
    Height = 21
    ReadOnly = True
    TabOrder = 6
    Text = '0'
  end
  object udClearMessages: TUpDown
    Left = 307
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
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edtZipParams: TEdit
    Left = 103
    Top = 81
    Width = 408
    Height = 21
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
    TabOrder = 13
  end
  object btnCancel: TBitBtn
    Left = 436
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 14
  end
  object btnCheckForUpdates: TBitBtn
    Left = 8
    Top = 360
    Width = 209
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Configure Check for Updates...'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDDDDDDDDDDDDDDDDCDCDCDDCDCDDDDDDCDCDCDDCDCDDDCDDCCCDCDDCDCC
      CDDDDCDC1CDDCDCDCDCDDCD9CCCCCCCCCDDDDDDD1DDDDDDDDDDDDDD91DDDDDA2
      DDDDDDD91DDDDDAA2DDDDDDD91DDDAAAA2DDDDDDD91DDA2DAA2DDDDDD91DAADD
      DAA2D91119DDADDDDDAADD999DDDDDDDDDDADDDDDDDDDDDDDDDD}
    TabOrder = 8
  end
  object hkShortcut: THotKey
    Left = 8
    Top = 332
    Width = 422
    Height = 19
    Anchors = [akLeft, akRight, akBottom]
    Modifiers = []
    TabOrder = 10
  end
  object btnAssign: TBitBtn
    Left = 436
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Assign'
    TabOrder = 11
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
    TabOrder = 9
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
    TabOrder = 12
    OnClick = btnHelpClick
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 381
    Top = 6
  end
end
