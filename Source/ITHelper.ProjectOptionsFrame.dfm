object frameProjectOptions: TframeProjectOptions
  Left = 0
  Top = 0
  Width = 603
  Height = 467
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    603
    467)
  object lblResExts: TLabel
    Left = 8
    Top = 8
    Width = 225
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Resource Extension Warning Exclusions'
    FocusControl = edtResExts
  end
  object lblVersionInfo: TLabel
    Left = 8
    Top = 80
    Width = 176
    Height = 16
    Caption = '&Copy Version Information from'
    FocusControl = edtVersionInfo
  end
  object btnOpenEXE: TButton
    Left = 566
    Top = 75
    Width = 34
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnOpenEXEClick
  end
  object chkIncrementBuildOnCompile: TCheckBox
    Left = 8
    Top = 54
    Width = 592
    Height = 17
    Caption = '&Increment Build On Compile.'
    TabOrder = 1
  end
  object edtResExts: TEdit
    Left = 8
    Top = 27
    Width = 592
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edtResExts'
  end
  object edtVersionInfo: TEdit
    Left = 204
    Top = 77
    Width = 356
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object gbxVersionInfo: TGroupBox
    Left = 8
    Top = 129
    Width = 592
    Height = 335
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Version Info'
    TabOrder = 5
    DesignSize = (
      592
      335)
    object lblMajor: TLabel
      Left = 8
      Top = 37
      Width = 33
      Height = 16
      Caption = '&Major'
    end
    object lblMinor: TLabel
      Left = 150
      Top = 37
      Width = 32
      Height = 16
      Caption = 'Mi&nor'
    end
    object lblRelease: TLabel
      Left = 285
      Top = 37
      Width = 45
      Height = 16
      Caption = '&Release'
    end
    object lblBuild: TLabel
      Left = 429
      Top = 37
      Width = 27
      Height = 16
      Caption = '&Build'
    end
    object lblResourceName: TLabel
      Left = 8
      Top = 307
      Width = 177
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Resource Path & &Name (exc Ext)'
    end
    object vleVersionInfo: TValueListEditor
      Left = 8
      Top = 61
      Width = 576
      Height = 191
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultRowHeight = 19
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goAlwaysShowEditor, goThumbTracking]
      Strings.Strings = (
        '')
      TabOrder = 8
      ColWidths = (
        150
        420)
      RowHeights = (
        19
        19)
    end
    object edtMajor: TEdit
      Left = 56
      Top = 34
      Width = 58
      Height = 24
      ReadOnly = True
      TabOrder = 0
      Text = '0'
    end
    object upMajor: TUpDown
      Left = 114
      Top = 34
      Width = 16
      Height = 24
      Associate = edtMajor
      TabOrder = 1
      OnClick = BuildChange
    end
    object edtMinor: TEdit
      Left = 196
      Top = 34
      Width = 58
      Height = 24
      ReadOnly = True
      TabOrder = 2
      Text = '0'
    end
    object upMinor: TUpDown
      Left = 254
      Top = 34
      Width = 16
      Height = 24
      Associate = edtMinor
      TabOrder = 3
      OnClick = BuildChange
    end
    object edtRelease: TEdit
      Left = 331
      Top = 34
      Width = 58
      Height = 24
      ReadOnly = True
      TabOrder = 4
      Text = '0'
    end
    object upRelease: TUpDown
      Left = 389
      Top = 34
      Width = 16
      Height = 24
      Associate = edtRelease
      TabOrder = 5
      OnClick = BuildChange
    end
    object edtBuild: TEdit
      Left = 475
      Top = 34
      Width = 58
      Height = 24
      ReadOnly = True
      TabOrder = 6
      Text = '0'
    end
    object upBuild: TUpDown
      Left = 533
      Top = 34
      Width = 16
      Height = 24
      Associate = edtBuild
      Max = 10000
      TabOrder = 7
      OnClick = BuildChange
    end
    object btnGetVersionInfo: TBitBtn
      Left = 450
      Top = 302
      Width = 134
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Get IDE Version Info'
      TabOrder = 12
      OnClick = btnGetVersionInfoClick
    end
    object chkIncludeInProject: TCheckBox
      Left = 8
      Top = 258
      Width = 576
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '&Include Resource in Project'
      TabOrder = 9
    end
    object chkCompileWithBRCC32: TCheckBox
      Left = 8
      Top = 281
      Width = 576
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Compile Resource with BRCC32'
      TabOrder = 10
    end
    object edtResourceName: TEdit
      Left = 196
      Top = 304
      Width = 248
      Height = 24
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 11
      Text = 'edtResourceName'
    end
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 106
    Width = 592
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Enabled ITHelper Version Control'
    TabOrder = 4
    OnClick = chkEnabledClick
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 513
    Top = 8
  end
end
