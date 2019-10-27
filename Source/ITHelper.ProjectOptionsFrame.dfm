object frameProjectOptions: TframeProjectOptions
  Left = 0
  Top = 0
  Width = 777
  Height = 720
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object lblResExts: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 771
    Height = 16
    Align = alTop
    Caption = '&Resource Extension Warning Exclusions'
    FocusControl = edtResExts
    ExplicitTop = 0
  end
  object lblIncrementOnCompileMode: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 55
    Width = 771
    Height = 16
    Align = alTop
    Caption = '&Increment on Compile Mode'
    ExplicitLeft = 12
    ExplicitTop = 33
  end
  object edtResExts: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 771
    Height = 24
    Align = alTop
    TabOrder = 0
    Text = 'edtResExts'
    ExplicitTop = 11
  end
  object gbxVersionInfo: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 263
    Width = 771
    Height = 454
    Align = alClient
    Caption = 'Version Info'
    TabOrder = 2
    ExplicitLeft = 8
    ExplicitTop = 392
    ExplicitWidth = 766
    ExplicitHeight = 325
    object vleVersionInfo: TValueListEditor
      AlignWithMargins = True
      Left = 5
      Top = 60
      Width = 761
      Height = 310
      Align = alClient
      DefaultRowHeight = 19
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goAlwaysShowEditor, goThumbTracking]
      Strings.Strings = (
        '')
      TabOrder = 0
      ExplicitHeight = 173
      ColWidths = (
        150
        605)
      RowHeights = (
        19
        19)
    end
    object chkIncludeInProject: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 376
      Width = 761
      Height = 17
      Align = alBottom
      Caption = '&Include Resource in Project'
      TabOrder = 1
      ExplicitLeft = 8
      ExplicitTop = 258
      ExplicitWidth = 576
    end
    object chkCompileWithBRCC32: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 399
      Width = 761
      Height = 17
      Align = alBottom
      Caption = 'Compile Resource with BRCC32'
      TabOrder = 2
      ExplicitLeft = 8
      ExplicitTop = 281
      ExplicitWidth = 576
    end
    object gpnlVerInfo: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 21
      Width = 761
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 12.499997708437440000
        end
        item
          Value = 12.500029613517490000
        end
        item
          SizeStyle = ssAuto
          Value = 7.501227548529042000
        end
        item
          Value = 12.500021384995450000
        end
        item
          Value = 12.500001683628540000
        end
        item
          SizeStyle = ssAuto
          Value = 8.807331748579425000
        end
        item
          Value = 12.499986640766420000
        end
        item
          Value = 12.499982087832750000
        end
        item
          SizeStyle = ssAuto
          Value = 10.558335945974690000
        end
        item
          Value = 12.499986489798520000
        end
        item
          Value = 12.499994391023410000
        end
        item
          SizeStyle = ssAuto
          Value = 12.884905129971940000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblMajor
          Row = 0
        end
        item
          Column = 1
          Control = edtMajor
          Row = 0
        end
        item
          Column = 3
          Control = lblMinor
          Row = 0
        end
        item
          Column = 4
          Control = edtMinor
          Row = 0
        end
        item
          Column = 7
          Control = edtRelease
          Row = 0
        end
        item
          Column = 6
          Control = lblRelease
          Row = 0
        end
        item
          Column = 9
          Control = lblBuild
          Row = 0
        end
        item
          Column = 10
          Control = edtBuild
          Row = 0
        end
        item
          Column = 2
          Control = upMajor
          Row = 0
        end
        item
          Column = 5
          Control = upMinor
          Row = 0
        end
        item
          Column = 8
          Control = upRelease
          Row = 0
        end
        item
          Column = 11
          Control = upBuild
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 3
      ExplicitLeft = 16
      ExplicitTop = 128
      ExplicitWidth = 710
      DesignSize = (
        761
        33)
      object lblMajor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        Caption = '&Major'
        Layout = tlCenter
        ExplicitLeft = 8
        ExplicitTop = 37
        ExplicitWidth = 33
        ExplicitHeight = 16
      end
      object edtMajor: TEdit
        AlignWithMargins = True
        Left = 89
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        ReadOnly = True
        TabOrder = 0
        Text = '0'
        ExplicitLeft = 99
        ExplicitTop = 40
        ExplicitWidth = 42
        ExplicitHeight = 24
      end
      object lblMinor: TLabel
        AlignWithMargins = True
        Left = 191
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        Caption = 'Mi&nor'
        Layout = tlCenter
        ExplicitLeft = 150
        ExplicitTop = 37
        ExplicitWidth = 32
        ExplicitHeight = 16
      end
      object edtMinor: TEdit
        AlignWithMargins = True
        Left = 277
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        ReadOnly = True
        TabOrder = 1
        Text = '0'
        ExplicitLeft = 108
        ExplicitTop = 32
        ExplicitWidth = 42
        ExplicitHeight = 24
      end
      object edtRelease: TEdit
        AlignWithMargins = True
        Left = 465
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        ReadOnly = True
        TabOrder = 2
        Text = '0'
        ExplicitLeft = 150
        ExplicitTop = 32
        ExplicitWidth = 42
        ExplicitHeight = 24
      end
      object lblRelease: TLabel
        AlignWithMargins = True
        Left = 379
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        Caption = '&Release'
        Layout = tlCenter
        ExplicitLeft = 285
        ExplicitTop = 37
        ExplicitWidth = 45
        ExplicitHeight = 16
      end
      object lblBuild: TLabel
        AlignWithMargins = True
        Left = 567
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        Caption = '&Build'
        Layout = tlCenter
        ExplicitLeft = 429
        ExplicitTop = 37
        ExplicitWidth = 27
        ExplicitHeight = 16
      end
      object edtBuild: TEdit
        AlignWithMargins = True
        Left = 653
        Top = 3
        Width = 80
        Height = 25
        Align = alClient
        ReadOnly = True
        TabOrder = 3
        Text = '0'
        ExplicitLeft = 668
        ExplicitTop = 4
        ExplicitWidth = 66
      end
      object upMajor: TUpDown
        Left = 172
        Top = 3
        Width = 16
        Height = 24
        Anchors = []
        Associate = edtMajor
        Max = 10000
        TabOrder = 4
        OnClick = BuildChange
        ExplicitLeft = 76
        ExplicitTop = 32
      end
      object upMinor: TUpDown
        Left = 360
        Top = 3
        Width = 16
        Height = 24
        Anchors = []
        Associate = edtMinor
        Max = 10000
        TabOrder = 5
        OnClick = BuildChange
        ExplicitLeft = 150
        ExplicitTop = 32
      end
      object upRelease: TUpDown
        Left = 548
        Top = 3
        Width = 16
        Height = 24
        Anchors = []
        Associate = edtRelease
        Max = 26
        TabOrder = 6
        OnClick = BuildChange
        ExplicitLeft = 192
        ExplicitTop = 32
      end
      object upBuild: TUpDown
        Left = 736
        Top = 3
        Width = 16
        Height = 25
        Anchors = []
        Associate = edtBuild
        Max = 1000000
        TabOrder = 7
        OnClick = BuildChange
        ExplicitLeft = 734
        ExplicitTop = 4
      end
    end
    object gpnlResource: TGridPanel
      Left = 2
      Top = 419
      Width = 767
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'gpnlResource'
      ColumnCollection = <
        item
          Value = 49.999992053046100000
        end
        item
          Value = 50.000007946953910000
        end
        item
          SizeStyle = ssAbsolute
          Value = 175.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblResourceName
          Row = 0
        end
        item
          Column = 1
          Control = edtResourceName
          Row = 0
        end
        item
          Column = 2
          Control = btnGetVersionInfo
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 4
      ExplicitTop = 282
      DesignSize = (
        767
        33)
      object lblResourceName: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 289
        Height = 27
        Align = alClient
        Caption = 'Resource Path & &Name (exc Ext)'
        Layout = tlCenter
        ExplicitLeft = 8
        ExplicitTop = 307
        ExplicitWidth = 177
        ExplicitHeight = 16
      end
      object edtResourceName: TEdit
        Left = 295
        Top = 4
        Width = 296
        Height = 24
        Anchors = []
        TabOrder = 0
        Text = 'edtResourceName'
        ExplicitLeft = 196
        ExplicitTop = 518
      end
      object btnGetVersionInfo: TBitBtn
        AlignWithMargins = True
        Left = 594
        Top = 3
        Width = 169
        Height = 27
        Align = alClient
        Caption = '&Get IDE Version Info'
        TabOrder = 1
        OnClick = btnGetVersionInfoClick
        ExplicitLeft = 624
        ExplicitTop = 516
        ExplicitWidth = 134
        ExplicitHeight = 25
      end
    end
  end
  object chkEnabled: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 240
    Width = 771
    Height = 17
    Align = alTop
    Caption = '&Enabled ITHelper Version Control'
    TabOrder = 1
    OnClick = chkEnabledClick
    ExplicitLeft = 24
    ExplicitTop = 290
    ExplicitWidth = 766
  end
  object gpnlCopyVerInfo: TGridPanel
    Left = 0
    Top = 204
    Width = 777
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblVersionInfo
        Row = 0
      end
      item
        Column = 1
        Control = edtVersionInfo
        Row = 0
      end
      item
        Column = 2
        Control = btnOpenEXE
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 3
    ExplicitTop = 338
    object lblVersionInfo: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 356
      Height = 25
      Align = alClient
      Caption = '&Copy Version Information from'
      FocusControl = edtVersionInfo
      Layout = tlCenter
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitHeight = 30
    end
    object edtVersionInfo: TEdit
      AlignWithMargins = True
      Left = 365
      Top = 3
      Width = 356
      Height = 25
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 204
      ExplicitTop = 197
      ExplicitWidth = 530
      ExplicitHeight = 24
    end
    object btnOpenEXE: TButton
      AlignWithMargins = True
      Left = 727
      Top = 3
      Width = 44
      Height = 25
      Align = alClient
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenEXEClick
      ExplicitLeft = 740
      ExplicitTop = 195
      ExplicitWidth = 34
    end
  end
  object lvIncrementOnCompileMode: TListView
    AlignWithMargins = True
    Left = 3
    Top = 77
    Width = 771
    Height = 124
    Align = alTop
    Columns = <
      item
        Caption = 'Configuration'
      end
      item
        Alignment = taCenter
        Caption = 'Make'
        Width = 100
      end
      item
        Alignment = taCenter
        Caption = 'Build'
        Width = 100
      end
      item
        Alignment = taCenter
        Caption = 'Check'
        Width = 100
      end
      item
        Alignment = taCenter
        Caption = 'Make Unit'
        Width = 100
      end>
    ColumnClick = False
    FlatScrollBars = True
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 4
    ViewStyle = vsReport
    OnCustomDrawSubItem = lvIncrementOnCompileModeCustomDrawSubItem
    OnMouseUp = lvIncrementOnCompileModeMouseUp
    OnResize = lvIncrementOnCompileModeResize
    ExplicitTop = 133
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 513
    Top = 8
  end
end
