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
    ExplicitWidth = 225
  end
  object lblIncrementOnCompileMode: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 55
    Width = 771
    Height = 16
    Align = alTop
    Caption = '&Increment on Compile Mode'
    ExplicitWidth = 161
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
  end
  object gbxVersionInfo: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 263
    Width = 771
    Height = 454
    Align = alClient
    Caption = 'Version Info'
    TabOrder = 4
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
          Value = 12.500001683628530000
        end
        item
          SizeStyle = ssAuto
          Value = 8.807331748579426000
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
      DesignSize = (
        761
        33)
      object lblMajor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        Caption = '&Major'
        Layout = tlCenter
        ExplicitWidth = 33
        ExplicitHeight = 16
      end
      object edtMajor: TEdit
        AlignWithMargins = True
        Left = 90
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        ReadOnly = True
        TabOrder = 0
        Text = '0'
        ExplicitWidth = 65
      end
      object lblMinor: TLabel
        AlignWithMargins = True
        Left = 193
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        Caption = 'Mi&nor'
        Layout = tlCenter
        ExplicitWidth = 32
        ExplicitHeight = 16
      end
      object edtMinor: TEdit
        AlignWithMargins = True
        Left = 280
        Top = 3
        Width = 82
        Height = 27
        Align = alClient
        ReadOnly = True
        TabOrder = 1
        Text = '0'
        ExplicitWidth = 65
      end
      object edtRelease: TEdit
        AlignWithMargins = True
        Left = 471
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        ReadOnly = True
        TabOrder = 2
        Text = '0'
        ExplicitLeft = 470
        ExplicitWidth = 65
      end
      object lblRelease: TLabel
        AlignWithMargins = True
        Left = 384
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        Caption = '&Release'
        Layout = tlCenter
        ExplicitLeft = 383
        ExplicitWidth = 45
        ExplicitHeight = 16
      end
      object lblBuild: TLabel
        AlignWithMargins = True
        Left = 574
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        Caption = '&Build'
        Layout = tlCenter
        ExplicitLeft = 573
        ExplicitWidth = 27
        ExplicitHeight = 16
      end
      object edtBuild: TEdit
        AlignWithMargins = True
        Left = 661
        Top = 3
        Width = 81
        Height = 27
        Align = alClient
        ReadOnly = True
        TabOrder = 3
        Text = '0'
        ExplicitLeft = 660
        ExplicitWidth = 65
      end
      object upMajor: TUpDown
        Left = 174
        Top = 3
        Width = 16
        Height = 27
        Anchors = []
        Associate = edtMajor
        Max = 10000
        TabOrder = 4
        OnClick = BuildChange
        ExplicitLeft = 155
      end
      object upMinor: TUpDown
        Left = 365
        Top = 3
        Width = 16
        Height = 27
        Anchors = []
        Associate = edtMinor
        Max = 10000
        TabOrder = 5
        OnClick = BuildChange
        ExplicitLeft = 345
      end
      object upRelease: TUpDown
        Left = 555
        Top = 3
        Width = 16
        Height = 27
        Anchors = []
        Associate = edtRelease
        Max = 26
        TabOrder = 6
        OnClick = BuildChange
        ExplicitLeft = 535
      end
      object upBuild: TUpDown
        Left = 745
        Top = 3
        Width = 16
        Height = 27
        Anchors = []
        Associate = edtBuild
        Max = 1000000
        TabOrder = 7
        OnClick = BuildChange
        ExplicitLeft = 725
      end
    end
    object gpnlResource: TGridPanel
      Left = 2
      Top = 419
      Width = 767
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
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
        end
        item
          SizeStyle = ssAuto
        end>
      TabOrder = 4
      DesignSize = (
        767
        33)
      object lblResourceName: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 290
        Height = 27
        Align = alClient
        Caption = 'Resource Path & &Name (exc Ext)'
        Layout = tlCenter
        ExplicitWidth = 177
        ExplicitHeight = 16
      end
      object edtResourceName: TEdit
        Left = 296
        Top = 4
        Width = 296
        Height = 24
        Anchors = []
        TabOrder = 0
        Text = 'edtResourceName'
        ExplicitLeft = 295
      end
      object btnGetVersionInfo: TButton
        AlignWithMargins = True
        Left = 595
        Top = 3
        Width = 169
        Height = 27
        Align = alClient
        Caption = '&Get IDE Version Info'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        OnClick = btnGetVersionInfoClick
        ExplicitLeft = 594
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
    TabOrder = 3
    OnClick = chkEnabledClick
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
    TabOrder = 2
    object lblVersionInfo: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 176
      Height = 16
      Align = alClient
      Caption = '&Copy Version Information from'
      FocusControl = edtVersionInfo
      Layout = tlCenter
    end
    object edtVersionInfo: TEdit
      AlignWithMargins = True
      Left = 366
      Top = 3
      Width = 357
      Height = 27
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 24
    end
    object btnOpenEXE: TButton
      AlignWithMargins = True
      Left = 729
      Top = 3
      Width = 44
      Height = 27
      Align = alClient
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenEXEClick
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
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawSubItem = lvIncrementOnCompileModeCustomDrawSubItem
    OnMouseUp = lvIncrementOnCompileModeMouseUp
    OnResize = lvIncrementOnCompileModeResize
  end
  object dlgOpenEXE: TOpenDialog
    Filter = 'Executables|*.exe;*.dll;*.bpl'
    Title = 'Executable Files'
    Left = 513
    Top = 8
  end
end
