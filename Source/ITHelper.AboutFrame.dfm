object frameAboutITHelper: TframeAboutITHelper
  Left = 0
  Top = 0
  Width = 509
  Height = 485
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
    Width = 509
    Height = 485
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblITHelper: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 309
      Height = 29
      Align = alTop
      Caption = 'Integrated Testing Helper'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object lblBuild: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 38
      Width = 40
      Height = 16
      Align = alTop
      Caption = 'lblBuild'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblAuthor: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 82
      Width = 229
      Height = 16
      Align = alTop
      Caption = 'Author: David Hoyle (c) 2019 GNU GPL 3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblBuildDate: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 60
      Width = 66
      Height = 16
      Align = alTop
      Caption = 'lblBuildDate'
    end
    object lblPleaseSelect: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 402
      Width = 220
      Height = 16
      Align = alBottom
      Caption = 'Please select a sub-options category...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lblEurekaLog: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 424
      Width = 503
      Height = 58
      Align = alBottom
      AutoSize = False
      Caption = 'EurekaLog'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlBottom
    end
    object mmoInformation: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 104
      Width = 503
      Height = 292
      Align = alClient
      ParentColor = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
