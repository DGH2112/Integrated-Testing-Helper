object frameAboutITHelper: TframeAboutITHelper
  Left = 0
  Top = 0
  Width = 491
  Height = 433
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
    Width = 491
    Height = 433
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblITHelper: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 485
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
      ExplicitWidth = 309
    end
    object lblBuild: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 38
      Width = 485
      Height = 16
      Align = alTop
      Caption = 'lblBuild'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 40
    end
    object lblAuthor: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 82
      Width = 485
      Height = 16
      Align = alTop
      Caption = 'Author: David Hoyle (c) Season'#39's Fall Music'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 248
    end
    object lblBuildDate: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 60
      Width = 485
      Height = 16
      Align = alTop
      Caption = 'lblBuildDate'
      ExplicitWidth = 66
    end
    object lblInformation: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 104
      Width = 485
      Height = 165
      Align = alTop
      AutoSize = False
      Caption = 
        'A RAD Studio IDE plug-in to help you manage the application buil' +
        'd process:'#13' * Manage Version Control;'#13' * Run tools before or aft' +
        'er a compilation;'#13' * ZIP the project for backups, uploads and ar' +
        'chiving.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitTop = 92
      ExplicitWidth = 523
    end
    object lblPleaseSelect: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 275
      Width = 485
      Height = 16
      Align = alTop
      Caption = 'Please select a sub-options category...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 220
    end
    object lblEurekaLog: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 297
      Width = 485
      Height = 133
      Align = alClient
      AutoSize = False
      Caption = 'EurekaLog'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlBottom
      ExplicitLeft = 6
      ExplicitTop = 209
      ExplicitWidth = 523
      ExplicitHeight = 429
    end
  end
end
