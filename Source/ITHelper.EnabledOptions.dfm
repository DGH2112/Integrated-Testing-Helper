object frmEnabledOptions: TfrmEnabledOptions
  Left = 740
  Top = 548
  BorderStyle = bsToolWindow
  Caption = 'Enabled Options'
  ClientHeight = 209
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    337
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object chkEnable: TCheckBox
    Left = 8
    Top = 8
    Width = 240
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Enable'
    TabOrder = 0
    OnClick = EnabledClick
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 31
    Width = 240
    Height = 170
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      240
      170)
    object chkBefore: TCheckBox
      Left = 8
      Top = 25
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Before Compilation Tools'
      TabOrder = 0
    end
    object chkAfter: TCheckBox
      Left = 8
      Top = 48
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&After Compilation Tools'
      TabOrder = 1
    end
    object chkZip: TCheckBox
      Left = 8
      Top = 71
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Zipping of Project Files'
      TabOrder = 2
    end
    object chkIncBuild: TCheckBox
      Left = 8
      Top = 94
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Increment Build on Successful Compilation'
      TabOrder = 3
    end
    object chkBuildRes: TCheckBox
      Left = 8
      Top = 117
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Build Project Version Infomation Resource'
      TabOrder = 4
    end
    object chkCopyVerInfo: TCheckBox
      Left = 8
      Top = 140
      Width = 224
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Copy &Project Version Information'
      TabOrder = 5
    end
  end
  object btnOK: TBitBtn
    Left = 254
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 254
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 254
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnHelpClick
  end
end
