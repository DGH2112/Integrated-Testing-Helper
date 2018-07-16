object frmITHProjectOptionsDialogue: TfrmITHProjectOptionsDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Project Options'
  ClientHeight = 659
  ClientWidth = 766
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    766
    659)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 602
    Top = 626
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 683
    Top = 626
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 521
    Top = 626
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
  end
  object pgcProjectOptions: TPageControl
    Left = 8
    Top = 8
    Width = 750
    Height = 612
    ActivePage = tabProjectOptions
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabProjectOptions: TTabSheet
      Caption = '&Project Options'
    end
  end
end
