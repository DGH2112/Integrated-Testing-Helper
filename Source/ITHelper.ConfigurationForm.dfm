object frmITHConfigureDlg: TfrmITHConfigureDlg
  Left = 392
  Top = 473
  Caption = 'Testing Helper Configuration Information'
  ClientHeight = 375
  ClientWidth = 804
  Color = clBtnFace
  Constraints.MinHeight = 410
  Constraints.MinWidth = 820
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  DesignSize = (
    804
    375)
  PixelsPerInch = 96
  TextHeight = 16
  object btnOK: TBitBtn
    Left = 640
    Top = 342
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnCancel: TBitBtn
    Left = 721
    Top = 343
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnHelp: TBitBtn
    Left = 559
    Top = 342
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 0
    OnClick = btnHelpClick
  end
end
