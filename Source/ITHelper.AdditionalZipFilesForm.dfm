object frmITHAdditionalZipFiles: TfrmITHAdditionalZipFiles
  Left = 0
  Top = 0
  Caption = 'Additional Zip Files'
  ClientHeight = 85
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MaxHeight = 120
  Constraints.MinHeight = 120
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    624
    85)
  PixelsPerInch = 96
  TextHeight = 16
  object lblWildcard: TLabel
    Left = 8
    Top = 8
    Width = 136
    Height = 16
    Caption = 'Additional Zip &Wildcard:'
    FocusControl = edtWildcard
  end
  object edtWildcard: TEdit
    Left = 8
    Top = 27
    Width = 535
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 549
    Top = 25
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Browse'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnOK: TBitBtn
    Left = 468
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 549
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
