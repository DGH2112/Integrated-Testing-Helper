object frmITHProgrammeInfo: TfrmITHProgrammeInfo
  Left = 588
  Top = 551
  BorderIcons = []
  Caption = 'Programme Info'
  ClientHeight = 151
  ClientWidth = 599
  Color = clBtnFace
  Constraints.MaxHeight = 190
  Constraints.MinHeight = 190
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    599
    151)
  PixelsPerInch = 96
  TextHeight = 16
  object lblProgramme: TLabel
    Left = 8
    Top = 38
    Width = 67
    Height = 16
    Caption = '&Programme'
    FocusControl = edtProgramme
  end
  object lblParameters: TLabel
    Left = 8
    Top = 65
    Width = 66
    Height = 16
    Caption = 'P&arameters'
    FocusControl = edtParameters
  end
  object lblWorkingDirectrory: TLabel
    Left = 8
    Top = 92
    Width = 102
    Height = 16
    Caption = '&Working Directory'
    FocusControl = edtWorkingDirectory
  end
  object lblTitle: TLabel
    Left = 8
    Top = 11
    Width = 25
    Height = 16
    Caption = '&Title'
    FocusControl = edtTitle
  end
  object edtProgramme: TEdit
    Left = 128
    Top = 35
    Width = 424
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edtParameters: TEdit
    Left = 128
    Top = 62
    Width = 463
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edtWorkingDirectory: TEdit
    Left = 128
    Top = 89
    Width = 424
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object btnEXE: TButton
    Left = 558
    Top = 35
    Width = 33
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnEXEClick
  end
  object btnDirectory: TButton
    Left = 558
    Top = 89
    Width = 33
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = btnDirectoryClick
  end
  object btnOK: TBitBtn
    Left = 435
    Top = 122
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
  end
  object btnCancel: TBitBtn
    Left = 516
    Top = 122
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object edtTitle: TEdit
    Left = 128
    Top = 8
    Width = 463
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'Programme Files (*.exe;*.bat;*.btm)|*.exe;*.bat;*.btm|All Files ' +
      '(*.*)|*.*'
    Title = 'Programme Files'
    Left = 175
    Top = 42
  end
end
