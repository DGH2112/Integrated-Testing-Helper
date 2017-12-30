object frmProgrammeInfo: TfrmProgrammeInfo
  Left = 588
  Top = 551
  BorderIcons = []
  Caption = 'Programme Info'
  ClientHeight = 210
  ClientWidth = 571
  Color = clBtnFace
  Constraints.MaxHeight = 245
  Constraints.MinHeight = 245
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    571
    210)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgramme: TLabel
    Left = 8
    Top = 38
    Width = 54
    Height = 13
    Caption = '&Programme'
    FocusControl = edtProgramme
  end
  object lblParameters: TLabel
    Left = 8
    Top = 65
    Width = 55
    Height = 13
    Caption = 'P&arameters'
    FocusControl = edtParameters
  end
  object lblWorkingDirectrory: TLabel
    Left = 8
    Top = 92
    Width = 86
    Height = 13
    Caption = '&Working Directory'
    FocusControl = edtWorkingDirectory
  end
  object lblTitle: TLabel
    Left = 8
    Top = 11
    Width = 20
    Height = 13
    Caption = '&Title'
    FocusControl = edtTitle
  end
  object edtProgramme: TEdit
    Left = 100
    Top = 35
    Width = 424
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edtParameters: TEdit
    Left = 100
    Top = 62
    Width = 463
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object edtWorkingDirectory: TEdit
    Left = 100
    Top = 89
    Width = 424
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object btnEXE: TButton
    Left = 530
    Top = 35
    Width = 33
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnEXEClick
  end
  object btnDirectory: TButton
    Left = 530
    Top = 89
    Width = 33
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = btnDirectoryClick
  end
  object btnOK: TBitBtn
    Left = 407
    Top = 174
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
  end
  object btnCancel: TBitBtn
    Left = 488
    Top = 174
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object rgpStartingWindowState: TRadioGroup
    Left = 8
    Top = 116
    Width = 555
    Height = 52
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Starting Window State'
    Columns = 3
    Items.Strings = (
      '&Normal'
      'Mi&nimised'
      'Ma&ximised')
    TabOrder = 8
  end
  object edtTitle: TEdit
    Left = 100
    Top = 8
    Width = 463
    Height = 21
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
