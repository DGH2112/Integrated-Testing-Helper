object frmFontDialogue: TfrmFontDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Message Fonts'
  ClientHeight = 209
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    421
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object lblHeaderFontName: TLabel
    Left = 8
    Top = 11
    Width = 147
    Height = 13
    Caption = 'Header && Standard Font Name'
    FocusControl = cbxHeaderFontName
  end
  object lblToolFontName: TLabel
    Left = 8
    Top = 38
    Width = 112
    Height = 13
    Caption = 'Tool Output Font Name'
    FocusControl = cbxToolFontName
  end
  object gbxMessageColours: TGroupBox
    Left = 8
    Top = 62
    Width = 405
    Height = 108
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Message Colours'
    TabOrder = 2
    DesignSize = (
      405
      108)
    object lblMessageType: TLabel
      Left = 12
      Top = 25
      Width = 69
      Height = 13
      Caption = 'Message &Type'
      FocusControl = cbxMessageType
    end
    object lblMessageFontClour: TLabel
      Left = 12
      Top = 52
      Width = 101
      Height = 13
      Caption = 'Message Font &Colour'
      FocusControl = gbxMessageColours
    end
    object clbxFontColour: TColorBox
      Left = 171
      Top = 49
      Width = 222
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnClick = clbxFontColourClick
    end
    object chkFontStyleBold: TCheckBox
      Left = 13
      Top = 77
      Width = 100
      Height = 17
      Caption = '&Bold'
      TabOrder = 2
      OnClick = FontStyleClick
    end
    object chkFontStyleItalic: TCheckBox
      Left = 100
      Top = 77
      Width = 100
      Height = 17
      Caption = '&Italic'
      TabOrder = 3
      OnClick = FontStyleClick
    end
    object chkFontStyleUnderline: TCheckBox
      Left = 171
      Top = 77
      Width = 100
      Height = 17
      Caption = '&Underline'
      TabOrder = 4
      OnClick = FontStyleClick
    end
    object chkFontStyleStrikeout: TCheckBox
      Left = 267
      Top = 77
      Width = 100
      Height = 17
      Caption = '&Strikeout'
      TabOrder = 5
      OnClick = FontStyleClick
    end
    object cbxMessageType: TComboBox
      Left = 171
      Top = 22
      Width = 222
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnClick = cbxMessageTypeClick
    end
  end
  object cbxHeaderFontName: TComboBox
    Left = 179
    Top = 8
    Width = 234
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbxToolFontName: TComboBox
    Left = 179
    Top = 35
    Width = 234
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 257
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
  end
  object btnCancel: TBitBtn
    Left = 338
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
  end
  object btnHelp: TBitBtn
    Left = 176
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
