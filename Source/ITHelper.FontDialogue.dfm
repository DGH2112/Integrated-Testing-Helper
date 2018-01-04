object frmITHFontDialogue: TfrmITHFontDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Message Fonts'
  ClientHeight = 223
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    503
    223)
  PixelsPerInch = 96
  TextHeight = 16
  object lblHeaderFontName: TLabel
    Left = 8
    Top = 11
    Width = 176
    Height = 16
    Caption = 'Header && Standard Font Name'
    FocusControl = cbxHeaderFontName
  end
  object lblToolFontName: TLabel
    Left = 8
    Top = 38
    Width = 133
    Height = 16
    Caption = 'Tool Output Font Name'
    FocusControl = cbxToolFontName
  end
  object gbxMessageColours: TGroupBox
    Left = 8
    Top = 62
    Width = 487
    Height = 122
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Message Colours'
    TabOrder = 2
    DesignSize = (
      487
      122)
    object lblMessageType: TLabel
      Left = 12
      Top = 25
      Width = 82
      Height = 16
      Caption = 'Message &Type'
      FocusControl = cbxMessageType
    end
    object lblMessageFontClour: TLabel
      Left = 12
      Top = 52
      Width = 120
      Height = 16
      Caption = 'Message Font &Colour'
      FocusControl = gbxMessageColours
    end
    object clbxFontColour: TColorBox
      Left = 192
      Top = 49
      Width = 283
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnClick = clbxFontColourClick
    end
    object chkFontStyleBold: TCheckBox
      Left = 12
      Top = 85
      Width = 100
      Height = 23
      Caption = '&Bold'
      TabOrder = 2
      OnClick = FontStyleClick
    end
    object chkFontStyleItalic: TCheckBox
      Left = 99
      Top = 85
      Width = 100
      Height = 23
      Caption = '&Italic'
      TabOrder = 3
      OnClick = FontStyleClick
    end
    object chkFontStyleUnderline: TCheckBox
      Left = 170
      Top = 85
      Width = 100
      Height = 23
      Caption = '&Underline'
      TabOrder = 4
      OnClick = FontStyleClick
    end
    object chkFontStyleStrikeout: TCheckBox
      Left = 266
      Top = 85
      Width = 100
      Height = 23
      Caption = '&Strikeout'
      TabOrder = 5
      OnClick = FontStyleClick
    end
    object cbxMessageType: TComboBox
      Left = 192
      Top = 19
      Width = 283
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnClick = cbxMessageTypeClick
    end
  end
  object cbxHeaderFontName: TComboBox
    Left = 200
    Top = 8
    Width = 295
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbxToolFontName: TComboBox
    Left = 200
    Top = 35
    Width = 295
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 339
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
  end
  object btnCancel: TBitBtn
    Left = 420
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
  end
  object btnHelp: TBitBtn
    Left = 258
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
