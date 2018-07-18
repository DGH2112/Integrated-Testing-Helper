object frameFonts: TframeFonts
  Left = 0
  Top = 0
  Width = 544
  Height = 185
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    544
    185)
  object lblToolFontName: TLabel
    Left = 8
    Top = 38
    Width = 133
    Height = 16
    Caption = 'Tool Output Font Name'
    FocusControl = cbxToolFontName
  end
  object lblHeaderFontName: TLabel
    Left = 8
    Top = 11
    Width = 176
    Height = 16
    Caption = 'Header && Standard Font Name'
    FocusControl = cbxHeaderFontName
  end
  object gbxMessageColours: TGroupBox
    Left = 8
    Top = 62
    Width = 533
    Height = 120
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Message Colours'
    TabOrder = 2
    DesignSize = (
      533
      120)
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
      Width = 281
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = clbxFontColourClick
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
      Width = 281
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnClick = cbxMessageTypeClick
    end
  end
  object cbxToolFontName: TComboBox
    Left = 200
    Top = 35
    Width = 341
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object cbxHeaderFontName: TComboBox
    Left = 200
    Top = 8
    Width = 341
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
