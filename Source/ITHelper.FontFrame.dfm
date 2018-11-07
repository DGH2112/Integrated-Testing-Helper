object frameFonts: TframeFonts
  Left = 0
  Top = 0
  Width = 544
  Height = 404
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
    Width = 544
    Height = 404
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 249
    object gbxMessageColours: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 68
      Width = 538
      Height = 333
      Align = alClient
      Caption = 'Message Colours'
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 238
      ExplicitWidth = 529
      ExplicitHeight = 123
      DesignSize = (
        538
        333)
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
        Width = 329
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = clbxFontColourClick
      end
      object cbxMessageType: TComboBox
        Left = 192
        Top = 19
        Width = 329
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnClick = cbxMessageTypeClick
      end
      object gpnlFontStyles: TGridPanel
        Left = 12
        Top = 77
        Width = 509
        Height = 36
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 25.000000000209280000
          end
          item
            Value = 25.000000000062780000
          end
          item
            Value = 24.999999999816510000
          end
          item
            Value = 24.999999999911420000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = chkFontStyleBold
            Row = 0
          end
          item
            Column = 1
            Control = chkFontStyleItalic
            Row = 0
          end
          item
            Column = 2
            Control = chkFontStyleStrikeout
            Row = 0
          end
          item
            Column = 3
            Control = chkFontStyleUnderline
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 2
        DesignSize = (
          509
          36)
        object chkFontStyleBold: TCheckBox
          Left = 13
          Top = 6
          Width = 100
          Height = 23
          Anchors = []
          Caption = '&Bold'
          TabOrder = 0
          OnClick = FontStyleClick
        end
        object chkFontStyleItalic: TCheckBox
          Left = 140
          Top = 6
          Width = 100
          Height = 23
          Anchors = []
          Caption = '&Italic'
          TabOrder = 1
          OnClick = FontStyleClick
        end
        object chkFontStyleStrikeout: TCheckBox
          Left = 267
          Top = 6
          Width = 100
          Height = 23
          Anchors = []
          Caption = '&Strikeout'
          TabOrder = 2
          OnClick = FontStyleClick
        end
        object chkFontStyleUnderline: TCheckBox
          Left = 395
          Top = 6
          Width = 100
          Height = 23
          Anchors = []
          Caption = '&Underline'
          TabOrder = 3
          OnClick = FontStyleClick
        end
      end
    end
    object pnlFontNames: TPanel
      Left = 0
      Top = 0
      Width = 544
      Height = 65
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        544
        65)
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
      object cbxHeaderFontName: TComboBox
        Left = 200
        Top = 8
        Width = 337
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object cbxToolFontName: TComboBox
        Left = 200
        Top = 35
        Width = 337
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
    end
  end
end
