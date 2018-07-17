object frameZipping: TframeZipping
  Left = 0
  Top = 0
  Width = 646
  Height = 426
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    646
    426)
  object lblZIPBasePath: TLabel
    Left = 8
    Top = 86
    Width = 299
    Height = 16
    Caption = 'ZIP &Base Path (used for relative paths in the ZIP file)'
    FocusControl = edtBasePath
  end
  object lblFilePatternsToExclude: TLabel
    Left = 8
    Top = 310
    Width = 137
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'File Patterns To &Exclude'
    FocusControl = mmoExclusionPatterns
  end
  object lblAdditionalFiles: TLabel
    Left = 8
    Top = 138
    Width = 86
    Height = 16
    Caption = '&Additional Files'
    FocusControl = lbAdditionalWildcards
  end
  object lblZIPName: TLabel
    Left = 8
    Top = 37
    Width = 128
    Height = 16
    Caption = 'ZIP Path and File&name'
    FocusControl = edtZipName
  end
  object lblZipInfo: TLabel
    Left = 8
    Top = 406
    Width = 635
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'In order for this to work you need a ZIP command-line tool insta' +
      'lled and on your path.'
  end
  object chkModifiedFiles: TCheckBox
    Left = 8
    Top = 383
    Width = 635
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&Save modified files in the project before Zipping'
    TabOrder = 0
  end
  object mmoExclusionPatterns: TMemo
    Left = 8
    Top = 332
    Width = 537
    Height = 45
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object btnBrowseBasePath: TButton
    Left = 551
    Top = 112
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse &Path'
    TabOrder = 2
    OnClick = btnBrowseBasePathClick
  end
  object edtBasePath: TEdit
    Left = 8
    Top = 108
    Width = 537
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object btnBrowseZip: TButton
    Left = 551
    Top = 56
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse &File'
    TabOrder = 4
    OnClick = btnBrowseZipClick
  end
  object edtZipName: TEdit
    Left = 8
    Top = 56
    Width = 537
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnExit = edtZipEXEExit
  end
  object btnDeleteZip: TBitBtn
    Left = 551
    Top = 226
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333FF33333333333330003333333333333777333333333333
      300033FFFFFF3333377739999993333333333777777F3333333F399999933333
      3300377777733333337733333333333333003333333333333377333333333333
      3333333333333333333F333333333333330033333F33333333773333C3333333
      330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
      333333377F33333333FF3333C333333330003333733333333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnDeleteZipClick
  end
  object btnEditZip: TBitBtn
    Left = 551
    Top = 195
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Edit'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
      000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
      00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
      F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
      0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
      FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
      FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
      0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
      00333377737FFFFF773333303300000003333337337777777333}
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnEditZipClick
  end
  object btnAddZip: TBitBtn
    Left = 551
    Top = 164
    Width = 92
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333FF33333333FF333993333333300033377F3333333777333993333333
      300033F77FFF3333377739999993333333333777777F3333333F399999933333
      33003777777333333377333993333333330033377F3333333377333993333333
      3333333773333333333F333333333333330033333333F33333773333333C3333
      330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
      333333333337733333FF3333333C333330003333333733333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 8
    OnClick = btnAddZipClick
  end
  object lbAdditionalWildcards: TListBox
    Left = 8
    Top = 160
    Width = 537
    Height = 144
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 9
  end
  object cbxEnabledZipping: TCheckBox
    Left = 8
    Top = 8
    Width = 625
    Height = 17
    Caption = '&Enabled Zipping'
    TabOrder = 10
    OnClick = cbxEnabledZippingClick
  end
  object dlgOpenZIP: TOpenDialog
    DefaultExt = 'ZIP'
    Filter = 'ZIP Files (*.zip)|*.zip'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'ZIP File'
    Left = 175
    Top = 151
  end
end
