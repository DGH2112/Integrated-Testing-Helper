object frmITHProjectOptionsDialogue: TfrmITHProjectOptionsDialogue
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Project Options'
  ClientHeight = 565
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    784
    565)
  PixelsPerInch = 96
  TextHeight = 16
  object btnOK: TBitBtn
    Left = 620
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 701
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 539
    Top = 532
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
    Width = 768
    Height = 518
    ActivePage = tabZipping
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabProjectOptions: TTabSheet
      Caption = '&Project Options'
    end
    object tabBeforeCompileTools: TTabSheet
      Caption = '&Before Compile Tools'
      ImageIndex = 1
    end
    object tabAfterCompileTools: TTabSheet
      Caption = '&After Compile Tools'
      ImageIndex = 2
    end
    object tabZipping: TTabSheet
      Caption = '&Zipping'
      ImageIndex = 3
    end
  end
end
