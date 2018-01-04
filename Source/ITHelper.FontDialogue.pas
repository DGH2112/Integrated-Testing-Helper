(**

  This module contains a class ewhich represents a form for editing the font styles and
  colours used in the messages that are output by the expert.

  @Author  David Hoyle
  @Version 1.0
  @Date    04 Jan 2018

**)
Unit ITHelper.FontDialogue;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ITHelper.GlobalOptions, 
  ITHelper.Types;

Type
  (** A class to represent the form interface. **)
  TfrmITHFontDialogue = Class(TForm)
    lblHeaderFontName: TLabel;
    lblToolFontName: TLabel;
    gbxMessageColours: TGroupBox;
    clbxFontColour: TColorBox;
    chkFontStyleBold: TCheckBox;
    chkFontStyleItalic: TCheckBox;
    chkFontStyleUnderline: TCheckBox;
    chkFontStyleStrikeout: TCheckBox;
    cbxHeaderFontName: TComboBox;
    cbxToolFontName: TComboBox;
    cbxMessageType: TComboBox;
    lblMessageType: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblMessageFontClour: TLabel;
    btnHelp: TBitBtn;
    Procedure FontStyleClick(Sender: TObject);
    Procedure cbxMessageTypeClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure clbxFontColourClick(Sender: TObject);
  Private
    { Private declarations }
    FFontColour: Array [Low(TITHFonts) .. High(TITHFonts)] Of TColor;
    FFontStyle : Array [Low(TITHFonts) .. High(TITHFonts)] Of TFontStyles;
    FUpdating  : Boolean;
    Procedure InitialiseMessageOptions(Const GlobalOps: TITHGlobalOptions);
    Procedure SaveMessageOptions(Const GlobalOps: TITHGlobalOptions);
  Public
    { Public declarations }
    Class Procedure Execute(Const GlobalOptions: TITHGlobalOptions);
  End;

Implementation

{$R *.dfm}

Uses
  ITHelper.TestingHelperUtils;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Message Fonts help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHFontDialogue.btnHelpClick(Sender: TObject);

Const
  strMessageFonts = 'MessageFonts';

Begin
  HTMLHelp(0, PChar(ITHHTMLHelpFile(strMessageFonts)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the Message Type combo box.

  @precon  None.
  @postcon Updates the font colour and styles based on the selected message type.

  @param   Sender as a TObject

**)
Procedure TfrmITHFontDialogue.cbxMessageTypeClick(Sender: TObject);

Var
  iMessageType: TITHFonts;

Begin
  FUpdating := True;
  Try
    iMessageType                  := TITHFonts(Byte(cbxMessageType.ItemIndex));
    clbxFontColour.Selected       := FFontColour[iMessageType];
    chkFontStyleBold.Checked      := fsBold In FFontStyle[iMessageType];
    chkFontStyleItalic.Checked    := fsItalic In FFontStyle[iMessageType];
    chkFontStyleUnderline.Checked := fsUnderline In FFontStyle[iMessageType];
    chkFontStyleStrikeout.Checked := fsStrikeOut In FFontStyle[iMessageType];
  Finally
    FUpdating := False;
  End;
End;

(**

  This is an on click event handler for the Colour drop down control.

  @precon  None.
  @postcon Updates the appropriate Font colour with the selected colour.

  @param   Sender as a TObject

**)
Procedure TfrmITHFontDialogue.clbxFontColourClick(Sender: TObject);

Var
  iMessageType : TITHFonts;

Begin
  If Not FUpdating Then
    Begin
      iMessageType := TITHFonts(Byte(cbxMessageType.ItemIndex));
      FFontColour[iMessageType] := clbxFontColour.Selected;
    End;
End;

(**

  This is the forms main interface method for invoking the dialogue.

  @precon  GlobalOptions must be a valid instance.
  @postcon Displays the dialogue.

  @param   GlobalOptions as a TITHGlobalOptions as a constant

**)
Class Procedure TfrmITHFontDialogue.Execute(Const GlobalOptions: TITHGlobalOptions);

Var
  frm: TfrmITHFontDialogue;

Begin
  frm := TfrmITHFontDialogue.Create(Nil);
  Try
    frm.InitialiseMessageOptions(GlobalOptions);
    If frm.ShowModal = mrOK Then
      frm.SaveMessageOptions(GlobalOptions);
  Finally
    frm.Free;
  End;
End;

(**

  This is an on click event handler for the Font Style check boxes.

  @precon  None.
  @postcon Updates the internal font style array with the changes.

  @param   Sender as a TObject

**)
Procedure TfrmITHFontDialogue.FontStyleClick(Sender: TObject);

Var
  iMessageType: TITHFonts;

Begin
  If Not FUpdating Then
    Begin
      iMessageType             := TITHFonts(Byte(cbxMessageType.ItemIndex));
      FFontStyle[iMessageType] := [];
      If chkFontStyleBold.Checked Then
        Include(FFontStyle[iMessageType], fsBold);
      If chkFontStyleItalic.Checked Then
        Include(FFontStyle[iMessageType], fsItalic);
      If chkFontStyleUnderline.Checked Then
        Include(FFontStyle[iMessageType], fsUnderline);
      If chkFontStyleStrikeout.Checked Then
        Include(FFontStyle[iMessageType], fsStrikeOut);
    End;
End;

(**

  This method initialises the message font checkboxes in the dialogue.

  @precon  None.
  @postcon Initialises the message font checkboxes in the dialogue.

  @param   GlobalOps as a TITHGlobalOptions as a constant

**)
Procedure TfrmITHFontDialogue.InitialiseMessageOptions(Const GlobalOps: TITHGlobalOptions);

Const
  strFontDescriptions: Array [Low(TITHFonts) .. High(TITHFonts)
    ] Of String = ('Header Messages', 'Default Messages', 'Success Messages',
    'Failure Messages', 'Warning Messages');
Var
  iMessageType: TITHFonts;

Begin
  FUpdating := False;
  cbxHeaderFontName.Items.Assign(Screen.Fonts);
  cbxHeaderFontName.ItemIndex := cbxHeaderFontName.Items.IndexOf
    (GlobalOps.FontName[fnHeader]);
  cbxToolFontName.Items.Assign(Screen.Fonts);
  cbxToolFontName.ItemIndex := cbxToolFontName.Items.IndexOf(GlobalOps.FontName[fnTools]);
  For iMessageType := Low(TITHFonts) To High(TITHFonts) Do
    Begin
      FFontColour[iMessageType] := GlobalOps.FontColour[iMessageType];
      FFontStyle[iMessageType]  := GlobalOps.FontStyles[iMessageType];
      cbxMessageType.Items.Add(strFontDescriptions[iMessageType]);
    End;
  cbxMessageType.ItemIndex := 0;
  cbxMessageTypeClick(Nil);
End;

(**

  This method saves the message options to the Options record structure.

  @precon  None.
  @postcon Saves the message options to the Options record structure.

  @param   GlobalOps as a TITHGlobalOptions as a constant

**)
Procedure TfrmITHFontDialogue.SaveMessageOptions(Const GlobalOps: TITHGlobalOptions);

Var
  iMessageType: TITHFonts;

Begin
  GlobalOps.FontName[fnHeader] := cbxHeaderFontName.Text;
  GlobalOps.FontName[fnTools]  := cbxToolFontName.Text;
  For iMessageType             := Low(TITHFonts) To High(TITHFonts) Do
    Begin
      GlobalOps.FontColour[iMessageType] := FFontColour[iMessageType];
      GlobalOps.FontStyles[iMessageType] := FFontStyle[iMessageType];
    End;
End;

End.
