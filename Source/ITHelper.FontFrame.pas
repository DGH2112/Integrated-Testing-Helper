(**
  
  This module contains a frame for the font settings for the custom messages.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Jul 2018
  
**)
Unit ITHelper.FontFrame;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A frame to holf the font settings for custom messages int he IDEs options dialogue. **)
  TframeFonts = Class(TFrame, IITHOptionsFrame)
    gbxMessageColours: TGroupBox;
    lblMessageType: TLabel;
    lblMessageFontClour: TLabel;
    clbxFontColour: TColorBox;
    chkFontStyleBold: TCheckBox;
    chkFontStyleItalic: TCheckBox;
    chkFontStyleUnderline: TCheckBox;
    chkFontStyleStrikeout: TCheckBox;
    cbxMessageType: TComboBox;
    cbxToolFontName: TComboBox;
    cbxHeaderFontName: TComboBox;
    lblToolFontName: TLabel;
    lblHeaderFontName: TLabel;
    Procedure FontStyleClick(Sender: TObject);
    Procedure cbxMessageTypeClick(Sender: TObject);
    procedure clbxFontColourClick(Sender: TObject);
  Strict Private
    FFontColour: Array [Low(TITHFonts) .. High(TITHFonts)] Of TColor;
    FFontStyle : Array [Low(TITHFonts) .. High(TITHFonts)] Of TFontStyles;
    FUpdating  : Boolean;
  Strict Protected
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
    Function IsValidated: Boolean;
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$IFDEF DEBUG}
Uses
  CodeSiteLogging;
{$ENDIF}

{$R *.dfm}

(**

  This is an on click event handler for the Message Type combo box.

  @precon  None.
  @postcon Updates the font colour and styles based on the selected message type.

  @param   Sender as a TObject

**)
Procedure TframeFonts.cbxMessageTypeClick(Sender: TObject);

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
Procedure TframeFonts.clbxFontColourClick(Sender: TObject);

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

  A constructor for the TframeFonts class.

  @precon  None.
  @postcon Does nothing but is used for CodeSite tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner

  @param   AOwner as a TComponent

**)
Constructor TframeFonts.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
End;

(**

  A destructor for the TframeFonts class.

  @precon  None.
  @postcon Does nothing but is used for CodeSite tracing.

**)
Destructor TframeFonts.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This is an on click event handler for the Font Style check boxes.

  @precon  None.
  @postcon Updates the internal font style array with the changes.

  @param   Sender as a TObject

**)
Procedure TframeFonts.FontStyleClick(Sender: TObject);

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

  @nohint  Project DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeFonts.InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject; //FI:O804
  Const DlgType: TITHDlgType); //FI:O804

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

  This method validates the frame.

  @precon  None.
  @postcon Nothing to validate.

  @return  a Boolean

**)
Function TframeFonts.IsValidated: Boolean;

Begin
  Result := True;
End;

(**

  This method saves the message options to the Options record structure.

  @precon  None.
  @postcon Saves the message options to the Options record structure.

  @nohint  Project DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeFonts.SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject; //FI:O804
  Const DlgType: TITHDlgType); //FI:O804

Var
  iMessageType: TITHFonts;

Begin
  GlobalOps.FontName[fnHeader] := cbxHeaderFontName.Text;
  GlobalOps.FontName[fnTools]  := cbxToolFontName.Text;
  For iMessageType := Low(TITHFonts) To High(TITHFonts) Do
    Begin
      GlobalOps.FontColour[iMessageType] := FFontColour[iMessageType];
      GlobalOps.FontStyles[iMessageType] := FFontStyle[iMessageType];
    End;
End;

End.

