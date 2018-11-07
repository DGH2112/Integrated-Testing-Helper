(**
  
  This module contains a class for implementing IOTACustomMessages in the IDE for displaying messages
  for this plug-in (custom colours and fonts).

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Nov 2018
  
**)
Unit ITHelper.CustomMessages;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  ToolsAPI,
  Graphics,
  Windows,
  ITHelper.Interfaces;

Type
  (** This class defined a custom message for the IDE. **)
  TITHCustomMessage = Class(TInterfacedObject, IUnknown, IOTACustomMessage, INTACustomDrawMessage,
    IITHCustomMessage)
  Strict Private
    FMsg         : String;
    FFontName    : String;
    FForeColour  : TColor;
    FStyle       : TFontStyles;
    FBackColour  : TColor;
    FMessagePntr : Pointer;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IOTACustomMessage
    Function GetColumnNumber: Integer;
    Function GetFileName: String;
    Function GetLineNumber: Integer;
    Function GetLineText: String;
    Procedure ShowHelp;
    // INTACustomDrawMessage
    Function CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect;
    Procedure Draw(Canvas: TCanvas; Const Rect: TRect; Wrap: Boolean);
    // IITHCustomMessage
    Procedure SetForeColour(Const iColour : TColor);
    Function  GetMessagePntr : Pointer;
    Procedure SetMessagePntr(Const ptrValue : Pointer);
    // General Methods
  Public
    Constructor Create(Const strMsg : String; Const FontName : String;
      Const ForeColour : TColor = clBlack; Const Style : TFontStyles = [];
      Const BackColour : TColor = clWindow);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils;
  
(**

  Calculates the bounding rectangle. CalcRect computes the bounding box required by the entire message. 
  The message view itself always displays messages in a single line of a fixed size. If the user hovers 
  the cursor over a long message, a tooltip displays the entire message. CalcRect returns the size of the
  tooltip window. The Canvas parameter is the canvas for drawing the message. The MaxWidth parameter is 
  the maximum allowed width of the bounding box (e.g., the screen width). The Wrap Parameter is true to 
  word-wrap the message onto multiple lines. It is false if the message must be kept to one line. The 
  Return value is the bounding rectangle required by the message.

  @precon  None.
  @postcon We calculate the size of the message here.

  @nocheck MissingCONSTInParam
  @nohint  MaxWidth Wrap

  @param   Canvas   as a TCanvas
  @param   MaxWidth as an Integer
  @param   Wrap     as a Boolean
  @return  a TRect

**)
Function TITHCustomMessage.CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect; //FI:O804

Const
  strTextHeightTest = 'Wp';

Begin
  Canvas.Font.Name := FFontName;
  Canvas.Font.Style := FStyle;
  Result := Canvas.ClipRect;
  Result.Bottom := Result.Top + Canvas.TextHeight(strTextHeightTest);
  Result.Right := Result.Left + Canvas.TextWidth(FMsg);
End;

(**

  This is the constructor for the TCustomMessage class.

  @precon  None.
  @postcon Creates a custom message with fore and background colours and font styles.

  @param   strMsg     as a String as a constant
  @param   FontName   as a String as a constant
  @param   ForeColour as a TColor as a constant
  @param   Style      as a TFontStyles as a constant
  @param   BackColour as a TColor as a constant

**)
Constructor TITHCustomMessage.Create(Const strMsg: String; Const FontName: String;
  Const ForeColour: TColor = clBlack; Const Style: TFontStyles = [];
  Const BackColour: TColor = clWindow);

Const
  strValidChars: Set Of AnsiChar = [#10, #13, #32 .. #128];

Var
  i: Integer;
  iLength: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  SetLength(FMsg, Length(strMsg));
  iLength := 0;
  For i := 1 To Length(strMsg) Do
    {$IFDEF D2009}
    If CharInSet(strMsg[i], strValidChars) Then
    {$ELSE}
    If strMsg[i] In strValidChars Then
    {$ENDIF}
      Begin
        FMsg[iLength + 1] := strMsg[i];
        Inc(iLength);
      End;
  SetLength(FMsg, iLength);
  FFontName := FontName;
  FForeColour := ForeColour;
  FStyle := Style;
  FBackColour := BackColour;
  FMessagePntr := Nil;
End;

(**

  A destructor for the TITHCustomMessage class.

  @precon  None.
  @postcon Does nothing but used for codes site tracing.

**)
Destructor TITHCustomMessage.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  Draws the message. Draw draws the message in the message view window or in a tooltip window. The Canvas
  parameter is the canvas on which to draw the message. The Rect parameter is the bounding box for the 
  message. If you draw outside this rectangle, you might obscure other messages. The Wrap Parameter is 
  true to word-wrap the message on multiple lines or false to keep the message on a single line. The 
  message view window always uses a single line for each message, but the tooltip (which the user sees by
  hovering the cursor over the message) can be multiple lines. The drawing objects (brush, pen, and font
  ) are set up appropriately for drawing messages that look like all the other messages in the message 
  view. In particular, the brush and font colors are set differently depending on whether the message is 
  selected. A custom-drawn message should not alter the colors or other graphic parameters without good 
  reason.

  @precon  None.
  @postcon This is where we draw the message on the canvas.

  @nocheck MissingCONSTInParam
  @nohint  Wrap

  @param   Canvas as a TCanvas
  @param   Rect   as a TRect as a constant
  @param   Wrap   as a Boolean

**)
Procedure TITHCustomMessage.Draw(Canvas: TCanvas; Const Rect: TRect; Wrap: Boolean); //FI:O804

Var
  R: TRect;
  strMsg: String;

Begin
  If Canvas.Brush.Color = clWindow Then
    Begin
      Canvas.Font.Color := FForeColour;
      Canvas.Brush.Color := FBackColour;
      Canvas.FillRect(Rect);
    End;
  R := Rect;
  strMsg := FMsg;
  Canvas.Font.Name := FFontName;
  Canvas.Font.Style := FStyle;
  Canvas.TextRect(R, strMsg, [tfLeft, tfVerticalCenter, tfEndEllipsis]);
End;

(**

  Returns the column number.

  GetColumnNumber returns the column number in the associated source file. When
  the user double-clicks the message in the message view, the IDE shows the
  source file and positions the cursor at the location given by the line number
  and column number.

  @precon  None.
  @postcon We does use this in this implementation but you would return the
           column number for your message here.

  @return  an Integer

**)
Function TITHCustomMessage.GetColumnNumber: Integer;

Begin
  Result := 0;
End;

(**

  Returns the source file name.

  GetFileName returns the complete path to the associated source file. When the
  user double-clicks the message in the message view, the IDE shows the source
  file and positions the cursor at the location given by the line number and
  column number.

  Return an empty string if the message is not associated with a source file.

  @precon  None.
  @postcon We return an empty string for this implementation othereise you would
           return the full name and path of the file associated with the
           message.

  @return  a String

**)
Function TITHCustomMessage.GetFileName: String;

Begin
  Result := '';
End;

(**

  Returns the line number.

  GetLineNumber returns the line number in the associated source file. When the
  user double-clicks the message in the message view, the IDE shows the source
  file and positions the cursor at the location given by the line number and
  column number.

  @precon  None.
  @postcon We return 0 for out implementation but you would return the line
           number of the message here.

  @return  an Integer

**)
Function TITHCustomMessage.GetLineNumber: Integer;

Begin
  Result := 0;
End;

(**

  Returns the text of the message.

  GetLineText returns the text of the custom message.

  @precon  None.
  @postcon Here we return the message

  @return  a String

**)
Function TITHCustomMessage.GetLineText: String;

Begin
  Result := FMsg;
End;

(**

  This is a getter method for the MessagePtr property.

  @precon  None.
  @postcon Returns the message pointer (used in parenting messages).

  @return  a Pointer

**)
Function TITHCustomMessage.GetMessagePntr: Pointer;

Begin
  Result := FMessagePntr;
End;

(**

  This is a setter method for the ForeColour property.

  @precon  None.
  @postcon Sets the message fore colour.

  @param   iColour as a TColor as a constant

**)
Procedure TITHCustomMessage.SetForeColour(Const iColour: TColor);

Begin
  If FForeColour <> iColour Then
    FForeColour := iColour;
End;

(**

  This is a setter method for the MessagePtr property.

  @precon  None.
  @postcon Sets the message pointer (used in parenting messages).

  @param   ptrValue as a Pointer as a constant

**)
Procedure TITHCustomMessage.SetMessagePntr(Const ptrValue: Pointer);

Begin
  If FMessagePntr <> ptrValue Then
    FMessagePntr := ptrValue;
End;

(**

  Provides help for the message.

  When the user selects the custom message and presses the F1 key, the IDE calls
  the ShowHelp function to provide help to the user.

  @nocheck EmptyMethod
  
  @precon  None.
  @postcon Not implemented but you would display the custom help for the message
           here.

**)
Procedure TITHCustomMessage.ShowHelp;

Begin //FI:W519
End;

End.

