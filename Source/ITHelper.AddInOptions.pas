(**
  
  This modukle contains a generic implementation of the INTAAddInOptions interfaces for all global
  options frames to be inserted into the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Jul 2018
  
**)
Unit ITHelper.AddInOptions;

Interface

{$INCLUDE CompilerDefinitions.inc}

{$IFDEF DXE00}
Uses
  ToolsAPI,
  Forms,
  ITHelper.Interfaces;

Type
  (** A class of TFrames so we can pass the class as a param to the constructor. **)
  TITHFrameClass = Class Of TCustomFrame;

  (** An implementation of the INTAAddInOptions interfaces for the global options frames for
      ITHelper. **)
  TITHAddInOptions = Class(TInterfacedObject, INTAAddInOptions)
  Strict Private
    FGlobalOptions: IITHGlobalOptions;
    FFrame:         TCustomFrame;
    FFrameClass:    TITHFrameClass;
    FOptionsPath:   String;
  Strict Protected
    // INTAddInOptions
    Procedure DialogClosed(Accepted: Boolean);
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function  GetArea: String;
    Function  GetCaption: String;
    Function  GetFrameClass: TCustomFrameClass;
    Function  GetHelpContext: Integer;
    Function  IncludeInIDEInsight: Boolean;
    Function  ValidateContents: Boolean;
  Public
    Constructor Create(Const GlobalOps : IITHGlobalOptions; Const FrameCls: TITHFrameClass;
      Const strOptionsPath: String);
    Destructor Destroy; Override;
  End;
{$ENDIF}

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils;

{$IFDEF DXE00}
(**

  A constructor for the TITHAddInOptions class.

  @precon  None.
  @postcon Stores references for later and registers the options frame with the IDE.

  @param   GlobalOps      as an IITHGlobalOptions as a constant
  @param   FrameCls       as a TITHFrameClass as a constant
  @param   strOptionsPath as a String as a constant

**)
Constructor TITHAddInOptions.Create(Const GlobalOps : IITHGlobalOptions; Const FrameCls: TITHFrameClass;
  Const strOptionsPath: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FGlobalOptions := GlobalOps;
  FFrameClass := FrameCls;
  FOptionsPath := strOptionsPath;
End;

(**

  A destructor for the TITHAddInOptions class.

  @precon  None.
  @postcon Unregisters the options frame.

**)
Destructor TITHAddInOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This method is called when the IDE options dialogue is closing.

  @precon  None.
  @postcon If the dialogue was confirmed we save our options frames information.

  @nocheck MissingCONSTInParam

  @param   Accepted as a Boolean

**)
Procedure TITHAddInOptions.DialogClosed(Accepted: Boolean);

Var
  F : IITHOptionsFrame;
  
Begin
  If Supports(FFrame, IITHOptionsFrame, F) And Accepted Then
    F.SaveOptions(FGlobalOptions);
End;

(**

  This method is called when the IDE creates the options frame for us.

  @precon  None.
  @postcon Stores a reference to the frame for later and initialises the frame.

  @nocheck MissingCONSTInParam

  @param   AFrame as a TCustomFrame

**)
Procedure TITHAddInOptions.FrameCreated(AFrame: TCustomFrame);

Var
  F : IITHOptionsFrame;
  
Begin
  FFrame := AFrame;
  If Supports(FFrame, IITHOptionsFrame, F) Then
    F.InitialiseOptions(FGlobalOptions);
End;

(**

  This is a getter method for the Area property.

  @precon  None.
  @postcon we return a null string as we want our options under the third party node.

  @return  a String

**)
Function TITHAddInOptions.GetArea: String;

Begin
  Result := '';
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon This returns the path to the optins frame in the IDE dialogue (under third party).

  @return  a String

**)
Function TITHAddInOptions.GetCaption: String;

Begin
  Result := FOptionsPath;
End;

(**

  This is a getter method for the FrameClass property.

  @precon  None.
  @postcon We return the frame class we want the IDE to create for us for this options frame.

  @return  a TCustomFrameClass

**)
Function TITHAddInOptions.GetFrameClass: TCustomFrameClass;

Begin
  Result := FFrameClass;
End;

(**

  This is a getter method for the HelpContext property.

  @precon  None.
  @postcon We returns zero.

  @return  an Integer

**)
Function TITHAddInOptions.GetHelpContext: Integer;

Begin
  Result := 0;
End;

(**

  This method is called to determine of the options frame should be shown in IDE Insight.

  @precon  None.
  @postcon We return true to include this in the IDE Insight.

  @return  a Boolean

**)
Function TITHAddInOptions.IncludeInIDEInsight: Boolean;

Begin
  Result := True;
End;

(**

  This method is called to validate the options frame.

  @precon  None.
  @postcon We returns the validation of the frame options interface.

  @return  a Boolean

**)
Function TITHAddInOptions.ValidateContents: Boolean;

Var
  F : IITHOptionsFrame;
  
Begin
  Result := False;
  If Supports(FFrame, IITHOptionsFrame, F) Then
    Result := F.IsValidated();
End;
{$ENDIF}

End.

