(**
  
  This module contains a class which implements the IOTACompileNotifier in order to find out what mode
  the compilation is under (TOTACompileMode).

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2019
  
**)
Unit ITHelper.CompileNotifier;

Interface

Uses
  ToolsAPI;

{$INCLUDE 'CompilerDefinitions.inc'}

{$IFDEF D2010}
Type
  (** A custom interface to add a return function for the compile mode. **)
  IITHCompileNotifier = Interface(IOTACompileNotifier)
    Function  CompileMode : TOTACompileMode;
  End;

  (** A class which implements the IOTACompileNotifier interface. **)
  TITHCompileNotifier = Class(TNotifierObject, IOTACompileNotifier, IITHCompileNotifier)
  Strict Private
    FCompileMode: TOTACompileMode;
  Strict Protected
    // IOTACompileNotifier
    Procedure ProjectCompileFinished(Const Project: IOTAProject; Result: TOTACompileResult);
    Procedure ProjectCompileStarted(Const Project: IOTAProject; Mode: TOTACompileMode);
    Procedure ProjectGroupCompileFinished(Result: TOTACompileResult);
    Procedure ProjectGroupCompileStarted(Mode: TOTACompileMode);
    // IITHCompileNotifier
    Function  CompileMode: TOTACompileMode;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;
{$ENDIF D2010}

Implementation

{$IFDEF DEBUG}
Uses
  CodeSiteLogging;
{$ENDIF DEBUG}

{$IFDEF D2010}
(**

  This method returns the last compile mode notified by the IDE.

  @precon  None.
  @postcon The last compile mode is returned.

  @return  a TOTACompileMode

**)
Function TITHCompileNotifier.CompileMode: TOTACompileMode;

Begin
  Result := FCompileMode;
End;

(**

  A constructor for the TITHCompileNotifier class.

  @precon  None.
  @postcon Does nothing - used for code site tracing to check for memory leaks due to coupling.

**)
Constructor TITHCompileNotifier.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create;
End;

(**

  A destructor for the TITHCompileNotifier class.

  @precon  None.
  @postcon Does nothing - used for code site tracing to check for memory leaks due to coupling.

**)
Destructor TITHCompileNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This method is called when an individual project has finished compiling.

  @precon  None.
  @postcon Not used.

  @nohint  Project Result
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Project as an IOTAProject as a constant
  @param   Result  as a TOTACompileResult

**)
Procedure TITHCompileNotifier.ProjectCompileFinished(Const Project: IOTAProject;
  Result: TOTACompileResult);

Begin

End;

(**

  This method is called when an individual project starts compiling.

  @precon  None.
  @postcon Captures the compile mode.

  @nohint  Project
  @nocheck MissingCONSTInParam

  @param   Project as an IOTAProject as a constant
  @param   Mode    as a TOTACompileMode

**)
Procedure TITHCompileNotifier.ProjectCompileStarted(Const Project: IOTAProject; Mode: TOTACompileMode);

Begin
  FCompileMode := Mode;
End;

(**

  This method is called when the compile group is finished.

  @precon  None.
  @postcon Not used.

  @nohint  Result
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Result as a TOTACompileResult

**)
Procedure TITHCompileNotifier.ProjectGroupCompileFinished(Result: TOTACompileResult);

Begin

End;

(**

  This method is called when the compile group is started.

  @precon  None.
  @postcon Not used.

  @nohint  Mode
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Mode as a TOTACompileMode

**)
Procedure TITHCompileNotifier.ProjectGroupCompileStarted(Mode: TOTACompileMode);

Begin

End;
{$ENDIF D2010}

End.

