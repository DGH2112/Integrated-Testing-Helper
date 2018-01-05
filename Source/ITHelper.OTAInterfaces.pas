(**

  This module contains the code to install the main wizard into the IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    05 Jan 2018

**)
Unit ITHelper.OTAInterfaces;

Interface

Uses
  ToolsAPI;

  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices; RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation

Uses
  ITHelper.Wizard;

{$INCLUDE 'CompilerDefinitions.inc'}

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @nocheck MissingCONSTInParam
  @nohints

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices; RegisterProc : TWizardRegisterProc;
  Var Terminate: TWizardTerminateProc) : Boolean; StdCall; //FI:O804

Begin
  Result := BorlandIDEServices <> Nil;
  If Result Then
    RegisterProc(TITHWizard.Create);
End;

End.
