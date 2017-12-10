(**

  This module contains all the initialisation code for each of the interfaces and
  consolidates the mechanisms for creating them for both DLLs and Packages.

  @Version 1.0
  @Author  David Hoyle
  @Date    10 Dec 2017

**)
Unit ITHInitialiseOTAInterfaces;

Interface

Uses
  ToolsAPI;

  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
    RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation

Uses
  TestingHelperWizard,
  ProjectManagerMenuInterface,
  Forms,
  Windows,
  SysUtils,
  IDENotifierInterface, 
  ITHelper.ResourceStrings, 
  ITHelper.Constants;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** A type to distinguish between packages and DLL experts. **)
  TWizardType = (wtPackageWizard, wtDLLWizard);

Const
  (** A constant to define the failed state of a wizard / notifier interface. **)
  iWizardFailState = -1;

Var
  (** A private variable to hold the index number of the wizard returned in the
      Register procedure. **)
  iWizardIndex : Integer = iWizardFailState;
  (** A private variable to hold the index number of the IDE notifier returned in the
      Register procedure. **)
  iIDENotifierIndex : Integer = iWizardFailState;
  {$IFDEF D2005}
  (** A private variable to hold the index number of the project menu creator
      notifier so that it can be removed later. **)
  iPrjMgrMenu : Integer = iWizardFailState;
  {$ENDIF}

(**

  This method is called by both Register and InitWizard as common code for initialising the wizard 
  interfaces in the IDE adding.

  @precon  None.
  @postcon Returns a valid instance of the TTestingHelperWizard;

  @param   WizardType as a TWizardType as a constant
  @return  a TTestingHelperWizard

**)
Function InitialiseWizard(Const WizardType : TWizardType) : TTestingHelperWizard;

Var
  Svcs : IOTAServices;
  {$IFDEF D2005}
  MenuNotifier: TProjectManagerMenu;
  {$ENDIF}

Begin
  Svcs := BorlandIDEServices As IOTAServices;
  ToolsAPI.BorlandIDEServices := BorlandIDEServices;
  Application.Handle := Svcs.GetParentHandle;
  Result := TTestingHelperWizard.Create;
  If WizardType = wtPackageWizard Then
    iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Result);
  iIDENotifierIndex := (BorlandIDEServices As IOTAServices).AddNotifier(
    TTestingHelperIDENotifier.Create(Result.GlobalOps));
  {$IFDEF D2005}
  MenuNotifier := TProjectManagerMenu.Create(Result);
  {$IFNDEF D2010}
  iPrjMgrMenu := (BorlandIDEServices As IOTAProjectManager).AddMenuCreatorNotifier(
    MenuNotifier);
  {$ELSE}
  iPrjMgrMenu := (BorlandIDEServices As IOTAProjectManager).AddMenuItemCreatorNotifier(
    MenuNotifier);
  {$ENDIF}
  {$ENDIF}
End;

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @nometric MissingCONSTInParam
  @nohint   Terminate

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  Var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Begin
  Result := BorlandIDEServices <> Nil;
  If Result Then
    RegisterProc(InitialiseWizard(wtDLLWizard));
End;

(** Create a splash screen on BDS 2006 and above. **)
Initialization
(** Ensure that the wizard is removed from the IDE.  **)
Finalization
  If iIDENotifierIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAServices).RemoveNotifier(iIDENotifierIndex);
  {$IFDEF D2005}
  If iPrjMgrMenu > iWizardFailState Then
    {$IFNDEF D2010}
    (BorlandIDEServices As IOTAProjectManager).RemoveMenuCreatorNotifier(iPrjMgrMenu);
    {$ELSE}
    (BorlandIDEServices As IOTAProjectManager).RemoveMenuItemCreatorNotifier(iPrjMgrMenu);
    {$ENDIF}
  {$ENDIF}
  If iWizardIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAServices).RemoveNotifier(iWizardIndex);
End.
