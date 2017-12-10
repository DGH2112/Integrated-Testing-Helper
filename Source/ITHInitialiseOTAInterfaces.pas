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

  procedure Register;

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

{$IFDEF D2005}

Var
  (** This is a handle for the splash screen bitmap resource **)
  bmSplashScreen : HBITMAP;
  (** This is a variable to hold the major version number for the package. **)
  iMajor : Integer;
  (** This is a variable to hold the minor version number for the package. **)
  iMinor : Integer;
  (** This is a variable to hold the bug fix version number for the package. **)
  iBugFix : Integer;
  (** This is a variable to hold the build number for the package.  **)
  iBuild : Integer;
{$ENDIF}

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
  (** A private variable to hold the About Plugin reference for later release. **)
  iAboutPlugin : Integer = iWizardFailState;
  {$ENDIF}

(**

  This method is called by both Register and InitWizard as common code for initialising
  the wizard interfaces in the IDE adding.

  @precon  None.
  @postcon Returns a valid instance of the TTestingHelperWizard;

  @param   WizardType as a TWizardType
  @return  a TTestingHelperWizard

**)
Function InitialiseWizard(WizardType : TWizardType) : TTestingHelperWizard;

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
  bmSplashScreen := LoadBitmap(hInstance, 'TestingHelperSplashScreenBitMap');
  iAboutPlugin := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevisions, iBugFix + 1, 1), Application.Title]),
    'An IDE expert to allow the configuration of pre and post compilation processes and ' +
    'automatically ZIP the successfully compiled project for release.',
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
    Format('SKU Build %d.%d.%d.%d', [iMajor, iMinor, iBugfix, iBuild]));
  {$ENDIF}
End;

(**

  This method is called by the IDE to initialise the package in the IDE.

  @precon  None.
  @postcon Initialises the package wizard.

**)
procedure Register;

begin
  InitialiseWizard(wtPackageWizard);
end;

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; StdCall;

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
  {$IFDEF D2010}
  If iAboutPlugin > iWizardFailState Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPlugin);
  {$ENDIF}
End.
