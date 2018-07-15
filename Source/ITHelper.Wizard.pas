(**

  This module contains a Delphi IDE Wizard which implements support for running
  external tools before and after the compilation of the current project.

  @Version 1.0
  @Date    14 Jul 2018
  @Author  David Hoyle

**)
Unit ITHelper.Wizard;

Interface

Uses
  ToolsAPI,
  Menus,
  ITHelper.Interfaces;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** An enumerate to define which dialogue should be displayed. **)
  TSetting = (seProject, seBefore, seAfter, seZip);

  (** This class implements the IDE Wizard which provides the IDE interface
      to the package. **)
  TITHWizard = Class(TNotifierObject, IOTAWizard)
  Strict Private
    FAboutBoxIndex          : Integer;
    FGlobalOps              : IITHGlobalOptions;
    FTestingHelperMenu      : TMenuItem;
    FProjectMgrMenuIndex    : Integer;
    FProjectMgrMenuNotifier : IOTAProjectMenuItemCreatorNotifier;
    FIDENotifierIndex       : Integer;
    //FHTMLHelpCookie         : THandle;
  Strict Protected
    // IOTAWizard
    Procedure Execute;
    Function  GetIDString: String;
    Function  GetName: String;
    Function  GetState: TWizardState;
    // General Methods
    Procedure CreateMenus;
    Procedure ProjectOptions(Const Project: IOTAProject);
    Procedure BeforeCompilation(Const Project: IOTAProject);
    Procedure AfterCompilation(Const Project: IOTAProject);
    Procedure ZIPOptions(Const Project: IOTAProject);
    // Event Handler
    Procedure BeforeCompilationClick(Sender: TObject);
    Procedure AfterCompilationClick(Sender: TObject);
    Procedure ToggleEnabled(Sender: TObject);
    Procedure FontDialogueClick(Sender: TObject);
    Procedure ZIPDialogueClick(Sender: TObject);
    Procedure ZIPDialogueUpdate(Sender: TObject);
    Procedure GlobalOptionDialogueClick(Sender: TObject);
    Procedure ProjectOptionsClick(Sender: TObject);
    Procedure ProjectOptionsUpdate(Sender: TObject);
    Procedure UpdateEnabled(Sender: TObject);
    Procedure BeforeCompilationUpdate(Sender: TObject);
    Procedure AfterCompilationUpdate(Sender: TObject);
    Procedure HelpClick(Sender : TObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure ConfigureOptions(Const Project: IOTAProject; Const Setting: TSetting);
  End;

Implementation

Uses
  Windows,
  SysUtils,
  Dialogs,
  Graphics,
  Forms,
  StrUtils,
  Variants,
  ActnList,
  ITHelper.EnabledOptions,
  ITHelper.ProcessingForm,
  ITHelper.ExternalProcessInfo,
  ITHelper.ProjectManagerMenuInterface,
  ITHelper.FontDialogue,
  ITHelper.ZIPDialogue,
  ITHelper.GlobalOptionsDialogue,
  ITHelper.ProjectOptionsDialogue, 
  ITHelper.SplashScreen, 
  ITHelper.AboutBox, 
  ITHelper.IDENotifierInterface, 
  ITHelper.Types, 
  ITHelper.ConfigurationForm, 
  ITHelper.TestingHelperUtils, 
  ITHelper.GlobalOptions;

Const
  (** A constant to define the failed state of a wizard / notifier interface. **)
  iWizardFailState = -1;

(**

  This method displays the After Compilation options dialogue for configuring tools to be run after a 
  successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the After Compilation options dialogue for configuring tools to be run after a 
           successful compilation.

  @param   Project as an IOTAProject as a constant

**)
Procedure TITHWizard.AfterCompilation(Const Project: IOTAProject);

Begin
  If TfrmITHConfigureDlg.Execute(Project, FGlobalOps, dtAfter) Then
    FGlobalOps.Save;
End;

(**

  This is an on click event handler for the After Compilation action.

  @precon  None.
  @postcon Invokes the After Compilation dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.AfterCompilationClick(Sender: TObject);

Begin
  If TITHToolsAPIFunctions.ActiveProject <> Nil Then
    AfterCompilation(TITHToolsAPIFunctions.ActiveProject);
End;

(**

  This is an on update event handler for the After Compilation action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TITHWizard.AfterCompilationUpdate(Sender: TObject);

ResourceString
  strAfterCompilationOptionsFor = 'After Compilation Options for %s';
  strAfterCompilationOptionsNoActiveProject = 'After Compilation Options: No Active Project!';

Var
  strProject: String;
  P : IOTAProject;
  A : TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      P := TITHToolsAPIFunctions.ActiveProject;
      If P <> Nil Then
        Begin
          A.Enabled    := True;
          strProject := TITHToolsAPIFunctions.GetProjectName(P);
          A.Caption    := Format(strAfterCompilationOptionsFor, [strProject])
        End
      Else
        Begin
          A.Caption := strAfterCompilationOptionsNoActiveProject;
          A.Enabled := False;
        End;
    End;
End;

(**

  This method displays the Before Compilation options dialogue for configuring tools to be run before a 
  successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the Before Compilation options dialogue for configuring tools to be run before a 
           successful compilation.

  @param   Project as an IOTAProject as a constant

**)
Procedure TITHWizard.BeforeCompilation(Const Project: IOTAProject);

Begin
  If TfrmITHConfigureDlg.Execute(Project, FGlobalOps, dtBefore) Then
    FGlobalOps.Save;
End;

(**

  This is an on click event handler for the Before Compilation action.

  @precon  None.
  @postcon Displays the before compilation dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.BeforeCompilationClick(Sender: TObject);

Begin
  If TITHToolsAPIFunctions.ActiveProject <> Nil Then
    BeforeCompilation(TITHToolsAPIFunctions.ActiveProject);
End;

(**

  This is an on update event handler for the Before Compilation action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TITHWizard.BeforeCompilationUpdate(Sender: TObject);

ResourceString
  strBeforeCompilationOptionsFor = 'Before Compilation Options for %s';
  strBeforeCompilationOptionsNoActiveProject = 'Before Compilation Options: No Active Project!';

Var
  P         : IOTAProject;
  strProject: String;
  A : TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      P := TITHToolsAPIFunctions.ActiveProject;
      If P <> Nil Then
        Begin
          A.Enabled    := True;
          strProject := TITHToolsAPIFunctions.GetProjectName(P);
          A.Caption    := Format(strBeforeCompilationOptionsFor, [strProject])
        End
      Else
        Begin
          A.Caption := strBeforeCompilationOptionsNoActiveProject;
          A.Enabled := False;
        End;
    End;
End;

(**

  This method displays the options dialogue for the given project.

  @precon  None.
  @postcon Displays the options dialogue for the given project.

  @param   Project as an IOTAProject as a constant
  @param   Setting as a TSetting as a constant

**)
Procedure TITHWizard.ConfigureOptions(Const Project: IOTAProject; Const Setting: TSetting);

Begin
  If Project <> Nil Then
    Case Setting Of
      seProject: ProjectOptions(Project);
      seBefore:  BeforeCompilation(Project);
      seAfter:   AfterCompilation(Project);
      seZip:     ZIPOptions(Project);
    End;
End;

(**

  This is the constructor method for the TTestingHelperWizard class.

  @precon  None.
  @postcon Initialises the wizard and creates a menu item.

**)
Constructor TITHWizard.Create;

Var
  PM : IOTAProjectManager;
  S : IOTAServices;

Begin
  Inherited Create;
  FIDENotifierIndex := iWizardFailState;
  InstallSplashScreen;
  FAboutBoxIndex := AddAboutBoxEntry;
  FProjectMgrMenuNotifier := TITHProjectManagerMenu.Create(Self);
  If Supports(BorlandIDEServices, IOTAProjectManager, PM) Then
    {$IFNDEF D2010}
    FProjectMgrMenu := PM.AddMenuCreatorNotifier(FProjectMgrMenuNotifier);
    {$ELSE}
    FProjectMgrMenuIndex := PM.AddMenuItemCreatorNotifier(FProjectMgrMenuNotifier);
    {$ENDIF}
  CreateMenus;
  FGlobalOps := TITHGlobalOptions.Create;
  If Supports(BorlandIDEServices, IOTAServices, S) Then
    FIDENotifierIndex := S.AddNotifier(TITHelperIDENotifier.Create(FGlobalOps));
  //FHTMLHelpCookie := HTMLHelp(Application.Handle, Nil, HH_INITIALIZE, 0);
End;

(**

  This method creates the Integrated Testing Helper IDE menus.

  @precon  None.
  @postcon The menus are created.

  @nocheck HardCodedString

**)
Procedure TITHWizard.CreateMenus;

Begin
  FTestingHelperMenu := TITHToolsAPIFunctions.CreateMenuItem('ITHTestingHelper', '&Testing Helper', 'Tools', Nil, Nil, True, False, '');
  TITHToolsAPIFunctions.CreateMenuItem('ITHEnabled', 'Oops...', 'ITHTestingHelper', ToggleEnabled, UpdateEnabled, False, True, 'Ctrl+Shift+Alt+F9', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHSeparator1', '', 'ITHTestingHelper', Nil, Nil, False, True, '');
  TITHToolsAPIFunctions.CreateMenuItem('ITHGlobalOptions', '&Global Options...', 'ITHTestingHelper', GlobalOptionDialogueClick, Nil, False, True, '', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHProjectOptions', '&Project Options...', 'ITHTestingHelper', ProjectOptionsClick, ProjectOptionsUpdate, False, True, '', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHBeforeCompilation', '&Before Compilation Tools...', 'ITHTestingHelper', BeforeCompilationClick, BeforeCompilationUpdate, False, True, '', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHAfterCompilation', '&After Compilation Tools...', 'ITHTestingHelper', AfterCompilationClick, AfterCompilationUpdate, False, True, '', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHZIPDlg', '&ZIP Options...', 'ITHTestingHelper', ZIPDialogueClick, ZIPDialogueUpdate, False, True, '', clFuchsia);
  TITHToolsAPIFunctions.CreateMenuItem('ITHFontDlg', 'Message &Fonts...', 'ITHTestingHelper', FontDialogueClick, Nil, False, True, '', clOlive);
  TITHToolsAPIFunctions.CreateMenuItem('ITHSeparator2', '', 'ITHTestingHelper', Nil, Nil, False, True, '');
  TITHToolsAPIFunctions.CreateMenuItem('ITHHelp', '&Help...', 'ITHTestingHelper', HelpClick, Nil, False, True, '');
End;

(**

  This is the destructor method for the TTestingHelperWizard class.

  @precon  None.
  @postcon Removes the meun item from the IDE.

**)
Destructor TITHWizard.Destroy;

Var
  PM : IOTAProjectManager;
  S : IOTAServices;
  
Begin
  HTMLHelp(0, Nil, HH_CLOSE_ALL, 0);
  //HTMLHelp(Application.Handle, Nil, HH_UNINITIALIZE, FHTMLHelpCookie);
  If FIDENotifierIndex > iWizardFailState Then
    If Supports(BorlandIDEServices, IOTAServices, S) Then
      S.RemoveNotifier(FIDENotifierIndex);
  {$IFNDEF D2005}
  FMenuTimer.Free;
  {$ENDIF}
  FTestingHelperMenu.Free;
  FGlobalOps := Nil;
  If FProjectMgrMenuIndex > -1 Then
    If Supports(BorlandIDEServices, IOTAPRojectManager, PM) Then
      {$IFNDEF D2010}
      PM.RemoveMenuCreatorNotifier(FProjectMgrMenuIndex);
      {$ELSE}
      PM.RemoveMenuItemCreatorNotifier(FProjectMgrMenuIndex);
      {$ENDIF}
  RemoveAboutBoxEntry(FAboutBoxIndex);
  Inherited Destroy;
End;

(**

  This method is a null implementation of the Wizards Execute interface.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TITHWizard.Execute;

Begin //FI:W519
End;

(**

  This is an on click event handler for the Font Dialogue menu item.

  @precon  None.
  @postcon Displays the message fonts dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.FontDialogueClick(Sender: TObject);

Begin
  TfrmITHFontDialogue.Execute(FGlobalOps);
End;

(**

  This method is an implementation of the Wizards GetIDString interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s ID String.

  @return  a String

**)
Function TITHWizard.GetIDString: String;

Const
  strIDString = 'Seasons.Fall.Music.Integrated.Testing.Helper';

Begin
  Result := strIDString;
End;

(**

  This method is an implementation of the Wizards GetName interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s Name.

  @return  a string

**)
Function TITHWizard.GetName: String;

ResourceString
  strName = 'Integrated Testing Helper - Support for running external tools before and after ' +
    'compilations.';

Begin
  Result := strName;
End;

(**

  This method is an implementation of the Wizards GetState interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s State.

  @return  a TWizardState

**)
Function TITHWizard.GetState: TWizardState;

Begin
  Result := [wsEnabled];
End;

(**

  This is an on click event handler for the Global Options dialogue action.

  @precon  None.
  @postcon Displazs the Global Options dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.GlobalOptionDialogueClick(Sender: TObject);

Begin
  TfrmITHGlobalOptionsDialogue.Execute(FGlobalOps);
End;

(**

  This is an on click event handler for the Help action.

  @precon  None.
  @postcon Displays the HTML Help for the add-in.

  @param   Sender as a TObject

**)
Procedure TITHWizard.HelpClick(Sender: TObject);

Const
  strHelpStartPage = 'Welcome';

Begin
  HTMLHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strHelpStartPage)), HH_DISPLAY_TOC, 0);
End;

(**

  This method displays the Project Options dialogue.

  @precon  Project must be a valid instance.
  @postcon Displays the Project Options dialogue.

  @param   Project as an IOTAProject as a constant

**)
Procedure TITHWizard.ProjectOptions(Const Project: IOTAProject);

Begin
  TfrmITHProjectOptionsDialogue.Execute(FGlobalOps, Project);
End;

(**

  This is an on click event handler for the Project Options action.

  @precon  None.
  @postcon Dispays the projects options dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.ProjectOptionsClick(Sender: TObject);

Begin
  If TITHToolsAPIFunctions.ActiveProject <> Nil Then
    ProjectOptions(TITHToolsAPIFunctions.ActiveProject);
End;

(**

  This is an on update event handler for the Project Options action.

  @precon  None.
  @postcon Updates the project options action caption based on the avtive project.

  @param   Sender as a TObject

**)
Procedure TITHWizard.ProjectOptionsUpdate(Sender: TObject);

ResourceString
  strProjectOptionsFor = 'Project Options for %s';
  strProjectOptionsNoActiveProject = 'Project Options: No Active Project!';

Var
  P : IOTAProject;
  strProject: String;
  A : TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      P := TITHToolsAPIFunctions.ActiveProject;
      If P <> Nil Then
        Begin
          A.Enabled    := True;
          strProject := TITHToolsAPIFunctions.GetProjectName(P);
          A.Caption    := Format(strProjectOptionsFor, [strProject])
        End
      Else
        Begin
          A.Caption := strProjectOptionsNoActiveProject;
          A.Enabled := False;
        End;
    End;
End;

(**

  This is an on click event handler for the Enable Menu item.

  @precon  None.
  @postcon Enables / Disable the ITHelper for the current project.

  @param   Sender as a TObject

**)
Procedure TITHWizard.ToggleEnabled(Sender: TObject);

ResourceString
  strThereNotActiveProjectToEnabledOrDisable = 'There is not active project to enabled or disable.';

Var
  strProjectGroup: String;
  PG             : IOTAProjectGroup;
  Ops            : TITHEnabledOptions;

Begin
  PG := TITHToolsAPIFunctions.ProjectGroup;
  If PG <> Nil Then
    Begin
      Ops := FGlobalOps.ProjectGroupOps;
      If TfrmITHEnabledOptions.Execute(strProjectGroup, Ops) Then
        FGlobalOps.ProjectGroupOps := Ops;
    End
  Else
    ShowMessage(strThereNotActiveProjectToEnabledOrDisable);
End;

(**

  This method is an on update event handler for the Enabled action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TITHWizard.UpdateEnabled(Sender: TObject);

ResourceString
  strITHelperFor = 'ITHelper for %s';
  strNotAvailable = 'Not Available';

Var
  strProjectGroup: String;
  PG : IOTAProjectGroup;
  A : TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      PG := TITHToolsAPIFunctions.ProjectGroup;
      If PG <> Nil Then
        Begin
          A.Enabled         := True;
          strProjectGroup := ExtractFileName(PG.FileName);
          A.Caption         := Format(strITHelperFor, [strProjectGroup]);
        End
      Else
        Begin
          A.Caption := strNotAvailable;
          A.Enabled := False;
        End;
    End;
End;

(**

  This is an on click event handler for the ZIP Dialogue action.

  @precon  None.
  @postcon Displays thezipping options dialogue.

  @param   Sender as a TObject

**)
Procedure TITHWizard.ZIPDialogueClick(Sender: TObject);

Begin
  If TITHToolsAPIFunctions.ActiveProject <> Nil Then
    ZIPOptions(TITHToolsAPIFunctions.ActiveProject);
End;

(**

  This is an on update event handler for the ZIP Dialogue action.

  @precon  None.
  @postcon Updates the actions cption.

  @param   Sender as a TObject

**)
Procedure TITHWizard.ZIPDialogueUpdate(Sender: TObject);

ResourceString
  strZIPOptionsFor = 'ZIP Options for %s';
  strZIPOptionsNoActiveProject = 'ZIP Options: No Active Project!';

Var
  P         : IOTAProject;
  strProject: String;
  A : TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      P := TITHToolsAPIFunctions.ActiveProject;
      If P <> Nil Then
        Begin
          A.Enabled    := True;
          strProject := TITHToolsAPIFunctions.GetProjectName(P);
          A.Caption    := Format(strZIPOptionsFor, [strProject])
        End
      Else
        Begin
          A.Caption := strZIPOptionsNoActiveProject;
          A.Enabled := False;
        End;
    End;
End;

(**

  This method displays the ZIP Options dialogue for configuring the zipping of the project after a 
  successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the ZIP Options dialogue for configuring the zipping of the project after a 
           successful compilation.

  @param   Project as an IOTAProject as a constant

**)
Procedure TITHWizard.ZIPOptions(Const Project: IOTAProject);

Begin
  TfrmITHZIPDialogue.Execute(Project, FGlobalOps);
End;

End.
