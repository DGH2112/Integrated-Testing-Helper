(**

  This module contains a Delphi IDE Wizard which implements support for running
  external tools before and after the compilation of the current project.

  @Version 1.0
  @Date    08 Jul 2012
  @Author  David Hoyle

**)
Unit TestingHelperWizard;

Interface

Uses
  ToolsAPI,
  Classes,
  Menus,
  Contnrs,
  TestingHelperUtils,
  ExtCtrls,
  ConfigurationForm,
  IniFiles,
  GlobalOptions;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

Type
  (** An enumerate to define which dialogue should be displayed. **)
  TSetting = (seProject, seBefore, seAfter, seZip);

  (** This class implements the IDE Wizard which provides the IDE interface
      to the package. **)
  TTestingHelperWizard = Class(TNotifierObject, IOTAWizard)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FGlobalOps        : TGlobalOptions;
    FTestingHelperMenu: TMenuItem;
    //FHTMLHelpCookie   : THandle;
    {$IFNDEF D2005}
    FMenuTimer        : TTimer;
    {$ENDIF}
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure BeforeCompilationClick(Sender: TObject);
    Procedure AfterCompilationClick(Sender: TObject);
    Procedure ToggleEnabled(Sender: TObject);
    Procedure CheckForUpdatesClick(Sender: TObject);
    Procedure FontDialogueClick(Sender: TObject);
    Procedure ZIPDialogueClick(Sender: TObject);
    Procedure ZIPDialogueUpdate(Sender: TObject);
    Procedure GlobalOptionDialogueClick(Sender: TObject);
    Procedure ProjectOptionsClick(Sender: TObject);
    Procedure ProjectOptionsUpdate(Sender: TObject);
    Procedure UpdateEnabled(Sender: TObject);
    Procedure BeforeCompilationUpdate(Sender: TObject);
    Procedure AfterCompilationUpdate(Sender: TObject);
    Procedure ProjectOptions(Project: IOTAProject);
    Procedure BeforeCompilation(Project: IOTAProject);
    Procedure AfterCompilation(Project: IOTAProject);
    Procedure ZIPOptions(Project: IOTAProject);
    Procedure AboutClick(Sender : TObject);
    Procedure HelpClick(Sender : TObject);
    {$IFNDEF D2005}
    Procedure MenuTimerEvent(Sender: TObject);
    {$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Execute;
    Function GetIDString: String;
    Function GetName: String;
    Function GetState: TWizardState;
    Procedure ConfigureOptions(Project: IOTAProject; Setting: TSetting);
    (**
      This property provides external code with access to the global options of the
      application.
      @precon  None.
      @postcon Returns the global options.
      @return  a TGlobalOptions
    **)
    Property GlobalOps: TGlobalOptions Read FGlobalOps;
  End;

Implementation

Uses
  Windows,
  SysUtils,
  Dialogs,
  ProcessingForm,
  Graphics,
  Forms,
  DGHLibrary,
  ExternalProcessInfo,
  CheckForUpdates,
  StrUtils,
  Variants,
  EnabledOptions,
  ActnList,
  FontDialogue,
  ZIPDialogue,
  GlobalOptionsDialogue,
  ProjectOptionsDialogue,
  About;

ResourceString
  (** This is the software ID for check for updates on the internet. **)
  strSoftwareID = 'ITHelper2006';

(**

  This is an on click event for the Check for Updates menu item.

  @precon  None.
  @postcon Invokes the checking for updates on the internet.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.CheckForUpdatesClick(Sender: TObject);

Begin
  TCheckForUpdates.Execute(strSoftwareID, FGlobalOps.INIFileName, Sender <> Nil);
End;

(**

  This is an on click event handler for the About action.

  @precon  None.
  @postcon Displays the about dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.AboutClick(Sender: TObject);

Var
  strTitle : String;

Begin
  strTitle := Application.Title;
  Try
    Application.Title := 'Integrated Testing Helper';
    TfrmAbout.ShowAbout;
  Finally
    Application.Title := strTitle;
  End;
End;

(**

  This method displays the After Compilation options dialogue for configuring tools to be
  run after a successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the After Compilation options dialogue for configuring tools to be
           run after a successful compilation.

  @param   Project as an IOTAProject

**)
Procedure TTestingHelperWizard.AfterCompilation(Project: IOTAProject);

Begin
  If TfrmConfigure.Execute(Project, FGlobalOps, dtAfter) Then
    FGlobalOps.Save;
End;

(**

  This is an on click event handler for the After Compilation action.

  @precon  None.
  @postcon Invokes the After Compilation dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.AfterCompilationClick(Sender: TObject);
Begin
  If ActiveProject <> Nil Then
    AfterCompilation(ActiveProject);
End;

(**

  This is an on update event handler for the After Compilation action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.AfterCompilationUpdate(Sender: TObject);

Var
  strProject: String;
  P         : IOTAProject;

Begin
  If Sender Is TAction Then
    With Sender As TAction Do
      Begin
        P := ActiveProject;
        If P <> Nil Then
          Begin
            Enabled    := True;
            strProject := GetProjectName(P);
            Caption    := Format('After Compilation Options for %s', [strProject])
          End
        Else
          Begin
            Caption := 'After Compilation Options: No Active Project!';
            Enabled := False;
          End;
      End;
End;

(**

  This method displays the Before Compilation options dialogue for configuring tools to be
  run before a successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the Before Compilation options dialogue for configuring tools to be
           run before a successful compilation.

  @param   Project as an IOTAProject

**)
Procedure TTestingHelperWizard.BeforeCompilation(Project: IOTAProject);

Begin
  If TfrmConfigure.Execute(Project, FGlobalOps, dtBefore) Then
    FGlobalOps.Save;
End;

(**

  This is an on click event handler for the Before Compilation action.

  @precon  None.
  @postcon Displays the before compilation dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.BeforeCompilationClick(Sender: TObject);

Begin
  If ActiveProject <> Nil Then
    BeforeCompilation(ActiveProject);
End;

(**

  This method displays the options dialogue for the given project.

  @precon  None.
  @postcon Displays the options dialogue for the given project.

  @param   Project as an IOTAProject
  @param   Setting as a TSetting

**)
Procedure TTestingHelperWizard.ConfigureOptions(Project: IOTAProject; Setting: TSetting);

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
Constructor TTestingHelperWizard.Create;

Begin
  FTestingHelperMenu := CreateMenuItem('ITHTestingHelper', '&Testing Helper', 'Tools',
    Nil, Nil, True, False, '');
  CreateMenuItem('ITHEnabled', 'Oops...', 'ITHTestingHelper', ToggleEnabled,
    UpdateEnabled, False, True, 'Ctrl+Shift+Alt+F9', clFuchsia);
  CreateMenuItem('ITHSeparator1', '', 'ITHTestingHelper', Nil, Nil, False, True, '');
  CreateMenuItem('ITHGlobalOptions', '&Global Options...', 'ITHTestingHelper',
    GlobalOptionDialogueClick, Nil, False, True, '', clFuchsia);
  CreateMenuItem('ITHProjectOptions', '&Project Options...', 'ITHTestingHelper',
    ProjectOptionsClick, ProjectOptionsUpdate, False, True, '', clFuchsia);
  CreateMenuItem('ITHBeforeCompilation', '&Before Compilation Tools...',
    'ITHTestingHelper', BeforeCompilationClick, BeforeCompilationUpdate, False, True, '',
    clFuchsia);
  CreateMenuItem('ITHAfterCompilation', '&After Compilation Tools...', 'ITHTestingHelper',
    AfterCompilationClick, AfterCompilationUpdate, False, True, '', clFuchsia);
  CreateMenuItem('ITHZIPDlg', '&ZIP Options...', 'ITHTestingHelper', ZIPDialogueClick,
    ZIPDialogueUpdate, False, True, '', clFuchsia);
  CreateMenuItem('ITHFontDlg', 'Message &Fonts...', 'ITHTestingHelper', FontDialogueClick,
    Nil, False, True, '', clOlive);
  CreateMenuItem('ITHSeparator2', '', 'ITHTestingHelper', Nil, Nil, False, True, '');
  CreateMenuItem('ITHCheckForUpdates', 'Check for &Updates...', 'ITHTestingHelper',
    CheckForUpdatesClick, Nil, False, True, '', clFuchsia);
  CreateMenuItem('ITHHelp', '&Help...', 'ITHTestingHelper', HelpClick, Nil, False,
    True, '');
  CreateMenuItem('ITHAbout', '&About...', 'ITHTestingHelper', AboutClick, Nil, False,
    True, '');
  {$IFNDEF D2005} // Code to patch shortcuts into the menus in D7 and below.
  FMenuTimer          := TTimer.Create(Nil);
  FMenuTimer.OnTimer  := MenuTimerEvent;
  FMenuTimer.Interval := 1000;
  FMenuTimer.Enabled  := True;
  {$ENDIF}
  FGlobalOps := TGlobalOptions.Create;
  CheckForUpdatesClick(Nil);
  //FHTMLHelpCookie := HTMLHelp(Application.Handle, Nil, HH_INITIALIZE, 0);
End;

(**

  This is the destructor method for the TTestingHelperWizard class.

  @precon  None.
  @postcon Removes the meun item from the IDE.

**)
Destructor TTestingHelperWizard.Destroy;

Begin
  HTMLHelp(0, Nil, HH_CLOSE_ALL, 0);
  //HTMLHelp(Application.Handle, Nil, HH_UNINITIALIZE, FHTMLHelpCookie);
  {$IFNDEF D2005}
  FMenuTimer.Free;
  {$ENDIF}
  FTestingHelperMenu.Free;
  {$IFNDEF DXE20}
  //: @bug This has been removed from Delphi XE2 as it generates an Access Violation as
  //: the IDE closes down.
  ClearMessages([cmCompiler]);
  {$ENDIF}
  FGlobalOps.Free;
  Inherited Destroy;
End;

{$IFNDEF D2005}

(**

  This is an on timer event handler for the menu timer.

  @precon  None.
  @postcon In Delphi 7 and below - it patches the shortcuts onto the menu items
           as the Open Tools API "looses" the shortcuts.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.MenuTimerEvent(Sender: TObject);

Begin
  If Application.MainForm.Visible Then
    Begin
      PatchActionShortcuts(Sender);
      FMenuTimer.Enabled := False;
    End;
End;
{$ENDIF}

(**

  This method displays the Project Options dialogue.

  @precon  Project must be a valid instance.
  @postcon Displays the Project Options dialogue.

  @param   Project as an IOTAProject

**)
Procedure TTestingHelperWizard.ProjectOptions(Project: IOTAProject);

Begin
  TfrmProjectOptionsDialogue.Execute(FGlobalOps, Project);
End;

(**

  This is an on click event handler for the Project Options action.

  @precon  None.
  @postcon Dispays the projects options dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.ProjectOptionsClick(Sender: TObject);

Begin
  If ActiveProject <> Nil Then
    ProjectOptions(ActiveProject);
End;

(**

  This is an on update event handler for the Project Options action.

  @precon  None.
  @postcon Updates the project options action caption based on the avtive project.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.ProjectOptionsUpdate(Sender: TObject);

Var
  P         : IOTAProject;
  strProject: String;

Begin
  If Sender Is TAction Then
    With Sender As TAction Do
      Begin
        P := ActiveProject;
        If P <> Nil Then
          Begin
            Enabled    := True;
            strProject := GetProjectName(P);
            Caption    := Format('Project Options for %s', [strProject])
          End
        Else
          Begin
            Caption := 'Project Options: No Active Project!';
            Enabled := False;
          End;
      End;
End;

(**

  This method is a null implementation of the Wizards Execute interface.

  @precon  None.
  @postcon None.

**)
Procedure TTestingHelperWizard.Execute;

Begin
End;

(**

  This is an on click event handler for the Font Dialogue menu item.

  @precon  None.
  @postcon Displays the message fonts dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.FontDialogueClick(Sender: TObject);

Begin
  TfrmFontDialogue.Execute(FGlobalOps);
End;

(**

  This method is an implementation of the Wizards GetIDString interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s ID String.

  @return  a String

**)
Function TTestingHelperWizard.GetIDString: String;

Begin
  Result := 'TestingHelper';
End;

(**

  This method is an implementation of the Wizards GetName interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s Name.

  @return  a string

**)
Function TTestingHelperWizard.GetName: String;

Begin
  Result := 'Testing Helper - Support for running external tools before and ' +
    'after compilations.';
End;

(**

  This method is an implementation of the Wizards GetState interface.

  @precon  None.
  @postcon Returns the IDE Wizard`s State.

  @return  a TWizardState

**)
Function TTestingHelperWizard.GetState: TWizardState;

Begin
  Result := [wsEnabled];
End;

(**

  This is an on click event handler for the Global Options dialogue action.

  @precon  None.
  @postcon Displazs the Global Options dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.GlobalOptionDialogueClick(Sender: TObject);

Begin
  TfrmGlobalOptionsDialogue.Execute(FGlobalOps);
End;

(**

  This is an on click event handler for the Help action.

  @precon  None.
  @postcon Displays the HTML Help for the add-in.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.HelpClick(Sender: TObject);

Begin
  HTMLHelp(0, PChar(ITHHTMLHelpFile('Welcome')), HH_DISPLAY_TOC, 0);
End;

(**

  This is an on click event handler for the Enable Menu item.

  @precon  None.
  @postcon Enables / Disable the ITHelper for the current project.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.ToggleEnabled(Sender: TObject);

Var
  strProjectGroup: String;
  PG             : IOTAProjectGroup;
  Ops            : TEnabledOptions;

Begin
  PG := ProjectGroup;
  If PG <> Nil Then
    Begin
      Ops := FGlobalOps.ProjectGroupOps;
      If TfrmEnabledOptions.Execute(strProjectGroup, Ops) Then
        FGlobalOps.ProjectGroupOps := Ops;
    End
  Else
    ShowMessage('There is not active project to enabled or disable.');
End;

(**

  This is an on update event handler for the Before Compilation action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.BeforeCompilationUpdate(Sender: TObject);

Var
  P         : IOTAProject;
  strProject: String;

Begin
  If Sender Is TAction Then
    With Sender As TAction Do
      Begin
        P := ActiveProject;
        If P <> Nil Then
          Begin
            Enabled    := True;
            strProject := GetProjectName(P);
            Caption    := Format('Before Compilation Options for %s', [strProject])
          End
        Else
          Begin
            Caption := 'Before Compilation Options: No Active Project!';
            Enabled := False;
          End;
      End;
End;

(**

  This method is an on update event handler for the Enabled action.

  @precon  None.
  @postcon Enables or disables the action based on the availability of a project.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.UpdateEnabled(Sender: TObject);

Var
  strProjectGroup: String;
  PG             : IOTAProjectGroup;

Begin
  If Sender Is TAction Then
    With Sender As TAction Do
      Begin
        PG := ProjectGroup;
        If PG <> Nil Then
          Begin
            Enabled         := True;
            strProjectGroup := ExtractFileName(PG.FileName);
            Caption         := Format('ITHelper for %s', [strProjectGroup]);
          End
        Else
          Begin
            Caption := 'Not Available';
            Enabled := False;
          End;
      End;
End;

(**

  This is an on click event handler for the ZIP Dialogue action.

  @precon  None.
  @postcon Displays thezipping options dialogue.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.ZIPDialogueClick(Sender: TObject);

Begin
  If ActiveProject <> Nil Then
    ZIPOptions(ActiveProject);
End;

(**

  This is an on update event handler for the ZIP Dialogue action.

  @precon  None.
  @postcon Updates the actions cption.

  @param   Sender as a TObject

**)
Procedure TTestingHelperWizard.ZIPDialogueUpdate(Sender: TObject);

Var
  P         : IOTAProject;
  strProject: String;

Begin
  If Sender Is TAction Then
    With Sender As TAction Do
      Begin
        P := ActiveProject;
        If P <> Nil Then
          Begin
            Enabled    := True;
            strProject := GetProjectName(P);
            Caption    := Format('ZIP Options for %s', [strProject])
          End
        Else
          Begin
            Caption := 'ZIP Options: No Active Project!';
            Enabled := False;
          End;
      End;
End;

(**

  This method displays the ZIP Options dialogue for configuring the zipping of the project
  after a successful compilation.

  @precon  Project must be a valid instance.
  @postcon Displays the ZIP Options dialogue for configuring the zipping of the project
           after a successful compilation.

  @param   Project as an IOTAProject

**)
Procedure TTestingHelperWizard.ZIPOptions(Project: IOTAProject);

Begin
  TfrmZIPDialogue.Execute(ActiveProject, FGlobalOps);
End;

End.
