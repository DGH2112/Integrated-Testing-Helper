(**

  This module contains a class that implements the IDE notifiers so that before and after
  compilation events can be handled.

  @Author  David Hoyle.
  @Version 1.0
  @Date    16 Jul 2018

**)
Unit ITHelper.IDENotifierInterface;

Interface

Uses
  ToolsAPI,
  Contnrs,
  ITHelper.TestingHelperUtils,
  ExtCtrls,
  Classes,
  IniFiles,
  ITHelper.ExternalProcessInfo,
  ITHelper.Interfaces;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** A class to implement the IDE notifier interfaces **)
  TITHelperIDENotifier = Class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50,
    IOTAIDENotifier80)
  Strict Private
    { Private declarations }
    FGlobalOps             : IITHGlobalOptions;
    FSuccessfulCompile     : TTimer;
    FLastSuccessfulCompile : Int64;
    FShouldBuildList       : TStringList;
    FMsgMgr                : IITHMessageManager;
  Strict Protected
    // IOTANotifier
    // IOTAIDENotifier
    Procedure FileNotification(NotifyCode: TOTAFileNotification; Const FileName: String;
      Var Cancel: Boolean);
    Procedure BeforeCompile(Const Project: IOTAProject; Var Cancel: Boolean); Overload;
    Procedure AfterCompile(Succeeded: Boolean); Overload;
    // IOTAIDENotifier50
    Procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); Overload;
    Procedure BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
      Var Cancel: Boolean); Overload;
    // IOTAIDENotifier80
    Procedure AfterCompile(Const Project: IOTAProject; Succeeded: Boolean;
      IsCodeInsight: Boolean); Overload;
    // General Methods
    Function ProcessCompileInformation(Const ProjectOps : IITHProjectOptions; Const Project: IOTAProject;
      Const strWhere: String): Integer;
    Procedure IncrementBuild(Const ProjectOps : IITHProjectOptions; Const Project: IOTAProject;
      Const strProject: String);
    Procedure CopyVersionInfoFromDependency(Const Project: IOTAProject; Const strProject: String;
      Const ProjectOps: IITHProjectOptions);
    Procedure ProcessAfterCompile(Const Project: IOTAProject;
      Succeeded, IsCodeInsight: Boolean);
    Procedure SuccessfulCompile(Sender: TObject);
    Procedure ExpandProcessMacro(Const Processes: TITHProcessCollection; Const Project: IOTAProject);
    Procedure ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean);
    Procedure IdleHandler;
  Public
    { Public declarations }
    Constructor Create(Const GlobalOps: IITHGlobalOptions);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  ITHelper.EnabledOptions,
  ITHelper.ProcessingForm,
  Windows,
  SysUtils,
  StrUtils,
  Graphics,
  Forms,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  Variants, 
  ITHelper.CommonFunctions, 
  ITHelper.Types, 
  ITHelper.ZIPManager, 
  ITHelper.MessageManager, 
  ITHelper.VersionManager;

Type
  (** A custom exception for ITHelper problems. **)
  EITHException = Class(Exception);

ResourceString
  (** This is the warning shown if there are no after compilation tools. **)
  strAfterCompileWARNING = 'WARNING: There are no Post-Compilation tools configured (%s).';
  (** This is the warning shown if there are no before compilation tools. **)
  strBeforeCompileWARNING = 'WARNING: There are no Pre-Compilation tools configured (%s).';
{$IFDEF DXE20}
  (** A resource string messge for broken Open Tools API version control XE2 ONLY!!!! **)
  strMsgBroken = 'The Open Tools API''s ability to manipulale the build number of the ' +
    'version information is broken. Althought the number can be incremented this is ' +
    'not included in the EXE/DLL and this incrementation is lost when the project is ' +
    'closed. Please turn off the IDE''s version numbering and use ITHelper''s own ' +
    'mechanism for handling version information in the project options dialogue.';
  (** A resource string messge for incrementing build number but ITHelpers version control is not
      enabled. **)
  strMsgIncBuildDisabled = 'You have enabled the incrementation of the build number on ' +
    'successful compilation but you have not enabled ITHelper''s handling of version ' +
    'information in the project options dialogue.';
  (** A resource string messge for broken Open Tools API version control XE2 ONLY!!!! **)
  strMsgCopyDisabled = 'You have enabled the copying of version information from an ' +
    'existing executabe but you have not enabled ITHelper''s handling of version ' +
    'information in the project options dialogue.';
{$ENDIF}

Const
  (** This number of milliseconds in a second. **)
  iMilliSecInSec = 1000;

(**

  This is an null implementation for the AfterCompile interface.

  @precon  None.
  @postcon Not implemented.

  @nocheck EmptyMethod MissingCONSTInParam
  @nohint  Succeeded

  @param   Succeeded as a Boolean

**)
Procedure TITHelperIDENotifier.AfterCompile(Succeeded: Boolean);

Begin //FI:W519
End;

(**

  This is an null implementation for the AfterCompile interface.

  @precon  None.
  @postcon Not implemented.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Succeeded IsCodeInsight

  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TITHelperIDENotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);

Begin
  {$IFNDEF D2005} // For D7 and below
  ProcessAfterCompile(ActiveProject, Succeeded, IsCodeInsight);
  {$ENDIF}
End;

{$IFDEF D2005}

(**

  This is an implementation for the AfterCompile interface.

  @precon  None.
  @postcon If the compile is not from CodeInsight then the before compilation
           external tools associated with the compiled project are run.

  @nocheck MissingCONSTInParam

  @param   Project       as an IOTAProject as a constant
  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TITHelperIDENotifier.AfterCompile(Const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);

Begin // For D2005 and above
  ProcessAfterCompile(Project, Succeeded, IsCodeInsight);
End;
{$ENDIF}

(**

  This is an implementation for the BeforeCompile interface.

  @precon  None.
  @postcon If the compile is not from CodeInsight then the before compilation
           external tools associated with the compiled project are run.

  @nocheck MissingCONSTInParam
  
  @param   Project       as an IOTAProject as a constant
  @param   IsCodeInsight as a Boolean
  @param   Cancel        as a Boolean as a reference

**)
Procedure TITHelperIDENotifier.BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
  Var Cancel: Boolean);

ResourceString
  strPreCompilation = 'Pre-Compilation';
  strPreCompilationToolsFailed = 'Pre-Compilation Tools Failed (%s).';

Var
  iResult: Integer;
  strProject: String;
  ProjectOps: IITHProjectOptions;
  iInterval: Int64;
  Ops: TITHEnabledOptions;
  MS: IOTAMessageServices;
  VersionMgr : IITHVersionManager;

Begin
  If Project = Nil Then
    Exit;
  If IsCodeInsight Then
    Exit;
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    iInterval := FGlobalOps.ClearMessages;
    If iInterval > 0 Then
      If FMsgMgr.LastMessage < GetTickCount - iInterval * iMilliSecInSec Then
        TITHToolsAPIFunctions.ClearMessages([cmCompiler, cmGroup]);
    If Project.ProjectBuilder.ShouldBuild Then
      Begin
        FShouldBuildList.Add(Project.FileName);
        Ops := FGlobalOps.ProjectGroupOps;
        If eoGroupEnabled In Ops Then
          Begin
            Try
              strProject := TITHToolsAPIFunctions.GetProjectName(Project);
              If eoCopyVersionInfo In Ops Then
                CopyVersionInfoFromDependency(Project, strProject, ProjectOps);
              If eoBefore In Ops Then
                If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
                  Begin
                    iResult := ProcessCompileInformation(ProjectOps, Project, strPreCompilation);
                    If iResult > 0 Then
                      Begin
                        Cancel := True;
                        TfrmITHProcessing.ShowProcessing
                          (Format(strPreCompilationToolsFailed, [strProject]),
                          FGlobalOps.FontColour[ithfWarning], True);
                        TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
                      End
                    Else If iResult < 0 Then
                      If ProjectOps.WarnBefore Then
                        Begin
                          FMsgMgr.AddMsg(Format(strBeforeCompileWARNING, [strProject]), fnHeader,
                            ithfWarning);
                          TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
                        End;
                  End;
              If eoBuildVersionResource In Ops Then
                If ProjectOps.IncITHVerInfo Then
                  Begin
                    VersionMgr := TITHVersionManager.Create(FGlobalOps, ProjectOps, Project, FMsgMgr);
                    VersionMgr.BuildProjectVersionResource;
                  End;
            Finally
              TfrmITHProcessing.HideProcessing;
            End;
          End;
      End;
  Finally
    ProjectOps := Nil;
  End;
End;

(**

  This is an null implementation for the BeforeCompile interface.

  @precon  None.
  @postcon Not implemented.

  @nocheck EmptyMethod
  @nohint  Project Cancel

  @param   Project as an IOTAProject as a constant
  @param   Cancel  as a Boolean as a reference

**)
Procedure TITHelperIDENotifier.BeforeCompile(Const Project: IOTAProject;
  Var Cancel: Boolean);
  
Begin //FI:W519
End;

(**

  This method copies the version information from the first project dependency to this project IF the 
  option is enabled.

  @precon  Project must be a valid IOTAProject.
  @postcon Copies the version information from the first project dependency to this project IF the 
           option is enabled.

  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant
  @param   ProjectOps as a IITHProjectOptions as a constant

**)
Procedure TITHelperIDENotifier.CopyVersionInfoFromDependency(Const Project: IOTAProject;
  Const strProject: String; Const ProjectOps: IITHProjectOptions);

Const
  strBugFix = ' abcedfghijklmnopqrstuvwxyz';
  {$IFNDEF DXE20}
  strMajorVersion = 'MajorVersion';
  strMinorVersion = 'MinorVersion';
  strRelease = 'Release';
  strBuild = 'Build';
  {$ENDIF}

ResourceString
  strVersionDependencyFoundForProject = 'Version Dependency (%s) found for project %s.';
  strDependentBuild = 'Dependent Build %d.%d%s (%d.%d.%d.%d).';
  strDependencyWasNotFound = 'The dependency "%s" was not found. (%s)';

Var
  iMajor, iMinor, iBugfix, iBuild: Integer;
  Group: IOTAProjectGroup;
  strTargetName: String;
  {$IFDEF DXE20}
  POC: IOTAProjectOptionsConfigurations;
  AC: IOTABuildConfiguration;
  {$ENDIF}
  
Begin
  Group := TITHToolsAPIFunctions.ProjectGroup;
  If Group = Nil Then
    Exit;
  strTargetName := TITHToolsAPIFunctions.ExpandMacro(ProjectOps.CopyVerInfo, Project.FileName);
  If strTargetName <> '' Then
    Begin
      FMsgMgr.AddMsg(Format(strVersionDependencyFoundForProject,
        [ExtractFileName(strTargetName), strProject]), fnHeader, ithfDefault);
      If FileExists(strTargetName) Then
        Begin
          Try
            BuildNumber(strTargetName, iMajor, iMinor, iBugfix, iBuild);
            FMsgMgr.AddMsg(Format(strDependentBuild,
              [iMajor, iMinor, strBugFix[iBugfix + 1], iMajor, iMinor, iBugfix, iBuild]),
              fnHeader, ithfDefault);
            {$IFDEF DXE20}
            If Project.ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, POC) = S_OK Then
              Begin
                AC := POC.ActiveConfiguration;
                If AC.PropertyExists(sVerInfo_IncludeVerInfo) Then
                  If AC.AsBoolean[sVerInfo_IncludeVerInfo] Then
                    Raise EITHException.Create(strMsgBroken);
                  If Not ProjectOps.IncITHVerInfo Then
                    Raise EITHException.Create(strMsgCopyDisabled)
              End;
            {$ELSE}
            If Not ProjectOps.IncITHVerInfo Then
              Begin
                If Project.ProjectOptions.Values[strMajorVersion] <> iMajor Then
                  Project.ProjectOptions.Values[strMajorVersion] := iMajor;
                If Project.ProjectOptions.Values[strMinorVersion] <> iMinor Then
                  Project.ProjectOptions.Values[strMinorVersion] := iMinor;
                If Project.ProjectOptions.Values[strRelease] <> iBugfix Then
                  Project.ProjectOptions.Values[strRelease] := iBugfix;
                If Project.ProjectOptions.Values[strBuild] <> iBuild Then
                  Project.ProjectOptions.Values[strBuild] := iBuild;
              End;
            {$ENDIF}
              If ProjectOps.IncITHVerInfo Then
                Begin
                  If ProjectOps.Major <> iMajor Then
                    ProjectOps.Major := iMajor;
                  If ProjectOps.Minor <> iMinor Then
                    ProjectOps.Minor := iMinor;
                  If ProjectOps.Release <> iBugfix Then
                    ProjectOps.Release := iBugfix;
                  If ProjectOps.Build <> iBuild Then
                    ProjectOps.Build := iBuild;
                End;
          Except
            On E: EITHException Do
              FMsgMgr.AddMsg(Format(E.Message + ' (%s)', [strProject]), fnHeader, ithfWarning);
          End;
          TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
        End
      Else
        FMsgMgr.AddMsg(Format(strDependencyWasNotFound,
          [strTargetName, strProject]), fnHeader, ithfWarning);
    End;
End;

(**

  A constructor for the TTestingHelperIDENotifier class.

  @precon  None.
  @postcon Initialises the class.

  @param   GlobalOps as a IITHGlobalOptions as a constant

**)
Constructor TITHelperIDENotifier.Create(Const GlobalOps: IITHGlobalOptions);

Const
  iTimerIntervalInMSec = 100;

Begin
  Inherited Create;
  FGlobalOps := GlobalOps;
  FMsgMgr := TITHMessageManager.Create(GlobalOps);
  FLastSuccessfulCompile := 0;
  FSuccessfulCompile := TTimer.Create(Nil);
  FSuccessfulCompile.OnTimer := SuccessfulCompile;
  FSuccessfulCompile.Interval := iTimerIntervalInMSec;
  FSuccessfulCompile.Enabled := True;
  FShouldBuildList := TStringList.Create;
  FShouldBuildList.CaseSensitive := False;
End;

(**

  A destructor for the TTestingHelperIDENotifier class.

  @precon  None.
  @postcon Frees memory used by the class.

**)
Destructor TITHelperIDENotifier.Destroy;

Begin
  FShouldBuildList.Free;
  FSuccessfulCompile.Enabled := False;
  FSuccessfulCompile.OnTimer := Nil;
  FSuccessfulCompile.Free;
  Inherited Destroy;
End;

(**

  This method expands any macros found in the process information.

  @precon  Project must be a valid instance.
  @postcon Expands any macros found in the process information.

  @param   Processes as a TITHProcessCollection as a constant
  @param   Project   as an IOTAProject as a constant

**)
Procedure TITHelperIDENotifier.ExpandProcessMacro(Const Processes: TITHProcessCollection;
  Const Project: IOTAProject);

Var
  i: Integer;
  P: TITHProcessInfo;

Begin
  For i := 0 To Processes.Count - 1 Do
    Begin
      P := Processes[i];
      P.FEXE := TITHToolsAPIFunctions.ExpandMacro(P.FEXE, Project.FileName);
      P.FParams := TITHToolsAPIFunctions.ExpandMacro(P.FParams, Project.FileName);
      P.FDir := TITHToolsAPIFunctions.ExpandMacro(P.FDir, Project.FileName);
      Processes[i] := P;
    End;
End;

(**

  This method is a null implementation of the Wizards FileNotification interface.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod MissingCONSTInParam
  @nohint  NotifyCode FileName Cancel

  @param   NotifyCode as a TOTAFileNotification
  @param   FileName   as a String as a constant
  @param   cancel     as a Boolean as a reference

**)
Procedure TITHelperIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  Const FileName: String; Var Cancel: Boolean);

Begin //FI:W519
End;

(**

  This is an on idle message handler for the DGHCreateProcess method.

  @precon  None.
  @postcon Ensures the application processes its message queue.

**)
Procedure TITHelperIDENotifier.IdleHandler;

Begin
  Application.ProcessMessages;
End;

(**

  This method increments the buld number of the passed project IF this is enabled in the options.

  @precon  ProjectOps must be a valid instance.
  @postcon Increments the buld number of the passed project IF this is enabled in the options.

  @param   ProjectOps as a IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant

**)
Procedure TITHelperIDENotifier.IncrementBuild(Const ProjectOps : IITHProjectOptions;
  Const Project: IOTAProject; Const strProject: String);

ResourceString
  strIncrementingBuildFromTo = 'Incrementing %s''s Build from %d to %d.';

{$IFNDEF DXE20}
Const
  strBuild = 'Build';
{$ENDIF}

Var
  iBuild: Integer;
  {$IFDEF DXE20}
  POC: IOTAProjectOptionsConfigurations;
  AC: IOTABuildConfiguration;
  {$ENDIF}

Begin
  If ProjectOps.IncOnCompile Then
    Begin
      iBuild := -1;
      {$IFDEF DXE20}
      If Project.ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, POC) = S_OK Then
        Begin
          AC := POC.ActiveConfiguration;
          If AC.PropertyExists(sVerInfo_IncludeVerInfo) Then
            If AC.AsBoolean[sVerInfo_IncludeVerInfo] Then
              Raise EITHException.Create(strMsgBroken);
            If Not ProjectOps.IncITHVerInfo Then
              Raise EITHException.Create(strMsgIncBuildDisabled);
        End;
      {$ELSE}
      If Not ProjectOps.IncITHVerInfo Then
        iBuild := Project.ProjectOptions.Values[strBuild];
      {$ENDIF}
      If ProjectOps.IncITHVerInfo Then
        iBuild := ProjectOps.Build;
      If iBuild > -1 Then
        FMsgMgr.AddMsg(Format(strIncrementingBuildFromTo,
          [strProject, iBuild, iBuild + 1]), fnHeader, ithfDefault);
      {$IFNDEF DXE20}
      If Not ProjectOps.IncITHVerInfo Then
        Project.ProjectOptions.Values[strBuild] := iBuild + 1;
      {$ENDIF}
      If ProjectOps.IncITHVerInfo Then
        ProjectOps.Build := iBuild + 1;
    End;
End;

(**

  This method centralises the processing of the AfterCompile information.
  This is so that its can be called from different versions of the
  implementation.

  @precon  None.
  @postcon Processes the AfterCompile information.

  @nocheck MissingCONSTInParam

  @param   Project       as an IOTAProject as a constant
  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TITHelperIDENotifier.ProcessAfterCompile(Const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);

Const
  strZIPToolFailure = 'ZIP Tool Failure (%s).';

ResourceString
  strPostCompilation = 'Post-Compilation';
  strPostCompilationToolsFailed = 'Post-Compilation Tools Failed (%s).';

Var
  iResult: Integer;
  strProject: String;
  ProjectOps: IITHProjectOptions;
  iIndex: Integer;
  Ops: TITHEnabledOptions;
  ZipMgr : IITHZipManager;

Begin
  If Project = Nil Then
    Exit;
  If IsCodeInsight Or Not Succeeded Then
    Exit;
  iIndex := FShouldBuildList.IndexOf(Project.FileName);
  If iIndex > -1 Then
    Begin
      FShouldBuildList.Delete(iIndex);
      ProjectOps := FGlobalOps.ProjectOptions(Project);
      Try
        Ops := FGlobalOps.ProjectGroupOps;
        If eoGroupEnabled In Ops Then
          Try
            strProject := TITHToolsAPIFunctions.GetProjectName(Project);
            If eoIncrementBuild In Ops Then
              IncrementBuild(ProjectOps, Project, strProject);
            If eoZip In Ops Then
              Begin
                ZipMgr := TITHZIPManager.Create(Project, FGlobalOps, ProjectOps, FMsgMgr);
                iResult := ZipMgr.ZipProjectInformation;
                If iResult > 0 Then
                  Begin
                    TfrmITHProcessing.ShowProcessing(Format(strZIPToolFailure,
                      [strProject]), FGlobalOps.FontColour[ithfFailure], True);
                    TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
                    Abort;
                  End;
              End;
            If eoAfter In Ops Then
              Begin
                iResult := ProcessCompileInformation(ProjectOps, Project, strPostCompilation);
                If iResult > 0 Then
                  Begin
                    TfrmITHProcessing.ShowProcessing
                      (Format(strPostCompilationToolsFailed, [strProject]),
                      FGlobalOps.FontColour[ithfWarning], True);
                    TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
                    Abort;
                  End
                Else If iResult < 0 Then
                  If ProjectOps.WarnAfter Then
                    Begin
                      FMsgMgr.AddMsg(Format(strAfterCompileWARNING, [strProject]),
                        fnHeader, ithfWarning);
                      TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
                    End;
              End;
            FLastSuccessfulCompile := GetTickCount;
          Finally
            TfrmITHProcessing.HideProcessing;
          End;
      Finally
        ProjectOps := Nil;
      End;
    End;
End;

(**

  This method processes the list of external tools in the given section and runs each in a process and 
  captures the output. If any of the tools raises an Exit Code > 0 then this is added to the return 
  result.

  @precon  ProjectOps and Project must be a valid instances.
  @postcon Processes the list of external tools in the given section and runs each in a process and 
           captures the output. If any of the tools raises an Exit Code > 0 then this is added to the 
           return result.

  @param   ProjectOps as a IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strWhere   as a String as a constant
  @return  an Integer

**)
Function TITHelperIDENotifier.ProcessCompileInformation(Const ProjectOps : IITHProjectOptions;
  Const Project: IOTAProject; Const strWhere: String): Integer;

ResourceString
  strRunning = 'Running: %s (%s %s)';
  strProcessing = 'Processing %s...';

Var
  Processes: TITHProcessCollection;
  i, j: Integer;
  strProject: String;

Begin
  Result := -1; // Signifies no tools configured.
  strProject := TITHToolsAPIFunctions.GetProjectName(Project);
  Processes := TITHProcessCollection.Create;
  Try
    Processes.LoadFromINI(ProjectOps.INIFile, strWhere);
    ExpandProcessMacro(Processes, Project);
    If Processes.Count > 0 Then
      Result := 0;
    For i := 0 To Processes.Count - 1 Do
      Begin
        FMsgMgr.Clear;
        If Processes[i].FEnabled Then
          Begin
            FMsgMgr.ParentMsg :=
              FMsgMgr.AddMsg(Format(strRunning,
              [ExtractFileName(Processes[i].FTitle), strProject, strWhere]), fnHeader, ithfHeader);
            TfrmITHProcessing.ShowProcessing(Format(strProcessing,
              [ExtractFileName(Processes[i].FTitle)]));
            Inc(Result, DGHCreateProcess(Processes[i], ProcessMsgHandler, IdleHandler));
            For j := 0 To FMsgMgr.Count - 1 Do
              Case Result Of
                0:FMsgMgr[j].ForeColour := FGlobalOps.FontColour[ithfSuccess];
              Else
                FMsgMgr[j].ForeColour := FGlobalOps.FontColour[ithfFailure];
              End;
            If Result <> 0 Then
              FMsgMgr.ParentMsg.ForeColour := FGlobalOps.FontColour[ithfFailure];
            TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
            If Result > 0 Then
                Break;
          End;
      End;
  Finally
    Processes.Free;
  End;
End;

(**

  This is an on process Msg handler for the DGHCreateProcess method.

  @precon  None.
  @postcon Outputs the messages from the process to the message window.

  @nohint  boolAbort

  @param   strMsg    as a String as a constant
  @param   boolAbort as a Boolean as a reference

**)
Procedure TITHelperIDENotifier.ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean); //FI:O804

Begin
  If strMsg <> '' Then
    FMsgMgr.AddMsg(strMsg, fnTools, ithfDefault, FMsgMgr.ParentMsg.MessagePntr);
End;

(**

  This is an on timer event handler for the SuccessfulCompile timer.

  @precon  None.
  @postcon If the options is enabled this will switch the message view to the
           helper messages 1 second after a successful compile.

  @param   Sender as a TObject

**)
Procedure TITHelperIDENotifier.SuccessfulCompile(Sender: TObject);

Begin
  If FLastSuccessfulCompile + iMilliSecInSec > GetTickCount Then
    Begin
      FLastSuccessfulCompile := 0;
      If FGlobalOps.SwitchToMessages Then
        TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
    End;
End;

End.


