(**

  This module contains a class that implements the IDE notifiers so that before and after
  compilation events can be handled.

  @Author  David Hoyle.
  @Version 1.0
  @Date    03 Jan 2020

  @license

    Integrated Testing helper is a RAD Studio plug-in for running pre and post
    build processes.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit ITHelper.IDENotifierInterface;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  ToolsAPI,
  Contnrs,
  ExtCtrls,
  Classes,
  IniFiles,
  ITHelper.Types, 
  ITHelper.Interfaces,
  ITHelper.ExternalProcessInfo,
  ITHelper.CommonFunctions,
  {$IFDEF D2010}
  ITHelper.CompileNotifier,
  {$ENDIF D2010}
  ITHelper.TestingHelperUtils;

Type
  (** A class to implement the IDE notifier interfaces **)
  TITHelperIDENotifier = Class(TNotifierObject, IUnknown, IOTANotifier, IOTAIDENotifier,
    IOTAIDENotifier50, IOTAIDENotifier80)
  Strict Private
    FGlobalOps                  : IITHGlobalOptions;
    FSuccessfulCompile          : TTimer;
    FLastSuccessfulCompile      : Int64;
    FShouldBuildList            : TStringList;
    FMsgMgr                     : IITHMessageManager;
    {$IFDEF DXE00}
    FCompileNotifierIndex       : Integer;
    {$ENDIF DXE00}
    FProjectNotifierList        : IITHModuleNotifierList;
    FProjectCompileNotifierList : IITHModuleNotifierList;
    {$IFDEF DXE00}
    FCompileConfiguration       : String;
    FCompilePlatform            : String;
    {$ENDIF DXE00}
    {$IFDEF D2010}
    FCompileMode                : TOTACompileMode;
    {$ENDIF D2010}
  {$IFDEF D2010} Strict {$ENDIF D2010} Protected
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
    Procedure IncrementBuild(Const Ops : TITHEnabledOptions; Const ProjectOps : IITHProjectOptions;
      Const Project: IOTAProject; Const strProject: String);
    Procedure CopyVersionInfoFromDependency(Const Ops : TITHEnabledOptions; Const Project: IOTAProject;
      Const strProject: String; Const ProjectOps: IITHProjectOptions);
    Procedure ProcessBeforeCompile(Const Project: IOTAProject; Const IsCodeInsight: Boolean;
      Var Cancel: Boolean);
    Procedure ProcessAfterCompile(Const Project: IOTAProject;
      Succeeded, IsCodeInsight: Boolean);
    Procedure SuccessfulCompile(Sender: TObject);
    Procedure ExpandProcessMacro(Const Processes: TITHProcessCollection; Const Project: IOTAProject);
    Procedure ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean);
    Procedure IdleHandler;
    Procedure ProcessVersionInformation(Const Ops : TITHEnabledOptions;
      Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject);
    Procedure ClearMessages;
    Procedure WarnBeforeCompile(Const iResult : Integer; Const ProjectOps : IITHProjectOptions;
      Const strProject : String);
    Function RunPrecompilationTools(Const Ops : TITHEnabledOptions;
      Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject;
      Const strProject : String) : Boolean;
    Procedure WarnAfterCompile(Const iResult : Integer; Const ProjectOps : IITHProjectOptions;
      Const strProject : String);
    Function RunPostcompilationTools(Const Ops : TITHEnabledOptions;
      Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject;
      Const strProject : String) : Boolean;
    Procedure ZipProjectFiles(Const Ops : TITHEnabledOptions; Const ProjectOps : IITHProjectOptions;
      Const Project : IOTAProject; Const strProject : String);
    Function  CheckProjectOptions (Const Project : IOTAProject) : IOTABuildConfiguration;
    Procedure IncrementITHelperVerInfo(
      {$IFDEF DXE20} Const ActiveConfig : IOTABuildConfiguration; {$ENDIF}
      Const ProjectOps : IITHProjectOptions; Const strProject: String;
      Const eCompileMode : TOTACompileMode);
    Procedure IncrementIDEVerInfo(
      Const {$IFDEF DXE20} ActiveConfig: IOTABuildConfiguration; {$ELSE} Project : IOTAProject; {$ENDIF}
      Const strProject: String; Const eCompileMode : TOTACompileMode);
    Procedure UpdateITHelperVerInfo(Const ProjectOps: IITHProjectOptions;
      {$IFDEF DXE20} Const ActiveConfig : IOTABuildConfiguration; {$ENDIF}
      Const recVersionInfo : TITHVersionInfo);
    {$IFDEF DXE20}
    Procedure UpdateIDEVerInfo(Const ActiveConfig : IOTABuildConfiguration;
      Const recVersionInfo : TITHVersionInfo);
    {$ELSE}
    Procedure UpdateIDEVerInfo(Const ProjectOps: IITHProjectOptions; Const Project : IOTAProject;
      Const recVersionInfo : TITHVersionInfo);
    {$ENDIF}
    Procedure InstallNotifier(Const M: IOTAModule);
    procedure UninstallNotifiers(const M: IOTAModule);
    Procedure Rename(Const strOldFileName: String; Const strNewFileName: String);
    Function  GetCompileInformation : TOTAProjectCompileInfo;
    Procedure SetCompileInformation(Const CompileInfo: TOTAProjectCompileInfo);
  Public
    Constructor Create(Const MessageMgr: IITHMessageManager; Const GlobalOps : IITHGlobalOptions);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
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
  ITHelper.ZIPManager, 
  ITHelper.MessageManager, 
  ITHelper.VersionManager,
  ITHelper.Constants,
  ITHelper.ProjectNotifier,
  ITHelper.ModuleNotifierList, ITHelper.ProjectCompileNotifier;

ResourceString
  (** This is the warning shown if there are no after compilation tools. **)
  strAfterCompileWARNING = 'WARNING: There are no Post-Compilation tools configured (%s).';
  (** This is the warning shown if there are no before compilation tools. **)
  strBeforeCompileWARNING = 'WARNING: There are no Pre-Compilation tools configured (%s).';
  {$IFDEF VER230}
  (** A resource string messge for broken Open Tools API version control XE2 ONLY!!!! **)
  strMsgBroken = 'The Open Tools API''s ability to manipulale the build number of the ' +
    'version information is broken. Althought the number can be incremented this is ' +
    'not included in the EXE/DLL and this incrementation is lost when the project is ' +
    'closed. Please turn off the IDE''s version numbering and use ITHelper''s own ' +
    'mechanism for handling version information in the project options dialogue.';
  {$ENDIF}
  {$IFDEF DXE20}
  (** A resource string for enabling the IDEs version information. **)
  strBuildConfigVerInfoEnabled = 'Build Configuration "%s" Version Information Enabled!';
  (** A resource string for disabling the IDEs version information. **)
  strBuildConfigVerInfoDisabled = 'Build Configuration "%s" Version Information Disabled!';
  {$ENDIF}
  (** A resource string for the incrementing build number message. **)
  strIncrementingBuildFromTo = 'Incrementing %s''s (Mode: %s, Config: %s, Platform: %s) ' +
    'Build from %d to %d.';
  (** A constant string for a base configuration in XE and below. **)
  strBASE = 'BASE';
  (** A constant string for a base platform in XE and below. **)
  strWin32 = 'Win32';

Const
  (** This number of milliseconds in a second. **)
  iMilliSecInSec : Int64 = 1000;

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

Begin
  ProcessBeforeCompile(Project, IsCodeInsight, Cancel);
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

  This method returns the active build configruation for the application and additinally checks for
  XE2 and whether the version information is switched on as this is broken in XE2.

  @precon  Project must be a valid reference.
  @postcon Returns the active build configuration.

  @param   Project as an IOTAProject as a constant
  @return  an IOTABuildConfiguration

**)
Function TITHelperIDENotifier.CheckProjectOptions(Const Project : IOTAProject) : IOTABuildConfiguration;

Var
  POC : IOTAProjectOptionsConfigurations;

Begin
  Result := Nil;
  If Project.ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, POC) = S_OK Then
    Begin
      Result := POC.ActiveConfiguration;
      // Deliberate exception for XE2 where the Version information in the IDE was broken.
      {$IFDEF VER230}
      If Result.PropertyExists(sVerInfo_IncludeVerInfo) Then
        Begin
          If Result.AsBoolean[sVerInfo_IncludeVerInfo] Then
            Raise EITHException.Create(strMsgBroken);
        End;
      {$ENDIF}
    End;
End;

(**

  This method clears the messages inthe message view of the appropriate period of time has elapsed.

  @precon  None.
  @postcon The message view is cleared is the time periodf has elapsed.

**)
Procedure TITHelperIDENotifier.ClearMessages;

Begin
  If FGlobalOps.ClearMessages > 0 Then
    If FMsgMgr.LastMessage < GetTickCount - FGlobalOps.ClearMessages * iMilliSecInSec Then
      TITHToolsAPIFunctions.ClearMessages([cmCompiler, cmGroup]);
End;

(**

  This method copies the version information from the first project dependency to this project IF the 
  option is enabled.

  @precon  Project must be a valid IOTAProject.
  @postcon Copies the version information from the first project dependency to this project IF the 
           option is enabled.

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant

**)
Procedure TITHelperIDENotifier.CopyVersionInfoFromDependency(Const Ops : TITHEnabledOptions;
  Const Project: IOTAProject; Const strProject: String; Const ProjectOps: IITHProjectOptions);

ResourceString
  strVersionDependencyFoundForProject = 'Version Dependency (%s) found for project %s.';
  strDependentBuild = 'Dependent Build %d.%d%s (%d.%d.%d.%d).';
  strDependencyWasNotFound = 'The dependency "%s" was not found. (%s)';

Var
  recVersionInfo : TITHVersionInfo;
  Group: IOTAProjectGroup;
  strTargetName: String;
  {$IFDEF DXE20}
  ActiveConfig: IOTABuildConfiguration;
  {$ENDIF}
  
Begin
  If eoCopyVersionInfo In Ops Then
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
                BuildNumber(strTargetName, recVersionInfo);
                FMsgMgr.AddMsg(Format(strDependentBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor,
                  strRevisions[recVersionInfo.FBugfix + 1], recVersionInfo.FMajor, recVersionInfo.FMinor,
                  recVersionInfo.FBugfix, recVersionInfo.FBuild]), fnHeader, ithfDefault);
                {$IFDEF DXE20}
                ActiveConfig := CheckProjectOptions(Project);
                UpdateIDEVerInfo(ActiveConfig, recVersionInfo);
                {$ELSE}
                UpdateIDEVerInfo(ProjectOps, Project, recVersionInfo);
                {$ENDIF}
                UpdateITHelperVerInfo(ProjectOps, {$IFDEF DXE20} ActiveConfig, {$ENDIF} recVersionInfo);
              Except
                On E: EITHException Do
                  FMsgMgr.AddMsg(Format(E.Message + ' (%s)', [strProject]), fnHeader, ithfWarning);
              End;
              TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
            End Else
              FMsgMgr.AddMsg(Format(strDependencyWasNotFound, [strTargetName, strProject]), fnHeader,
                ithfWarning);
        End;
    End;
End;

(**

  A constructor for the TTestingHelperIDENotifier class.

  @precon  None.
  @postcon Initialises the class.

  @param   MessageMgr     as an IITHMessageManager as a constant
  @param   GlobalOps      as an IITHGlobalOptions as a constant

**)
Constructor TITHelperIDENotifier.Create(Const MessageMgr: IITHMessageManager;
  Const GlobalOps : IITHGlobalOptions);

Const
  iTimerIntervalInMSec = 100;

Var
  {$IFDEF D2010}
  CS : IOTACompileServices;
  CompileNotifier : {$IFDEF DXE00} IITHCompileNotifier {$ELSE} IOTACompileNotifier {$ENDIF DXE00};
  {$ENDIF D2010}

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create;
  FGlobalOps := GlobalOps;
  FMsgMgr := MessageMgr;
  {$IFDEF D2010}
  If Supports(BorlandIDEServices, IOTACompileServices, CS) Then
    Begin
      CompileNotifier := TITHCompileNotifier.Create(MessageMgr, GetCompileInformation);
      FCompileNotifierIndex := CS.AddNotifier(CompileNotifier);
    End;
  {$ENDIF D2010}
  FProjectNotifierList := TITHModuleNotifierList.Create;
  FProjectCompileNotifierList := TITHModuleNotifierList.Create;
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

Var
  {$IFDEF D2010}
  CS : IOTACompileServices;
  {$ENDIF D2010}

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FShouldBuildList.Free;
  FSuccessfulCompile.Enabled := False;
  FSuccessfulCompile.OnTimer := Nil;
  FSuccessfulCompile.Free;
  {$IFDEF D2010}
  If Supports(BorlandIDEServices, IOTACompileServices, CS) Then
    CS.RemoveNotifier(FCompileNotifierIndex);
  {$ENDIF D2010}
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

Var
  MS : IOTAModuleServices;
  M: IOTAModule;
  
Begin
  If Not Cancel And Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Case NotifyCode Of
      ofnFileOpened:
        Begin
          M := MS.FindModule(FileName);
          InstallNotifier(M);
        End;
      ofnFileClosing:
        Begin
          M := MS.FindModule(FileName);
          UninstallNotifiers(M);
        End;
    End;
End;

{$IFDEF DXE00}
(**

  This is a getter method for the CompielInformation property.

  @precon  None.
  @postcon Retreives the compiling information and returns it.

  @return  a TOTAProjectCompileInfo

**)
Function TITHelperIDENotifier.GetCompileInformation : TOTAProjectCompileInfo;

Begin
  Result.Mode := FCompileMode;
  Result.Configuration := FCompileConfiguration;
  Result.Platform := FCompilePlatform;
End;
{$ENDIF DXE00}

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

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant

**)
Procedure TITHelperIDENotifier.IncrementBuild(Const Ops : TITHEnabledOptions;
  Const ProjectOps : IITHProjectOptions; Const Project: IOTAProject; Const strProject: String);

Const
  strBasicConfig = 'BasicConfig';

Var
  strConfigName: String;
  {$IFDEF DXE20}
  ActiveConfig: IOTABuildConfiguration;
  {$ENDIF}

Begin
  If eoIncrementBuild In Ops Then
    Begin
      strConfigName := strBasicConfig;
      {$IFDEF DXE20}
      ActiveConfig := CheckProjectOptions(Project);
      strConfigName := ActiveConfig.Name;
      {$ENDIF}
      If ProjectOps.IncOnCompile[
        {$IFDEF D2010} FCompileMode {$ELSE D2010} cmOTAMake {$ENDIF D2010},
        strConfigName
        ] Then
        Begin
          If ProjectOps.IncITHVerInfo Then
            IncrementITHelperVerInfo( {$IFDEF DXE20} ActiveConfig, {$ENDIF} ProjectOps, strProject,
              {$IFDEF D2010} FCompileMode {$ELSE D2010} cmOTAMake {$ENDIF D2010})
          Else
            IncrementIDEVerInfo( {$IFDEF DXE20} ActiveConfig {$ELSE} Project {$ENDIF}, strProject,
              {$IFDEF D2010} FCompileMode {$ELSE D2010} cmOTAMake {$ENDIF D2010});
        End;
    End;
End;

(**

  This method increments the IDE version information.

  @precon  ActiveConfig must be valid instance.
  @postcon The IDEs build number is incremented.

  @param   ActiveConfig as an IOTABuildConfiguration as a constant
  @param   strProject   as a String as a constant
  @param   eCompileMode as a TOTACompileMode as a constant

**)
Procedure TITHelperIDENotifier.IncrementIDEVerInfo(
  Const {$IFDEF DXE20} ActiveConfig: IOTABuildConfiguration; {$ELSE} Project : IOTAProject; {$ENDIF}
  Const strProject : String; Const eCompileMode : TOTACompileMode);


{$IFNDEF DXE20}
Const
  strBuild = 'Build';
{$ENDIF}

Var
  iBuild: Integer;
  strConfigName : String;
  strPlatformName : String;
  
Begin
  strConfigName := strBASE;
  strPlatformName := strWin32;
  {$IFDEF DXE20} // Multiple Configurations in XE2 and above
  If Not ActiveConfig.GetBoolean(sVerInfo_IncludeVerInfo, True) Then
    Begin
      ActiveConfig.SetBoolean(sVerInfo_IncludeVerInfo, True);
      FMsgMgr.AddMsg(Format(strBuildConfigVerInfoEnabled, [ActiveConfig.Name]),fnHeader, ithfDefault);
    End;
  iBuild := ActiveConfig.GetInteger(sVerInfo_Build, True);
  ActiveConfig.SetInteger(sVerInfo_Build, iBuild + 1);
  strConfigName := ActiveConfig.Name;
  strPlatformName := ActiveConfig.Platform;
  {$ELSE}        // Single Configuration in XE and below
  iBuild := Project.ProjectOptions.Values[strBuild];
  Project.ProjectOptions.Values[strBuild] := iBuild + 1;
  {$ENDIF}
  FMsgMgr.AddMsg(
    Format(
      strIncrementingBuildFromTo, [
        strProject,
        astrCompileMode[eCompileMode],
        strConfigName,
        strPlatformName,
        iBuild,
        iBuild + 1
      ]
    ),
    fnHeader,ithfDefault);
End;

(**

  This method increments the ITHelper version information.

  @precon  ActiveConfig and ProjectOps must be valid instances.
  @postcon The ITHelper build number is incremented.

  @param   ActiveConfig as an IOTABuildConfiguration as a constant
  @param   ProjectOps   as an IITHProjectOptions as a constant
  @param   strProject   as a String as a constant
  @param   eCompileMode as a TOTACompileMode as a constant

**)
Procedure TITHelperIDENotifier.IncrementITHelperVerInfo(
  {$IFDEF DXE20} Const ActiveConfig : IOTABuildConfiguration; {$ENDIF}
  Const ProjectOps : IITHProjectOptions; Const strProject : String;
  Const eCompileMode : TOTACompileMode);

Var
  iBuild: Integer;
  strConfigName : String;
  strPlatformName : String;

Begin
  strConfigName := strBASE;
  strPlatformName := strWin32;
  {$IFDEF DXE20} // Multiple Configurations in XE2 and above
  If ActiveConfig.GetBoolean(sVerInfo_IncludeVerInfo, True) Then
    Begin
      ActiveConfig.SetBoolean(sVerInfo_IncludeVerInfo, False);
      FMsgMgr.AddMsg(Format(strBuildConfigVerInfoDisabled, [ActiveConfig.Name]),fnHeader, ithfDefault);
    End;
  strConfigName := ActiveConfig.Name;
  strPlatformName := ActiveConfig.Platform;
  {$ENDIF}
  iBuild := ProjectOps.Build;
  FMsgMgr.AddMsg(
    Format(
      strIncrementingBuildFromTo, [
        strProject,
        astrCompileMode[eCompileMode],
        strConfigName,
        strPlatformName,
        iBuild,
        iBuild + 1
      ]
    ),
    fnHeader,
    ithfDefault
  );
  ProjectOps.Build := iBuild + 1;
End;

(**

  This method uninstall the project notifiers if the given module contains an IOTAProject interface.

  @precon  M must be a valid instance.
  @postcon If the module is an IOTAProject, the Project Notifier and Project Compile Notifier are
           uninstalled.

  @param   M as an IOTAModule as a constant

**)
Procedure TITHelperIDENotifier.InstallNotifier(Const M: IOTAModule);

Var
  P: IOTAProject;
  PN: IOTAProjectNotifier;
  iModuleIndex: Integer;
  {$IFDEF DXE00}
  PCN: TITHProjectCompileNotifier;
  {$ENDIF DXE00}

Begin
  If Supports(M, IOTAProject, P) Then
    Begin
      PN := TITHProjectNotifier.Create(FMsgMgr, Rename);
      iModuleIndex := M.AddNotifier(PN);
      FProjectNotifierList.Add(M.FileName, iModuleIndex);
      {$IFDEF DXE00}
      If Assigned(P.ProjectBuilder) Then
        Begin
          PCN := TITHProjectCompileNotifier.Create(SetCompileInformation);
          iModuleIndex := P.ProjectBuilder.AddCompileNotifier(PCN);
          FProjectCompileNotifierList.Add(M.FileName, iModuleIndex);
        End;
      {$ENDIF DXE00}
    End;
End;

(**

  This method centralises the processing of the AfterCompile information.
  This is so that its can be called from different versions of the
  implementation.

  @precon  None.
  @postcon Processes the AfterCompile information.

  @nocheck MissingCONSTInParam
  @nometric Toxicity

  @param   Project       as an IOTAProject as a constant
  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TITHelperIDENotifier.ProcessAfterCompile(Const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);

Var
  strProject: String;
  ProjectOps: IITHProjectOptions;
  iIndex: Integer;
  Ops: TITHEnabledOptions;

Begin
  If Assigned(Project) And Not IsCodeInsight And Succeeded Then
    Begin
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
                IncrementBuild(Ops, ProjectOps, Project, strProject);
                ZipProjectFiles(Ops, ProjectOps, Project, strProject);
                If RunPostcompilationTools(Ops, ProjectOps, Project, strProject) Then
                  Abort;
                FLastSuccessfulCompile := GetTickCount;
              Finally
                TfrmITHProcessing.HideProcessing;
              End;
          Finally
            ProjectOps := Nil;
          End;
        End;
    End;
End;

(**

  This method processes information beofer a project is compiled.

  @precon  Project must be a valid instance.
  @postcon If the options are enabled those elements are processed before the application is compiled.

  @param   Project       as an IOTAProject as a constant
  @param   IsCodeInsight as a Boolean as a constant
  @param   Cancel        as a Boolean as a reference

**)
Procedure TITHelperIDENotifier.ProcessBeforeCompile(Const Project: IOTAProject;
  Const IsCodeInsight: Boolean; Var Cancel: Boolean);

Var
  strProject: String;
  ProjectOps: IITHProjectOptions;
  Ops: TITHEnabledOptions;

Begin
  If Assigned(Project) And Not IsCodeInsight Then
    Begin
      ProjectOps := FGlobalOps.ProjectOptions(Project);
      ClearMessages;
      If Project.ProjectBuilder.ShouldBuild Then
        Begin
          FShouldBuildList.Add(Project.FileName);
          Ops := FGlobalOps.ProjectGroupOps;
          If eoGroupEnabled In Ops Then
            Begin
              Try
                strProject := TITHToolsAPIFunctions.GetProjectName(Project);
                CopyVersionInfoFromDependency(Ops, Project, strProject, ProjectOps);
                Cancel := RunPrecompilationTools(Ops, ProjectOps, Project, strProject);
                If Not Cancel Then
                  ProcessVersionInformation(Ops, ProjectOps, Project);
              Finally
                TfrmITHProcessing.HideProcessing;
              End;
            End;
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

  This method creates an instance of the version manager to construct a version control resource.

  @precon  ProjectOps and Project must be valid instances.
  @postcon If enabled in the options a project version resource is created to be compiled into the
           project.

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant

**)
Procedure TITHelperIDENotifier.ProcessVersionInformation(Const Ops : TITHEnabledOptions;
  Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject);

Var
  VersionMgr: IITHVersionManager;

Begin
  If eoBuildVersionResource In Ops Then
    If ProjectOps.IncITHVerInfo Then
      Begin
        VersionMgr := TITHVersionManager.Create(FGlobalOps, ProjectOps, Project, FMsgMgr);
        VersionMgr.BuildProjectVersionResource;
      End;
End;

(**

  This method is called when a project is renamed to allow the project notifier collections to be updated
  with the new filername.

  @precon  None.
  @postcon The project notifier collections are updated with the new filename.

  @param   strOldFileName as a String as a constant
  @param   strNewFileName as a String as a constant

**)
Procedure TITHelperIDENotifier.Rename(Const strOldFileName, strNewFileName: String);

Begin
  FProjectNotifierList.Rename(strOldFileName, strNewFileName);
  FProjectCompileNotifierList.Rename(strOldFileName, strNewFileName);
End;

(**

  This method runs any configured post-conpilation tools for the given project.

  @precon  ProjectOps an Project must be valid references.
  @postcon Runs any configured post-conpilation tools for the given project.

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant
  @return  a Boolean

**)
Function TITHelperIDENotifier.RunPostcompilationTools(Const Ops: TITHEnabledOptions;
  Const ProjectOps: IITHProjectOptions; Const Project: IOTAProject; Const strProject: String): Boolean;

ResourceString
  strPostCompilation = 'Post-Compilation';
  strPostCompilationToolsFailed = 'Post-Compilation Tools Failed (%s).';

Var
  iResult: Integer;

Begin
  Result := False;
  If eoAfter In Ops Then
    Begin
      iResult := ProcessCompileInformation(ProjectOps, Project, strPostCompilation);
      If iResult > 0 Then
        Begin
          TfrmITHProcessing.ShowProcessing
            (Format(strPostCompilationToolsFailed, [strProject]),
            FGlobalOps.FontColour[ithfWarning], True);
          TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
          Result := True;
        End Else
          WarnAfterCompile(iResult, ProjectOps, strProject);
    End;
End;

(**

  This method runs any configured pre-conpilation tools for the given project.

  @precon  ProjectOps an Project must be valid references.
  @postcon Runs any configured pre-conpilation tools for the given project.

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant
  @return  a Boolean

**)
Function TITHelperIDENotifier.RunPrecompilationTools(Const Ops : TITHEnabledOptions;
  Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject;
  Const strProject : String) : Boolean;

ResourceString
  strPreCompilation = 'Pre-Compilation';
  strPreCompilationToolsFailed = 'Pre-Compilation Tools Failed (%s).';

Var
  MS: IOTAMessageServices;
  iResult: Integer;

Begin
  Result := False;
  If eoBefore In Ops Then
    If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
      Begin
        iResult := ProcessCompileInformation(ProjectOps, Project, strPreCompilation);
        If iResult > 0 Then
          Begin
            Result := True;
            TfrmITHProcessing.ShowProcessing(Format(strPreCompilationToolsFailed, [
              strProject]), FGlobalOps.FontColour[ithfWarning], True);
            TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
          End Else
        WarnBeforeCompile(iResult, ProjectOps, strProject);
      End;
End;

{$IFDEF DXE00}
(**

  This is a setter method for the CompileInformation property.

  @precon  None.
  @postcon Stores the compile information.

  @param   CompileInfo as a TOTAProjectCompileInfo as a constant

**)
Procedure TITHelperIDENotifier.SetCompileInformation(Const CompileInfo: TOTAProjectCompileInfo);

Begin
  FCompileMode := CompileInfo.Mode;
  FCompileConfiguration := CompileInfo.Configuration;
  FCompilePlatform := CompileInfo.Platform;
End;
{$ENDIF DXE00}

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

(**

  This method install the project notifiers if the given module contains an IOTAProject interface.

  @precon  M must be a valid instance.
  @postcon If the module is an IOTAProject, a Project Notifier and Project Compile Notifier are
           installed.

  @param   M as an IOTAModule as a constant

**)
Procedure TITHelperIDENotifier.UninstallNotifiers(Const M: IOTAModule);

Var
  P: IOTAProject;
  iModuleIndex: Integer;

Begin
  If Supports(M, IOTAProject, P) Then
    Begin
      iModuleIndex := FProjectNotifierList.Remove(M.FileName);
      M.RemoveNotifier(iModuleIndex);
      {$IFDEF DXE00}
      If Assigned(P.ProjectBuilder) Then
        Begin
          iModuleIndex := FProjectCompileNotifierList.Remove(M.FileName);
          P.ProjectBuilder.RemoveCompileNotifier(iModuleIndex);
        End;
      {$ENDIF DXE00}
    End;
End;

{$IFDEF DXE20}
(**

  This method updates the project version information in RAD Studio XE2 and above (multiple configs).

  @precon  ActiveConfig must be a valid reference.
  @postcon Udates the project version information in RAD Studio XE2 and above (multiple configs).

  @param   ActiveConfig   as an IOTABuildConfiguration as a constant
  @param   recVersionInfo as a TITHVersionInfo as a constant

**)
Procedure TITHelperIDENotifier.UpdateIDEVerInfo(Const ActiveConfig: IOTABuildConfiguration;
  Const recVersionInfo: TITHVersionInfo);

Begin
  If Not ActiveConfig.GetBoolean(sVerInfo_IncludeVerInfo, True) Then
    Begin
      ActiveConfig.SetBoolean(sVerInfo_IncludeVerInfo, True);
      FMsgMgr.AddMsg(Format(strBuildConfigVerInfoEnabled, [ActiveConfig.Name]),fnHeader,
        ithfDefault);
    End;
  If ActiveConfig.GetInteger(sVerInfo_MajorVer, True) <> recVersionInfo.FMajor Then
    ActiveConfig.SetInteger(sVerInfo_MajorVer, recVersionInfo.FMajor);
  If ActiveConfig.GetInteger(sVerInfo_MinorVer, True) <> recVersionInfo.FMinor Then
    ActiveConfig.SetInteger(sVerInfo_MinorVer, recVersionInfo.FMinor);
  If ActiveConfig.GetInteger(sVerInfo_Release, True) <> recVersionInfo.FBugFix Then
    ActiveConfig.SetInteger(sVerInfo_Release, recVersionInfo.FBugFix);
  If ActiveConfig.GetInteger(sVerInfo_Build, True) <> recVersionInfo.FBuild Then
    ActiveConfig.SetInteger(sVerInfo_Build, recVersionInfo.FBuild);
End;
{$ELSE}
(**

  This method updates the project version information in RAD Studio XE and below.

  @precon  ProjectOps and Project must be valid references.
  @postcon The IOTAProjects version information is updated.

  @param   ProjectOps     as an IITHProjectOptions as a constant
  @param   Project        as an IOTAProject as a constant
  @param   recVersionInfo as a TITHVersionInfo as a constant

**)
Procedure TITHelperIDENotifier.UpdateIDEVerInfo(Const ProjectOps: IITHProjectOptions;
  Const Project: IOTAProject; Const recVersionInfo: TITHVersionInfo);

Const
  strMajorVersion = 'MajorVersion';
  strMinorVersion = 'MinorVersion';
  strRelease = 'Release';
  strBuild = 'Build';

Begin
  If Not ProjectOps.IncITHVerInfo Then
    Begin
      If Project.ProjectOptions.Values[strMajorVersion] <> recVersionInfo.FMajor Then
        Project.ProjectOptions.Values[strMajorVersion] := recVersionInfo.FMajor;
      If Project.ProjectOptions.Values[strMinorVersion] <> recVersionInfo.FMinor Then
        Project.ProjectOptions.Values[strMinorVersion] := recVersionInfo.FMinor;
      If Project.ProjectOptions.Values[strRelease] <> recVersionInfo.FBugfix Then
        Project.ProjectOptions.Values[strRelease] := recVersionInfo.FBugfix;
      If Project.ProjectOptions.Values[strBuild] <> recVersionInfo.FBuild Then
        Project.ProjectOptions.Values[strBuild] := recVersionInfo.FBuild;
    End;
End;
{$ENDIF}

(**

  This method updates the ITHelper version information to those values in the version information record.

  @precon  ProjectOps must be a valid instance.
  @postcon The ITHelper Project options version information is updated.

  @param   ProjectOps     as an IITHProjectOptions as a constant
  @param   ActiveConfig   as an IOTABuildConfiguration as a constant
  @param   recVersionInfo as a TITHVersionInfo as a constant

**)
procedure TITHelperIDENotifier.UpdateITHelperVerInfo(Const ProjectOps: IITHProjectOptions;
  {$IFDEF DXE20} Const ActiveConfig : IOTABuildConfiguration; {$ENDIF}
  Const recVersionInfo : TITHVersionInfo);

Begin
  If ProjectOps.IncITHVerInfo Then
    Begin
      {$IFDEF DXE20} // Multiple Configurations in XE2 and above
      If ActiveConfig.GetBoolean(sVerInfo_IncludeVerInfo, True) Then
        Begin
          ActiveConfig.SetBoolean(sVerInfo_IncludeVerInfo, False);
          FMsgMgr.AddMsg(Format(strBuildConfigVerInfoDisabled, [ActiveConfig.Name]),fnHeader, ithfDefault);
        End;
      {$ENDIF}
      If ProjectOps.Major <> recVersionInfo.FMajor Then
        ProjectOps.Major := recVersionInfo.FMajor;
      If ProjectOps.Minor <> recVersionInfo.FMinor Then
        ProjectOps.Minor := recVersionInfo.FMinor;
      If ProjectOps.Release <> recVersionInfo.FBugfix Then
        ProjectOps.Release := recVersionInfo.FBugfix;
      If ProjectOps.Build <> recVersionInfo.FBuild Then
        ProjectOps.Build := recVersionInfo.FBuild;
    End;
End;

(**

  This method outputs a message warning that there are not post-compilation tools configured.

  @precon  ProjectOps must be a valid instance.
  @postcon Outputs a message warning that there are not post-compilation tools configured.

  @param   iResult    as an Integer as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   strProject as a String as a constant

**)
Procedure TITHelperIDENotifier.WarnAfterCompile(Const iResult: Integer;
  Const ProjectOps: IITHProjectOptions; Const strProject: String);

Begin
  If iResult < 0 Then
    If ProjectOps.WarnAfter Then
      Begin
        FMsgMgr.AddMsg(Format(strAfterCompileWARNING, [strProject]), fnHeader, ithfWarning);
        TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
      End;
End;

(**

  This method outputs a message warning that there are not pre-compilation tools configured.

  @precon  ProjectOps must be a valid instance.
  @postcon Outputs a message warning that there are not pre-compilation tools configured.

  @param   iResult    as an Integer as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   strProject as a String as a constant

**)
Procedure TITHelperIDENotifier.WarnBeforeCompile(Const iResult : Integer;
  Const ProjectOps : IITHProjectOptions; Const strProject : String);

Begin
  If iResult < 0 Then
    If ProjectOps.WarnBefore Then
      Begin
        FMsgMgr.AddMsg(Format(strBeforeCompileWARNING, [strProject]), fnHeader, ithfWarning);
        TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
      End;
End;

(**

  This method zips the projects files if this options is enabled.

  @precon  ProjectOps and Project must be vaild references.
  @postcon Zips the projects files if this options is enabled.

  @param   Ops        as a TITHEnabledOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strProject as a String as a constant

**)
Procedure TITHelperIDENotifier.ZipProjectFiles(Const Ops : TITHEnabledOptions;
  Const ProjectOps : IITHProjectOptions; Const Project : IOTAProject; Const strProject : String);

ResourceString
  strZIPToolFailure = 'ZIP Tool Failure (%s).';

Var
  iResult: Integer;
  ZipMgr : IITHZipManager;
  
Begin
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
End;

End.


