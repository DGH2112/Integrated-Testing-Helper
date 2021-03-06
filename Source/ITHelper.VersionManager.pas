(**
  
  This module contains a class / interface for creating and managing a version RC file for the project.

  @Author  David Hoyle
  @Version 1.001
  @Date    05 Jun 2020
  
  @license

    Integrated Testing helper is a RAD Studio plug-in for running pre and post
    build processes.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)

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
Unit ITHelper.VersionManager;

Interface

Uses
  ToolsAPI,
  ITHelper.Interfaces;

Type
  (** A class / interface to manage the create a version RC file for the project. **)
  TITHVersionManager = Class(TInterfacedObject, IITHVersionManager)
  Strict Private
    FGlobalOps  : IITHGlobalOptions;
    FProjectOps : IITHProjectOptions;
    FProject    : IOTAProject;
    FMsgMgr     : IITHMessageManager;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IITHVersionManager
    Procedure BuildProjectVersionResource();
    // General Methods
    Function  ExpandMacro(Const strText, strMacroName, strValue: String): String;
    Function  CreateVersionRC : String;
    Procedure IncrementBuildNumber(Const strFileName : String);
    Procedure CompileVersionRC(Const strFileName : String);
    Procedure ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean);
    Procedure IdleHandler;
  Public
    Constructor Create(Const GloablOps: IITHGlobalOptions; Const ProjectOps: IITHProjectOptions;
      Const Project: IOTAProject; Const MsgMgr : IITHMessageManager);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Classes, 
  Forms,
  ITHelper.ExternalProcessInfo, 
  ITHelper.Types, 
  ITHelper.ProcessingForm,
  ITHelper.CommonFunctions, 
  ITHelper.TestingHelperUtils;

(**

  This method builds the ITHelper version resource for inclusion in the project.

  @precon  None.
  @postcon Builds the ITHelper version resource for inclusion in the project.

**)
Procedure TITHVersionManager.BuildProjectVersionResource();

Var
  strFileName : String;

Begin
  strFileName := CreateVersionRC;
  IncrementBuildNumber(strFileName);
  CompileVersionRC(strFileName);
End;

(**

  This method compiles the created RC file if the projects options require it.

  @precon  None.
  @postcon The RC file is compiled and the output messages sent to the message view.

  @param   strFileName as a String as a constant

**)
Procedure TITHVersionManager.CompileVersionRC(Const strFileName : String);

ResourceString
  strCompilingWithBRCC = 'Compiling %s with BRCC32';
  strRunning = 'Running: %s (%s)';
  strProcessing = 'Processing %s...';
  strResourceRCCompiledForProject = 'Resource %s.RC compiled for project %s.';

Const
  strBRCCExe = 'BRCC32.exe';

Var
  Process : TITHProcessInfo;
  iResult: Integer;
  j: Integer;

Begin
  If FProjectOps.CompileRes Then
    Begin
      Process.FEnabled := True;
      Process.FTitle := Format(strCompilingWithBRCC, [FProjectOps.ResourceName]);
      Process.FEXE := strBRCCExe;
      Process.FParams := Format('-v "%s"', [strFileName]);
      Process.FDir := GetCurrentDir;
      FMsgMgr.ParentMsg :=
        FMsgMgr.AddMsg(Format(strRunning,
        [ExtractFileName(Process.FTitle), TITHToolsAPIFunctions.GetProjectName(FProject)]), fnHeader, ithfHeader);
      TfrmITHProcessing.ShowProcessing(Format(strProcessing,
        [ExtractFileName(Process.FTitle)]));
      FMsgMgr.Clear;
      iResult := DGHCreateProcess(Process, ProcessMsgHandler, IdleHandler);
      For j := 0 To FMsgMgr.Count - 1 Do
        Case iResult Of
          0: FMsgMgr[j].ForeColour := FGlobalOps.FontColour[ithfSuccess];
        Else
          FMsgMgr[j].ForeColour := FGlobalOps.FontColour[ithfFailure];
        End;
      If iResult <> 0 Then
        FMsgMgr.ParentMsg.ForeColour := FGlobalOps.FontColour[ithfFailure];
      TITHToolsAPIFunctions.ShowHelperMessages(FGlobalOps.GroupMessages);
      If iResult > 0 Then
        Abort;
      FMsgMgr.AddMsg(Format(strResourceRCCompiledForProject, [
        FProjectOps.ResourceName, TITHToolsAPIFunctions.GetProjectName(FProject)]), fnHeader, ithfDefault);
    End;
End;

(**

  A constructor for the TITHVersionManager class.

  @precon  None.
  @postcon Stores references to the various interfaces for later use.

  @param   GloablOps  as an IITHGlobalOptions as a constant
  @param   ProjectOps as an IITHProjectOptions as a constant
  @param   Project    as an IOTAProject as a constant
  @param   MsgMgr     as an IITHMessageManager as a constant

**)
Constructor TITHVersionManager.Create(Const GloablOps: IITHGlobalOptions; Const ProjectOps: IITHProjectOptions;
      Const Project: IOTAProject; Const MsgMgr : IITHMessageManager);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FGlobalOps := GloablOps;
  FProjectOps := ProjectOps;
  FProject := Project;
  FMsgMgr := MsgMgr;
End;

(**

  This method creates a version RC file for compiling into the application.

  @precon  None.
  @postcon The RC file is created with the projects version information.

  @return  a String

**)
Function TITHVersionManager.CreateVersionRC: String;

Const
  strVersionTemplate = 
    'LANGUAGE LANG_ENGLISH,SUBLANG_ENGLISH_US'#13#10 +
    ''#13#10 +
    '1 VERSIONINFO LOADONCALL MOVEABLE DISCARDABLE IMPURE'#13#10 +
    'FILEVERSION $(Major), $(Minor), $(Release), $(Build)'#13#10 +
    'PRODUCTVERSION $(Major), $(Minor), $(Release), $(Build)'#13#10 +
    'FILEFLAGSMASK VS_FFI_FILEFLAGSMASK'#13#10 +
    'FILEOS VOS__WINDOWS32'#13#10 +
    'FILETYPE VFT_APP'#13#10 +
    '{'#13#10 +
    '  BLOCK "StringFileInfo"'#13#10 +
    '  {'#13#10 +
    '    BLOCK "040904E4"'#13#10 +
    '    {'#13#10 +
    '$(Details)    }'#13#10 +
    '  }'#13#10 +
    '  BLOCK "VarFileInfo"'#13#10 +
    '  {'#13#10 +
    '    VALUE "Translation", 1033, 1252'#13#10 +
    '  }'#13#10 +
    '}';
  strMajor = '$(Major)';
  strMinor = '$(Minor)';
  strRelease = '$(Release)';
  strBuild = '$(Build)';
  strVALUE = '      VALUE "%s", "%s\000"'#13#10;
  strRCDetails = '$(Details)';
  strRCExt = '.RC';

Var
  sl, slDetails : TStringList;
  strDetails : String;
  i : Integer;
  
Begin
  sl := TStringList.Create;
  Try
    sl.Text := strVersionTemplate;
    sl.Text := ExpandMacro(sl.Text, strMajor, IntToStr(FProjectOps.Major));
    sl.Text := ExpandMacro(sl.Text, strMinor, IntToStr(FProjectOps.Minor));
    sl.Text := ExpandMacro(sl.Text, strRelease, IntToStr(FProjectOps.Release));
    sl.Text := ExpandMacro(sl.Text, strBuild, IntToStr(FProjectOps.Build));
    slDetails := FProjectOps.VerInfo;
    For i := 0 To slDetails.Count - 1 Do
      strDetails := strDetails + Format(strVALUE, [slDetails.Names[i],
        slDetails.ValueFromIndex[i]]);
    sl.Text := ExpandMacro(sl.Text, strRCDetails, strDetails);
    Result := ExtractFilePath(FProject.FileName) + FProjectOps.ResourceName + strRCExt;
    sl.SaveToFile(Result);
  Finally
    sl.Free;
  End;
End;

(**

  A destructor for the TITHVersionManager class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TITHVersionManager.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method replaces all instances of the given macro within the given text with the given string
  value and returns the updates string.

  @precon  None.
  @postcon If the macro is found in the text it is replaced with the value.

  @param   strText      as a String as a constant
  @param   strMacroName as a String as a constant
  @param   strValue     as a String as a constant
  @return  a String

**)
Function TITHVersionManager.ExpandMacro(Const strText, strMacroName, strValue: String): String;

Begin
  Result := StringReplace(strText, strMacroName, strValue, [rfReplaceAll, rfIgnoreCase]);
End;

(**

  This is an idle handler for them DGHCreateProcess fucntion to ensure that the IDE does not freeze.

  @precon  None.
  @postcon Processes the message query.

**)
Procedure TITHVersionManager.IdleHandler;

Begin
  Application.ProcessMessages;
End;

(**

  This method increments the build number if that project option is enabled.

  @precon  None.
  @postcon The project build number is incremented if required and the RC file added to the project.

  @param   strFileName as a String as a constant

**)
Procedure TITHVersionManager.IncrementBuildNumber(Const strFileName : String);

ResourceString
  strVerInfoRRCCreated = 'Version information resource %s.RC created for project %s.';

Const
  strResourceRCAdded = 'Resource %s.RC added to project %s.';

Var
  boolFound: Boolean;
  iModule: Integer;

Begin
  FMsgMgr.AddMsg(Format(strVerInfoRRCCreated, [
    FProjectOps.ResourceName, TITHToolsAPIFunctions.GetProjectName(FProject)]), fnHeader, ithfDefault);
  If FProjectOps.IncResInProj Then
    Begin
      boolFound := False;
      For iModule := 0 To FProject.GetModuleCount - 1 Do
        If CompareText(FProject.GetModule(iModule).FileName, strFileName) = 0 Then
          Begin
            boolFound := True;
            Break;
          End;
      If Not boolFound Then
        Begin
          FProject.AddFile(strFileName, True);
          FMsgMgr.AddMsg(Format(strResourceRCAdded, [
            FProjectOps.ResourceName, TITHToolsAPIFunctions.GetProjectName(FProject)]), fnHeader, ithfDefault);
        End;
    End;
End;

(**

  This method processses the messages from DGHCreateProcess and outputs them to the messages window.

  @precon  None.
  @postcon External process messages are output to the message window.

  @nohint  boolAbort

  @param   strMsg    as a String as a constant
  @param   boolAbort as a Boolean as a reference

**)
Procedure TITHVersionManager.ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean); //FI:O804

Begin
  If strMsg <> '' Then
    FMsgMgr.AddMsg(strMsg, fnTools, ithfDefault, FMsgMgr.ParentMsg.MessagePntr);
End;

End.

