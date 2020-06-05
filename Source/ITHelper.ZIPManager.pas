(**
  
  This module contains class / interface for managing the zipping of a project to an archive.

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
Unit ITHelper.ZIPManager;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  Classes,
  Contnrs,
  ToolsAPI,
  ITHelper.Interfaces;

Type
  (** A class which implements the IITHZipManager interface for managing zip file creation. **)
  TITHZipManager = Class(TInterfacedObject, IITHZipManager)
  Strict Private
    FProject    : IOTAProject;
    FGlobalOps  : IITHGlobalOptions;
    FProjectOps : IITHProjectOptions;
    FMsgMgr     : IITHMessageManager;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IITHZipManager
    Function ZipProjectInformation : Integer;
    // General Methods
    Procedure ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean);
    Procedure IdleHandler;
  Public
    Constructor Create(Const Project: IOTAProject; Const GlobalOps : IITHGlobalOptions;
      Const ProjectOps : IITHProjectOptions; Const MessageManager : IITHMessageManager);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Forms,
  ITHelper.Types, 
  ITHelper.ExternalProcessInfo,
  ITHelper.TestingHelperUtils, 
  ITHelper.ProcessingForm,
  ITHelper.CommonFunctions,
  ITHelper.CustomMessages, 
  ITHelper.ResponseFile;

(**

  A constructor for the TITHZipManager class.

  @precon  None.
  @postcon Stores reference to interfaces for use later on.

  @param   Project        as an IOTAProject as a constant
  @param   GlobalOps      as an IITHGlobalOptions as a constant
  @param   ProjectOps     as an IITHProjectOptions as a constant
  @param   MessageManager as an IITHMessageManager as a constant

**)
Constructor TITHZipManager.Create(Const Project: IOTAProject; Const GlobalOps : IITHGlobalOptions;
  Const ProjectOps : IITHProjectOptions; Const MessageManager : IITHMessageManager);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FProject := Project;
  FGlobalOps := GlobalOps;
  FProjectOps := ProjectOps;
  FMsgMgr := MessageManager;
End;

(**

  A destructor for the TITHZipManager class.

  @precon  None.
  @postcon Does nothing - used to check destruction using CodeSite.

**)
Destructor TITHZipManager.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This method is called by the DGHCreateProcess function to ensure the IDE does not lockup and become
  unresponsive while background process are running.

  @precon  None.
  @postcon Updates the aplciation message query.

**)
Procedure TITHZipManager.IdleHandler;

Begin
  Application.ProcessMessages;
End;

(**

  This method is called by DGHCreateProcess to capture the comman line messages from the tools and output
  them to the message window.

  @precon  None.
  @postcon A message is output to the message view.

  @nohint  boolAbort

  @param   strMsg    as a String as a constant
  @param   boolAbort as a Boolean as a reference

**)
Procedure TITHZipManager.ProcessMsgHandler(Const strMsg: String; Var boolAbort: Boolean); //FI:O804

Begin
  If strMsg <> '' Then
    FMsgMgr.AddMsg(strMsg, fnTools, ithfDefault, FMsgMgr.ParentMsg.MessagePntr);
End;

(**

  This method build a response file for a command line zip tool in order to backup all the files required
  to build this project.

  @precon  ProjectOps and Project must be valid instances.
  @postcon Build a response file for a command line zip tool in order to backup all the files required 
           to build this project.

  @return  an Integer

**)
Function TITHZipManager.ZipProjectInformation: Integer;

ResourceString
  strZIPToolNotFound = 'ZIP Tool (%s) not found! (%s).';
  strRunning = 'Running: %s (%s %s)';
  strZipping = 'Zipping';
  strProcessing = 'Processing %s...';
  strZIPFileNotConfigured = 'ZIP File not configured (%s).';

Const
  strRESPONSEFILE = '$RESPONSEFILE$';
  strFILELIST = '$FILELIST$';
  strZIPFILE = '$ZIPFILE$';

Var
  ResponseFile: IITHResponseFile;
  iMsg: Integer;
  strZIPName: String;
  strBasePath: String;
  strProject: String;
  Process: TITHProcessInfo;

Begin
  Result := 0;
  strProject := TITHToolsAPIFunctions.GetProjectName(FProject);
  If FProjectOps.EnableZipping Then
    Begin
      If FProjectOps.SaveModifiedFiles Then
        TITHToolsAPIFunctions.SaveProjectModifiedFiles(FMsgMgr, FProject);
      ResponseFile := TITHResponseFile.Create(FProject, FGlobalOps, FProjectOps, FMsgMgr);
      strZIPName := FProjectOps.ZipName;
      If strZIPName <> '' Then
        Begin
          strBasePath := TITHToolsAPIFunctions.ExpandMacro(FProjectOps.BasePath, FProject.FileName);
          Process.FEnabled := True;
          Process.FEXE := TITHToolsAPIFunctions.ExpandMacro(FGlobalOps.ZipEXE, FProject.FileName);
          Process.FParams := TITHToolsAPIFunctions.ExpandMacro(FGlobalOps.ZipParameters, FProject.FileName);
          If Not FileExists(Process.FEXE) Then
            Begin
              Inc(Result);
              FMsgMgr.AddMsg(Format(strZIPToolNotFound, [Process.FEXE, strProject]),
                fnHeader, ithfFailure);
              Exit;
            End;
          strZIPName := TITHToolsAPIFunctions.ExpandMacro(strZIPName, FProject.FileName);
          If ResponseFile.BuildResponseFile(strBasePath, strProject, strZIPName) Then
            Begin
              Process.FParams := StringReplace(Process.FParams, strRESPONSEFILE, ResponseFile.FileName, []);
              Process.FParams := StringReplace(Process.FParams, strFILELIST, ResponseFile.FileList, []);
              Process.FParams := StringReplace(Process.FParams, strZIPFILE, strZIPName, []);
              Process.FDir := TITHToolsAPIFunctions.ExpandMacro(strBasePath, FProject.FileName);
              FMsgMgr.ParentMsg := FMsgMgr.AddMsg(Format(strRunning,
                [ExtractFileName(Process.FEXE), strProject, strZipping]), fnHeader, ithfHeader);
              TfrmITHProcessing.ShowProcessing(Format(strProcessing, [ExtractFileName(Process.FEXE)]));
              FMsgMgr.Clear;
              Result := DGHCreateProcess(Process, ProcessMsgHandler, IdleHandler);
              For iMsg := 0 To FMsgMgr.Count - 1 Do
                Case Result Of
                  0: FMsgMgr[iMsg].ForeColour := FGlobalOps.FontColour[ithfSuccess];
                Else
                  FMsgMgr[iMsg].ForeColour := FGlobalOps.FontColour[ithfFailure];
                End;
              If Result <> 0 Then
                FMsgMgr.ParentMsg.ForeColour := FGlobalOps.FontColour[ithfFailure];
            End Else
              Inc(Result);
        End Else
          FMsgMgr.AddMsg(Format(strZIPFileNotConfigured, [strProject]), fnHeader, ithfFailure);
    End;
End;

End.



