(**

  This module contains a class that implements a Project Notifiers so that changes int he project can be
  tracked. Used to rename to .ITHelper support file.

  @Author  David Hoyle.
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
Unit ITHelper.ProjectNotifier;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A class which implements the IOTAProjectNotifier interface for getting project changes. **)
  TITHProjectNotifier = Class(TNotifierObject, IOTANotifier, IOTAModuleNotifier, IOTAModuleNotifier80,
    IOTAModuleNotifier90, IOTAProjectNotifier)
  Strict Private
    Type 
      (** An enumerate to determine whether a rename or copy operation is required (default is
          rename). **)
      TITHRenameMode = (rmRename, rmCopy);
  Strict Private
    FMessageMgr     : IITHMessageManager;
    FRenameNotifier : TITHRenameNotifier;
    FOldFilename    : String;
    FRenameMode     : TITHRenameMode;
  {$IFDEF D2010} Strict {$ENDIF D2010} Protected
    // IOTANotifier
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    // IOTAModule
    Function  CheckOverwrite: Boolean;
    Procedure ModuleRenamed(Const NewName: String); Overload;
    // IOTAMOdule80
    Function  AllowSave: Boolean;
    Function  GetOverwriteFileName(Index: Integer): String;
    Function  GetOverwriteFileNameCount: Integer;
    Procedure SetSaveFileName(Const FileName: String);
    // IOTAMOdule90
    Procedure AfterRename(Const OldFileName: String; Const NewFileName: String);
    Procedure BeforeRename(Const OldFileName: String; Const NewFileName: String);
    // IOTAProjectNotifier
    Procedure ModuleAdded(Const AFileName: String);
    Procedure ModuleRemoved(Const AFileName: String);
    Procedure ModuleRenamed(Const AOldFileName: String; Const ANewFileName: String); Overload;
    // General Methods
    Procedure Rename(Const AOldFileName: String; Const ANewFileName: String);
  Public
    Constructor Create(Const MessageMgr : IITHMessageManager; Const RenameNotifier : TITHRenameNotifier);
    Destructor  Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils,
  Windows;

{

  RENAME

  TITHProjectNotifier.Modified
  TITHProjectNotifier.BeforeRename
    OldFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject07.dproj
    NewFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject08.dproj
  TITHProjectNotifier.Modified
  TITHProjectNotifier.ModuleRenamed
    D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject08.dproj
  TITHProjectNotifier.Modified
  TITHProjectNotifier.Modified
  TITHProjectNotifier.AfterRename
    OldFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject07.dproj
    NewFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject08.dproj
  TITHProjectNotifier.AllowSave
  TITHProjectNotifier.AllowSave
  TITHProjectNotifier.BeforeSave
  TITHProjectNotifier.AfterSave
  TITHProjectNotifier.AllowSave

  SAVEAS

  TITHProjectNotifier.AllowSave
  TITHProjectNotifier.AllowSave
  TITHProjectNotifier.CheckOverwrite               // Called ONLY on SaveAs with NEW File
  TITHProjectNotifier.SetSaveFileName              // Called ONLY on SaveAs with NEW File
    D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject10.dproj
  TITHProjectNotifier.GetOverwriteFileNameCount    // Called ONLY on SaveAs
  TITHProjectNotifier.GetOverwriteFileName         // Called ONLY on SaveAs
  TITHProjectNotifier.BeforeRename
    OldFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject09.dproj
    NewFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject10.dproj
  TITHProjectNotifier.Modified
  TITHProjectNotifier.ModuleRenamed
    D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject10.dproj
  TITHProjectNotifier.Modified
  TITHProjectNotifier.Modified
  TITHProjectNotifier.AfterRename
    OldFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject09.dproj
    NewFileName = D:\Documents\RAD Studio\Test Projects\SingleDLLProject\SingleDLLProject10.dproj
  TITHProjectNotifier.BeforeSave
  TITHProjectNotifier.AfterSave
  TITHProjectNotifier.AllowSave
 
}

Const
  (** A constant for the IT Helper filename extension. **)
  strITHelper = '.ithelper';

(**

  This method is called after a renaming of a module.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod
  @nohint  OldFilename NewFilename

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TITHProjectNotifier.AfterRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterRename', tmoTiming);{$ENDIF}
  Rename(OldFileName, NewFileName);
End;

(**

  This method is called after a module is saved.

  @precon  None.
  @postcon Not Used

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.AfterSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterSave', tmoTiming);{$ENDIF}
End;

(**

  This method is called before a module is saved.

  @precon  None.
  @postcon Returns true to save the module.

  @return  a Boolean

**)
Function TITHProjectNotifier.AllowSave: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AllowSave', tmoTiming);{$ENDIF}
  Result := True;
  FRenameMode := rmRename; // Reset to default as this is always the last call.
End;

(**

  This method is called before a module is renamed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  OldFilename NewFileName

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TITHProjectNotifier.BeforeRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeRename', tmoTiming);{$ENDIF}
  FOldFilename := OldFilename;
End;

(**

  This method is called before a module is saved.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.BeforeSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeSave', tmoTiming);{$ENDIF}
End;

(**

  This method is called to check for overwrite.

  @precon  None.
  @postcon Returns true to allow overwriting.

  @return  a Boolean

**)
Function TITHProjectNotifier.CheckOverwrite: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckOverwrite', tmoTiming);{$ENDIF}
  Result := True;
End;

(**

  A constructor for the TITHProjectNotifier class.

  @precon  None.
  @postcon Saves a reference tot he Mesage Manager.

  @param   MessageMgr     as an IITHMessageManager as a constant
  @param   RenameNotifier as a TITHRenameNotifier as a constant

**)
Constructor TITHProjectNotifier.Create(Const MessageMgr : IITHMessageManager;
  Const RenameNotifier : TITHRenameNotifier);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FMessageMgr := MessageMgr;
  FRenameNotifier := RenameNotifier;
End;

(**

  A destructor for the TITHProjectNotifier class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TITHProjectNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This method is called (after) the notifier is destroyed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.Destroyed;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroyed', tmoTiming);{$ENDIF}
End;

(**

  This is a getter method for the OverwriteFileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck MissingCONSTInParam
  @nohint  Index

  @param   Index as an Integer
  @return  a String

**)
Function TITHProjectNotifier.GetOverwriteFileName(Index: Integer): String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileName', tmoTiming);{$ENDIF}
  Result := '';
  If Index = 0 Then
    Result := ChangeFileExt(FOldFilename, strITHelper);
End;

(**

  This is a getter method for the OverwriteFileNameCount property.

  @precon  None.
  @postcon Returns zero sa we are not using this functionality.

  @return  an Integer

**)
Function TITHProjectNotifier.GetOverwriteFileNameCount: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileNameCount', tmoTiming);{$ENDIF}
  Result := 1; //: @note .ITHelper file.
  FRenameMode := rmCopy; // Set to Copy as this method is only called on a Save As operation.
End;

(**

  This method is called whena  project is modified.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.Modified;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Modified', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a module is added to the project.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  AFileName

  @param   AFileName as a String as a constant

**)
Procedure TITHProjectNotifier.ModuleAdded(Const AFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleAdded', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a module is removed from the project.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  AFileName

  @param   AFileName as a String as a constant

**)
Procedure TITHProjectNotifier.ModuleRemoved(Const AFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRemoved', tmoTiming);{$ENDIF}
End;

(**

  This method is called then a module is renamed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  NewName

  @param   NewName as a String as a constant

**)
Procedure TITHProjectNotifier.ModuleRenamed(Const NewName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenamed', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a project is renamed.

  @precon  None.
  @postcon Renames the .ITHelper file in-line with th name change of the project.

  @nocheck EmptyMethod
  @nohint  AOldFilename ANewFilename

  @param   AOldFileName as a String as a constant
  @param   ANewFileName as a String as a constant

**)
Procedure TITHProjectNotifier.ModuleRenamed(Const AOldFileName, ANewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenamed', tmoTiming);{$ENDIF}
  //: @note Doesn`t get called on renaming a Project!!!
End;

(**

  This method renames the .ITHelper file and calls the rename notifier call back for any other renaming
  that needs to happen.

  @precon  None.
  @postcon The .ITHelper file is renamed.

  @param   AOldFileName as a String as a constant
  @param   ANewFileName as a String as a constant

**)
Procedure TITHProjectNotifier.Rename(Const AOldFileName, ANewFileName: String);

ResourceString
  strMsg = '%s the ITHelper file from "%s" to "%s"!';
  strFailedToRename = 'Failed to rename "%s" to "%s"!';
  strFailedToCopy = 'Failed to copy "%s" to "%s"!';

Const
  astrRenameMode : Array[TITHRenameMode] Of String = ('Renaming', 'Copying');

Var
  strOldFileName, strNewFileName : String;

Begin
  strOldFileName := ChangeFileExt(AOldFileName, strITHelper);
  strNewFileName := ChangeFileExt(ANewFileName, strITHelper);
  If FileExists(strOldFileName) Then
    Begin
      Case FRenameMode Of
        rmRename:
          If Not RenameFile(strOldFileName, strNewFileName) Then
            FMessageMgr.AddMsg(Format(strFailedToRename, [AOldFileName, ANewFileName]), fnTools,
              ithfFailure);
        rmCopy:
          If Not CopyFile(PChar(strOldFileName), PChar(strNewFileName), False) Then
            FMessageMgr.AddMsg(Format(strFailedToCopy, [AOldFileName, ANewFileName]), fnTools,
              ithfFailure);
      End;
      FMessageMgr.AddMsg(
        Format(
          strMsg, [
            astrRenameMode[FRenameMode],
            ExtractFileName(strOldFileName),
            ExtractFileName(strNewFileName)
          ]
        ),
        fnTools,
        ithfDefault
      );
    End;
  If Assigned(FRenameNotifier) Then
    FRenameNotifier(AOldFileName, ANewFileName);
End;

(**

  This is a setter method for the SaveFileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  Filename

  @param   FileName as a String as a constant

**)
Procedure TITHProjectNotifier.SetSaveFileName(Const FileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetSaveFileName', tmoTiming);{$ENDIF}
End;

End.



