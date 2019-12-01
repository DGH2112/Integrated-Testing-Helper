(**

  This module contains a class that implements a Project Notifiers so that changes int he project can be
  tracked. Used to rename to .ITHelper support file.

  @Author  David Hoyle.
  @Version 1.0
  @Date    04 Nov 2019

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
    FMessageMgr : IITHMessageManager;
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
  Public
    Constructor Create(Const MessageMgr : IITHMessageManager);
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils;

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
End;

(**

  This method is called after a module is saved.

  @precon  None.
  @postcon Not Used

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.AfterSave;

Begin
End;

(**

  This method is called before a module is saved.

  @precon  None.
  @postcon Returns true to save the module.

  @return  a Boolean

**)
Function TITHProjectNotifier.AllowSave: Boolean;

Begin
  Result := True;
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
End;

(**

  This method is called before a module is saved.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.BeforeSave;

Begin
End;

(**

  This method is called to check for overwrite.

  @precon  None.
  @postcon Returns true to allow overwriting.

  @return  a Boolean

**)
Function TITHProjectNotifier.CheckOverwrite: Boolean;

Begin
  Result := True;
End;

(**

  A constructor for the TITHProjectNotifier class.

  @precon  None.
  @postcon Saves a reference tot he Mesage Manager.

  @param   MessageMgr as an IITHMessageManager as a constant

**)
Constructor TITHProjectNotifier.Create(Const MessageMgr: IITHMessageManager);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FMessageMgr := MessageMgr;
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
  Result := '';
End;

(**

  This is a getter method for the OverwriteFileNameCount property.

  @precon  None.
  @postcon Returns zero sa we are not using this functionality.

  @return  an Integer

**)
Function TITHProjectNotifier.GetOverwriteFileNameCount: Integer;

Begin
  Result := 0;
End;

(**

  This method is called whena  project is modified.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TITHProjectNotifier.Modified;

Begin
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
End;

(**

  This method is called when a project is renamed.

  @precon  None.
  @postcon Renames the .ITHelper file in-line with th name change of the project.

  @param   AOldFileName as a String as a constant
  @param   ANewFileName as a String as a constant

**)
Procedure TITHProjectNotifier.ModuleRenamed(Const AOldFileName, ANewFileName: String);

ResourceString
  strMsg = 'Renaming the ITHelper file from "%s" to "%s"!';

Const
  strITHelper = '.ithelper';

Var
  strOldFileName, strNewFileName : String;

Begin
  strOldFileName := ChangeFileExt(AOldFileName, strITHelper);
  strNewFileName := ChangeFileExt(ANewFileName, strITHelper);
  If FileExists(strOldFileName) Then
    Begin
      RenameFile(strOldFileName, strNewFileName);
      FMessageMgr.AddMsg(
        Format(
          strMsg, [
            ExtractFileName(strOldFileName),
            ExtractFileName(strNewFileName)
          ]
        ),
        fnTools,
        ithfDefault
      );
    End;
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
End;

End.


