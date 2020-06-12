(**

  This module contains a class that implements the IITHModuleNotifierList interface for managing module
  notifiers in the IDE.

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
Unit ITHelper.ModuleNotifierList;

Interface

Uses
  Generics.Collections,
  ITHelper.Interfaces;

Type
  (** A class which implements the IITHModuleNotifierList interface for managing module notifiers. **)
  TITHModuleNotifierList = Class(TInterfacedObject, IITHModuleNotifierList)
  Strict Private
    Type
      (** A record to hold the module notifier filename and its notifier index. @nohints **)
      TModuleNotifierRec = Record
      Strict Private
        FFileName      : String;
        FNotifierIndex : Integer;
      Public
        Constructor Create(Const strFileName: String; Const iIndex: Integer);
        (**
          This property returns the filename associated with the module notifier.
          @precon  None.
          @postcon Returns the filename associated with the module notifier.
          @return  a String
        **)
        Property FileName: String Read FFileName Write FFileName;
        (**
          This property returns the notifier index associated with the module notifier.
          @precon  None.
          @postcon Returns the notifier index associated with the module notifier.
          @return  an Integer
        **)
        Property NotifierIndex: Integer Read FNotifierIndex;
      End;
  Strict Private
    FModuleNotifierList: TList<TModuleNotifierRec>;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Procedure Add(Const strFileName: String; Const iIndex: Integer);
    Function  Remove(Const strFileName: String): Integer;
    Procedure Rename(Const strOldFileName: String; Const strNewFileName: String);
    Function  Find(Const strFileName: String; Var iIndex: Integer): Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils;

(**

  A constructor for the TModuleNotifierRec record.

  @precon  None.
  @postcon Initialises the record with the passed parameters.

  @param   strFileName as a String as a constant
  @param   iIndex      as an Integer as a constant

**)
Constructor TITHModuleNotifierList.TModuleNotifierRec.Create(Const strFileName: String;
  Const iIndex: Integer);
  
Begin
  FFileName := strFileName;
  FNotifierIndex := iIndex;
End;

(**

  This method adds the filename and module notifier index to the collection.

  @precon  None.
  @postcon The filename and index are added to the collection.

  @param   strFileName as a String as a constant
  @param   iIndex      as an Integer as a constant

**)
Procedure TITHModuleNotifierList.Add(Const strFileName: String; Const iIndex: Integer);

Begin
  FModuleNotifierList.Add(
    TModuleNotifierRec.Create(strFileName, iIndex)
  );
End;

(**

  A constructor for the TITHModuleNotifierList class.

  @precon  None.
  @postcon Creates a generic collection to contain the module notifier information.

**)
Constructor TITHModuleNotifierList.Create;

Begin
  FModuleNotifierList := TList<TModuleNotifierRec>.Create;
End;

(**

  A destructor for the TITHModuleNotifierList class.

  @precon  None.
  @postcon Removes any remaining notifier records from the collection (shoudln't be any).

**)
Destructor TITHModuleNotifierList.Destroy;

{$IFDEF DEBUG}
Const
  strMsg = 'Failed to remove module notifier for "%s" (%d)!';
{$ENDIF DEBUG}

Var
  iModule: Integer;

Begin
  For iModule := FModuleNotifierList.Count - 1 DownTo 0 Do
    Begin
      FModuleNotifierList.Delete(iModule);
      //: @note Cannot remove any left over notifiers here as the module#
      //:       is most likely closed at ths point however there should be any anyway.
      {$IFDEF DEBUG}
      CodeSite.SendFmtMsg(strMsg, [
        FModuleNotifierList[iModule].FileName,
        FModuleNotifierList[iModule].NotifierIndex]
      );
      {$ENDIF DEBUG}
    End;
  FModuleNotifierList.Free;
  Inherited Destroy;
End;

(**

  This method attempts to find the notifier with the given filename.

  @precon  None.
  @postcon If found the function returns true and the index is passed back in the var parameter else the
           function returns false.

  @param   strFileName as a String as a constant
  @param   iIndex      as an Integer as a reference
  @return  a Boolean

**)
Function TITHModuleNotifierList.Find(Const strFileName: String; Var iIndex: Integer): Boolean;

Var
  iModNotIdx: Integer;
  R: TModuleNotifierRec;

Begin
  Result := False;
  iIndex := -1;
  For iModNotIdx := 0 To FModuleNotifierList.Count - 1 Do
    Begin
      R := FModuleNotifierList.Items[iModNotIdx];
      If CompareText(R.FileName, strFileName) = 0 Then
        Begin
          iIndex := iModNotIdx;
          Result := True;
          Break;
        End;
    End;
End;

(**

  This method attempts to remove the module notifier record from the collection associated with the
  given filename.

  @precon  None.
  @postcon If the filename is foudn the record is removed from the collection and its notifier index is
           returned else if not found the return value is -1.

  @param   strFileName as a String as a constant
  @return  an Integer

**)
Function TITHModuleNotifierList.Remove(Const strFileName: String): Integer;

Var
  iModuleIndex: Integer;
  R: TModuleNotifierRec;

Begin
  Result := - 1;
  If Find(strFileName, iModuleIndex) Then
    Begin
      R := FModuleNotifierList[iModuleIndex];
      Result := R.NotifierIndex;
      FModuleNotifierList.Delete(iModuleIndex);
    End;
End;

(**

  This method attempts to rename the record in the collection associated with the old filename and
  change it to the new filename.

  @precon  None.
  @postcon If the old filename is found it is updated to the new filename.

  @param   strOldFileName as a String as a constant
  @param   strNewFileName as a String as a constant

**)
Procedure TITHModuleNotifierList.Rename(Const strOldFileName, strNewFileName: String);

Var
  iIndex: Integer;
  R: TModuleNotifierRec;

Begin
  If Find(strOldFileName, iIndex) Then
    Begin
      R := FModuleNotifierList[iIndex];
      R.FileName := strNewFileName;
      FModuleNotifierList[iIndex] := R;
    End;
End;

End.
