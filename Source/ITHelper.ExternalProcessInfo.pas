(**

  This module contains a class which represents a dynamic collection of process
  information.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Sep 2018

**)
Unit ITHelper.ExternalProcessInfo;

Interface

Uses
  INIFiles,
  Generics.Collections;

Type
  (** A record to describe the information required by DGHCreateProcess. **)
  TITHProcessInfo = Record
    FEnabled : Boolean;
    FEXE    : String;
    FParams : String;
    FDir    : String;
    FTitle  : String;
  End;

  (** A class to represent a collection of TProcessInfo classes. **)
  TITHProcessCollection = Class
  Strict Private
    FProcesses : TList<TITHProcessInfo>;
  Strict Protected
    Function  GetCount : Integer;
    Function  GetProcess(Const iIndex : Integer) : TITHProcessInfo;
    Procedure SetProcess(Const iIndex : Integer; Const PInfo : TITHProcessInfo);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure AddProcessInfo(Const boolEnabled : Boolean; Const strEXE, strParams, strDir,
      strTitle : String);
    Procedure Delete(Const iIndex : Integer);
    Procedure SwapItems(Const iIndex1, iIndex2: Integer);
    Procedure Clear;
    Procedure LoadFromINI(Const iniFile : TMemIniFile; Const strSection : String);
    Procedure SaveToINI(Const iniFile : TMemIniFile; Const strSection : String);
    (**
      A property to return the number of processes in the collection.
      @precon  None.
      @postcon Returns the numnber of process in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      A property to return a reference to a process info record.
      @precon  iIndex must be a valid index in the collection.
      @postcon Returns a reference to the indexed process info record.
      @param   iIndex as an Integer as a constant
      @return  a TITHProcessInfo
    **)
    Property Process[Const iIndex : Integer] : TITHProcessInfo Read GetProcess Write SetProcess;
      Default;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  SysUtils, 
  ITHelper.CommonFunctions;

Const
  (** This position of the executable in a zero based string array split from the ini file. **)
  iProcessExe = 0;
  (** This position of the parameters in a zero based string array split from the ini file. **)
  iProcessParams = 1;
  (** This position of the Directory in a zero based string array split from the ini file. **)
  iProcessDir = 2;
  (** This position of the Title in a zero based string array split from the ini file. **)
  iProcessTitle = 3;

{ TProcessCollection }

(**

  This method adds a set of process information to the collection, and grows the collection if necessary.

  @precon  None.
  @postcon Adds a set of process information to the collection, and grows the collection if neceaary.

  @param   boolEnabled as a Boolean as a constant
  @param   strEXE      as a String as a constant
  @param   strParams   as a String as a constant
  @param   strDir      as a String as a constant
  @param   strTitle    as a String as a constant

**)
Procedure TITHProcessCollection.AddProcessInfo(Const boolEnabled: Boolean; Const strEXE,
  strParams, strDir, strTitle: String);

Var
  PI : TITHProcessInfo;

Begin
  If strEXE <> '' Then
    Begin
      PI.FEnabled := boolEnabled;
      PI.FEXE     := strEXE;
      PI.FParams  := strParams;
      PI.FDir     := strDir;
      PI.FTitle   := strTitle;
      FProcesses.Add(PI);
    End;
End;

(**

  This method clears the existing collection and creates a new empty collection.

  @precon  None.
  @postcon Clears the existing collection and creates a new empty collection.

**)
Procedure TITHProcessCollection.Clear;

Begin
  FProcesses.Clear;
End;

(**

  This is the constructor method for the TProcessCollection class.

  @precon  None.
  @postcon Creates the empty collection.

**)
Constructor TITHProcessCollection.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FProcesses := TList<TITHProcessInfo>.Create;
End;

(**

  This method deletes the indexed item from the list.

  @precon  iIndex must be between 0 and Count - 1.
  @postcon The indexed item is deleted from the list.

  @param   iIndex as an Integer as a constant

**)
Procedure TITHProcessCollection.Delete(Const iIndex: Integer);

Begin
  FProcesses.Delete(iIndex);
End;

(**

  This is the destructor method for the TProcessCollection class.

  @precon  None.
  @postcon Clears the collection.

**)
Destructor TITHProcessCollection.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FProcesses.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the collection.

  @return  an Integer

**)
Function TITHProcessCollection.GetCount: Integer;

Begin
  Result := FProcesses.Count;
End;

(**

  This is a getter method for the Process property.

  @precon  iIndex must eb a valid index in the collection.
  @postcon Returns the reference to the indexed procress info record.

  @param   iIndex as an Integer as a constant
  @return  a TITHProcessInfo

**)
Function TITHProcessCollection.GetProcess(Const iIndex: Integer): TITHProcessInfo;

Begin
  Result := FProcesses[iIndex];
End;

(**

  This method clears the current collection and loads a new set of process information from the inifile 
  and section provided.

  @precon  iniFile must be a valid instance.
  @postcon Clears the current collection and loads a new set of process information from the inifile and
           section provided.

  @param   iniFile    as a TMemIniFile as a constant
  @param   strSection as a String as a constant

**)
Procedure TITHProcessCollection.LoadFromINI(Const iniFile: TMemIniFile; Const strSection: String);

Var
  sl: TStringList;
  i: Integer;
  strTitle: String;
  astrProcessInfo: TDGHArrayOfString;

Begin
  Clear;
  sl := TStringList.Create;
  Try
    iniFile.ReadSection(strSection, sl);
    For i := 0 To sl.Count - 1 Do
      Begin
        astrProcessInfo := DGHSplit(sl[i], '|');
        strTitle := astrProcessInfo[iProcessTitle];
        If strTitle = '' Then
          strTitle := ExtractFilename(astrProcessInfo[iProcessExe]);
        AddProcessInfo(
          iniFile.ReadBool(strSection, sl[i], True),
          astrProcessInfo[iProcessExe],
          astrProcessInfo[iProcessParams],
          astrProcessInfo[iProcessDir],
          strTitle
        );
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method saves the collection of process information recofrds to the ini file and section provided.

  @precon  iniFile must be a valid instance.
  @postcon Saves the collection of process information recofrds to the ini file and section provided.

  @param   iniFile    as a TMemIniFile as a constant
  @param   strSection as a String as a constant

**)
Procedure TITHProcessCollection.SaveToINI(Const iniFile: TMemIniFile; Const strSection: String);

Var
  i: Integer;

Begin
  iniFile.EraseSection(strSection);
  For i := 0 To Count - 1 Do
    iniFile.WriteBool(strSection, Format('%s|%s|%s|%s', [
      FProcesses[i].FEXE,
      FProcesses[i].FParams,
      FProcesses[i].FDir,
      FProcesses[i].FTitle
    ]), FProcesses[i].FEnabled);
  iniFile.UpdateFile;
End;

(**

  This is a setter method for the Process property.

  @precon  iIndex must be a valid index between 0 and Count - 1.
  @postcon Updates the indexed process with the passed information.

  @param   iIndex as an Integer as a constant
  @param   PInfo  as a TITHProcessInfo as a constant

**)
Procedure TITHProcessCollection.SetProcess(Const iIndex: Integer; Const PInfo: TITHProcessInfo);

Var
  PI : TITHProcessInfo;
  
Begin
  PI.FEnabled := PInfo.FEnabled;
  PI.FEXE     := PInfo.FEXE;
  PI.FParams  := PInfo.FParams;
  PI.FDir     := PInfo.FDir;
  PI.FTitle   := PInfo.FTitle;
  FProcesses[iIndex] := PI;
End;

(**

  This method swaps the values contained in the 2 indexed positions.

  @precon  iIndex1 and iIndex2 must be between 0 and Count -1.
  @postcon The items in the list are swapped.

  @param   iIndex1 as an Integer as a constant
  @param   iIndex2 as an Integer as a constant

**)
Procedure TITHProcessCollection.SwapItems(Const iIndex1, iIndex2: Integer);

{$IFNDEF D2010}
var
  R: TITHProcessInfo;
{$ENDIF}

Begin
  {$IFDEF D2010}
  FProcesses.Exchange(iIndex1, iIndex2);
  {$ELSE}
  R := FProcesses[iIndex1];
  FProcesses[iIndex1] := FProcesses[iIndex2];
  FProcesses[iIndex2] := R;
  {$ENDIF}
End;

End.

