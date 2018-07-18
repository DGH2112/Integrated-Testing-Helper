(**
  
  This module contains common functions (non-OTA) for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Jul 2018
  
**)
Unit ITHelper.CommonFunctions;

Interface

Uses
  Classes,
  ITHelper.ExternalProcessInfo;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A type to define an array of strings. **)
  TDGHArrayOfString = Array Of String;
  (** A method signature for the DGHCreateProcess message event handler. **)
  TITHProcessMsgHandler = Procedure(Const strMsg : String; Var boolAbort : Boolean) Of Object;
  (** A method signature for the DGHCreateProcess idle event handler. **)
  TITHIdleHandler = Procedure Of Object;
  (** A record to hold the version information. **)
  TITHVersionInfo = Record
    FMajor, FMinor, FBugFix, FBuild : Integer;
  End;

  Procedure BuildNumber(Const strFileName : String; Var recVersionInfo : TITHVersionInfo);
  Function  BuildRootKey : String;
  Function  ComputerName : String;
  Function  DGHCreateProcess(Const Process : TITHProcessInfo;
    Const ProcessMsgHandler : TITHProcessMsgHandler; Const IdleHandler : TITHIdleHandler) : Integer;
  Function  DGHFindOnPath(Var strEXEName : String; Const strDirs : String) : Boolean;
  Function  DGHPos(Const cDelimiter : Char; Const strText : String; Const iStartPos : Integer) : Integer;
  Function  DGHPathRelativePathTo(Const strBasePath : String; Var strFileName : String) : Boolean;
  Function  DGHSplit(Const strText : String; Const cDelimiter : Char) : TDGHArrayOfString;
  Function  Like(Const strPattern, strText : String) : Boolean;
  Function  UserName : String;
  
Implementation

Uses
  SysUtils,
  Windows,
  SHFolder;

(**

  This is a method which obtains information about the package from is version information with the 
  package resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within the EXE file.

  @param   strFileName    as a String as a constant
  @param   recVersionInfo as a TITHVersionInfo as a reference

**)
Procedure BuildNumber(Const strFileName : String; Var recVersionInfo : TITHVersionInfo);

Const
  iShiftRight16 = 16;
  iWordMask = $FFFF;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  recVersionInfo.FMajor := 0;
  recVersionInfo.FMinor := 0;
  recVersionInfo.FBugFix := 0;
  recVersionInfo.FBuild := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(strFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        recVersionInfo.FMajor := VerValue^.dwFileVersionMS Shr iShiftRight16;
        recVersionInfo.FMinor := VerValue^.dwFileVersionMS And iWordMask;
        recVersionInfo.FBugFix := VerValue^.dwFileVersionLS Shr iShiftRight16;
        recVersionInfo.FBuild := VerValue^.dwFileVersionLS And iWordMask;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;

(**

  This method builds the root key INI filename for the loading and saving of
  settings from the instance handle for the module.

  @precon  None.
  @postcon Builds the root key INI filename for the loading and saving of
           settings from the instance handle for the module.

  @return  a String

**)
Function BuildRootKey : String;

Const
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

Var
  strModuleName: String;
  strINIFileName: String;
  strUserAppDataPath: String;
  strBuffer: String;
  iSize: Integer;

Begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strINIFileName) > 0) And
    (CharInSet(strINIFileName[Length(strINIFileName)], ['0' .. '9'])) Do
    strINIFileName := Copy(strINIFileName, 1, Length(strINIFileName) - 1);
  strINIFileName := Format(strINIPattern, [strINIFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
End;

(**

  This function returns the users computer name as a String.

  @precon  None.
  @postcon Returns the users computer name as a String.

  @return  a String

**)
Function ComputerName : String;

Var
  iSize : Cardinal;

Begin
  iSize := MAX_PATH;
  SetLength(Result, iSize);
  GetComputerName(@Result[1], iSize);
  Win32Check(LongBool(iSize));
  SetLength(Result, iSize);
End;

(**

  This function creates a process with message handlers which must be implemented by the passed interface
  in order for the calling process to get messages from the process console and handle idle and abort.

  @precon  ProcMsgHndr must be a valid class implementing TDGHCreateProcessEvent.
  @postcon Creates a process with message handlers which must be implemented by the passed interface in 
           order for the calling process to get messages from the process console and handle idle and 
           abort.

  @param   Process           as a TITHProcessInfo as a constant
  @param   ProcessMsgHandler as a TITHProcessMsgHandler as a constant
  @param   IdleHandler       as a TITHIdleHandler as a constant
  @return  an Integer

**)
Function  DGHCreateProcess(Const Process : TITHProcessInfo;
  Const ProcessMsgHandler : TITHProcessMsgHandler; Const IdleHandler : TITHIdleHandler) : Integer;

Type
  EDGHCreateProcessException = Exception;

ResourceString
  strDirectoryNotFound = 'The directory "%s" does not exist.';
  strUserAbort = 'User Abort!';
  strEXENotFound = 'The executable file "%s" does not exist.';

Const
  iPipeSize = 4096;
  iWaitInMilliSec = 50;

Var
  boolAbort: Boolean;
  strEXE: String;

  (**

    This prcoedure is called periodically by the process handler in order to retreive console output from
    the running process. Output everything from the console (pipe the anonymous pipe) but the last line 
    as this may not be a complete line of information from the console (except if boolPurge is true).

    @precon  slLines must be a valid instance of a TStringList class to accumulate the console output.
    @postcon Outputs to the IDGHCreareProcessEvent interface output information from the console.

    @param   slLines as a TStringList as a constant
    @param   hRead   as a THandle as a constant
    @param   Purge   as a Boolean as a constant

  **)
  Procedure ProcessOutput(Const slLines : TStringList; Const hRead : THandle;
    Const Purge : Boolean = False);

  Const
    iLFCRLen = 2;

  Var
    iTotalBytesInPipe : Cardinal;
    iBytesRead : Cardinal;
    {$IFNDEF D2009}
    strOutput : String;
    {$ELSE}
    strOutput : AnsiString;
    {$ENDIF}

  Begin
    If Assigned(Idlehandler) Then
      IdleHandler;
    If boolAbort Then
      Begin
        If Assigned(ProcessMsgHandler) Then
          ProcessMsgHandler(strUserAbort, boolAbort);
        Exit;
      End;
    Win32Check(PeekNamedPipe(hRead, Nil, 0, Nil, @iTotalBytesInPipe, Nil));
    If iTotalBytesInPipe > 0 Then
      Begin
        SetLength(strOutput, iTotalBytesInPipe);
        ReadFile(hRead, strOutput[1], iTotalBytesInPipe, iBytesRead, Nil);
        SetLength(strOutput, iBytesRead);
        {$IFNDEF D2009}
        slLines.Text := Copy(slLines.Text, 1, Length(slLines.Text) - iLFCRLen) + strOutput;
        {$ELSE}
        slLines.Text := Copy(slLines.Text, 1, Length(slLines.Text) - iLFCRLen) + UTF8ToString(strOutput);
        {$ENDIF}
      End;
    // Use a string list to output each line except the last as it may not
    // be complete yet.
    If Assigned(ProcessMsgHandler) Then
      While slLines.Count > 1 - Integer(Purge) Do
        Begin
          ProcessMsgHandler(slLines[0], boolAbort);
          slLines.Delete(0);
        End;
  End;

Var
  hRead, hWrite : THandle;
  slLines : TStringList;
  SecurityAttrib : TSecurityAttributes;
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  iExitCode : Cardinal;

Begin
  Result := 0;
  boolAbort := False;
  FillChar(SecurityAttrib, SizeOf(SecurityAttrib), 0);
  SecurityAttrib.nLength := SizeOf(SecurityAttrib);
  SecurityAttrib.bInheritHandle := True;
  SecurityAttrib.lpSecurityDescriptor := nil;
  Win32Check(CreatePipe(hRead, hWrite, @SecurityAttrib, iPipeSize));
  Try
    If Process.FEnabled Then
      Try
        If Not DirectoryExists(Process.FDir) Then
          Raise EDGHCreateProcessException.CreateFmt(strDirectoryNotFound, [Process.FDir]);
        If Not FileExists(Process.FEXE) Then
          Begin
            strEXE := Process.FEXE;
            If Not DGHFindOnPath(strEXE, '') Then
              Raise EDGHCreateProcessException.CreateFmt(strEXENotFound, [strEXE]);
          End;
        FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
        StartupInfo.cb := SizeOf(TStartupInfo);
        StartupInfo.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        StartupInfo.wShowWindow := SW_HIDE;
        StartupInfo.hStdOutput  := hWrite;
        StartupInfo.hStdError   := hWrite;
        Win32Check(CreateProcess(PChar(Process.FEXE),
          PChar('"' + Process.FEXE + '" ' + Process.FParams), @SecurityAttrib,
          Nil, True, CREATE_NEW_CONSOLE, Nil, PChar(Process.FDir), StartupInfo,
          ProcessInfo));
        Try
          slLines := TStringList.Create;
          Try
            While WaitforSingleObject(ProcessInfo.hProcess, iWaitInMilliSec) = WAIT_TIMEOUT Do
              Begin
                ProcessOutput(slLines, hRead);
                If boolAbort Then
                  Begin
                    TerminateProcess(ProcessInfo.hProcess, 0);
                    Break;
                  End;
              End;
            ProcessOutput(slLines, hRead, True);
          Finally
            slLines.Free;
          End;
          If GetExitCodeProcess(ProcessInfo.hProcess, iExitCode) Then
            Inc(Result, iExitCode)
        Finally
          Win32Check(CloseHandle(ProcessInfo.hThread));
          Win32Check(CloseHandle(ProcessInfo.hProcess));
        End;
      Except
        On E : EDGHCreateProcessException Do
          If Assigned(ProcessMsgHandler) Then
            Begin
              ProcessMsgHandler(E.Message, boolAbort);
              Inc(Result);
            End;
      End;
  Finally
    Win32Check(CloseHandle(hWrite));
    Win32Check(CloseHandle(hRead));
  End;
End;

(**

  This method searches the given paths (semi-colon delimited) and the enironment path for the exe file 
  name. If found the result is true and the full path to the file is returned in strEXEName.

  @precon  None.
  @postcon Searches the given paths (semi-colon delimited) and the enironment path for the exe file name
           . If found the result is true and the full path to the file is returned in strEXEName.

  @param   strEXEName as a String as a reference
  @param   strDirs    as a String as a constant
  @return  a Boolean

**)
Function DGHFindOnPath(Var strEXEName : String; Const strDirs : String) : Boolean;

  (**

    This procedure checks the paths for being empty (and deltees them) and ensures they termiante with
    a backslash.

    @precon  None.
    @postcon Empty paths are deleted and all end with a backslash.

    @param   slPaths as a TStringList as a constant

  **)
  Procedure CheckPaths(Const slPaths : TStringList);

  Var
    iPath : Integer;
    iLength: Integer;

  Begin
    If strDirs <> '' Then
      slPaths.Text := strDirs + ';' + slPaths.Text;
    slPaths.Text := StringReplace(slPaths.Text, ';', #13#10, [rfReplaceAll]);
    For iPath := slPaths.Count - 1 DownTo 0 Do
      Begin
        iLength := Length(slPaths[iPath]);
        If iLength = 0 Then
          slPaths.Delete(iPath)
        Else
          If slPaths[iPath][iLength] <> '\' Then
            slPaths[iPath] := slPaths[iPath] + '\';
      End;
  End;

  (**

    This method attempts to find the executable on the given path.

    @precon  None.
    @postcon Returns true if the EXE is found.

    @param   strExPath as a String as a constant
    @return  a Boolean

  **)
  Function FindPath(Const strExPath : String) : Boolean;

  Var
    recSearch: TSearchRec;
    iResult: Integer;
  
  Begin
    Result := False;
    iResult := FindFirst(strExPath + strEXEName, faAnyFile, recSearch);
    Try
      If iResult = 0 Then
        Begin
          strEXEName := strExPath + strEXEName;
          Result := True;
        End;
    Finally
      SysUtils.FindClose(recSearch);
    End;
  End;

Const
  strPathEnvName = 'path';

Var
  slPaths : TStringList;
  iPath: Integer;
  strPath, strExPath : String;
  iSize: Integer;

Begin
  Result := False;
  slPaths := TStringList.Create;
  Try
    slPaths.Text := GetEnvironmentVariable(strPathEnvName);
    CheckPaths(slPaths);
    strEXEName := ExtractFileName(strEXEName);
    For iPath := 0 To slPaths.Count - 1 Do
      Begin
        strPath := slPaths[iPath];
        SetLength(strExPath, MAX_PATH);
        iSize := ExpandEnvironmentStrings(PChar(strPath), PChar(strExPath), MAX_PATH);
        SetLength(strExPath, Pred(iSize));
        If FindPath(strExPath) Then
          Begin
            Result := True;
            Break;
          End;
      End;
  Finally
    slPaths.Free;
  End;
End;

(**

  This method returns true and the given filename as a relative path IF the filename and the base path 
  share the same root else returns false.

  @precon  None.
  @postcon Returns true and the given filename as a relative path IF the filename and the base path 
           share the same root else returns false.

  @param   strBasePath as a String as a constant
  @param   strFileName as a String as a reference
  @return  a Boolean

**)
Function DGHPathRelativePathTo(Const strBasePath : String; Var strFileName : String) : Boolean;

Const
  iLenOfUnicodePreamble = 2;

Var
  iPos : Integer;
  strFile : String;
  strNewBasePath : String;

Begin
  Result := False;
  strFile := strFileName;
  strNewBasePath := strBasePath;
  If (Copy(strNewBasePath, 1, iLenOfUnicodePreamble) = '\\') And
    (Copy(strFile, 1, iLenOfUnicodePreamble) = '\\') Then
    Begin
      strNewBasePath := Copy(strNewBasePath, iLenOfUnicodePreamble + 1, MAX_PATH);
      strFile := Copy(strFile, iLenOfUnicodePreamble + 1, MAX_PATH);
    End;
  iPos := Pos('\', strNewBasePath);
  While (iPos > 0) And (CompareText(Copy(strNewBasePath, 1, iPos), Copy(strFile, 1, iPos)) = 0) Do
    Begin
      Result := True;
      strNewBasePath := Copy(strNewBasePath, iPos + 1, MAX_PATH);
      strFile := Copy(strFile, iPos + 1, MAX_PATH);
      iPos := Pos('\', strNewBasePath);
    End;
  If Result Then
    Begin
      strFileName := '';
      While iPos > 0 Do
        Begin
          strNewBasePath := Copy(strNewBasePath, iPos + 1, MAX_PATH);
          iPos := Pos('\', strNewBasePath);
          strFileName := strFileName + '..\';
        End;
      strFileName := strFileName + strFile;
    End;
End;

(**

  This function returns the first position of the delimiter character in the given string on or
  after the starting point.

  @precon  None.
  @postcon Returns the position of the firrst delimiter after the starting point.

  @note    Used to workaround backward compatability issues with String.Split and StringSplit.

  @param   cDelimiter as a Char as a constant
  @param   strText    as a String as a constant
  @param   iStartPos  as an Integer as a constant
  @return  an Integer

**)
Function DGHPos(Const cDelimiter : Char; Const strText : String; Const iStartPos : Integer) : Integer;

Var
  I : Integer;

Begin
  Result := 0;
  For i := iStartPos To Length(strText) Do
    If strText[i] = cDelimiter Then
      Begin
        Result := i;
        Break;
      End;
End;

(**

  This function splits a string into an array of strings based on the given delimiter character.

  @precon  None.
  @postcon Splits the given string by the delimmiters and returns an array of strings.

  @note    Used to workaround backward compatability issues with String.Split and StringSplit.

  @param   strText    as a String as a constant
  @param   cDelimiter as a Char as a constant
  @return  a TDGHArrayOfString

**)
Function DGHSplit(Const strText : String; Const cDelimiter : Char) : TDGHArrayOfString;

Var
  iSplits : Integer;
  i: Integer;
  iStart, iEnd : Integer;

Begin
  iSplits := 0;
  For i := 1 To Length(strText) Do
    If strText[i] = cDelimiter Then
      Inc(iSplits);
  SetLength(Result, Succ(iSplits));
  i := 0;
  iStart := 1;
  While DGHPos(cDelimiter, strText, iStart) > 0 Do
    Begin
      iEnd := DGHPos(cDelimiter, strText, iStart);
      Result[i] := Copy(strText, iStart, iEnd - iStart);
      Inc(i);
      iStart := iEnd + 1;
    End;
  Result[i] := Copy(strText, iStart, Length(strText) - iStart + 1);
End;

(**

  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.

  @param   strPattern as a String as a constant
  @param   strText    as a String as a constant
  @return  a Boolean

**)
Function Like(Const strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

  (**

    This method checks the start, middle and end of the text against the pattern.

    @precon  None.
    @postcon Returns true of the text matches the pattern. 

    @param   strLocalPattern as a String as a constant
    @param   MatchTypes      as a TMatchTypes as a constant
    @return  a Boolean

  **)
  Function CheckPattern(Const strLocalPattern : String; Const MatchTypes : TMatchTypes) : Boolean;

    (**

      This fucntion checks the end of the text against the given pattern stored in the string list.

      @precon  sl must be a valid instance.
      @postcon Returns true of the pattern does not matches the end of the text.

      @param   sl         as a TStringList as a constant
      @param   MatchTypes as a TMatchTypes as a constant
      @return  a Boolean

    **)
    Function CheckEnd(Const sl : TStringList; Const MatchTypes : TMatchTypes) : Boolean;

    Begin
      Result := False;
      If sl.Count > 0 Then
        If mtEnd In MatchTypes Then
          If CompareText(sl[sl.Count - 1], Copy(strText, Length(strText) -
            Length(sl[sl.Count - 1]) + 1, Length(sl[sl.Count - 1]))) <> 0 Then
            Result := True;
    End;

    (**

      This method checks the inner part of the pattern against the text.

      @precon  sl must be a valid instance.
      @postcon Returns true if the pattern does not match the text.

      @param   sl          as a TStringList as a constant
      @param   MatchTypes  as a TMatchTypes as a constant
      @param   iStartIndex as an Integer as a reference
      @return  a Boolean

    **)
    Function CheckInBetween(Const sl : TStringList; Const MatchTypes : TMatchTypes;
      Var iStartIndex : Integer) : Boolean;

    Var
      i : Integer;
      iPos : Integer;
    
    Begin
      Result := False;
      For i := Integer(mtStart In MatchTypes) To sl.Count - 1 - Integer(mtEnd In MatchTypes) Do
        Begin
          iPos := Pos(sl[i], lowercase(strText));
          If (iPos = 0) Or (iPos < iStartIndex) Then
            Begin
              Result := True;
              Break;
            End;;
          Inc(iStartIndex, Length(sl[i]));
        End;
    End;

    (**

      This method checks the start of the tetx against the pattern.

      @precon  sl must be a valid instance.
      @postcon Returns true if the pattern does not match.

      @param   sl          as a TStringList as a constant
      @param   MatchTypes  as a TMatchTypes as a constant
      @param   iStartIndex as an Integer as a reference
      @return  a Boolean

    **)
    Function CheckStart(Const sl : TStringList; Const MatchTypes : TMatchTypes;
      Var iStartIndex : Integer) : Boolean;

    Begin
      Result := False;
      iStartIndex := 1;
      If sl.Count > 0 Then
        If mtStart In MatchTypes Then
          If CompareText(sl[0], Copy(strText, 1, Length(sl[0]))) <> 0 Then
            Begin
              Result := True;
              Exit;
            End Else
              Inc(iStartIndex, Length(sl[0]));
    End;

  Var
    sl : TStringList;
    astrParts : TDGHArrayOfString;
    i: Integer;
    iStartIndex : Integer;
  
  Begin
    Result := False;
    sl := TStringList.Create;
    Try
      astrParts := DGHSplit(strLocalPattern, '*');
      For i := Low(astrParts) To High(astrParts) Do
        sl.Add(LowerCase(astrParts[i]));
      If CheckStart(sl, MatchTypes, iStartIndex) Then
        Exit;
      If CheckInBetween(sl, MatchTypes, iStartIndex) Then
        Exit;
      If CheckEnd(sl, MatchTypes) Then
        Exit;
      Result := True;
    Finally
      sl.Free;
    End;
  End;
  

Var
  MatchTypes : TMatchTypes;
  strLocalPattern : String;

Begin
  strLocalPattern := strPattern;
  Result := False;
  MatchTypes := [];
  If Length(strLocalPattern) = 0 Then
    Exit;
  If strLocalPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strLocalPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strLocalPattern, 1, 1);
  If Length(strLocalPattern) > 0 Then
    If strLocalPattern[Length(strLocalPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strLocalPattern, Length(strLocalPattern), 1);
  Result := CheckPattern(strLocalPattern, MatchTypes);
End;

(**

  This function returns the users logon name as a String.

  @precon  None.
  @postcon Returns the users logon name as a String.

  @return  a String

**)
Function UserName : String;

Var
  iSize : Cardinal;

Begin
  iSize := MAX_PATH;
  SetLength(Result, iSize);
  GetUserName(@Result[1], iSize);
  Win32Check(LongBool(iSize));
  SetLength(Result, iSize - 1);
End;

End.
