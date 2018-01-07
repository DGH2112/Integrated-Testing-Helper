(**
  
  This module contains a class / interface for managing a response file for collecting the files from
  the IDE project for zipping into an archive.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Jan 2018
  
**)
Unit ITHelper.ResponseFile;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  ToolsAPI,
  Classes,
  ITHelper.Interfaces;

Type
  (** A class which implements tjhe ITHResponseFile interface for creating and managing a zip 
      response file. **)
  TITHResponseFile = Class(TInterfacedObject, IITHResponseFile)
  Strict Private
    FProject      : IOTAProject;
    FGlobalOps    : IITHGlobalOptions;
    FProjectOps   : IITHProjectOptions;
    FResponseFile : TStringList;
    FFileName     : String;
    FMsgMgr       : IITHMessageManager;
  Strict Protected
    // IITHResponseFile
    Function  GetFileName : String;
    // General Methods
    Procedure BuildTargetFileName(Const strBasePath: String);
    Procedure CheckResponseFileAgainExclusions;
    Procedure CheckResponseFilesForIncludeAndRes(Const strProject,  strBasePath: String);
    Function  IsProject(Const strExtType: String): Boolean;
    Function  ProjectOptions(Const strOptions: Array Of String): String;
    Procedure AddAdditionalZipFilesToResponseFile(Const strBasePath: String);
    Procedure AddProjectFilesToResponseFile(Const strBasePath: String);
    Function  AddToList(Const strBasePath, strModuleName: String): Boolean;
    Procedure BuildResponseFile(Const strBasePath, strProject, strZIPName: String);
    Function  CheckResExts(Const slResExts: TStringList; Const strFileName: String): Boolean;
    Procedure CheckSourceForInclude(Const strFileName, strInclude : String; 
      Const slIncludeFiles: TStringList);
    Function  FileList : String;
  Public
    Constructor Create(Const Project: IOTAProject; Const GlobalOps : IITHGlobalOptions;
      Const ProjectOps: IITHProjectOptions; Const MessageManager : IITHMessageManager);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  ITHelper.TestingHelperUtils, 
  ITHelper.ProcessingForm, 
  ITHelper.CommonFunctions, 
  ITHelper.Types, 
  System.StrUtils, 
  ITHelper.PascalParsing, 
  System.Variants;

(**

  This method adds additional files list for the zip process to the responce file.

  @precon  INIFile must be a valid instance of a TMemIniFile class and slRepsonse must be a valid 
           instance of a TStringList.
  @postcon Adds additional files list for the zip process to the responce file.

  @param   strBasePath as a String as a constant

**)
Procedure TITHResponseFile.AddAdditionalZipFilesToResponseFile(Const strBasePath: String);

Var
  slWildcards: TStringList;
  iModule: Integer;

Begin
  slWildcards := FProjectOps.AddZipFiles;
  For iModule := 0 To slWildcards.Count - 1 Do
    AddToList(strBasePath, ExpandMacro(slWildcards[iModule], FProject.FileName));
End;

(**

  This method adds project files to the response file for zipping.

  @precon  slRepsonse file must be a valid instance of a TStringList. Project must be a valid instance 
           of a IOTAProject interface.
  @postcon Adds project files to the response file for zipping.

  @param   strBasePath as a String as a constant

**)
Procedure TITHResponseFile.AddProjectFilesToResponseFile(Const strBasePath: String);

Var
  iModule: Integer;
  ModuleInfo: IOTAModuleInfo;
  iFile: Integer;
  slFiles : TStringList;

Begin
  slFiles := TStringList.Create;
  Try
    // 160 and above (Delphi 8)
    FProject.GetCompleteFileList(slFiles);
    For iFile := 0 To slFiles.Count - 1 Do
      AddToList(strBasePath, slFiles[iFile]);
    // Manually
    For iModule := 0 To FProject.GetModuleCount - 1 Do
      Begin
        ModuleInfo := FProject.GetModule(iModule);
        AddToList(strBasePath, ModuleInfo.FileName);
        ModuleInfo.GetAdditionalFiles(slFiles);
        For iFile := 0 To slFiles.Count - 1 Do
          AddToList(strBasePath, slFiles[iFile]);
      End;
    FProject.GetAssociatedFilesFromModule(slFiles);
    For iFile := 0 To FProject.ModuleFileCount - 1 Do
      AddToList(strBasePath, FProject.ModuleFileEditors[iFile].FileName);
  Finally
    slFiles.Free;
  End;
End;

(**

  This procedure adds the module name to the list of zip files after changing it to a relative path. If 
  the file already exists in the list true is returned else false.

  @precon  None.
  @postcon Adds the module name to the list of zip files after changing it to a relative path. If the 
           file already exists in the list true is returned else false.

  @param   strBasePath    as a String as a constant
  @param   strModuleName  as a String as a constant
  @return  a Boolean

**)
Function TITHResponseFile.AddToList(Const strBasePath, strModuleName: String): Boolean;

Const
  strModelSupportPath = '\modelsupport_';

Var
  iIndex: Integer;
  strNewModuleName: String;

Begin
  Result := False;
  If Pos(strModelSupportPath, Lowercase(strModuleName)) = 0 Then
    Begin
      strNewModuleName := strModuleName;
      If DGHPathRelativePathTo(strBasePath, strNewModuleName) Then
        Begin
          If strNewModuleName <> '' Then
            Begin
              Result := FResponseFile.Find(strNewModuleName, iIndex);
              FResponseFile.Add(strNewModuleName);
            End;
        End;
    End;
End;

(**

  This method adds files to the response file starting with project files and then project modules.

  @precon  INIFile and slReponse should be valid instances.
  @postcon Adds files to the response file starting with project files and then project modules.

  @param   strBasePath as a String as a constant
  @param   strProject  as a String as a constant
  @param   strZIPName  as a String as a constant

**)
Procedure TITHResponseFile.BuildResponseFile(Const strBasePath, strProject, strZIPName: String);

ResourceString
  strBuildingFilelistFor = 'Building Filelist for %s...';
  strCheckingFilelistForResources = 'Checking Filelist for Resources...';

Const
  //: @debug strRESExt = '.res';
  strCFGExt = '.cfg';
  strResponseFileExt = '.response';

Begin
  TfrmITHProcessing.ShowProcessing(Format(strBuildingFilelistFor, [ExtractFileName(strProject)]));
  //: @debug AddToList(strBasePath, FProject.FileName);
  //: @debug AddToList(strBasePath, ChangeFileExt(FProject.FileName, strRESExt));
  AddToList(strBasePath, ChangeFileExt(FProject.FileName, strCFGExt));
  BuildTargetFileName(strBasePath);
  AddProjectFilesToResponseFile(strBasePath);
  TfrmITHProcessing.ShowProcessing(strCheckingFilelistForResources);
  CheckResponseFilesForIncludeAndRes(strProject, strBasePath);
  AddAdditionalZipFilesToResponseFile(strBasePath);
  FFileName := ChangeFileExt(strZIPName, strResponseFileExt);
  CheckResponseFileAgainExclusions;
  Try
    {$IFDEF CODESITE}CodeSite.Send('Response File', FResponseFile);{$ENDIF}
    FResponseFile.SaveToFile(FFileName);
  Except
    On E: EWriteError Do
      FMsgMgr.AddMsg(Format(E.Message + '(%s)', [strProject]), fnHeader, ithfFailure);
  End;
End;

(**

  This method builds the target file name from scratch using project options.

  @precon  slResponse must be a valid instance of a TStringList and Project must be a valid instance of 
           a IOTAProject interface.
  @postcon Builds the target file name from scratch using project options.

  @note    This will not handle project options in INC files.

  @param   strBasePath    as a String as a constant

**)
Procedure TITHResponseFile.BuildTargetFileName(Const strBasePath: String);

Const
  iExtLen = 3;
  strDPKExt = '*.dpk';
  strBPLExt = '.bpl';
  strDPRExt = '*.dpr';
  strEXEExt = '.exe';
  strLibraryPattern = '*library*%s*;*';
  strDLLExt = '.dll';
  strOutputDirOpName = 'OutputDir';
  strPkgDllDirOpName = 'PkgDllDir';
  strDllPrefixOpName = 'DllPrefix';
  strSOPrefixOpName = 'SOPrefix';
  strDllSuffixOpName = 'DllSuffix';
  strSOSuffixOpName = 'SOSuffix';
  strDllVersionOpName = 'DllVersion';
  strSOVersionOpName = 'SOVersion';
  strDPRExtChange = '.dpr';

Var
  strExt: String;
  i: Integer;
  strTargetName: String;
  sl: TStringList;
  iStart: Integer;
  iEnd: Integer;

Begin
  If IsProject(strDPKExt) Then
    strExt := strBPLExt
  Else If IsProject(strDPRExt) Then
    Begin
      strExt := strEXEExt;
      sl := TStringList.Create;
      Try
        sl.LoadFromFile(ChangeFileExt(FProject.FileName, strDPRExtChange));
        For i := 0 To sl.Count - 1 Do
          Begin
            If Like(Format(strLibraryPattern,
              [ChangeFileExt(ExtractFileName(FProject.FileName), '')]), sl[i]) Then
              strExt := strDLLExt;
            If Like('{$e *}*', sl[i]) Or Like('{$e'#9'*}*', sl[i]) Then
            // Search for alternate extension
              Begin
                iStart := Pos('{$e', Lowercase(sl[i]));
                iEnd := Pos('}', sl[i]);
                If (iStart > 0) And (iEnd > 0) And (iStart < iEnd) Then
                  strExt := '.' + Trim(Copy(sl[i], iStart + iExtLen, iEnd - iStart - iExtLen));
              End;
          End;
      Finally
        sl.Free;
      End;
    End;
  //: @todo Check that this code works in XE2 and above properly.
  strTargetName := ExtractFilePath(FProject.FileName) + 
    ProjectOptions([strOutputDirOpName, strPkgDllDirOpName]) +
    ProjectOptions([strDllPrefixOpName, strSOPrefixOpName]) +
    ExtractFileName(ChangeFileExt(FProject.FileName, '')) + 
    ProjectOptions([strDllSuffixOpName, strSOSuffixOpName]) + strExt + '.' +
    ProjectOptions([strDllVersionOpName, strSOVersionOpName]);
  strTargetName := ExpandFileName(strTargetName);
  AddToList(strBasePath, strTargetName);
End;

(**

  This method checks whether the extension of the given file exists in the semi-colon separated list of 
  extension to be excluded resource warnings. If it does then it returns false.

  @precon  None.
  @postcon Checks whether the extension of the given file exists in the semi-colon separated list of 
           extension to be excluded resource warnings. If it does then it returns false.

  @param   slResExts   as a TStringList as a constant
  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TITHResponseFile.CheckResExts(Const slResExts: TStringList; Const strFileName: String): Boolean;

Var
  iExt: Integer;

Begin
  Result := True;
  For iExt := 0 To slResExts.Count - 1 Do
    Result := Result And Not Like(slResExts[iExt], strFileName);
End;

(**

  This method processes the response file information removing any items that make the wildcard (LIKE) 
  exclusions.

  @precon  None.
  @postcon Processes the response file information removing any items that make the wildcard (LIKE) 
           exclusions.

**)
Procedure TITHResponseFile.CheckResponseFileAgainExclusions;

Var
  i, j: Integer;
  sl: TStringList;

Begin
  sl := TStringList.Create;
  Try
    sl.Text := FProjectOps.ExcPatterns;
    For j := 0 To sl.Count - 1 Do
      For i := FResponseFile.Count - 1 DownTo 0 Do
        If Like(sl[j], FResponseFile[i]) Then
          Begin
            FResponseFile.Delete(i);
            Break;
          End;
  Finally
    sl.Free;
  End;
End;

(**

  This method searches the list of files in the response file for INCLUDE statements where additional INC
  and RC/RES files are requested.

  @precon  Project must be a valid instance of a IOTAProject interface, slResponse must be a valid 
           instance of a TStringList and INIFile must be a valid instance of a TMemINIFile class.
  @postcon Searches the list of files in the response file for INCLUDE statements where additional INC 
           and RC/RES files are requested.

  @param   strProject  as a String as a Constant
  @param   strBasePath as a String as a constant

**)
Procedure TITHResponseFile.CheckResponseFilesForIncludeAndRes(Const strProject, strBasePath: String);

Const
  strINCLUDEPattern = '{$INCLUDE';
  strRESOURCEPattern = '{$RESOURCE';

ResourceString
  strNotReferencedInProject = '%s is not referenced in the project (%s).';
  strNotReferencedInProjectRes = '%s is not referenced in the project (%s). Consider using an RC ' + 
    'file and the form {$R resfile.res rcfile.rc}.';

Var
  strFileName: String;
  slIncludeFiles: TStringList;
  strExts: String;
  iModule: Integer;
  iExt: Integer;
  slResExts: TStringList;
  i: Integer;
  astrResExts: TDGHArrayOfString;

Begin
  // Check Source files (*.pas) for INCLUDE & RES files.
  For iModule := 0 To FProject.GetModuleCount - 1 Do
    Begin
      strFileName := FProject.GetModule(iModule).FileName;
      TfrmITHProcessing.ProcessFileName(ExtractFileName(strFileName));
      slIncludeFiles := TStringList.Create;
      Try
        CheckSourceForInclude(FProject.GetModule(iModule).FileName, '{$I', slIncludeFiles);
        CheckSourceForInclude(FProject.GetModule(iModule).FileName, strINCLUDEPattern,
          slIncludeFiles);
        For i := 0 To slIncludeFiles.Count - 1 Do
          If Not AddToList(strBasePath, slIncludeFiles[i]) Then
            FMsgMgr.AddMsg(Format(strNotReferencedInProject,
              [slIncludeFiles[i], strProject]), fnTools, ithfWarning);
        slIncludeFiles.Clear;
        CheckSourceForInclude(FProject.GetModule(iModule).FileName, '{$R', slIncludeFiles);
        CheckSourceForInclude(FProject.GetModule(iModule).FileName, strRESOURCEPattern,
          slIncludeFiles);
        slResExts := TStringList.Create;
        Try
          strExts := FProjectOps.ResExtExc;
          astrResExts := DGHSplit(strExts, ';');
          For iExt := Low(astrResExts) To High(astrResExts) Do
            Begin
              If (astrResExts[iExt] <> '') And (astrResExts[iExt][1] = '.') Then
                astrResExts[iExt] := '*' + astrResExts[iExt];
              slResExts.Add(astrResExts[iExt]);
            End;
          For i := 0 To slIncludeFiles.Count - 1 Do
            If Not AddToList(strBasePath, slIncludeFiles[i]) Then
              If CheckResExts(slResExts, slIncludeFiles[i]) Then
                FMsgMgr.AddMsg(Format(strNotReferencedInProjectRes,
                  [slIncludeFiles[i], strProject]), fnTools, ithfWarning);
        Finally
          slResExts.Free;
        End;
      Finally
        slIncludeFiles.Free;
      End;
    End;
End;

(**

  This method searches for include / res files in source code and extracts the file name and adds it to 
  the list of files to be included in the zip response.

  @precon  slResponse must be a valid instance.
  @postcon Searches for include / res files in source code and extracts the file name and adds it to the
           list of files to be included in the zip response.

  @param   strFileName    as a String as a constant
  @param   strInclude     as a String as a constant
  @param   slIncludeFiles as a TStringList as a constant

**)
Procedure TITHResponseFile.CheckSourceForInclude(Const strFileName, strInclude : String;
  Const slIncludeFiles: TStringList);

  (**

    This function returns the positions of the closing brace before the end of the line containing 
    iOffset or the line end. If the brace is found a positive offset is returned else if the line end is 
    found a negative offset is returned.

    @precon  None.
    @postcon Returns the positions of the closing brace before the end of the line containing iOffset or
             the line end. If the brace is found a positive offset is returned else if the line end is
             found a negative offset is returned.

    @param   strText as a String as a constant
    @param   iOffset as an Integer as a constant
    @return  an Integer

  **)
  Function FindClosingBrace(Const strText: String; Const iOffset: Integer): Integer;

  Var
    i: Integer;

  Begin
    Result := 0;
    For i := iOffset To Length(strText) Do
      {$IFNDEF D2009}
      If strText[i] In ['}', #13, #10] Then
      {$ELSE}
      If CharInSet(strText[i], ['}', #13, #10]) Then
        {$ENDIF}
        Begin
          If strText[i] = '}' Then
            Result := i
          Else
            Result := -i;
          Exit;
        End;
  End;

//ResourceString
//  strExceptionPrefix = 'ITHelper: ';

Var
  sl: TStringList;
  iOffset: Integer;
  iEnd: Integer;
  strIncFile: String;
  iPos: Integer;
  slTokens: TStringList;

Begin
  sl := TStringList.Create;
  Try
    If FileExists(strFileName) Then
      Begin
        //Try
          sl.LoadFromFile(strFileName);
          iOffset := 1;
          While iOffset > 0 Do
            Begin
              iOffset := PosEx(strInclude, sl.Text, iOffset);
              If iOffset > 0 Then
                Begin
                  Inc(iOffset, Length(strInclude));
                  {$IFNDEF D2009}
                  If sl.Text[iOffset] In [#32, #9] Then
                  {$ELSE}
                  If CharInSet(sl.Text[iOffset], [#32, #9]) Then
                    {$ENDIF}
                    Begin
                      iEnd := FindClosingBrace(sl.Text, iOffset);
                      If iEnd > iOffset Then
                        Begin
                          slTokens := Tokenize(Copy(sl.Text, iOffset, iEnd - iOffset));
                          Try
                            If slTokens.Count > 0 Then
                              Begin
                                strIncFile := slTokens[0];
                                iPos := Pos('*', strIncFile);
                                While iPos > 0 Do
                                  Begin
                                    strIncFile := Copy(strIncFile, 1, iPos - 1) +
                                      ChangeFileExt(ExtractFileName(strFileName), '') +
                                      Copy(strIncFile, iPos + 1, Length(strIncFile));
                                    iPos := Pos('*', strIncFile);
                                  End;
                                strIncFile := StringReplace(strIncFile, '''', '',
                                  [rfReplaceAll]);
                                If ExtractFilePath(strIncFile) = '' Then
                                  strIncFile := ExtractFilePath(strFileName) + strIncFile;
                                strIncFile := ExpandFileName(strIncFile);
                                If FileExists(strIncFile) Then
                                  slIncludeFiles.Add(strIncFile);
                              End;
                          Finally
                            slTokens.Free;
                          End;
                          iOffset := iEnd + 1;
                        End
                      Else
                        iOffset := Abs(iEnd) + 1;
                    End;
                End;
            End;
        //Except
        //  On E: Exception Do
        //    FMsgMgr.AddMsg(strExceptionPrefix + E.Message, fnHeader, ithfFailure);
        //End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  A constructor for the TITHResponseFile class.

  @precon  None.
  @postcon Stores interface references for later use and creates a string list for the response file.

  @param   Project        as an IOTAProject as a constant
  @param   GlobalOps      as an IITHGlobalOptions as a constant
  @param   ProjectOps     as an IITHProjectOptions as a constant
  @param   MessageManager as an IITHMessageManager as a constant

**)
Constructor TITHResponseFile.Create(Const Project: IOTAProject; Const GlobalOps : IITHGlobalOptions;
  Const ProjectOps: IITHProjectOptions; Const MessageManager : IITHMessageManager);

Begin
  FProject := Project;
  FGlobalOps := GlobalOps;
  FProjectOps := ProjectOps;
  FMsgMgr := MessageManager;
  FResponseFile := TStringList.Create;
  FResponseFile.Duplicates := dupIgnore;
  FResponseFile.Sorted := True;
End;

(**

  A destructor for the TITHResponseFile class.

  @precon  None.
  @postcon Deletes the response file if found and then frees the memory used by the string list.

**)
Destructor TITHResponseFile.Destroy;

Begin
  If FileExists(FFileName) Then
    DeleteFile(FFileName);
  FResponseFile.Free;
  Inherited Destroy;
End;

(**

  This method expands the list of files to include as a space separated list of double quoted files.

  @precon  slResponseFile must be a valid instance of a string list.
  @postcon Returns a list of files to include as a space separated list of double quoted files.

  @return  a String

**)
Function TITHResponseFile.FileList : String;

Var
  i: Integer;

Begin
  Result := '';
  For i := 0 To FResponseFile.Count - 1 Do
    Result := Result + Format('"%s" ', [FResponseFile[i]]);
End;

(**

  This is a getter method for the FileName property.

  @precon  None.
  @postcon Returns the filename of the response file.

  @return  a String

**)
Function TITHResponseFile.GetFileName: String;

Begin
  Result := FFileName;
End;

(**

  This method searches the projects module files in search of the given file extension pattern.

  @precon  Project must be a valid instance of a IOTAProject interface.
  @postcon Returns return if the file extension pattern was found in the module files.

  @param   strExtType as a String as a constant
  @return  a Boolean

**)
Function TITHResponseFile.IsProject(Const strExtType: String): Boolean;

Var
  i: Integer;

Begin
  Result := Like(strExtType, FProject.FileName);
  For i := 0 To FProject.ModuleFileCount - 1 Do
    Result := Result Or Like(strExtType, FProject.ModuleFileEditors[i].FileName);
End;

(**

  This method returns the first non-null occurance of the list of project option values.

  @precon  Project must be a valid instance of a IOTAProject interface.
  @postcon Returns the first non-null occurance of the list of project option values.

  @param   strOptions as an Array Of String as a constant
  @return  a String

**)
Function TITHResponseFile.ProjectOptions(Const strOptions: Array Of String): String;

Var
  i: Integer;
  varValue: Variant;

Begin
  Result := '';
  For i := Low(strOptions) To High(strOptions) Do
    Begin
      varValue := FProject.ProjectOptions.Values[strOptions[i]];
      If VarType(varValue) And varString > 0 Then
        Result := varValue;
      If Result <> '' Then
        Break;
    End;
End;

End.




