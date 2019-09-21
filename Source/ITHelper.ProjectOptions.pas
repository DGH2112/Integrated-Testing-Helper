(**
  
  This module contains a class which implements the IITHProjectOptions interface for providing
  project options.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Sep 2019
  
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
Unit ITHelper.ProjectOptions;

Interface

Uses
  ITHelper.Types,
  ITHElper.Interfaces,
  Classes,
  IniFiles,
  ToolsAPI;

Type
  (** A class to provide access to the projects options in the INI file. **)
  TITHProjectOptions = Class(TInterfacedObject, IITHProjectOptions)
  Strict Private
    FProject    : IOTAProject;
    FINIFile    : TMemIniFile;
    FModified   : Boolean;
    FVerInfo    : TStringList;
    FAddZipFiles: TStringList;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Function  GetResExtExc: String;
    Function  GetIncOnCompile: Boolean;
    Function  GetCopyVerInfo: String;
    Function  GetIncITHVerInfo: Boolean;
    Function  GetMajor: Integer;
    Function  GetMinor: Integer;
    Function  GetRelease: Integer;
    Function  GetBuild: Integer;
    Function  GetIncResInProj: Boolean;
    Function  GetCompileRes: Boolean;
    Function  GetResourceName: String;
    Function  GetWarnBefore: Boolean;
    Function  GetWarnAfter: Boolean;
    Function  GetVerInfo: TStringList;
    Function  GetEnableZipping: Boolean;
    Function  GetZipName: String;
    Function  GetBasePath: String;
    Function  GetExcPatterns: String;
    Function  GetAddZipFiles: TStringList;
    Function  GetIniFile : TMemIniFile;
    Function  GetSaveModifiedFiles: Boolean;
    Procedure SetResExtExc(Const strValue: String);
    Procedure SetIncOnCompile(Const boolValue: Boolean);
    Procedure SetCopyVerInfo(Const strValue: String);
    Procedure SetIncITHVerInfo(Const boolValue: Boolean);
    Procedure SetMajor(Const iValue: Integer);
    Procedure SetMinor(Const iValue: Integer);
    Procedure SetRelease(Const iValue: Integer);
    Procedure SetBuild(Const iValue: Integer);
    Procedure SetIncResInProj(Const boolValue: Boolean);
    Procedure SetCompileRes(Const boolValue: Boolean);
    Procedure SetResourceName(Const strValue: String);
    Procedure SetWarnBefore(Const boolValue: Boolean);
    Procedure SetWarnAfter(Const boolValue: Boolean);
    Procedure SetEnableZipping(Const boolValue: Boolean);
    Procedure SetZipName(Const strValue: String);
    Procedure SetBasePath(Const strValue: String);
    Procedure SetExcPatterns(Const strValue: String);
    Procedure UpdateVerInfo(Sender: TObject);
    Procedure UpdateAddZipFiles(Sender: TObject);
    Procedure UpdateFileVersion;
    Procedure SetSaveModifiedFiles(Const boolValue: Boolean);
  Public
    Constructor Create(Const strINIFileName: String; Const Project: IOTAProject);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils;

Const
  (** An INI Section name for additional file information. **)
  strAdditionalZipFilesSection = 'Additional Zip Files';
  (** An INI Key for the ZIP base path **)
  strBasePathKey = 'BasePath';
  (** An INI Key for the Build Version **)
  strBuildVerKey = 'BuildVer';
  (** An INI Key for the Resource compiler **)
  strCompileWithBRCCKey = 'CompileWithBRCC32';
  (** An INI Key for the Copy Version Information **)
  strCopyVersionInfoFromKey = 'CopyVersionInfoFrom';
  (** An INI Key for the Enalbed **)
  strEnabledKey = 'Enabled';
  (** An INI Key for the Enabled version info **)
  strEnabledVersionInfoKey = 'EnabledVersionInfo';
  (** An INI Key for the excluded resource file extensions **)
  strExcludedResExtsKey = 'ExcludedResExts';
  (** An INI Key for the Excluded zip files **)
  strExclusionFilesKey = 'ExclusionFiles';
  (** An INI Key for the File version **)
  strFileVersionKey = 'FileVersion';
  (** An INI Key for the INcrement on compile **)
  strIncBuildKey = 'IncBuild';
  (** An INI Key for the include in project **)
  strIncludeInProjectKey = 'IncludeInProject';
  (** An INI Key for the major version **)
  strMajorVerKey = 'MajorVer';
  (** An INI Key for the minor version **)
  strMinorVerKey = 'MinorVer';
  (** An INI Key for the release version **)
  strReleaseVerKey = 'ReleaseVer';
  (** An INI Key for the resource name **)
  strResourceNameKey = 'ResourceName';
  (** An INI Section name for general information. **)
  strSetupSection = 'Setup';
  (** An INI Section name for version information. **)
  strVersionInfoSection = 'VersionInfo';
  (** An INI Key for the warn after compile **)
  strWarnAfterKey = 'WarnAfter';
  (** An INI Key for the warn before compile **)
  strWarnBeforeKey = 'WarnBefore';
  (** An INI Key for the zip filename **)
  strZipFileKey = 'ZipFile';
  (** An INI Section name for zipping information. **)
  strZippingSection = 'Zipping';
  (** An INI Key for the SaveModifiedFiles property. **)
  strSaveModifiedFilesBeforeZippingKey = 'SaveModifiedFilesBeforeZipping';

(**

  A constructor for the TProjectOptions class.

  @precon  Project must be a valid instance.
  @postcon Creates the class and loads the projects INI file.

  @param   strINIFileName as a String as a constant
  @param   Project        as an IOTAProject as a constant

**)
Constructor TITHProjectOptions.Create(Const strINIFileName: String; Const Project: IOTAProject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FProject := Project;
  FINIFile := TMemIniFile.Create(strINIFileName);
  FVerInfo              := TStringList.Create;
  FVerInfo.OnChange     := UpdateVerInfo;
  FAddZipFiles          := TStringList.Create;
  FAddZipFiles.OnChange := UpdateAddZipFiles;
  FModified             := False;
End;

(**

  A destructor for the TProjectOptions class.

  @precon  None.
  @postcon saves the inin file if modified and frees the memory used by the class.

**)
Destructor TITHProjectOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  If FModified Then
    FINIFile.UpdateFile;
  FAddZipFiles.Free;
  FVerInfo.Free;
  FINIFile.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the AddFileFiles property.

  @precon  None.
  @postcon Returns a string list of the additional zip files.

  @return  a TStringList

**)
Function TITHProjectOptions.GetAddZipFiles: TStringList;

Var
  sl     : TStringList;
  i      : Integer;
  strLine: String;

Begin
  Result := FAddZipFiles;
  sl     := TStringList.Create;
  Try
    FINIFile.ReadSection(strAdditionalZipFilesSection, sl);
    FAddZipFiles.OnChange := Nil;
    Try
      FAddZipFiles.Clear;
    Finally
      FAddZipFiles.OnChange := UpdateAddZipFiles;
    End;
    For i := 0 To sl.Count - 1 Do
      Begin
        strLine := FINIFile.ReadString(strAdditionalZipFilesSection, sl[i], '');
        FAddZipFiles.OnChange := Nil;
        Try
          If strLine <> '' Then
            FAddZipFiles.Add(strLine);
        Finally
          FAddZipFiles.OnChange := UpdateAddZipFiles;
        End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This is a getter method for the Base Path property.

  @precon  None.
  @postcon Returns the base path for th zipping of files.

  @return  a String

**)
Function TITHProjectOptions.GetBasePath: String;

Begin
  Result := FINIFile.ReadString(strZippingSection, strBasePathKey, ExtractFilePath(FProject.FileName));
End;

(**

  This is a getter method for the Build property.

  @precon  None.
  @postcon returns the build number from the project options version information.

  @return  an Integer

**)
Function TITHProjectOptions.GetBuild: Integer;

Begin
  Result := FINIFile.ReadInteger(strSetupSection, strBuildVerKey, 0);
End;

(**

  This is a getter method for the CompileRes property.

  @precon  None.
  @postcon Returns whether the project version resource should be pre-compiled.

  @return  a Boolean

**)
Function TITHProjectOptions.GetCompileRes: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strCompileWithBRCCKey, False);
End;

(**

  This is a getter method for the CopyVerInfo property.

  @precon  None.
  @postcon Returns the path and filename of an executable from which to coppy the version
           informaton: Major, Minor, Release and Build.

  @return  a String

**)
Function TITHProjectOptions.GetCopyVerInfo: String;

Begin
  Result := FINIFile.ReadString(strSetupSection, strCopyVersionInfoFromKey, '');
End;

(**

  This is a getter method for the EnableZipping property.

  @precon  None.
  @postcon Returns whether zipping of the projects files.

  @return  a Boolean

**)
Function TITHProjectOptions.GetEnableZipping: Boolean;

Begin
  Result := FINIFile.ReadBool(strZippingSection, strEnabledKey, False);
End;

(**

  This is a getter method for the ExcPatterns property.

  @precon  None.
  @postcon Returns a string of patterns to be excluded from the zipping processs.

  @return  a String

**)
Function TITHProjectOptions.GetExcPatterns: String;

Begin
  Result := StringReplace(FINIFile.ReadString(strZippingSection, strExclusionFilesKey, ''), '|',
    #13#10, [rfReplaceAll]);
End;

(**

  This is a getter method for the IncITHVerInfo property.

  @precon  None.
  @postcon Returns whether ITHelpers version information should be included in the project
           executable.

  @return  a Boolean

**)
Function TITHProjectOptions.GetIncITHVerInfo: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strEnabledVersionInfoKey, False);
End;

(**

  This is a getter method for the IncOnCompile property.

  @precon  None.
  @postcon Returns whether the build number should be incemented on a successful
           compilation.

  @return  a Boolean

**)
Function TITHProjectOptions.GetIncOnCompile: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strIncBuildKey, False);
End;

(**

  This is a getter method for the IncResInProj property.

  @precon  None.
  @postcon Returns whether the version resource should be included in the project.

  @return  a Boolean

**)
Function TITHProjectOptions.GetIncResInProj: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strIncludeInProjectKey, False);
End;

(**

  This is a getter method for the MemINIFile property.

  @precon  None.
  @postcon Returns an instance of the internal Memory INI File.

  @return  a TMemIniFile

**)
Function TITHProjectOptions.GetIniFile: TMemIniFile;

Begin
  Result := FINIFile;
End;

(**

  This is a getter method for the Major property.

  @precon  None.
  @postcon Returns the major version number of the project.

  @return  an Integer

**)
Function TITHProjectOptions.GetMajor: Integer;

Begin
  Result := FINIFile.ReadInteger(strSetupSection, strMajorVerKey, 1);
End;

(**

  This is a getter method for the Minor property.

  @precon  None.
  @postcon Returns the minor version number of the project.

  @return  an Integer

**)
Function TITHProjectOptions.GetMinor: Integer;

Begin
  Result := FINIFile.ReadInteger(strSetupSection, strMinorVerKey, 0);
End;

(**

  This is a getter method for the Release property.

  @precon  None.
  @postcon Returns the release version number of the project.

  @return  an Integer

**)
Function TITHProjectOptions.GetRelease: Integer;

Begin
  Result := FINIFile.ReadInteger(strSetupSection, strReleaseVerKey, 0);
End;

(**

  This is a getter method for the ResExtExc property.

  @precon  None.
  @postcon Returns a semi-colon sepapated list of file extensions to be skipped when
           checking for resources that are not included in the project.

  @return  a String

**)
Function TITHProjectOptions.GetResExtExc: String;

Const
  strDefaultResExclusions = '.dcr';

Begin
  Result := FINIFile.ReadString(strSetupSection, strExcludedResExtsKey, strDefaultResExclusions);
End;

(**

  This is a getter method for the ResourceName property.

  @precon  None.
  @postcon Returns the name of the resource which will contains the version information.

  @return  a String

**)
Function TITHProjectOptions.GetResourceName: String;

Const
  strDefaultVerInfoName = 'ITHVerInfo';

Begin
  Result := FINIFile.ReadString(strSetupSection, strResourceNameKey, strDefaultVerInfoName);
End;

(**

  This is a getter method for the SaveModifiedFiles property.

  @precon  None.
  @postcon Gets this property from the ITHelper ini file.

  @return  a Boolean

**)
Function TITHProjectOptions.GetSaveModifiedFiles: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strSaveModifiedFilesBeforeZippingKey, False);
End;

(**

  This is a getter method for the VerInfo property.

  @precon  None.
  @postcon Returns a string list of version information strings.

  @return  a TStringList

**)
Function TITHProjectOptions.GetVerInfo: TStringList;

Const
  strVerInfo =
    'CompanyName='#13#10 +
    'FileDescription='#13#10 +
    'FileVersion=%d.%d.%d.%d'#13#10 + 
    'InternalName='#13#10 +
    'LegalCopyright='#13#10 +
    'LegalTrademarks='#13#10 +
    'OriginalFilename='#13#10 +
    'ProductName='#13#10 +
    'ProductVersion=%d.%d'#13#10 +
    'Comments=';
  
Var
  sl: TStringList;
  i : Integer;

Begin
  Result := FVerInfo;
  If Not FINIFile.SectionExists(strVersionInfoSection) Then
    FVerInfo.Text := Format(strVerInfo, [GetMajor, GetMinor, GetRelease, GetBuild, GetMajor, GetMinor])
  Else
    Begin
      sl := TStringList.Create;
      Try
        FVerInfo.OnChange := Nil;
        Try
          FVerInfo.Clear;
          FINIFile.ReadSection(strVersionInfoSection, sl);
          For i := 0 To sl.Count - 1 Do
            FVerInfo.Add(Format('%s=%s', [sl[i], FINIFile.ReadString(strVersionInfoSection, sl[i], '')]));
        Finally
          FVerInfo.OnChange := UpdateVerInfo;
        End;
      Finally
        sl.Free;
      End;
    End;
End;

(**

  This is a getter method for the WarnAfter property.

  @precon  None.
  @postcon Returns whether warns of missing tools are displayed after compilation.

  @return  a Boolean

**)
Function TITHProjectOptions.GetWarnAfter: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strWarnAfterKey, True);
End;

(**

  This is a getter method for the WarnBefore property.

  @precon  None.
  @postcon Returns whether warns of missing tools are displayed before compilation.

  @return  a Boolean

**)
Function TITHProjectOptions.GetWarnBefore: Boolean;

Begin
  Result := FINIFile.ReadBool(strSetupSection, strWarnBeforeKey, True);
End;

(**

  This is a getter method for the ZipName property.

  @precon  None.
  @postcon Return the name of the zip file.

  @return  a String

**)
Function TITHProjectOptions.GetZipName: String;

Begin
  Result := FINIFile.ReadString(strZippingSection, strZipFileKey, '');
End;

(**

  This is a setter method for the BasePath property.

  @precon  None.
  @postcon Sets the value of the base path for zipping files.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetBasePath(Const strValue: String);

Begin
  FINIFile.WriteString(strZippingSection, strBasePathKey, strValue);
  FModified := True;
End;

(**

  This is a setter method for the Build property.

  @precon  None.
  @postcon Sets the value of the build verion information.

  @param   iValue as an Integer as a constant

**)
Procedure TITHProjectOptions.SetBuild(Const iValue: Integer);

Begin
  FINIFile.WriteInteger(strSetupSection, strBuildVerKey, iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the CompileRes property.

  @precon  None.
  @postcon Sets the value of whether a verwion resource should be pre-compiled.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetCompileRes(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strCompileWithBRCCKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the CopyVerinfo property.

  @precon  None.
  @postcon Sets the value of the executable from which version information should be copied.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetCopyVerInfo(Const strValue: String);

Begin
  FINIFile.WriteString(strSetupSection, strCopyVersionInfoFromKey, strValue);
  FModified := True;
End;

(**

  This is a setter method for the EnableZipping property.

  @precon  None.
  @postcon Sets the value for whether zipping of the projects files should take place.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetEnableZipping(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strZippingSection, strEnabledKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the ExcPatterns property.

  @precon  None.
  @postcon Sets the value of the list of excision to the zipping process.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetExcPatterns(Const strValue: String);

Begin
  FINIFile.WriteString(strZippingSection, strExclusionFilesKey, StringReplace(strValue, #13#10, '|',
      [rfReplaceAll]));
  FModified := True;
End;

(**

  This is a setter method for the IncITHVerinfo property.

  @precon  None.
  @postcon Sets whether version information should be included in the executable.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetIncITHVerInfo(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strEnabledVersionInfoKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the IncOnCompile property.

  @precon  None.
  @postcon Sets whether the build number should be incremented on a successful compilation.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetIncOnCompile(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strIncBuildKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the IncResInProj property.

  @precon  None.
  @postcon Sets whether the version resource should be included in the project.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetIncResInProj(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strIncludeInProjectKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the Major property.

  @precon  None.
  @postcon Sets the major version number of the projects version information.

  @param   iValue as an Integer as a constant

**)
Procedure TITHProjectOptions.SetMajor(Const iValue: Integer);

Begin
  FINIFile.WriteInteger(strSetupSection, strMajorVerKey, iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the Minor property.

  @precon  None.
  @postcon Sets the minor version number of the projects version information.

  @param   iValue as an Integer as a constant

**)
Procedure TITHProjectOptions.SetMinor(Const iValue: Integer);

Begin
  FINIFile.WriteInteger(strSetupSection, strMinorVerKey, iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the Release property.

  @precon  None.
  @postcon Sets the release version number of the projects version information.

  @param   iValue as an Integer as a constant

**)
Procedure TITHProjectOptions.SetRelease(Const iValue: Integer);

Begin
  FINIFile.WriteInteger(strSetupSection, strReleaseVerKey, iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the ResExtExc property.

  @precon  None.
  @postcon Sets the resource extensions to be excluded from checks.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetResExtExc(Const strValue: String);

Begin
  FINIFile.WriteString(strSetupSection, strExcludedResExtsKey, strValue);
  FModified := True;
End;

(**

  This is a setter method for the ResourceName property.

  @precon  None.
  @postcon Sets the resource name of the version information.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetResourceName(Const strValue: String);

Begin
  FINIFile.WriteString(strSetupSection, strResourceNameKey, strValue);
  FModified := True;
End;

(**

  This is a setter method for the SaveModifiedFiles property.

  @precon  None.
  @postcon Sets this property in the ITHelper inni file.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetSaveModifiedFiles(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strSaveModifiedFilesBeforeZippingKey, boolValue);
End;

(**

  This is a setter method for the WarnAfter property.

  @precon  None.
  @postcon Sets whether warns should be issed for not after compilation tools.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetWarnAfter(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strWarnAfterKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the WarnBefore property.

  @precon  None.
  @postcon Sets whether warns should be issed for not before compilation tools.

  @param   boolValue as a Boolean as a constant

**)
Procedure TITHProjectOptions.SetWarnBefore(Const boolValue: Boolean);

Begin
  FINIFile.WriteBool(strSetupSection, strWarnBeforeKey, boolValue);
  FModified := True;
End;

(**

  This is a setter method for the ZipName property.

  @precon  None.
  @postcon Sets the filename of the zip file.

  @param   strValue as a String as a constant

**)
Procedure TITHProjectOptions.SetZipName(Const strValue: String);

Begin
  FINIFile.WriteString(strZippingSection, strZipFileKey, strValue);
  FModified := True;
End;

(**

  This is an on update event handler for the Additional Zip files string list.

  @precon  None.
  @postcon If the strings are updated then the updates are written back to the INI file.

  @param   Sender as a TObject

**)
Procedure TITHProjectOptions.UpdateAddZipFiles(Sender: TObject);

Const
  strItemMask = 'Item%d';

Var
  i: Integer;

Begin
  FINIFile.EraseSection(strAdditionalZipFilesSection);
  For i := 0 To FAddZipFiles.Count - 1 Do
    FINIFile.WriteString(strAdditionalZipFilesSection, Format(strItemMask, [i]), FAddZipFiles[i]);
  FModified := True;
End;

(**

  This method updates the file version information field FileVersion when either the
  major, minor, releaase or build change.

  @precon  None.
  @postcon Updates the file version information field FileVersion when either the
           major, minor, releaase or build change.

**)
Procedure TITHProjectOptions.UpdateFileVersion;

Begin
  FINIFile.WriteString(strVersionInfoSection, strFileVersionKey,
    Format('%d.%d.%d.%d', [GetMajor, GetMinor, GetRelease, GetBuild]));
  FModified := True;
End;

(**

  This is an on update event handler for the Version Information string list.

  @precon  None.
  @postcon If the strings are updated then the updates are written back to the INI file.

  @param   Sender as a TObject

**)
Procedure TITHProjectOptions.UpdateVerInfo(Sender: TObject);

Var
  i: Integer;

Begin
  FINIFile.EraseSection(strVersionInfoSection);
  For i := 0 To FVerInfo.Count - 1 Do
    FINIFile.WriteString(strVersionInfoSection, FVerInfo.Names[i], FVerInfo.ValueFromIndex[i]);
  FModified := True;
End;

End.

