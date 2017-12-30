(**

  This module contains a class to manage the global options of the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    29 Dec 2017

**)
Unit ITHelper.GlobalOptions;

Interface

Uses
  Graphics,
  Classes,
  IniFiles,
  ToolsAPI;

Type
  (** An enumerate to describe which font is to be read or written. **)
  TITHFonts = (ithfHeader, ithfDefault, ithfSuccess, ithfFailure, ithfWarning);
  (** An enumerate to describe the different font names. **)
  TITHFontNames = (fnHeader, fnTools);

  (** An enumerate for the different parts of the process. **)
  TITHEnabledOption = (
    eoBefore,
    eoAfter,
    eoZip,
    eoGroupEnabled,
    eoCopyVersionInfo,
    eoBuildVersionResource,
    eoIncrementBuild
  );
  (** A set of enumerates. **)
  TITHEnabledOptions = Set Of TITHEnabledOption;

  (** A class to provide access to the projects options in the INI file. **)
  TITHProjectOptions = Class
  Strict Private
    FProject    : IOTAProject;
    FINIFile    : TMemIniFile;
    FModified   : Boolean;
    FVerInfo    : TStringList;
    FAddZipFiles: TStringList;
  Strict Protected
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
  Public
    Constructor Create(Const strINIFileName: String; Const Project: IOTAProject);
    Destructor Destroy; Override;
    (**
      This property gets and sets the resource extensions to be excluded from checks.
      @precon  None.
      @postcon Gets and sets the resource extensions to be excluded from checks.
      @return  a String
    **)
    Property ResExtExc: String Read GetResExtExc Write SetResExtExc;
    (**
      This property gets and sets whether the build number should be incremented on a
      successful file.
      @precon  None.
      @postcon Gets and sets whether the build number should be incremented on a
               successful file.
      @return  a Boolean
    **)
    Property IncOnCompile: Boolean Read GetIncOnCompile Write SetIncOnCompile;
    (**
      This property gets and sets the executable from which version information should be
      copied.
      @precon  None.
      @postcon Gets and sets the executable from which version information should be
               copied.
      @return  a String
    **)
    Property CopyVerInfo: String Read GetCopyVerInfo Write SetCopyVerInfo;
    (**
      This property gets and sets the inclusion of the version information in the project
      executable.
      @precon  None.
      @postcon Gets and sets the inclusion of the version information in the project
               executable.
      @return  a Boolean
    **)
    Property IncITHVerInfo: Boolean Read GetIncITHVerInfo Write SetIncITHVerInfo;
    (**
      This property gets and sets the major version number of the version information.
      @precon  None.
      @postcon Gets and sets the major version number of the version information.
      @return  an Integer
    **)
    Property Major: Integer Read GetMajor Write SetMajor;
    (**
      This property gets and sets the minor version number of the version information.
      @precon  None.
      @postcon Gets and sets the minor version number of the version information.
      @return  an Integer
    **)
    Property Minor: Integer Read GetMinor Write SetMinor;
    (**
      This property gets and sets the release version number of the version information.
      @precon  None.
      @postcon Gets and sets the release version number of the version information.
      @return  an Integer
    **)
    Property Release: Integer Read GetRelease Write SetRelease;
    (**
      This property gets and sets the build version number of the version information.
      @precon  None.
      @postcon Gets and sets the build version number of the version information.
      @return  an Integer
    **)
    Property Build: Integer Read GetBuild Write SetBuild;
    (**
      This property gets and sets the string list which contains the version information
      strings.
      @precon  None.
      @postcon Gets and sets the string list which contains the version information
               strings.
      @return  a TStringList
    **)
    Property VerInfo: TStringList Read GetVerInfo;
    (**
      This property gets and sets the inclusion of the version information resource in the
      project.
      @precon  None.
      @postcon Gets and sets the inclusion of the version information resource in the
               project.
      @return  a Boolean
    **)
    Property IncResInProj: Boolean Read GetIncResInProj Write SetIncResInProj;
    (**
      This property gets and sets whether the resource information should be pre-compiled
      with BRCC32.
      @precon  None.
      @postcon Gets and sets whether the resource information should be pre-compiled
               with BRCC32.
      @return  a Boolean
    **)
    Property CompileRes: Boolean Read GetCompileRes Write SetCompileRes;
    (**
      This property gets and sets the resource file name for the version information.
      @precon  None.
      @postcon Gets and sets the resource file name for the version information.
      @return  a String
    **)
    Property ResourceName: String Read GetResourceName Write SetResourceName;
    (**
      This property gets and sets whether a warning should be issued for missing
      pre-compilation tools.
      @precon  None.
      @postcon Gets and sets whether a warning should be issued for missing
               pre-compilation tools.
      @return  a Boolean
    **)
    Property WarnBefore: Boolean Read GetWarnBefore Write SetWarnBefore;
    (**
      This property gets and sets whether a warning should be issued for missing
      post-compilation tools.
      @precon  None.
      @postcon Gets and sets whether a warning should be issued for missing
               post-compilation tools.
      @return  a Boolean
    **)
    Property WarnAfter: Boolean Read GetWarnAfter Write SetWarnAfter;
    (**
      This property gets the underlying INI that the project options are stored in.
      @precon  None.
      @postcon Gets the underlying INI that the project options are stored in.
      @return  a TMemIniFile
    **)
    Property INIFile: TMemIniFile Read FINIFile;
    (**
      This property gets and sets the zipping of project sources.
      @precon  None.
      @postcon Gets and sets the zipping of project sources.
      @return  a Boolean
    **)
    Property EnableZipping: Boolean Read GetEnableZipping Write SetEnableZipping;
    (**
      This property gets and sets the zip file name.
      @precon  None.
      @postcon Gets and sets the zip file name.
      @return  a String
    **)
    Property ZipName: String Read GetZipName Write SetZipName;
    (**
      This property gets and sets the base path for the file zipping (relative paths).
      @precon  None.
      @postcon Gets and sets the base path for the file zipping (relative paths).
      @return  a String
    **)
    Property BasePath: String Read GetBasePath Write SetBasePath;
    (**
      This property gets and sets the exclusion patterns for zipping.
      @precon  None.
      @postcon Gets and sets the exclusion patterns for zipping.
      @return  a String
    **)
    Property ExcPatterns: String Read GetExcPatterns Write SetExcPatterns;
    (**
      This property gets and sets the additional fiels to zip.
      @precon  None.
      @postcon Gets and sets the additional fiels to zip.
      @return  a TStringList
    **)
    Property AddZipFiles: TStringList Read GetAddZipFiles;
  End;

  (** A class to manage the gloabl options of the application. **)
  TITHGlobalOptions = Class
  Strict Private
    FINIFileName       : String;
    FFontName          : Array [Low(TITHFontNames) .. High(TITHFontNames)] Of String;
    FFontColour        : Array [Low(TITHFonts) .. High(TITHFonts)] Of TColor;
    FFontStyle         : Array [Low(TITHFonts) .. High(TITHFonts)] Of TFontStyles;
    FSwitchToMessages  : Boolean;
    FProjectGroupOps   : TStringList;
    FZipEXE            : String;
    FZipParameters     : String;
    FGroupMessages     : Boolean;
    FAutoScrollMessages: Boolean;
    FClearMessages     : Integer;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Function  GetFontName(Const iFont: TITHFontNames): String;
    Procedure SetFontName(Const iFont: TITHFontNames; Const strValue: String);
    Function  GetFontColour(Const iFont: TITHFonts): TColor;
    Procedure SetFontColour(Const iFont: TITHFonts; Const iValue: TColor);
    Function  GetFontStyles(Const iFont: TITHFonts): TFontStyles;
    Procedure SetFontStyles(Const iFont: TITHFonts; Const iValue: TFontStyles);
    Function  GetProjectGroupOps: TITHEnabledOptions;
    Procedure SetProjectGroupOps(Const Ops: TITHEnabledOptions);
    Procedure ExceptionsMgs(Const strExceptionMsg: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Save;
    Function ProjectOptions(Const Project: IOTAProject): TITHProjectOptions;
    (**
      A property to return the Main INI file name for the application.
      @precon  None.
      @postcon Return the Main INI file name for the application.
      @return  a String
    **)
    Property INIFileName: String Read FINIFileName;
    (**
      This property determines the font name of the enumerated item.
      @precon  None.
      @postcon Returns the font name.
      @param   iFont as a TITHFontNames as a Constant
      @return  a String
    **)
    Property FontName[Const iFont: TITHFontNames]: String Read GetFontName Write SetFontName;
    (**
      A property that determines the font colour for the specific enumeration.
      @precon  None.
      @postcon Returns the font colour for the specific enumeration.
      @param   iFont as a TITHFonts as a Constant
      @return  a TColor
    **)
    Property FontColour[Const iFont: TITHFonts]: TColor Read GetFontColour Write SetFontColour;
    (**
      A property that determines the font styles for the specific enumeration.
      @precon  None.
      @postcon Returns the font styles for the specific enumeration.
      @param   iFont as a TITHFonts as a Constant
      @return  a TFontStyles
    **)
    Property FontStyles[Const iFont: TITHFonts]: TFontStyles Read GetFontStyles Write SetFontStyles;
    (**
      A property to determine whether the IDE should which to the output messages
               after a successful compilation.
      @precon  None.
      @postcon Returns whether the IDE should which to the output messages
               after a successful compilation.
      @return  a Boolean
    **)
    Property SwitchToMessages: Boolean Read FSwitchToMessages Write FSwitchToMessages;
    (**
      A property to determines the project group options for the Integrated Testing
      Helper.
      @precon  None.
      @postcon Returns the project group options.
      @return  a TITHEnabledOptions
    **)
    Property ProjectGroupOps: TITHEnabledOptions Read GetProjectGroupOps Write SetProjectGroupOps;
    (**
      A property to define the executable archive programme for zipping files.
      @precon  None.
      @postcon Returns the archiving programme for zipping files.
      @return  a String
    **)
    Property ZipEXE: String Read FZipEXE Write FZipEXE;
    (**
      A property to define the parameter to be passed to the archive programme for zipping
      files.
      @precon  None.
      @postcon Returns the parameter to be passed to the archive programme for zipping
               files.
      @return  a String
    **)
    Property ZipParameters: String Read FZipParameters Write FZipParameters;
    (**
      A property to determine whether messages are group under headings.
      @precon  None.
      @postcon Returns whether messages are group under headings.
      @return  a Boolean
    **)
    Property GroupMessages: Boolean Read FGroupMessages Write FGroupMessages;
    (**
      A property to determines of new messages should be scrolled to.
      @precon  None.
      @postcon Returns whether new messages should be scrolled to.
      @return  a Boolean
    **)
    Property AutoScrollMessages: Boolean Read FAutoScrollMessages Write FAutoScrollMessages;
    (**
      A property to determine the number of seconds since the last compiled that should
      elapse before messages are cleared.
      @precon  None.
      @postcon Returns the number of seconds since the last compiled that should
               elapse before messages are cleared.
      @return  an Integer
    **)
    Property ClearMessages: Integer Read FClearMessages Write FClearMessages;
  End;

Implementation

Uses
  Dialogs,
  ITHelper.TestingHelperUtils,
  SysUtils,
  ActnList,
  Windows, 
  ITHelper.CommonFunctions,
  UITypes,
  Contnrs;

Const
  (** A constant array of INI name prefixes for font information. **)
  strFontINIPrefixes: Array [Low(TITHFonts) .. High(TITHFonts)] Of String = (
    'MsgHeaderFont',
    'MsgDefault',
    'MsgSuccess',
    'MsgFailure',
    'MsgWarning'
  );
  (** A constant array of INI font name prefixes for font information. **)
  strFontNameINIPrefixes: Array [Low(TITHFontNames) .. High(TITHFontNames)] Of String = (
    'HeaderFontName',
    'ToolFontName'
  );
  (** An INI Section name for general information. **)
  strSetupSection = 'Setup';
  (** An INI Section name for message information. **)
  strMessagesSection = 'Messages';
  (** An INI Section name for new project group information. **)
  strNewProjectGroupOptionsSection = 'NewProjectGroupOptions';
  (** An INI Section name for project group information. **)
  strProjectGroupOptionsSection = 'ProjectGroupOptions';
  (** An INI Section name for shortcut information. **)
  strShortcutsSection = 'Shortcuts';
  (** An INI Section name for additional file information. **)
  strAdditionalZipFilesSection = 'Additional Zip Files';
  (** An INI Section name for zipping information. **)
  strZippingSection = 'Zipping';
  (** An INI Section name for version information. **)
  strVersionInfoSection = 'VersionInfo';
  (** An INI Key for the ZIP EXE **)
  strZIPEXEKey = 'ZIPEXE';
  (** An INI Key for the ZIP Parmeters **)
  strZIPParametersKey = 'ZIPParameters';
  (** An INI Key for the group messages **)
  strGroupMessagesKey = 'GroupMessages';
  (** An INI Key for the auto scroll messages **)
  strAutoScrollMessagesKey = 'AutoScrollMessages';
  (** An INI Key for the clear messages **)
  strClearMessagesKey = 'ClearMessages';
  (** An INI Key for the colours **)
  strColourKey = 'Colour';
  (** An INI Key for the font styles **)
  strStyleKey = 'Style';
  (** An INI Key for the switch to message **)
  strSwitchToMessagesKey = 'SwitchToMessages';
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
  (** An INI Key for the Excluded zip files **)
  strExclusionFilesKey = 'ExclusionFiles';
  (** An INI Key for the Enabled version info **)
  strEnabledVersionInfoKey = 'EnabledVersionInfo';
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
  (** An INI Key for the excluded resource file extensions **)
  strExcludedResExtsKey = 'ExcludedResExts';
  (** An INI Key for the resource name **)
  strResourceNameKey = 'ResourceName';
  (** An INI Key for the zip filename **)
  strZipFileKey = 'ZipFile';
  (** An INI Key for the warn before compile **)
  strWarnBeforeKey = 'WarnBefore';
  (** An INI Key for the warn after compile **)
  strWarnAfterKey = 'WarnAfter';
  (** An INI Key for the File version **)
  strFileVersionKey = 'FileVersion';
  (** An INI Key for the ITHelper extension **)
  strITHelperExt = '.ithelper';

{ TGlobalOptions }

(**

  A constructor for the TGlobalOptions class.

  @precon  None.
  @postcon Loads the global settings from an INI file.

**)
Constructor TITHGlobalOptions.Create;

Begin
  FINIFileName     := BuildRootKey;
  FProjectGroupOps := TStringList.Create;
  LoadSettings;
End;

(**

  A destructor for the TGlobalOptions class.

  @precon  None.
  @postcon Saves the global settings to an INI file.

**)
Destructor TITHGlobalOptions.Destroy;

Begin
  SaveSettings;
  FProjectGroupOps.Free;
  Inherited Destroy;
End;

(**

  This method displays an exception message on the screen.

  @precon  None.
  @postcon Displays an exception message on the screen.

  @param   strExceptionMsg as a String as a constant

**)
Procedure TITHGlobalOptions.ExceptionsMgs(Const strExceptionMsg: String);

Begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
End;

(**

  This is a getter method for the FontColour property.

  @precon  None.
  @postcon Returns the enumerated font colour.

  @param   iFont as a TITHFonts as a constant
  @return  a TColor

**)
Function TITHGlobalOptions.GetFontColour(Const iFont: TITHFonts): TColor;

Begin
  Result := FFontColour[iFont];
End;

(**

  This is a getter method for the FontName property.

  @precon  None.
  @postcon Returns the enumerated font name.

  @param   iFont as a TITHFontNames as a constant
  @return  a String

**)
Function TITHGlobalOptions.GetFontName(Const iFont: TITHFontNames): String;

Begin
  Result := FFontName[iFont];
End;

(**

  This is a getter method for the FontStyles property.

  @precon  None.
  @postcon Returns the enumerated font styles.

  @param   iFont as a TITHFonts as a constant
  @return  a TFontStyles

**)
Function TITHGlobalOptions.GetFontStyles(Const iFont: TITHFonts): TFontStyles;

Begin
  Result := FFontStyle[iFont];
End;

(**

  This is a getter method for the ProjectGroupOps property.

  @precon  None.
  @postcon Returns the projectc group options.

  @return  a TITHEnabledOptions

**)
Function TITHGlobalOptions.GetProjectGroupOps: TITHEnabledOptions;

Const
  strSetupIniSubSection = '%s.Setup';
  strEnabledOptionsKey = 'EnabledOptions';

Var
  PG             : IOTAProjectGroup;
  strProjectGroup: String;
  INIFile        : TMemIniFile;
  strSection     : String;
  Ops            : TITHEnabledOptions;
  iIndex         : Integer;
  boolNeedSave   : Boolean;

Begin
  Result := [];
  PG     := ProjectGroup;
  If PG = Nil Then
    Exit;
  strProjectGroup := ExtractFileName(PG.FileName);
  iIndex          := FProjectGroupOps.IndexOf(strProjectGroup);
  If iIndex = -1 Then
    Begin
      // Migrate old settings
      INIFile := TMemIniFile.Create(FINIFileName);
      Try
        boolNeedSave := False;
        strSection := Format(strSetupIniSubSection, [strProjectGroup]);
        If INIFile.ValueExists(strSection, strEnabledKey) Then
          Begin
            If INIFile.ReadBool(strSection, strEnabledKey, True) Then
              Include(Result, eoGroupEnabled);
            INIFile.DeleteKey(strSection, strEnabledKey);
            boolNeedSave := True;
          End Else
            Include(Result, eoGroupEnabled);
        If INIFile.ValueExists(strSection, strEnabledOptionsKey) Then
          Begin
            Ops    := [eoBefore, eoAfter, eoZip];
            Result := Result + TITHEnabledOptions(Byte(INIFile.ReadInteger(strSection,
              strEnabledOptionsKey, Byte(Ops))));
            FProjectGroupOps.AddObject(strProjectGroup, TObject(Byte(Result)));
            INIFile.DeleteKey(strSection, strEnabledOptionsKey);
            INIFile.EraseSection(strSection);
            boolNeedSave := True;
          End Else
          Begin
            Include(Result, eoBefore);
            Include(Result, eoAfter);
            Include(Result, eoZip);
          End;
        Include(Result, eoBuildVersionResource);
        Include(Result, eoCopyVersionInfo);
        Include(Result, eoIncrementBuild);
        If boolNeedSave Then
          INIFile.UpdateFile;
      Finally
        INIFile.Free;
      End;
    End
  Else
    Result := TITHEnabledOptions(Byte(FProjectGroupOps.Objects[iIndex]));
End;

(**

  This method loads the applications global settings from the main INI file.

  @precon  None.
  @postcon Loads the applications global settings from the main INI file.

**)
Procedure TITHGlobalOptions.LoadSettings;

Const
  strDefaultZipEXE = 'C:\Program Files\7-Zip\7Z.EXE';
  iDefaultClearMsgInterval = 30;
  strDefaultZIPParamsKey = '-ouexrPyb @"$RESPONSEFILE$" "$ZIPFILE$"';
  strDefaultFontName = 'Tahoma';
  strDefaultFontColour = 'clBlack';

Var
  iFont    : TITHFonts;
  iFontName: TITHFontNames;
  sl       : TStringList;
  i        : Integer;
  Ops      : TITHEnabledOptions;
  A        : TAction;
  iIndex : Integer;
  boolNeedsSaving : Boolean;
  iniFile : TMemIniFile;

Begin
  boolNeedsSaving := False;
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    FZipEXE := iniFile.ReadString(strSetupSection, strZIPEXEKey, strDefaultZipEXE);
    FZipParameters := iniFile.ReadString(strSetupSection, strZIPParametersKey, strDefaultZIPParamsKey);
    FGroupMessages         := iniFile.ReadBool(strSetupSection, strGroupMessagesKey, False);
    FAutoScrollMessages    := iniFile.ReadBool(strSetupSection, strAutoScrollMessagesKey, True);
    FClearMessages         := iniFile.ReadInteger(strSetupSection, strClearMessagesKey,
      iDefaultClearMsgInterval);
    For iFontName          := Low(TITHFontNames) To High(TITHFontNames) Do
      FFontName[iFontName] := iniFile.ReadString(strMessagesSection, strFontNameINIPrefixes[iFontName],
        strDefaultFontName);
    For iFont := Low(TITHFonts) To High(TITHFonts) Do
      Begin
        FFontColour[iFont] := StringToColor(iniFile.ReadString(strMessagesSection,
          strFontINIPrefixes[iFont] + strColourKey, strDefaultFontColour));
        FFontStyle[iFont] := TFontStyles(Byte(iniFile.ReadInteger(strMessagesSection,
          strFontINIPrefixes[iFont] + strStyleKey, 0)));
      End;
    FSwitchToMessages := iniFile.ReadBool(strMessagesSection, strSwitchToMessagesKey, True);
    // Project Group Options
    sl                := TStringList.Create;
    Try
      iniFile.ReadSection(strNewProjectGroupOptionsSection, sl);
      For i := 0 To sl.Count - 1 Do
        Begin
          Ops   := [eoAfter..eoIncrementBuild];
          Ops := TITHEnabledOptions(Byte(iniFile.ReadInteger(strNewProjectGroupOptionsSection, sl[i],
            Byte(Ops))));
          FProjectGroupOps.AddObject(sl[i], TObject(Byte(Ops)));
        End;
      iniFile.ReadSection(strProjectGroupOptionsSection, sl);
      For i := 0 To sl.Count - 1 Do
        Begin
          Ops   := [eoBefore .. eoGroupEnabled];
          Ops := TITHEnabledOptions(Byte(iniFile.ReadInteger(strProjectGroupOptionsSection, sl[i],
            Byte(Ops))));
          Ops := Ops + [eoBuildVersionResource, eoCopyVersionInfo, eoIncrementBuild];
          iIndex := FProjectGroupOps.IndexOf(sl[i]);
          If iIndex = -1 Then
            FProjectGroupOps.AddObject(sl[i], TObject(Byte(Ops)))
          Else
            FProjectGroupOps.Objects[iIndex] := TObject(Byte(Ops));
          iniFile.DeleteKey(strProjectGroupOptionsSection, sl[i]);
          boolNeedsSaving := True;
        End;
    Finally
      sl.Free;
    End;
    // Action Shortcuts
    For i := 0 To Actions.Count - 1 Do
      If Actions[i] Is TAction Then
        Begin
          A          := Actions[i] As TAction;
          A.ShortCut := iniFile.ReadInteger(strShortcutsSection, A.Name, A.ShortCut);
        End;
    If boolNeedsSaving Then
      iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This method returns an ini file for the local project settings. If this file does not exist any 
  settings from the main ini files are migrated to a new local project settings file.

  @precon  Project must be a valid instance.
  @postcon Returns an ini file for the local project settings. If this file does not exist any settings 
           from the main ini files are migrated to a new local project settings file. Reference 
           returned needs to be freed by the caller.

  @param   Project as an IOTAProject as a constant
  @return  a TITHProjectOptions

**)
Function TITHGlobalOptions.ProjectOptions(Const Project: IOTAProject): TITHProjectOptions;

Const
  strSections: Array [1 .. 5] Of String = (strSetupSection, 'Pre-Compilation', 'Post-Compilation',
    'Zipping', 'Additional Zip Files');
Var
  strINIFileName     : String;
  iniMain, iniProject: TMemIniFile;
  strProjectName     : String;
  iSection           : Integer;
  strSection         : String;
  sl                 : TStringList;
  i                  : Integer;

Begin
  strINIFileName := ChangeFileExt(Project.FileName, strITHelperExt);
  strProjectName := GetProjectName(Project);
  If Not FileExists(strINIFileName) Then
    Begin
      // Migrate settings from the main INI file to a local one
      iniMain := TMemIniFile.Create(INIFileName);
      Try
        iniProject := TMemIniFile.Create(strINIFileName);
        Try
          For iSection := Low(strSections) To High(strSections) Do
            Begin
              strSection := Format('%s.%s', [strProjectName, strSections[iSection]]);
              If iniMain.SectionExists(strSection) Then
                Begin
                  sl := TStringList.Create;
                  Try
                    iniMain.ReadSection(strSection, sl);
                    For i := 0 To sl.Count - 1 Do
                      iniProject.WriteString(strSections[iSection], sl[i],
                        iniMain.ReadString(strSection, sl[i], ''));
                  Finally
                    sl.Free;
                  End;
                  iniMain.EraseSection(strSection);
                End;
            End;
          iniProject.UpdateFile;
        Finally
          iniProject.Free;
        End;
        iniMain.UpdateFile;
      Finally
        iniMain.Free;
      End;
    End;
  Result := TITHProjectOptions.Create(strINIFileName, Project);
End;

(**

  A public method of the class to allow callers to save the global settings.

  @precon  None.
  @postcon Saves the global settings.

**)
Procedure TITHGlobalOptions.Save;

Begin
  SaveSettings;
End;

(**

  This method saves the applications global settings to the main INI file.

  @precon  None.
  @postcon Saves the applications global settings to the main INI file.

**)
Procedure TITHGlobalOptions.SaveSettings;

Var
  iFont    : TITHFonts;
  iFontName: TITHFontNames;
  i        : Integer;
  A        : TAction;
  iniFile  : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
    Try
      iniFile.WriteString(strSetupSection, strZIPEXEKey, FZipEXE);
      iniFile.WriteString(strSetupSection, strZIPParametersKey, FZipParameters);
      iniFile.WriteBool(strSetupSection, strGroupMessagesKey, FGroupMessages);
      iniFile.WriteBool(strSetupSection, strAutoScrollMessagesKey, FAutoScrollMessages);
      iniFile.WriteInteger(strSetupSection, strClearMessagesKey, FClearMessages);
      For iFontName := Low(TITHFontNames) To High(TITHFontNames) Do
        iniFile.WriteString(strMessagesSection, strFontNameINIPrefixes[iFontName], FFontName[iFontName]);
      For iFont := Low(TITHFonts) To High(TITHFonts) Do
        Begin
          iniFile.WriteString(strMessagesSection, strFontINIPrefixes[iFont] + strColourKey,
            ColorToString(FFontColour[iFont]));
          iniFile.WriteInteger(strMessagesSection, strFontINIPrefixes[iFont] + strStyleKey,
            Byte(FFontStyle[iFont]));
        End;
      iniFile.WriteBool(strMessagesSection, strSwitchToMessagesKey, FSwitchToMessages);
      iniFile.EraseSection(strNewProjectGroupOptionsSection);
      For i := 0 To FProjectGroupOps.Count - 1 Do
        iniFile.WriteInteger(strNewProjectGroupOptionsSection, FProjectGroupOps[i],
          Integer(FProjectGroupOps.Objects[i]));
      For i := 0 To Actions.Count - 1 Do
        If Actions[i] Is TAction Then
          Begin
            A := Actions[i] As TAction;
            iniFile.WriteInteger(strSetupSection, A.Name, A.ShortCut);
          End;
      iniFile.UpdateFile;
    Finally
      iniFile.Free;
    End;
End;

(**

  This is a setter method for the FontColour property.

  @precon  None.
  @postcon Sets the font colour of the enumerated font.

  @param   iFont  as a TITHFonts as a constant
  @param   iValue as a TColor as a constant

**)
Procedure TITHGlobalOptions.SetFontColour(Const iFont: TITHFonts; Const iValue: TColor);

Begin
  FFontColour[iFont] := iValue;
End;

(**

  This is a setter method for the FontName property.

  @precon  None.
  @postcon Sets the font name of the enumerated font.

  @param   iFont    as a TITHFontNames as a constant
  @param   strValue as a String as a constant

**)
Procedure TITHGlobalOptions.SetFontName(Const iFont: TITHFontNames; Const strValue: String);

Begin
  FFontName[iFont] := strValue;
End;

(**

  This is a setter method for the FontStyles property.

  @precon  None.
  @postcon Sets the font styles of the enumerated font.

  @param   iFont  as a TITHFonts as a constant
  @param   iValue as a TFontStyles as a constant

**)
Procedure TITHGlobalOptions.SetFontStyles(Const iFont: TITHFonts; Const iValue: TFontStyles);

Begin
  FFontStyle[iFont] := iValue;
End;

(**

  This is a setter method for the ProjectGroupOps property.

  @precon  None.
  @postcon Sets the project group options.

  @param   Ops as a TITHEnabledOptions as a constant

**)
Procedure TITHGlobalOptions.SetProjectGroupOps(Const Ops: TITHEnabledOptions);

Var
  PG             : IOTAProjectGroup;
  strProjectGroup: String;
  iIndex         : Integer;

Begin
  PG := ProjectGroup;
  If PG = Nil Then
    Exit;
  strProjectGroup := ExtractFileName(PG.FileName);
  iIndex          := FProjectGroupOps.IndexOf(strProjectGroup);
  If iIndex = -1 Then
    FProjectGroupOps.AddObject(strProjectGroup, TObject(Byte(Ops)))
  Else
    FProjectGroupOps.Objects[iIndex] := TObject(Byte(Ops));
End;

{ TProjectOptions }

(**

  A constructor for the TProjectOptions class.

  @precon  Project must be a valid instance.
  @postcon Creates the class and loads the projects INI file.

  @param   strINIFileName as a String as a constant
  @param   Project        as an IOTAProject as a constant

**)
Constructor TITHProjectOptions.Create(Const strINIFileName: String; Const Project: IOTAProject);

Begin
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
    Begin
      FVerInfo.Text := Format(strVerInfo, [Major, Minor, Release, Build, Major, Minor]);
    End
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
    Format('%d.%d.%d.%d', [Major, Minor, Release, Build]));
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
