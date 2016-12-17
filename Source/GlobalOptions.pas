(**

  This module contains a class to manage the global options of the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    11 Jul 2012

**)
Unit GlobalOptions;

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
  TEnabledOption = (
    eoBefore,
    eoAfter,
    eoZip,
    eoGroupEnabled,
    eoCopyVersionInfo,
    eoBuildVersionResource,
    eoIncrementBuild
  );
  (** A set of enumerates. **)
  TEnabledOptions = Set Of TEnabledOption;

  (** A class to provide access to the projects options in the INI file. **)
  TProjectOptions = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FProject    : IOTAProject;
    FINIFile    : TMemIniFile;
    FModified   : Boolean;
    FVerInfo    : TStringList;
    FAddZipFiles: TStringList;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetResExtExc: String;
    Function GetIncOnCompile: Boolean;
    Function GetCopyVerInfo: String;
    Function GetIncITHVerInfo: Boolean;
    Function GetMajor: Integer;
    Function GetMinor: Integer;
    Function GetRelease: Integer;
    Function GetBuild: Integer;
    Function GetIncResInProj: Boolean;
    Function GetCompileRes: Boolean;
    Function GetResourceName: String;
    Function GetWarnBefore: Boolean;
    Function GetWarnAfter: Boolean;
    Function GetVerInfo: TStringList;
    Function GetEnableZipping: Boolean;
    Function GetZipName: String;
    Function GetBasePath: String;
    Function GetExcPatterns: String;
    Function GetAddZipFiles: TStringList;
    Procedure SetResExtExc(strValue: String);
    Procedure SetIncOnCompile(boolValue: Boolean);
    Procedure SetCopyVerInfo(strValue: String);
    Procedure SetIncITHVerInfo(boolValue: Boolean);
    Procedure SetMajor(iValue: Integer);
    Procedure SetMinor(iValue: Integer);
    Procedure SetRelease(iValue: Integer);
    Procedure SetBuild(iValue: Integer);
    Procedure SetIncResInProj(boolValue: Boolean);
    Procedure SetCompileRes(boolValue: Boolean);
    Procedure SetResourceName(strValue: String);
    Procedure SetWarnBefore(boolValue: Boolean);
    Procedure SetWarnAfter(boolValue: Boolean);
    Procedure SetEnableZipping(boolValue: Boolean);
    Procedure SetZipName(strValue: String);
    Procedure SetBasePath(strValue: String);
    Procedure SetExcPatterns(strValue: String);
    Procedure UpdateVerInfo(Sender: TObject);
    Procedure UpdateAddZipFiles(Sender: TObject);
    Procedure UpdateFileVersion;
  Public
    Constructor Create(strINIFileName: String; Project: IOTAProject);
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
  TGlobalOptions = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
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
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Function GetFontName(iFont: TITHFontNames): String;
    Procedure SetFontName(iFont: TITHFontNames; strValue: String);
    Function GetFontColour(iFont: TITHFonts): TColor;
    Procedure SetFontColour(iFont: TITHFonts; iValue: TColor);
    Function GetFontStyles(iFont: TITHFonts): TFontStyles;
    Procedure SetFontStyles(iFont: TITHFonts; iValue: TFontStyles);
    Function GetProjectGroupOps: TEnabledOptions;
    Procedure SetProjectGroupOps(Ops: TEnabledOptions);
    Procedure ExceptionsMgs(strExceptionMsg: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Save;
    Function ProjectOptions(Project: IOTAProject): TProjectOptions;
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
      @param   iFont as a TITHFontNames
      @return  a String
    **)
    Property FontName[iFont: TITHFontNames]: String Read GetFontName Write SetFontName;
    (**
      A property that determines the font colour for the specific enumeration.
      @precon  None.
      @postcon Returns the font colour for the specific enumeration.
      @param   iFont as a TITHFonts
      @return  a TColor
    **)
    Property FontColour[iFont: TITHFonts]: TColor Read GetFontColour Write SetFontColour;
    (**
      A property that determines the font styles for the specific enumeration.
      @precon  None.
      @postcon Returns the font styles for the specific enumeration.
      @param   iFont as a TITHFonts
      @return  a TFontStyles
    **)
    Property FontStyles[iFont: TITHFonts]: TFontStyles Read GetFontStyles
      Write SetFontStyles;
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
      @return  a TEnabledOptions
    **)
    Property ProjectGroupOps: TEnabledOptions Read GetProjectGroupOps
      Write SetProjectGroupOps;
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
    Property AutoScrollMessages: Boolean Read FAutoScrollMessages
      Write FAutoScrollMessages;
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
  DGHLibrary,
  Dialogs,
  TestingHelperUtils,
  SysUtils,
  ActnList,
  Windows;

Const
  (** A constant array of INI name prefixes for font information. **)
  strFontINIPrefixes: Array [Low(TITHFonts) .. High(TITHFonts)
    ] Of String = ('MsgHeaderFont', 'MsgDefault', 'MsgSuccess', 'MsgFailure',
    'MsgWarning');
  (** A constant array of INI font name prefixes for font information. **)
  strFontNameINIPrefixes: Array [Low(TITHFontNames) .. High(TITHFontNames)
    ] Of String = ('HeaderFontName', 'ToolFontName');

{ TGlobalOptions }

(**

  A constructor for the TGlobalOptions class.

  @precon  None.
  @postcon Loads the global settings from an INI file.

**)
Constructor TGlobalOptions.Create;

Begin
  FINIFileName     := BuildRootKey(Nil, ExceptionsMgs);
  FProjectGroupOps := TStringList.Create;
  LoadSettings;
End;

(**

  A destructor for the TGlobalOptions class.

  @precon  None.
  @postcon Saves the global settings to an INI file.

**)
Destructor TGlobalOptions.Destroy;

Begin
  SaveSettings;
  FProjectGroupOps.Free;
  Inherited Destroy;
End;

(**

  This method displays an exception message on the screen.

  @precon  None.
  @postcon Displays an exception message on the screen.

  @param   strExceptionMsg as a String

**)
Procedure TGlobalOptions.ExceptionsMgs(strExceptionMsg: String);
Begin
  MessageDlg(strExceptionMsg, mtError, [mbOK], 0);
End;

(**

  This is a getter method for the FontColour property.

  @precon  None.
  @postcon Returns the enumerated font colour.

  @param   iFont as a TITHFonts
  @return  a TColor

**)
Function TGlobalOptions.GetFontColour(iFont: TITHFonts): TColor;
Begin
  Result := FFontColour[iFont];
End;

(**

  This is a getter method for the FontName property.

  @precon  None.
  @postcon Returns the enumerated font name.

  @param   iFont as a TITHFontNames
  @return  a String

**)
Function TGlobalOptions.GetFontName(iFont: TITHFontNames): String;
Begin
  Result := FFontName[iFont];
End;

(**

  This is a getter method for the FontStyles property.

  @precon  None.
  @postcon Returns the enumerated font styles.

  @param   iFont as a TITHFonts
  @return  a TFontStyles

**)
Function TGlobalOptions.GetFontStyles(iFont: TITHFonts): TFontStyles;
Begin
  Result := FFontStyle[iFont];
End;

(**

  This is a getter method for the ProjectGroupOps property.

  @precon  None.
  @postcon Returns the projectc group options.

  @return  a TEnabledOptions

**)
Function TGlobalOptions.GetProjectGroupOps: TEnabledOptions;

Var
  PG             : IOTAProjectGroup;
  strProjectGroup: String;
  INIFile        : TMemIniFile;
  strSection     : String;
  Ops            : TEnabledOptions;
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
        strSection := Format('%s.Setup', [strProjectGroup]);
        If INIFile.ValueExists(strSection, 'Enabled') Then
          Begin
            If INIFile.ReadBool(strSection, 'Enabled', True) Then
              Include(Result, eoGroupEnabled);
            INIFile.DeleteKey(strSection, 'Enabled');
            boolNeedSave := True;
          End Else
            Include(Result, eoGroupEnabled);
        If INIFile.ValueExists(strSection, 'EnabledOptions') Then
          Begin
            Ops    := [eoBefore, eoAfter, eoZip];
            Result := Result + TEnabledOptions
              (Byte(INIFile.ReadInteger(strSection, 'EnabledOptions', Byte(Ops))));
            FProjectGroupOps.AddObject(strProjectGroup, TObject(Byte(Result)));
            INIFile.DeleteKey(strSection, 'EnabledOptions');
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
    Result := TEnabledOptions(Byte(FProjectGroupOps.Objects[iIndex]));
End;

(**

  This method loads the applications global settings from the main INI file.

  @precon  None.
  @postcon Loads the applications global settings from the main INI file.

**)
Procedure TGlobalOptions.LoadSettings;

Var
  iFont    : TITHFonts;
  iFontName: TITHFontNames;
  sl       : TStringList;
  i        : Integer;
  Ops      : TEnabledOptions;
  A        : TAction;
  iIndex : Integer;
  boolNeedsSaving : Boolean;

Begin
  boolNeedsSaving := False;
  With TMemIniFile.Create(FINIFileName) Do
    Try
      FZipEXE := ReadString('Setup', 'ZIPEXE', 'C:\Program Files\WinZip\WZZip.EXE');
      FZipParameters := ReadString('Setup', 'ZIPParameters',
        '-ouexrPyb @"$RESPONSEFILE$" "$ZIPFILE$"');
      FGroupMessages         := ReadBool('Setup', 'GroupMessages', False);
      FAutoScrollMessages    := ReadBool('Setup', 'AutoScrollMessages', True);
      FClearMessages         := ReadInteger('Setup', 'ClearMessages', 30);
      For iFontName          := Low(TITHFontNames) To High(TITHFontNames) Do
        FFontName[iFontName] := ReadString('Messages', strFontNameINIPrefixes[iFontName],
          'Tahoma');
      For iFont := Low(TITHFonts) To High(TITHFonts) Do
        Begin
          FFontColour[iFont] :=
            StringToColor(ReadString('Messages', strFontINIPrefixes[iFont] + 'Colour',
              'clBlack'));
          FFontStyle[iFont] :=
            TFontStyles(Byte(ReadInteger('Messages', strFontINIPrefixes[iFont] +
                  'Style', 0)));
        End;
      FSwitchToMessages := ReadBool('Messages', 'SwitchToMessages', True);
      // Project Group Options
      sl                := TStringList.Create;
      Try
        ReadSection('NewProjectGroupOptions', sl);
        For i := 0 To sl.Count - 1 Do
          Begin
            Ops   := [eoAfter..eoIncrementBuild];
            Ops := TEnabledOptions(Byte(ReadInteger('NewProjectGroupOptions', sl[i],
              Byte(Ops))));
            FProjectGroupOps.AddObject(sl[i], TObject(Byte(Ops)));
          End;
        ReadSection('ProjectGroupOptions', sl);
        For i := 0 To sl.Count - 1 Do
          Begin
            Ops   := [eoBefore .. eoGroupEnabled];
            Ops := TEnabledOptions(Byte(ReadInteger('ProjectGroupOptions', sl[i], Byte(Ops))));
            Ops := Ops + [eoBuildVersionResource, eoCopyVersionInfo, eoIncrementBuild];
            iIndex := FProjectGroupOps.IndexOf(sl[i]);
            If iIndex = -1 Then
              FProjectGroupOps.AddObject(sl[i], TObject(Byte(Ops)))
            Else
              FProjectGroupOps.Objects[iIndex] := TObject(Byte(Ops));
            DeleteKey('ProjectGroupOptions', sl[i]);
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
            A.ShortCut := ReadInteger('Shortcuts', A.Name, A.ShortCut);
          End;
      If boolNeedsSaving Then
        UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This method returns an ini file for the local project settings. If this file does not
  exist any settings from the main ini files are migrated to a new local project settings
  file.

  @precon  Project must be a valid instance.
  @postcon Returns an ini file for the local project settings. If this file does not
           exist any settings from the main ini files are migrated to a new local
           project settings file. Reference returned needs to be freed by the caller.

  @param   Project as an IOTAProject
  @return  a TProjectOptions

**)
Function TGlobalOptions.ProjectOptions(Project: IOTAProject): TProjectOptions;

Const
  strSections: Array [1 .. 5] Of String = ('Setup', 'Pre-Compilation', 'Post-Compilation',
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
  strINIFileName := ChangeFileExt(Project.FileName, '.ithelper');
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
  Result := TProjectOptions.Create(strINIFileName, Project);
End;

(**

  A public method of the class to allow callers to save the global settings.

  @precon  None.
  @postcon Saves the global settings.

**)
Procedure TGlobalOptions.Save;

Begin
  SaveSettings;
End;

(**

  This method saves the applications global settings to the main INI file.

  @precon  None.
  @postcon Saves the applications global settings to the main INI file.

**)
Procedure TGlobalOptions.SaveSettings;

Var
  iFont    : TITHFonts;
  iFontName: TITHFontNames;
  i        : Integer;
  A        : TAction;

Begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      WriteString('Setup', 'ZIPEXE', FZipEXE);
      WriteString('Setup', 'ZIPParameters', FZipParameters);
      WriteBool('Setup', 'GroupMessages', FGroupMessages);
      WriteBool('Setup', 'AutoScrollMessages', FAutoScrollMessages);
      WriteInteger('Setup', 'ClearMessages', FClearMessages);
      For iFontName := Low(TITHFontNames) To High(TITHFontNames) Do
        WriteString('Messages', strFontNameINIPrefixes[iFontName], FFontName[iFontName]);
      For iFont := Low(TITHFonts) To High(TITHFonts) Do
        Begin
          WriteString('Messages', strFontINIPrefixes[iFont] + 'Colour',
            ColorToString(FFontColour[iFont]));
          WriteInteger('Messages', strFontINIPrefixes[iFont] + 'Style',
            Byte(FFontStyle[iFont]));
        End;
      WriteBool('Messages', 'SwitchToMessages', FSwitchToMessages);
      EraseSection('NewProjectGroupOptions');
      For i := 0 To FProjectGroupOps.Count - 1 Do
        WriteInteger('NewProjectGroupOptions', FProjectGroupOps[i],
          Integer(FProjectGroupOps.Objects[i]));
      For i := 0 To Actions.Count - 1 Do
        If Actions[i] Is TAction Then
          Begin
            A := Actions[i] As TAction;
            WriteInteger('Shortcuts', A.Name, A.ShortCut);
          End;
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This is a setter method for the FontColour property.

  @precon  None.
  @postcon Sets the font colour of the enumerated font.

  @param   iFont  as a TITHFonts
  @param   iValue as a TColor

**)
Procedure TGlobalOptions.SetFontColour(iFont: TITHFonts; iValue: TColor);
Begin
  FFontColour[iFont] := iValue;
End;

(**

  This is a setter method for the FontName property.

  @precon  None.
  @postcon Sets the font name of the enumerated font.

  @param   iFont    as a TITHFontNames
  @param   strValue as a String

**)
Procedure TGlobalOptions.SetFontName(iFont: TITHFontNames; strValue: String);
Begin
  FFontName[iFont] := strValue;
End;

(**

  This is a setter method for the FontStyles property.

  @precon  None.
  @postcon Sets the font styles of the enumerated font.

  @param   iFont  as a TITHFonts
  @param   iValue as a TFontStyles

**)
Procedure TGlobalOptions.SetFontStyles(iFont: TITHFonts; iValue: TFontStyles);
Begin
  FFontStyle[iFont] := iValue;
End;

(**

  This is a setter method for the ProjectGroupOps property.

  @precon  None.
  @postcon Sets the project group options.

  @param   Ops as a TEnabledOptions

**)
Procedure TGlobalOptions.SetProjectGroupOps(Ops: TEnabledOptions);

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

  @param   strINIFileName as a String
  @param   Project        as an IOTAProject

**)
Constructor TProjectOptions.Create(strINIFileName: String; Project: IOTAProject);

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
Destructor TProjectOptions.Destroy;

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
Function TProjectOptions.GetAddZipFiles: TStringList;

Var
  sl     : TStringList;
  i      : Integer;
  strLine: String;

Begin
  Result := FAddZipFiles;
  sl     := TStringList.Create;
  Try
    FINIFile.ReadSection('Additional Zip Files', sl);
    FAddZipFiles.OnChange := Nil;
    Try
      FAddZipFiles.Clear;
    Finally
      FAddZipFiles.OnChange := UpdateAddZipFiles;
    End;
    For i := 0 To sl.Count - 1 Do
      Begin
        strLine := FINIFile.ReadString('Additional Zip Files', sl[i], '');
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
Function TProjectOptions.GetBasePath: String;

Begin
  Result := FINIFile.ReadString('Zipping', 'BasePath',
    ExtractFilePath(FProject.FileName));
End;

(**

  This is a getter method for the Build property.

  @precon  None.
  @postcon returns the build number from the project options version information.

  @return  an Integer

**)
Function TProjectOptions.GetBuild: Integer;

Begin
  Result := FINIFile.ReadInteger('Setup', 'BuildVer', 0);
End;

(**

  This is a getter method for the CompileRes property.

  @precon  None.
  @postcon Returns whether the project version resource should be pre-compiled.

  @return  a Boolean

**)
Function TProjectOptions.GetCompileRes: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'CompileWithBRCC32', False);
End;

(**

  This is a getter method for the CopyVerInfo property.

  @precon  None.
  @postcon Returns the path and filename of an executable from which to coppy the version
           informaton: Major, Minor, Release and Build.

  @return  a String

**)
Function TProjectOptions.GetCopyVerInfo: String;

Begin
  Result := FINIFile.ReadString('Setup', 'CopyVersionInfoFrom', '');
End;

(**

  This is a getter method for the EnableZipping property.

  @precon  None.
  @postcon Returns whether zipping of the projects files.

  @return  a Boolean

**)
Function TProjectOptions.GetEnableZipping: Boolean;

Begin
  Result := FINIFile.ReadBool('Zipping', 'Enabled', False);
End;

(**

  This is a getter method for the ExcPatterns property.

  @precon  None.
  @postcon Returns a string of patterns to be excluded from the zipping processs.

  @return  a String

**)
Function TProjectOptions.GetExcPatterns: String;

Begin
  Result := StringReplace(FINIFile.ReadString('Zipping', 'ExclusionFiles', ''), '|',
    #13#10, [rfReplaceAll]);
End;

(**

  This is a getter method for the IncITHVerInfo property.

  @precon  None.
  @postcon Returns whether ITHelpers version information should be included in the project
           executable.

  @return  a Boolean

**)
Function TProjectOptions.GetIncITHVerInfo: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'EnabledVersionInfo', False);
End;

(**

  This is a getter method for the IncOnCompile property.

  @precon  None.
  @postcon Returns whether the build number should be incemented on a successful
           compilation.

  @return  a Boolean

**)
Function TProjectOptions.GetIncOnCompile: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'IncBuild', False);
End;

(**

  This is a getter method for the IncResInProj property.

  @precon  None.
  @postcon Returns whether the version resource should be included in the project.

  @return  a Boolean

**)
Function TProjectOptions.GetIncResInProj: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'IncludeInProject', False);
End;

(**

  This is a getter method for the Major property.

  @precon  None.
  @postcon Returns the major version number of the project.

  @return  an Integer

**)
Function TProjectOptions.GetMajor: Integer;

Begin
  Result := FINIFile.ReadInteger('Setup', 'MajorVer', 1);
End;

(**

  This is a getter method for the Minor property.

  @precon  None.
  @postcon Returns the minor version number of the project.

  @return  an Integer

**)
Function TProjectOptions.GetMinor: Integer;

Begin
  Result := FINIFile.ReadInteger('Setup', 'MinorVer', 0);
End;

(**

  This is a getter method for the Release property.

  @precon  None.
  @postcon Returns the release version number of the project.

  @return  an Integer

**)
Function TProjectOptions.GetRelease: Integer;

Begin
  Result := FINIFile.ReadInteger('Setup', 'ReleaseVer', 0);
End;

(**

  This is a getter method for the ResExtExc property.

  @precon  None.
  @postcon Returns a semi-colon sepapated list of file extensions to be skipped when
           checking for resources that are not included in the project.

  @return  a String

**)
Function TProjectOptions.GetResExtExc: String;

Begin
  Result := FINIFile.ReadString('Setup', 'ExcludedResExts', '.dcr');
End;

(**

  This is a getter method for the ResourceName property.

  @precon  None.
  @postcon Returns the name of the resource which will contains the version information.

  @return  a String

**)
Function TProjectOptions.GetResourceName: String;

Begin
  Result := FINIFile.ReadString('Setup', 'ResourceName', 'ITHelperVersionInfo');
End;

(**

  This is a getter method for the VerInfo property.

  @precon  None.
  @postcon Returns a string list of version information strings.

  @return  a TStringList

**)
Function TProjectOptions.GetVerInfo: TStringList;

Var
  sl: TStringList;
  i : Integer;

Begin
  Result := FVerInfo;
  If Not FINIFile.SectionExists('VersionInfo') Then
    Begin
      FVerInfo.Text := Format('CompanyName='#13#10 + 'FileDescription='#13#10 +
          'FileVersion=%d.%d.%d.%d'#13#10 + 'InternalName='#13#10 +
          'LegalCopyright='#13#10 + 'LegalTrademarks='#13#10 + 'OriginalFilename='#13#10 +
          'ProductName='#13#10 + 'ProductVersion=%d.%d'#13#10 + 'Comments=',
        [Major, Minor, Release, Build, Major, Minor]);
    End
  Else
    Begin
      sl := TStringList.Create;
      Try
        FVerInfo.OnChange := Nil;
        Try
          FVerInfo.Clear;
          FINIFile.ReadSection('VersionInfo', sl);
          For i := 0 To sl.Count - 1 Do
            FVerInfo.Add(Format('%s=%s', [sl[i], FINIFile.ReadString('VersionInfo',
                    sl[i], '')]));
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
Function TProjectOptions.GetWarnAfter: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'WarnAfter', True);
End;

(**

  This is a getter method for the WarnBefore property.

  @precon  None.
  @postcon Returns whether warns of missing tools are displayed before compilation.

  @return  a Boolean

**)
Function TProjectOptions.GetWarnBefore: Boolean;

Begin
  Result := FINIFile.ReadBool('Setup', 'WarnBefore', True);
End;

(**

  This is a getter method for the ZipName property.

  @precon  None.
  @postcon Return the name of the zip file.

  @return  a String

**)
Function TProjectOptions.GetZipName: String;

Begin
  Result := FINIFile.ReadString('Zipping', 'ZipFile', '');
End;

(**

  This is a setter method for the BasePath property.

  @precon  None.
  @postcon Sets the value of the base path for zipping files.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetBasePath(strValue: String);

Begin
  FINIFile.WriteString('Zipping', 'BasePath', strValue);
  FModified := True;
End;

(**

  This is a setter method for the Build property.

  @precon  None.
  @postcon Sets the value of the build verion information.

  @param   iValue as an Integer

**)
Procedure TProjectOptions.SetBuild(iValue: Integer);

Begin
  FINIFile.WriteInteger('Setup', 'BuildVer', iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the CompileRes property.

  @precon  None.
  @postcon Sets the value of whether a verwion resource should be pre-compiled.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetCompileRes(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'CompileWithBRCC32', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the CopyVerinfo property.

  @precon  None.
  @postcon Sets the value of the executable from which version information should be
           copied.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetCopyVerInfo(strValue: String);

Begin
  FINIFile.WriteString('Setup', 'CopyVersionInfoFrom', strValue);
  FModified := True;
End;

(**

  This is a setter method for the EnableZipping property.

  @precon  None.
  @postcon Sets the value for whether zipping of the projects files should take place.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetEnableZipping(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Zipping', 'Enabled', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the ExcPatterns property.

  @precon  None.
  @postcon Sets the value of the list of excision to the zipping process.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetExcPatterns(strValue: String);

Begin
  FINIFile.WriteString('Zipping', 'ExclusionFiles', StringReplace(strValue, #13#10, '|',
      [rfReplaceAll]));
  FModified := True;
End;

(**

  This is a setter method for the IncITHVerinfo property.

  @precon  None.
  @postcon Sets whether version information should be included in the executable.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetIncITHVerInfo(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'EnabledVersionInfo', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the IncOnCompile property.

  @precon  None.
  @postcon Sets whether the build number should be incremented on a successful
           compilation.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetIncOnCompile(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'IncBuild', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the IncResInProj property.

  @precon  None.
  @postcon Sets whether the version resource should be included in the project.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetIncResInProj(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'IncludeInProject', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the Major property.

  @precon  None.
  @postcon Sets the major version number of the projects version information.

  @param   iValue as an Integer

**)
Procedure TProjectOptions.SetMajor(iValue: Integer);

Begin
  FINIFile.WriteInteger('Setup', 'MajorVer', iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the Minor property.

  @precon  None.
  @postcon Sets the minor version number of the projects version information.

  @param   iValue as an Integer

**)
Procedure TProjectOptions.SetMinor(iValue: Integer);

Begin
  FINIFile.WriteInteger('Setup', 'MinorVer', iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the Release property.

  @precon  None.
  @postcon Sets the release version number of the projects version information.

  @param   iValue as an Integer

**)
Procedure TProjectOptions.SetRelease(iValue: Integer);

Begin
  FINIFile.WriteInteger('Setup', 'ReleaseVer', iValue);
  FModified := True;
  UpdateFileVersion;
End;

(**

  This is a setter method for the ResExtExc property.

  @precon  None.
  @postcon Sets the resource extensions to be excluded from checks.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetResExtExc(strValue: String);

Begin
  FINIFile.WriteString('Setup', 'ExcludedResExts', strValue);
  FModified := True;
End;

(**

  This is a setter method for the ResourceName property.

  @precon  None.
  @postcon Sets the resource name of the version information.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetResourceName(strValue: String);

Begin
  FINIFile.WriteString('Setup', 'ResourceName', strValue);
  FModified := True;
End;

(**

  This is a setter method for the WarnAfter property.

  @precon  None.
  @postcon Sets whether warns should be issed for not after compilation tools.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetWarnAfter(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'WarnAfter', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the WarnBefore property.

  @precon  None.
  @postcon Sets whether warns should be issed for not before compilation tools.

  @param   boolValue as a Boolean

**)
Procedure TProjectOptions.SetWarnBefore(boolValue: Boolean);

Begin
  FINIFile.WriteBool('Setup', 'WarnBefore', boolValue);
  FModified := True;
End;

(**

  This is a setter method for the ZipName property.

  @precon  None.
  @postcon Sets the filename of the zip file.

  @param   strValue as a String

**)
Procedure TProjectOptions.SetZipName(strValue: String);

Begin
  FINIFile.WriteString('Zipping', 'ZipFile', strValue);
  FModified := True;
End;

(**

  This is an on update event handler for the Additional Zip files string list.

  @precon  None.
  @postcon If the strings are updated then the updates are written back to the INI file.

  @param   Sender as a TObject

**)
Procedure TProjectOptions.UpdateAddZipFiles(Sender: TObject);

Var
  i: Integer;

Begin
  FINIFile.EraseSection('Additional Zip Files');
  For i := 0 To FAddZipFiles.Count - 1 Do
    FINIFile.WriteString('Additional Zip Files', Format('Item%d', [i]), FAddZipFiles[i]);
  FModified := True;
End;

(**

  This method updates the file version information field FileVersion when either the
  major, minor, releaase or build change.

  @precon  None.
  @postcon Updates the file version information field FileVersion when either the
           major, minor, releaase or build change.

**)
Procedure TProjectOptions.UpdateFileVersion;

Begin
  FINIFile.WriteString('VersionInfo', 'FileVersion',
    Format('%d.%d.%d.%D', [Major, Minor, Release, Build]));
  FModified := True;
End;

(**

  This is an on update event handler for the Version Information string list.

  @precon  None.
  @postcon If the strings are updated then the updates are written back to the INI file.

  @param   Sender as a TObject

**)
Procedure TProjectOptions.UpdateVerInfo(Sender: TObject);

Var
  i: Integer;

Begin
  FINIFile.EraseSection('VersionInfo');
  For i := 0 To FVerInfo.Count - 1 Do
    FINIFile.WriteString('VersionInfo', FVerInfo.Names[i], FVerInfo.ValueFromIndex[i]);
  FModified := True;
End;

End.
