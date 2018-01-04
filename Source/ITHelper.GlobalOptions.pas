(**

  This module contains a class to manage the global options of the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    04 Jan 2018

**)
Unit ITHelper.GlobalOptions;

Interface

Uses
  Graphics,
  Classes,
  IniFiles,
  ToolsAPI, 
  ITHelper.Interfaces, 
  ITHelper.Types;

Type
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
    Function ProjectOptions(Const Project: IOTAProject): IITHProjectOptions;
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
  Contnrs, 
  ITHelper.ProjectOptions;

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
  (** An INI Key for the Enalbed **)
  strEnabledKey = 'Enabled';
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
  @return  an IITHProjectOptions

**)
Function TITHGlobalOptions.ProjectOptions(Const Project: IOTAProject): IITHProjectOptions;

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

End.
