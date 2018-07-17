(**
  
  This module contains interfaces for use throughout the plug-in to minimise coupling.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Jul 2018

**)
Unit ITHelper.Interfaces;

Interface

Uses
  Classes,
  IniFiles, 
  Graphics,
  ToolsAPI,
  ITHelper.Types;

Type
  (** An interface for the Project Options. **)
  IITHProjectOptions = Interface
  ['{06926CE6-2293-4D1C-91A5-2D991CC1CF04}']
  // Getters and Setters
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
    Function  GetSaveModifiedFiles : Boolean;
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
    Procedure SetSaveModifiedFiles(Const boolValue : Boolean);
  // Properties
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
    Property INIFile: TMemIniFile Read GetINIFile;
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
    (**
      This property determines of any modified files in a project should be saved before zipping.
      @precon  None.
      @postcon Gets or Sets whether any modified files in a project should be saved before zipping.
      @return  a Boolean
    **)
    Property SaveModifiedFiles : Boolean Read GetSaveModifiedFiles Write SetSaveModifiedFiles;
  End;

  (** An interface for the global options. **)
  IITHGlobalOptions = Interface
  ['{F7C87122-0669-4BE6-9434-128C73AFC169}']
  // Getter and setters methods
    Function  GetFontName(Const iFont: TITHFontNames): String;
    Procedure SetFontName(Const iFont: TITHFontNames; Const strValue: String);
    Function  GetFontColour(Const iFont: TITHFonts): TColor;
    Procedure SetFontColour(Const iFont: TITHFonts; Const iValue: TColor);
    Function  GetFontStyles(Const iFont: TITHFonts): TFontStyles;
    Procedure SetFontStyles(Const iFont: TITHFonts; Const iValue: TFontStyles);
    Function  GetProjectGroupOps: TITHEnabledOptions;
    Procedure SetProjectGroupOps(Const Ops: TITHEnabledOptions);
    Function  GetINIFileName : String;
    Function  GetSwitchToMessages : Boolean;
    Procedure SetSwitchToMessages(Const boolValue : Boolean);
    Function  GetZipEXE : String;
    Procedure SetZipEXE(Const strValue : String);
    Function  GetZipParameters : String;
    Procedure SetZipParameters(Const strValue : String);
    Function  GetGroupMessages : Boolean;
    Procedure SetGroupMessages(Const boolValue : Boolean);
    Function  GetAutoScrollMessages : Boolean;
    Procedure SetAutoScrollMessages(Const boolValue : Boolean);
    Function  GetClearMessages : Integer;
    Procedure SetClearMessages(Const iValue : Integer);
  // General Methods
    Procedure Save;
    Function ProjectOptions(Const Project: IOTAProject): IITHProjectOptions;
  // Properties
    (**
      A property to return the Main INI file name for the application.
      @precon  None.
      @postcon Return the Main INI file name for the application.
      @return  a String
    **)
    Property INIFileName: String Read GetINIFileName;
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
    Property SwitchToMessages: Boolean Read GetSwitchToMessages Write SetSwitchToMessages;
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
    Property ZipEXE: String Read GetZipEXE Write SetZipEXE;
    (**
      A property to define the parameter to be passed to the archive programme for zipping
      files.
      @precon  None.
      @postcon Returns the parameter to be passed to the archive programme for zipping
               files.
      @return  a String
    **)
    Property ZipParameters: String Read GetZipParameters Write SetZipParameters;
    (**
      A property to determine whether messages are group under headings.
      @precon  None.
      @postcon Returns whether messages are group under headings.
      @return  a Boolean
    **)
    Property GroupMessages: Boolean Read GetGroupMessages Write SetGroupMessages;
    (**
      A property to determines of new messages should be scrolled to.
      @precon  None.
      @postcon Returns whether new messages should be scrolled to.
      @return  a Boolean
    **)
    Property AutoScrollMessages: Boolean Read GetAutoScrollMessages Write SetAutoScrollMessages;
    (**
      A property to determine the number of seconds since the last compiled that should
      elapse before messages are cleared.
      @precon  None.
      @postcon Returns the number of seconds since the last compiled that should
               elapse before messages are cleared.
      @return  an Integer
    **)
    Property ClearMessages: Integer Read GetClearMessages Write SetClearMessages;
  End;

  (** This interface provide access to creating a ZIP of a projects files. **)
  IITHZipManager = Interface
  ['{595AB8AC-3D75-498D-A294-AC22307DED8B}']
    Function ZipProjectInformation : Integer;
  End;
  
  (** This interface builds and provide access to the response file. **)
  IITHResponseFile = Interface
  ['{D00061A5-FAC9-41E5-97C2-8F33734623C2}']
    Function  GetFileName : String;
    Function  BuildResponseFile(Const strBasePath, strProject, strZIPName: String) : Boolean;
    Function  FileList : String;
    (**
      This property returns the filename of the repsonse file for zipping information.
      @precon  None.
      @postcon The response filename is returned.
      @return  a String
    **)
    Property FileName : String Read GetFileName;
  End;

  {$IFNDEF CONSOLE_TESTRUNNER}
  (** This interface is for handling custom ITH messages. **)
  IITHCustomMessage = Interface(INTACustomDrawMessage)
  ['{DB89170C-FBB6-440E-B10B-FFF662187B57}']
    Procedure SetForeColour(Const iValue : TColor);
    Function  GetMessagePntr : Pointer;
    Procedure SetMessagePntr(Const ptrValue : Pointer);
    (**
      This allows the colour of the font to be updated.
      @precon  None.
      @postcon Updates the font colour of the message.
      @return  a TColor
    **)
    Property ForeColour : TColor Write SetForeColour;
    (**
      This property returns the message pointer to be used as the parent for
      sub messages.
      @precon  None.
      @postcon Returns the message pointer to be used as the parent for
      @return  a Pointer
    **)
    Property MessagePntr: Pointer Read GetMessagePntr Write SetMessagePntr;
  End;

  (** This interface is a manager for ALL ITHelper custom messages. **)
  IITHMessageManager = Interface
  ['{836EC9B2-EB70-4CD7-95D0-DD6DD9BC4B5D}']
    Function  GetCount : Integer;
    Function  GetItem(Const iIndex : Integer) : IITHCustomMessage;
    Function  GetLastMessage : Int64;
    Function  GetParentMsg : IITHCustomMessage;
    Procedure SetParentMsg(Const ParentMsg : IITHCustomMessage);
    Function  AddMsg(Const strText: String; Const eFontName : TITHFontNames;
      Const eFont : TITHFonts; Const ptrParentMsg : Pointer = Nil): IITHCustomMessage;
    Procedure Clear;
    (**
      This property returns the current number of messages in the managers collection.
      @precon  None.
      @postcon The number of message managed is returned.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      This property returns a refernecee to the indexed message.
      @precon  iIndex must be a valid index.
      @postcon The indexed message is returned from the managers collection.
      @param   iIndex as an Integer as a constant
      @return  an IITHCustomMessage
    **)
    Property Item[Const iIndex : Integer] : IITHCustomMessage Read GetItem; Default;
    (**
      This property determines the last time a message was output.
      @precon  None.
      @postcon returns the tickcount (in milliseconds) from the last message.
      @return  an Int64
    **)
    Property LastMessage : Int64 Read GetLastMessage;
    (**
      This property defines the single parent message for any tool messages.
      @precon  None.
      @postcon Gets and sets the parent message.
      @return  an IITHCustomMessage
    **)
    Property ParentMsg : IITHCustomMessage Read GetParentMsg Write SetParentMsg;
  End;
  {$ENDIF}

  (** An interface for creating and managing a version RC file for the project. **)
  IITHVersionManager = Interface
  ['{F230054C-67E1-48D7-ACDA-5BC1AC19E04B}']
    Procedure BuildProjectVersionResource();
  End;

  (** An interface to specify the methods that need to be implemented by a Project Options frame. **)
  IITHOptionsFrame = Interface
  ['{277C1FF5-DA63-4936-84C1-CF21E88BE474}']
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject;
      Const DlgType : TITHDlgType);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject;
      Const DlgType : TITHDlgType);
    Function  IsValidated : Boolean;
  End;

Implementation

End.
