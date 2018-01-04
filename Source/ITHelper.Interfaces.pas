(**
  
  This module contains interfaces for use throughout the plug-in to minimise coupling.

  @Author  David Hoyle
  @Version 1.0
  @Date    04 Jan 2018
  
**)
Unit ITHelper.Interfaces;

Interface

Uses
  Classes,
  IniFiles;

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
  End;

Implementation

End.
