(**
  
  This module contains simple types to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Jul 2018
  
**)
Unit ITHelper.Types;

Interface

Uses
  SysUtils;

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

  (** An enumerate to define which set of data the dialogue is to work with. **)
  TITHDlgType = (dtNA, dtBefore, dtAfter);

  (** A custom exception for ITHelper problems. **)
  EITHException = Class(Exception);

Implementation

End.
