(**
  
  This module contains simple types to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    04 Jan 2018
  
**)
Unit ITHelper.Types;

Interface

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

Implementation

End.
