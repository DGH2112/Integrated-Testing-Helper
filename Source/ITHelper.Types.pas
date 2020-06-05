(**
  
  This module contains simple types to be used through the application.

  @Author  David Hoyle
  @Version 1.001
  @Date    05 Jun 2020
  
  @license

    Integrated Testing helper is a RAD Studio plug-in for running pre and post
    build processes.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)

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
