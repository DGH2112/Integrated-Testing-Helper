(**
  
  This module contaisn resource string for use throughout the application.

  @Author  David Hoyle
  @Version 1.002
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
Unit ITHelper.ResourceStrings;

Interface

ResourceString
  (** This is a message string to appear in the splash screen **)
  strSplashScreenName = 'Integrated Testing Helper %d.%d%s for %s';
  {$IFDEF DEBUG}
  (** This is another message string to appear in the splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2020 License GNU GPL 3 (DEBUG Build %d.%d.%d.%d)';
  {$ELSE}
  (** This is another message string to appear in the splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2020 License GNU GPL 3 (Build %d.%d.%d.%d)';
  {$ENDIF}
  (** This is a resourcestring for the message tab name. **)
  strITHelperGroup = 'ITHelper Messages';

Implementation

End.
