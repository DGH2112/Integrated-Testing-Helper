(**
  
  This module contains constants for use through the application.

  @Version 1.001
  @Author  David Hoyle
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
Unit ITHelper.Constants;

Interface

Uses
  ToolsAPI;

{$INCLUDE CompilerDefinitions.inc}

Const
  (** A constant to represent the bug fix revisions for the version number. **)
  strRevisions = ' abcdefghijklmnopqrstuvwxyz';
  (** A constant array of strings for label the compile modes. **)
  astrCompileMode : Array[TOTACompileMode] Of String = ('Make', 'Build', 'Check', 'MakeUnit');
  {$IFDEF D2010}
  (** A constant array of strings for label the compile results. **)
  astrCompileResult : Array[TOTACompileResult] Of String = ('Failed', 'Succeeded', 'Background');
  {$ENDIF D2010}

Implementation

End.
