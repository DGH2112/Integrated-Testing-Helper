(**
  
  This module contains code for installing a splash screen in the RAD Studio IDE.

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
Unit ITHelper.SplashScreen;

Interface

{$INCLUDE CompilerDefinitions.Inc}

  Procedure InstallSplashScreen;

Implementation

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  Forms,
  ITHelper.CommonFunctions,
  ITHelper.Constants, 
  ITHelper.ResourceStrings;

(**

  This procedure installs the splash screen entry into the RAD Studio IDE.

  @precon  None.
  @postcon The splash screen entry is installed.

**)
Procedure InstallSplashScreen;

Const
  {$IFDEF D2007}
  strITHelperSplashScreen = 'ITHelperSplashScreen24x24';
  {$ELSE}
  strITHelperSplashScreen = 'ITHelperSplashScreen48x48';
  {$ENDIF}

Var
  bmSplashScreen : HBITMAP;
  recVersionInfo : TITHVersionInfo;
  SS : IOTASplashScreenServices;
  strModuleName: String;
  iSize: Cardinal;

Begin
  bmSplashScreen := LoadBitmap(hInstance, strITHelperSplashScreen);
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  BuildNumber(strModuleName, recVersionInfo);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SS) Then
    Begin
      SS.AddPluginBitmap(
        Format(strSplashScreenName, [
          recVersionInfo.FMajor,
          recVersionInfo.FMinor,
          Copy(strRevisions, recVersionInfo.FBugFix + 1, 1),
          Application.Title]),
        bmSplashScreen,
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
        Format(strSplashScreenBuild, [
          recVersionInfo.FMajor,
          recVersionInfo.FMinor,
          recVersionInfo.FBugfix,
          recVersionInfo.FBuild]));
    End;
End;

End.
