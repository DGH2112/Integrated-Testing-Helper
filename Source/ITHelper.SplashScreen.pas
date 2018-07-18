(**
  
  This module contains code for installing a splash screen in the RAD Studio IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Jul 2018
  
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
