(**
  
  This module contains code for installing a splash screen in the RAD Studio IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    10 Dec 2017
  
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
  strITHelperSplashScreen24x24 = 'ITHelperSplashScreen24x24';
  {$ELSE}
  strITHelperSplashScreen48x48 = 'ITHelperSplashScreen48x48';
  {$ENDIF}

Var
  bmSplashScreen : HBITMAP;
  iMajor, iMinor, iBugFix, iBuild : Integer;
  SS : IOTASplashScreenServices;

Begin
  {$IFDEF D2007}
  bmSplashScreen := LoadBitmap(hInstance, strITHelperSplashScreen24x24);
  {$ELSE}
  bmSplashScreen := LoadBitmap(hInstance, strITHelperSplashScreen48x48);
  {$ENDIF}
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SS) Then
    Begin
      SS.AddPluginBitmap(
        Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevisions, iBugFix + 1, 1),
          Application.Title]),
        bmSplashScreen,
        False,
        Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]));
    End;
End;

End.
