(**
  
  This module contains code for added and removing about box entries in the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jan 2018
  
**)
Unit ITHelper.AboutBox;

Interface

  Function  AddAboutBoxEntry : Integer;
  Procedure RemoveAboutBoxEntry(Const iAboutBoxIndex : Integer);

Implementation

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  Forms, 
  ITHelper.ResourceStrings, 
  ITHelper.Constants, 
  ITHelper.CommonFunctions;

(**

  This method installs the about box entry in the IDE.

  @precon  None.
  @postcon The about box entry is installed and the return value is the index with whch to remove it
           later.

  @return  an Integer

**)
Function  AddAboutBoxEntry : Integer;

Const
  strITHelperSplashScreen48x48 = 'ITHelperSplashScreen48x48';
  strSKUBuild = 'SKU Build %d.%d.%d.%d';

ResourceString
  strPluginDescription = 'An IDE expert to allow the configuration of pre and post compilation ' + 
    'processes and automatically ZIP the successfully compiled project for release.';

Var
  bmSplashScreen : HBITMAP;
  iMajor, iMinor, iBugFix, iBuild : Integer;
  ABS : IOTAAboutBoxServices;
  strModuleName: String;
  iSize: Cardinal;

Begin
  Result := -1;
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  BuildNumber(strModuleName, iMajor, iMinor, iBugFix, iBuild);
  bmSplashScreen := LoadBitmap(hInstance, strITHelperSplashScreen48x48);
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
    Begin
      Result := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
        Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevisions, iBugFix + 1, 1),
          Application.Title]),
        strPluginDescription,
        bmSplashScreen,
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
        Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
        Format(strSKUBuild, [iMajor, iMinor, iBugfix, iBuild]));
    End;
End;

(**

  This method removes the about box entry from the IDE with the given index.

  @precon  Must be a valid index for an about box entry.
  @postcon The about box entry is removed.

  @param   iAboutBoxIndex as an Integer as a constant

**)
Procedure RemoveAboutBoxEntry(Const iAboutBoxIndex : Integer);

Begin
  If iAboutBoxIndex > -1 Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutBoxIndex);
End;

End.
