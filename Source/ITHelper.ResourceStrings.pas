(**
  
  This module contaisn resource string for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Jan 2018
  
**)
Unit ITHelper.ResourceStrings;

Interface

ResourceString
  (** This is a message string to appear in the splash screen **)
  strSplashScreenName = 'Integrated Testing Helper %d.%d%s for %s';
  {$IFDEF DEBUG}
  (** This is another message string to appear in the splash screen **)
  strSplashScreenBuild = 'Freeware by David Hoyle (Build %d.%d.%d.%d)';
  {$ELSE}
  (** This is another message string to appear in the splash screen **)
  strSplashScreenBuild = 'Freeware by David Hoyle (Build %d.%d.%d.%d)';
  {$ENDIF}
  (** This is a resourcestring for the message tab name. **)
  strITHelperGroup = 'ITHelper Messages';

Implementation

End.
