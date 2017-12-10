(**
  
  This module contains common functions (non-OTA) for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    10 Dec 2017
  
**)
Unit ITHelper.CommonFunctions;

Interface

  Procedure BuildNumber(Var iMajor, iMinor, iBugFix, iBuild : Integer);
  
Implementation

Uses
  Windows;

(**

  This is a method which obtains information about the package from is version
  information with the package resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer as a reference
  @param   iMinor  as an Integer as a reference
  @param   iBugFix as an Integer as a reference
  @param   iBuild  as an Integer as a reference

**)
Procedure BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);

Const
  iShiftRight16 = 16;
  iWordMask = $FFFF;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strBuffer : Array[0..MAX_PATH] Of Char;

Begin
  GetModuleFileName(hInstance, strBuffer, MAX_PATH);
  VerInfoSize := GetFileVersionInfoSize(strBuffer, Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(strBuffer, 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        iMajor := VerValue^.dwFileVersionMS Shr iShiftRight16;
        iMinor := VerValue^.dwFileVersionMS And iWordMask;
        iBugFix := VerValue^.dwFileVersionLS Shr iShiftRight16;
        iBuild := VerValue^.dwFileVersionLS And iWordMask;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;

End.
