//: @nodocumentation @nometrics Disabled metrics @nochecks Disbale checks
Unit TestGlobalOptions;

Interface

Uses
  TestFramework,
  ToolsAPI;

Type
  //
  // Test Class for the TITHProjectOptions Class Methods.
  //
  TestTITHProjectOptions = Class(TTestCase)
  Strict Private
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAddZipFiles;
    Procedure TestBasePath;
    Procedure TestBuild;
    Procedure TestCompileRes;
    Procedure TestCopyVerInfo;
    Procedure TestEnableZipping;
    Procedure TestExcPatterns;
    Procedure TestIncITHVerInfo;
    Procedure TestIncOnCompile;
    Procedure TestIncResInProj;
    Procedure TestINIFile;
    Procedure TestMajor;
    Procedure TestMinor;
    Procedure TestRelease;
    Procedure TestResExtExc;
    Procedure TestResourceName;
    Procedure TestVerInfo;
    Procedure TestWarnAfter;
    Procedure TestWarnBefore;
    Procedure TestZipName;
    Procedure TestUpdateVersionNumbers;
  End;

  //
  // Test Class for the TITHGlobalOptions Class Methods.
  //
  TestTITHGlobalOptions = Class(TTestCase)
  Strict Private
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestTITHProjectOptions;
    Procedure TestSave;
    Procedure TestAutoScrollMessages;
    Procedure TestClearMessages;
    Procedure TestFontColour;
    Procedure TestFontName;
    Procedure TestFontStyles;
    Procedure TestGroupMessages;
    Procedure TestINIFileName;
    Procedure TestProjectGroupOps_NewProject;
    Procedure TestProjectGroupOps_OldProject;
    Procedure TestSwitchToMessages;
    Procedure TestZipEXE;
    Procedure TestZipParameters;
  End;

Implementation

Uses
  SysUtils,
  Classes,
  Graphics, 
  ITHelper.CommonFunctions, 
  ITHelper.GlobalOptions,
  ITHelper.Interfaces, 
  ITHelper.Types, 
  ITHelper.ProjectOptions;

{$REGION 'TITHProjectOptions Tests'}
//
// Test Methods for Class TITHProjectOptions.
//
Procedure TestTITHProjectOptions.Setup;

Begin
End;

Procedure TestTITHProjectOptions.TearDown;

Begin
End;

Procedure TestTITHProjectOptions.TestAddZipFiles;

Var
  sl : TStringList;
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  sl := TStringList.Create;
  Try
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.AddZipFiles);
      CheckEquals(0, sl.Count);
      sl.Clear;
      sl.Add('D:\Path\File*.*');
      sl.Add('E:\MyPath\MyFile*.gif');
      FProjectOps.AddZipFiles.Assign(sl);
      sl.Assign(FProjectOps.AddZipFiles);
      CheckEquals(2, sl.Count);
      CheckEquals('D:\Path\File*.*', sl[0]);
      CheckEquals('E:\MyPath\MyFile*.gif', sl[1]);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.AddZipFiles);
      CheckEquals(2, sl.Count);
      CheckEquals('D:\Path\File*.*', sl[0]);
      CheckEquals('E:\MyPath\MyFile*.gif', sl[1]);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.AddZipFiles);
      sl.Delete(0);
      FProjectOps.AddZipFiles.Assign(sl);
      sl.Assign(FProjectOps.AddZipFiles);
      CheckEquals(1, sl.Count);
      CheckEquals('E:\MyPath\MyFile*.gif', sl[0]);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.AddZipFiles);
      CheckEquals(1, sl.Count);
      CheckEquals('E:\MyPath\MyFile*.gif', sl[0]);
    Finally
      FProjectOps := Nil;
    End;
  Finally
    sl.Free;
  End;
End;

Procedure TestTITHProjectOptions.TestBasePath;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(ExtractFilePath(ParamStr(0)), FProjectOps.BasePath, 'Default BasePath');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.BasePath := 'C:\HoylD\';
    CheckEquals('C:\HoylD\', FProjectOps.BasePath, 'BasePath First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TPRojectStub.Create);
  Try
    CheckEquals('C:\HoylD\', FProjectOps.BasePath, 'BasePath on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestBuild;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(0, FProjectOps.Build, 'Default Build');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Build := 123;
    CheckEquals(123, FProjectOps.Build, 'Build First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(123, FProjectOps.Build, 'Build on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Build := 321;
    CheckEquals(321, FProjectOps.Build, 'Build First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(321, FProjectOps.Build, 'Build on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestCompileRes;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.CompileRes, 'Default CompileRes');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.CompileRes := True;
    CheckEquals(True, FProjectOps.CompileRes, 'CompileRes First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.CompileRes, 'CompileRes on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.CompileRes := False;
    CheckEquals(False, FProjectOps.CompileRes, 'CompileRes Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.CompileRes, 'CompileRes on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestCopyVerInfo;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('', FProjectOps.CopyVerInfo, 'Default CopyVerInfo');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.CopyVerInfo := 'C:\Windows\Notepad.exe';
    CheckEquals('C:\Windows\Notepad.exe', FProjectOps.CopyVerInfo, 'CopyVerInfo First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('C:\Windows\Notepad.exe', FProjectOps.CopyVerInfo, 'CopyVerInfo on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.CopyVerInfo := 'C:\Windows\WinPad.exe';
    CheckEquals('C:\Windows\WinPad.exe', FProjectOps.CopyVerInfo, 'CopyVerInfo Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('C:\Windows\WinPad.exe', FProjectOps.CopyVerInfo, 'CopyVerInfo on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestEnableZipping;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.EnableZipping, 'Default EnableZipping');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.EnableZipping := True;
    CheckEquals(True, FProjectOps.EnableZipping, 'EnableZipping First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.EnableZipping, 'EnableZipping on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.EnableZipping := False;
    CheckEquals(False, FProjectOps.EnableZipping, 'EnableZipping Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.EnableZipping, 'EnableZipping on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestExcPatterns;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('', FProjectOps.ExcPatterns, 'Default ExcPatterns');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ExcPatterns := 'Qwerty*.*'#13#10'Ytrewq.bmp';
    CheckEquals('Qwerty*.*'#13#10'Ytrewq.bmp', FProjectOps.ExcPatterns, 'ExcPatterns First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('Qwerty*.*'#13#10'Ytrewq.bmp', FProjectOps.ExcPatterns, 'ExcPatterns on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ExcPatterns := 'Qwerty.*'#13#10'Source\Property.*'#13#10'Images\*.bmp';
    CheckEquals('Qwerty.*'#13#10'Source\Property.*'#13#10'Images\*.bmp', FProjectOps.ExcPatterns, 'ExcPatterns Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('Qwerty.*'#13#10'Source\Property.*'#13#10'Images\*.bmp', FProjectOps.ExcPatterns, 'ExcPatterns on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestIncITHVerInfo;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncITHVerInfo, 'Default IncITHVerInfo');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncITHVerInfo := True;
    CheckEquals(True, FProjectOps.IncITHVerInfo, 'IncITHVerInfo First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.IncITHVerInfo, 'IncITHVerInfo on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncITHVerInfo := False;
    CheckEquals(False, FProjectOps.IncITHVerInfo, 'IncITHVerInfo Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncITHVerInfo, 'IncITHVerInfo on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestIncOnCompile;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncOnCompile, 'Default IncOnCompile');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncOnCompile := True;
    CheckEquals(True, FProjectOps.IncOnCompile, 'IncOnCompile First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.IncOnCompile, 'IncOnCompile on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncOnCompile := False;
    CheckEquals(False, FProjectOps.IncOnCompile, 'IncOnCompile Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncOnCompile, 'IncOnCompile on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestIncResInProj;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncResInProj, 'Default IncResInProj');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncResInProj := True;
    CheckEquals(True, FProjectOps.IncResInProj, 'IncResInProj First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.IncResInProj, 'IncResInProj on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.IncResInProj := False;
    CheckEquals(False, FProjectOps.IncResInProj, 'IncResInProj Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.IncResInProj, 'IncResInProj on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestINIFile;

Var
  FProjectOps : IITHProjectOptions;

Begin
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(GetProjectINIFileName, FProjectOps.INIFile.FileName, 'INIFile');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestMajor;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(1, FProjectOps.Major, 'Default Major');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Major := 123;
    CheckEquals(123, FProjectOps.Major, 'Major First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(123, FProjectOps.Major, 'Major on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Major := 321;
    CheckEquals(321, FProjectOps.Major, 'Major First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(321, FProjectOps.Major, 'Major on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestMinor;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(0, FProjectOps.Minor, 'Default Minor');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Minor := 123;
    CheckEquals(123, FProjectOps.Minor, 'Minor First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(123, FProjectOps.Minor, 'Minor on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Minor := 321;
    CheckEquals(321, FProjectOps.Minor, 'Minor First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(321, FProjectOps.Minor, 'Minor on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestRelease;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(0, FProjectOps.Release, 'Default Release');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Release := 123;
    CheckEquals(123, FProjectOps.Release, 'Release First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(123, FProjectOps.Release, 'Release on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.Release := 321;
    CheckEquals(321, FProjectOps.Release, 'Release First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(321, FProjectOps.Release, 'Release on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestResExtExc;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('.dcr', FProjectOps.ResExtExc, 'Default ResExtExc');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ResExtExc := '.dcr;.res';
    CheckEquals('.dcr;.res', FProjectOps.ResExtExc, 'ResExtExc First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('.dcr;.res', FProjectOps.ResExtExc, 'ResExtExc on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ResExtExc := '.res';
    CheckEquals('.res', FProjectOps.ResExtExc, 'ResExtExc Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('.res', FProjectOps.ResExtExc, 'ResExtExc on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestResourceName;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('ITHVerInfo', FProjectOps.ResourceName, 'Default ResourceName');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ResourceName := 'MyApplicationVerInfo';
    CheckEquals('MyApplicationVerInfo', FProjectOps.ResourceName, 'ResourceName First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('MyApplicationVerInfo', FProjectOps.ResourceName, 'ResourceName on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ResourceName := 'DLLs\MyApplicationVerInfo';
    CheckEquals('DLLs\MyApplicationVerInfo', FProjectOps.ResourceName, 'ResourceName Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('DLLs\MyApplicationVerInfo', FProjectOps.ResourceName, 'ResourceName on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestUpdateVersionNumbers;

Var
  sl : TStringList;
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  sl := TStringList.Create;
  Try
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      FProjectOps.Major := 2;
      FProjectOps.Minor := 3;
      FProjectOps.Release := 4;
      FProjectOps.Build := 5;
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals('2.3.4.5', sl.Values['FileVersion']);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals('2.3.4.5', sl.Values['FileVersion']);
      FProjectOps.Build := 123;
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals('2.3.4.123', sl.Values['FileVersion']);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals('2.3.4.123', sl.Values['FileVersion']);
    Finally
      FProjectOps := Nil;
    End;
  Finally
    sl.Free;
  End;
End;

Procedure TestTITHProjectOptions.TestVerInfo;

Var
  sl : TStringList;
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  sl := TStringList.Create;
  Try
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals(10, sl.Count);
      CheckEquals('CompanyName=', sl[0]);
      CheckEquals('FileDescription=', sl[1]);
      CheckEquals('FileVersion=1.0.0.0', sl[2]);
      CheckEquals('InternalName=', sl[3]);
      CheckEquals('LegalCopyright=', sl[4]);
      CheckEquals('LegalTrademarks=', sl[5]);
      CheckEquals('OriginalFilename=', sl[6]);
      CheckEquals('ProductName=', sl[7]);
      CheckEquals('ProductVersion=1.0', sl[8]);
      CheckEquals('Comments=', sl[9]);
      sl.Values['CompanyName'] := 'Season''s Fall';
      sl.Values['FileDescription'] := 'TestProject';
      sl.Values['FileVersion'] := '2.3.4.5';
      sl.Values['InternalName'] := 'TestProject';
      sl.Values['LegalCopyright'] := 'Season''s Fall';
      sl.Values['LegalTrademarks'] := 'Season''s Fall';
      sl.Values['OriginalFilename'] := 'TestProject';
      sl.Values['ProductName'] := 'TestProject';
      sl.Values['ProductVersion'] := '2.3';
      sl.Values['Comments'] := 'My COmment!';
      FProjectOps.VerInfo.Assign(sl);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals(10, sl.Count);
      CheckEquals('CompanyName=Season''s Fall', sl[0]);
      CheckEquals('FileDescription=TestProject', sl[1]);
      CheckEquals('FileVersion=2.3.4.5', sl[2]);
      CheckEquals('InternalName=TestProject', sl[3]);
      CheckEquals('LegalCopyright=Season''s Fall', sl[4]);
      CheckEquals('LegalTrademarks=Season''s Fall', sl[5]);
      CheckEquals('OriginalFilename=TestProject', sl[6]);
      CheckEquals('ProductName=TestProject', sl[7]);
      CheckEquals('ProductVersion=2.3', sl[8]);
      CheckEquals('Comments=My COmment!', sl[9]);
      FProjectOps.VerInfo.Assign(sl);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      sl.Add('DGHKey=Hello');
      sl.Delete(5);
      sl.Delete(1);
      FProjectOps.VerInfo.Assign(sl);
    Finally
      FProjectOps := Nil;
    End;
    FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
    Try
      sl.Assign(FProjectOps.VerInfo);
      CheckEquals(9, sl.Count);
      CheckEquals('CompanyName=Season''s Fall', sl[0]);
      CheckEquals('FileVersion=2.3.4.5', sl[1]);
      CheckEquals('InternalName=TestProject', sl[2]);
      CheckEquals('LegalCopyright=Season''s Fall', sl[3]);
      CheckEquals('OriginalFilename=TestProject', sl[4]);
      CheckEquals('ProductName=TestProject', sl[5]);
      CheckEquals('ProductVersion=2.3', sl[6]);
      CheckEquals('Comments=My COmment!', sl[7]);
      CheckEquals('DGHKey=Hello', sl[8]);
      FProjectOps.VerInfo.Assign(sl);
    Finally
      FProjectOps := Nil;
    End;
  Finally
    sl.Free;
  End;
End;

Procedure TestTITHProjectOptions.TestWarnAfter;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.WarnAfter, 'Default WarnAfter');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.WarnAfter := False;
    CheckEquals(False, FProjectOps.WarnAfter, 'WarnAfter First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.WarnAfter, 'WarnAfter on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.WarnAfter := True;
    CheckEquals(True, FProjectOps.WarnAfter, 'WarnAfter Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.WarnAfter, 'WarnAfter on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestWarnBefore;

Var
  FProjectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.WarnBefore, 'Default WarnBefore');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.WarnBefore := False;
    CheckEquals(False, FProjectOps.WarnBefore, 'WarnBefore First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(False, FProjectOps.WarnBefore, 'WarnBefore on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.WarnBefore := True;
    CheckEquals(True, FProjectOps.WarnBefore, 'WarnBefore Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals(True, FProjectOps.WarnBefore, 'WarnBefore on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;

Procedure TestTITHProjectOptions.TestZipName;

Var
  FPRojectOps : IITHProjectOptions;

Begin
  DeleteFile(GetProjectINIFileName);
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('', FProjectOps.ZipName, 'Default ZipName');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ZipName := 'D:\Path\MyZipFile.zip';
    CheckEquals('D:\Path\MyZipFile.zip', FProjectOps.ZipName, 'ZipName First Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('D:\Path\MyZipFile.zip', FProjectOps.ZipName, 'ZipName on Reload');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    FProjectOps.ZipName := 'E:\MyPath\MyOtherZipFile.zip';
    CheckEquals('E:\MyPath\MyOtherZipFile.zip', FProjectOps.ZipName, 'ZipName Second Set');
  Finally
    FProjectOps := Nil;
  End;
  FProjectOps := TITHProjectOptions.Create(GetProjectINIFileName, TProjectStub.Create);
  Try
    CheckEquals('E:\MyPath\MyOtherZipFile.zip', FProjectOps.ZipName, 'ZipName on Reload');
  Finally
    FProjectOps := Nil;
  End;
End;
{$ENDREGION}

{$REGION 'TITHGlobalOptions Tests'}
//
// Test methods for the class TITHGlobalOptions.
//
Procedure TestTITHGlobalOptions.Setup;

Begin
End;

Procedure TestTITHGlobalOptions.TearDown;

Begin
End;

Procedure TestTITHGlobalOptions.TestTITHProjectOptions;

Var
  FGOps : IITHGlobalOptions;
  P : IITHProjectOptions;
  ProjectStub : IOTAProject;

Begin
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  WriteINIFile(
    '[TestProjectOptionsProject.dpr.Setup]'#13#10 +
    'WarnBefore=1'#13#10 +
    'WarnAfter=1'#13#10 +
    'IncBuild=1'#13#10 +
    'CopyVersionInfoFrom=D:\HoylD\Borland Studio Projects\IDE Addins\BrowseAndDocIt\Test\BrowseAndDocItTests2010.exe'#13#10 +
    'ExcludedResExts=.dcr'#13#10 +
    ''#13#10 +
    '[TestProjectOptionsProject.dpr.Zipping]'#13#10 +
    'Enabled=0'#13#10 +
    'ZipFile=D:\HoylD\Web Page\Zips\browseanddocit.zip'#13#10 +
    'BasePath=D:\HoylD\Borland Studio Projects\'#13#10 +
    ''#13#10 +
    '[TestProjectOptionsProject.dpr.Additional Zip Files]'#13#10 +
    'Item0=D:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\*.rtf'#13#10 +
    'Item1=D:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\*.hpj'#13#10 +
    'Item2=D:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\Images\*.bmp'#13#10 +
    'Item3=D:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\*.cnt'#13#10 +
    'Item4=D:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\*.hlp'#13#10 +
    ''#13#10,
    BuildRootKey
  );
  FGOPs := TITHGlobalOptions.Create;
  Try
    ProjectStub := TProjectStub.Create;
    P := FGOps.ProjectOptions(ProjectStub);
    CheckEquals(True, P.WarnBefore, 'WB');
    CheckEquals(True, P.WarnAfter, 'WA');
    CheckEquals(True, P.IncOnCompile, 'IOC');
    CheckEquals('D:\HoylD\Borland Studio Projects\IDE Addins\BrowseAndDocIt\Test\BrowseAndDocItTests2010.exe', P.CopyVerInfo);
    CheckEquals('.dcr', P.ResExtExc);
    CheckEquals(False, P.EnableZipping);
    CheckEquals('D:\HoylD\Web Page\Zips\browseanddocit.zip', P.ZipName);
    CheckEquals('D:\HoylD\Borland Studio Projects\', P.BasePath);
    CheckEquals(5, P.AddZipFiles.Count);
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    ProjectStub := TProjectStub.Create;
    P := FGOps.ProjectOptions(ProjectStub);
    CheckEquals(True, P.WarnBefore);
    CheckEquals(True, P.WarnAfter);
    CheckEquals(True, P.IncOnCompile);
    CheckEquals('D:\HoylD\Borland Studio Projects\IDE Addins\BrowseAndDocIt\Test\BrowseAndDocItTests2010.exe', P.CopyVerInfo);
    CheckEquals('.dcr', P.ResExtExc);
    CheckEquals(False, P.EnableZipping);
    CheckEquals('D:\HoylD\Web Page\Zips\browseanddocit.zip', P.ZipName);
    CheckEquals('D:\HoylD\Borland Studio Projects\', P.BasePath);
    CheckEquals(5, P.AddZipFiles.Count);
  Finally
    FGOps := Nil;
  End;
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  WriteINIFile(
    '[TestProjectOptionsProject.dpr.Setup]'#13#10 +
    'WarnBefore=0'#13#10 +
    'WarnAfter=0'#13#10 +
    'IncBuild=0'#13#10 +
    'CopyVersionInfoFrom=D:\HoylD\IDE Addins\BrowseAndDocIt\Test\BrowseAndDocItTests2010.exe'#13#10 +
    'ExcludedResExts=.dcr;.res'#13#10 +
    ''#13#10 +
    '[TestProjectOptionsProject.dpr.Zipping]'#13#10 +
    'Enabled=1'#13#10 +
    'ZipFile=E:\HoylD\Web Page\Zips\browseanddocit.zip'#13#10 +
    'BasePath=E:\HoylD\Borland Studio Projects\'#13#10 +
    ''#13#10 +
    '[TestProjectOptionsProject.dpr.Additional Zip Files]'#13#10 +
    'Item0=E:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\*.rtf'#13#10 +
    'Item1=E:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\*.hpj'#13#10 +
    'Item2=E:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\Help\Images\*.bmp'#13#10 +
    'Item3=E:\HoylD\Borland Studio Projects\Applications\Alignment Thingy\*.cnt'#13#10 +
    ''#13#10,
    BuildRootKey
  );
  FGOPs := TITHGlobalOptions.Create;
  Try
    ProjectStub := TProjectStub.Create;
    P := FGOps.ProjectOptions(ProjectStub);
    CheckEquals(False, P.WarnBefore, 'WB');
    CheckEquals(False, P.WarnAfter, 'WA');
    CheckEquals(False, P.IncOnCompile, 'IOC');
    CheckEquals('D:\HoylD\IDE Addins\BrowseAndDocIt\Test\BrowseAndDocItTests2010.exe', P.CopyVerInfo);
    CheckEquals('.dcr;.res', P.ResExtExc);
    CheckEquals(True, P.EnableZipping);
    CheckEquals('E:\HoylD\Web Page\Zips\browseanddocit.zip', P.ZipName);
    CheckEquals('E:\HoylD\Borland Studio Projects\', P.BasePath);
    CheckEquals(4, P.AddZipFiles.Count);
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestSave;

Var
  FGOps: IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  CheckEquals(False, FileExists(BuildRootKey));
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(False, FileExists(BuildRootKey));
    FGops.Save;
    CheckEquals(True, FileExists(BuildRootKey));
  Finally
    FGOPs :=  Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestAutoScrollMessages;

Var
  GOps : IITHGlobalOptions;

Begin
   DeleteFile(BuildRootKey);
   GOps := TITHGlobalOptions.Create;
   Try
     CheckEquals(True, GOps.AutoScrollMessages);
     GOps.AutoScrollMessages := False;
     CheckEquals(False, GOps.AutoScrollMessages);
   Finally
     GOps := Nil;
   End;
   GOps := TITHGlobalOptions.Create;
   Try
     CheckEquals(False, GOps.AutoScrollMessages);
     GOps.AutoScrollMessages := True;
     CheckEquals(True, GOps.AutoScrollMessages);
   Finally
     GOps := Nil;
   End;
   GOps := TITHGlobalOptions.Create;
   Try
     CheckEquals(True, GOps.AutoScrollMessages);
   Finally
     GOps := Nil;
   End;
End;

Procedure TestTITHGlobalOptions.TestClearMessages;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOPs := TITHGlobalOptions.Create;
  Try
    CheckEquals(30, GOps.ClearMessages);
    GOps.ClearMessages := 60;
    CheckEquals(60, GOps.ClearMessages);
  Finally
    GOps := Nil;
  End;
  GOPs := TITHGlobalOptions.Create;
  Try
    CheckEquals(60, GOps.ClearMessages);
    GOps.ClearMessages := 45;
    CheckEquals(45, GOps.ClearMessages);
  Finally
    GOps := Nil;
  End;
  GOPs := TITHGlobalOptions.Create;
  Try
    CheckEquals(45, GOps.ClearMessages);
  Finally
    GOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestFontColour;

Var
  FGOPs : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(clBlack, FGOps.FontColour[ithfDefault]);
    CheckEquals(clBlack, FGOps.FontColour[ithfFailure]);
    CheckEquals(clBlack, FGOps.FontColour[ithfHeader]);
    CheckEquals(clBlack, FGOps.FontColour[ithfSuccess]);
    CheckEquals(clBlack, FGOps.FontColour[ithfWarning]);
    FGOps.FontColour[ithfDefault] := clRed;
    FGOps.FontColour[ithfFailure] := clGreen;
    FGOps.FontColour[ithfHeader]  := clBlue;
    FGOps.FontColour[ithfSuccess] := clPurple;
    FGOps.FontColour[ithfWarning] := clYellow;
    CheckEquals(clRed, FGOps.FontColour[ithfDefault]);
    CheckEquals(clGreen, FGOps.FontColour[ithfFailure]);
    CheckEquals(clBlue, FGOps.FontColour[ithfHeader]);
    CheckEquals(clPurple, FGOps.FontColour[ithfSuccess]);
    CheckEquals(clYellow, FGOps.FontColour[ithfWarning]);
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(clRed, FGOps.FontColour[ithfDefault]);
    CheckEquals(clGreen, FGOps.FontColour[ithfFailure]);
    CheckEquals(clBlue, FGOps.FontColour[ithfHeader]);
    CheckEquals(clPurple, FGOps.FontColour[ithfSuccess]);
    CheckEquals(clYellow, FGOps.FontColour[ithfWarning]);
    FGOps.FontColour[ithfDefault] := clFuchsia;
    FGOps.FontColour[ithfFailure] := clLime;
    FGOps.FontColour[ithfHeader]  := clMaroon;
    FGOps.FontColour[ithfSuccess] := clNavy;
    FGOps.FontColour[ithfWarning] := clTeal;
    CheckEquals(clFuchsia, FGOps.FontColour[ithfDefault]);
    CheckEquals(clLime, FGOps.FontColour[ithfFailure]);
    CheckEquals(clMaroon, FGOps.FontColour[ithfHeader]);
    CheckEquals(clNavy, FGOps.FontColour[ithfSuccess]);
    CheckEquals(clTeal, FGOps.FontColour[ithfWarning]);
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(clFuchsia, FGOps.FontColour[ithfDefault]);
    CheckEquals(clLime, FGOps.FontColour[ithfFailure]);
    CheckEquals(clMaroon, FGOps.FontColour[ithfHeader]);
    CheckEquals(clNavy, FGOps.FontColour[ithfSuccess]);
    CheckEquals(clTeal, FGOps.FontColour[ithfWarning]);
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestFontName;

Var
  FGOPs : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('Tahoma', FGOps.FontName[fnHeader]);
    CheckEquals('Tahoma', FGOps.FontName[fnTools]);
    FGOps.FontName[fnHeader] := 'Arial';
    FGOps.FontName[fnTools] := 'Times New Roman';
    CheckEquals('Arial', FGOps.FontName[fnHeader]);
    CheckEquals('Times New Roman', FGOps.FontName[fnTools]);
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('Arial', FGOps.FontName[fnHeader]);
    CheckEquals('Times New Roman', FGOps.FontName[fnTools]);
    FGOps.FontName[fnHeader] := 'Courier';
    FGOps.FontName[fnTools] := 'Courier New';
    CheckEquals('Courier', FGOps.FontName[fnHeader]);
    CheckEquals('Courier New', FGOps.FontName[fnTools]);
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('Courier', FGOps.FontName[fnHeader]);
    CheckEquals('Courier New', FGOps.FontName[fnTools]);
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestFontStyles;

Var
  FGOps : IITHGlobalOptions;
  Ops : TFontStyles;

Begin
  DeleteFile(BuildRootKey);
  FGOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(0, Byte(FGOps.FontStyles[ithfDefault]));
    CheckEquals(0, Byte(FGOps.FontStyles[ithfFailure]));
    CheckEquals(0, Byte(FGOps.FontStyles[ithfHeader]));
    CheckEquals(0, Byte(FGOps.FontStyles[ithfSuccess]));
    CheckEquals(0, Byte(FGOps.FontStyles[ithfWarning]));
    FGOps.FontStyles[ithfDefault] := [fsBold];
    FGOps.FontStyles[ithfFailure] := [fsItalic];
    FGOps.FontStyles[ithfHeader]  := [fsUnderline];
    FGOps.FontStyles[ithfSuccess] := [fsStrikeout];
    FGOps.FontStyles[ithfWarning] := [fsBold];
    Ops := [fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfDefault]));
    Ops := [fsItalic];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfFailure]));
    Ops := [fsUnderline];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfHeader]));
    Ops := [fsStrikeOut];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfSuccess]));
    Ops := [fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfWarning]));
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    Ops := [fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfDefault]));
    Ops := [fsItalic];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfFailure]));
    Ops := [fsUnderline];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfHeader]));
    Ops := [fsStrikeOut];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfSuccess]));
    Ops := [fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfWarning]));
    FGOps.FontStyles[ithfDefault] := [fsItalic, fsUnderline];
    FGOps.FontStyles[ithfFailure] := [fsUnderline, fsStrikeOut];
    FGOps.FontStyles[ithfHeader]  := [fsStrikeOut, fsBold];
    FGOps.FontStyles[ithfSuccess] := [fsBold, fsItalic];
    FGOps.FontStyles[ithfWarning] := [fsItalic, fsUnderline];
    Ops := [fsItalic, fsUnderline];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfDefault]));
    Ops := [fsUnderline, fsStrikeout];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfFailure]));
    Ops := [fsStrikeOut, fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfHeader]));
    Ops := [fsBold, fsItalic];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfSuccess]));
    Ops := [fsItalic, fsUnderline];
  Finally
    FGOps := Nil;
  End;
  FGOps := TITHGlobalOptions.Create;
  Try
    Ops := [fsItalic, fsUnderline];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfDefault]));
    Ops := [fsUnderline, fsStrikeout];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfFailure]));
    Ops := [fsStrikeOut, fsBold];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfHeader]));
    Ops := [fsBold, fsItalic];
    CheckEquals(Byte(Ops), Byte(FGOps.FontStyles[ithfSuccess]));
    Ops := [fsItalic, fsUnderline];
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestGroupMessages;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(False, GOps.GroupMessages);
    GOps.GroupMessages := True;
    CheckEquals(True, GOps.GroupMessages);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(True, GOps.GroupMessages);
    GOps.GroupMessages := False;
    CheckEquals(False, GOps.GroupMessages);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(False, GOps.GroupMessages);
  Finally
    GOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestINIFileName;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(BuildRootKey, Gops.INIFileName);
  Finally
    GOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestProjectGroupOps_NewProject;

Var
  FGOps : IITHGlobalOptions;
  Ops : TITHEnabledOptions;

Begin
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore..eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
    FGOps.ProjectGroupOps := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
    FGOps.ProjectGroupOps := [eoAfter, eoBuildVersionResource, eoGroupEnabled];
    Ops := [eoAfter, eoBuildVersionResource, eoGroupEnabled];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoAfter, eoBuildVersionResource, eoGroupEnabled];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestProjectGroupOps_OldProject;

Var
  FGOps : IITHGlobalOptions;
  Ops : TITHEnabledOptions;

Begin
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  Ops := [eoAfter];
  WriteINIFile(
    '[ProjectGroupOptions]'#13#10 +
    'TestGlobalOptionsGroup.groupproj=' + IntToStr(Byte(Ops)),
    BuildRootKey
  );
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoAfter, eoBuildVersionResource, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
    FGOps.ProjectGroupOps := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  // Update VERY OLD Project
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  Ops := [eoAfter, eoBefore];
  WriteINIFile(
    '[TestGlobalOptionsGroup.groupproj.Setup]'#13#10 +
    'Enabled=0'#13#10 +
    'EnabledOptions=' + IntToStr(Byte(Ops)),
    BuildRootKey
  );
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoAfter, eoBefore, eoBuildVersionResource, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
    FGOps.ProjectGroupOps := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  // Update VERY OLD Project
  DeleteFile(BuildRootKey);
  DeleteFile(GetProjectINIFileName);
  Ops := [eoBefore];
  WriteINIFile(
    '[TestGlobalOptionsGroup.groupproj.Setup]'#13#10 +
    'Enabled=1'#13#10 +
    'EnabledOptions=' + IntToStr(Byte(Ops)),
    BuildRootKey
  );
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore, eoGroupEnabled, eoBuildVersionResource, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
    FGOps.ProjectGroupOps := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
  FGOPs := TITHGlobalOptions.Create;
  Try
    Ops := [eoBefore, eoZip, eoCopyVersionInfo, eoIncrementBuild];
    CheckEquals(Byte(Ops), Byte(FGOPs.ProjectGroupOps));
  Finally
    FGOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestSwitchToMessages;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(True, GOps.SwitchToMessages);
    GOps.SwitchToMessages := False;
    CheckEquals(False, GOps.SwitchToMessages);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(False, GOps.SwitchToMessages);
    GOps.SwitchToMessages := True;
    CheckEquals(True, GOps.SwitchToMessages);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals(True, GOps.SwitchToMessages);
  Finally
    GOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestZipEXE;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('C:\Program Files\7-Zip\7Z.EXE', GOps.ZipEXE);
    GOps.ZipEXE := 'C:\Program Files (x86)\WinZip\WZZip.exe';
    CheckEquals('C:\Program Files (x86)\WinZip\WZZip.exe', GOps.ZipEXE);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('C:\Program Files (x86)\WinZip\WZZip.exe', GOps.ZipEXE);
    GOps.ZipEXE := 'C:\Program Files\WinZip\WZZip.exe';
    CheckEquals('C:\Program Files\WinZip\WZZip.exe', GOps.ZipEXE);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('C:\Program Files\WinZip\WZZip.exe', GOps.ZipEXE);
  Finally
    GOps := Nil;
  End;
End;

Procedure TestTITHGlobalOptions.TestZipParameters;

Var
  GOps : IITHGlobalOptions;

Begin
  DeleteFile(BuildRootKey);
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('a "$ZIPFILE$" @"$RESPONSEFILE$"', GOps.ZipParameters);
    GOps.ZipParameters := '-exrp @"$RESPONSEFILE$" "$ZIPFILE$"';
    CheckEquals('-exrp @"$RESPONSEFILE$" "$ZIPFILE$"', GOps.ZipParameters);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('-exrp @"$RESPONSEFILE$" "$ZIPFILE$"', GOps.ZipParameters);
    GOps.ZipParameters := '-ouexrp @"$RESPONSEFILE$" "$ZIPFILE$"';
    CheckEquals('-ouexrp @"$RESPONSEFILE$" "$ZIPFILE$"', GOps.ZipParameters);
  Finally
    GOps := Nil;
  End;
  GOps := TITHGlobalOptions.Create;
  Try
    CheckEquals('-ouexrp @"$RESPONSEFILE$" "$ZIPFILE$"', GOps.ZipParameters);
  Finally
    GOps := Nil;
  End;
End;
{$ENDREGION}

Initialization
  RegisterTest('Project Options Adapter', TestTITHProjectOptions.Suite);
  RegisterTest('Global Options Adapter', TestTITHGlobalOptions.Suite);
End.