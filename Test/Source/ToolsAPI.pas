(**

  THIS IS A STUB FOR TESTING THE OPEN TOOLS API.

  //: @nodocumentation @nometrics Disabled metrics @nochecks Disbale checks

**)
unit ToolsAPI;

interface

Uses
  IniFiles;

Type
  IOTAEditor = Interface
  ['{FFB5DA0A-8833-4780-BE4F-FCDB41A91E87}']
    Function GetFileName : String;
    Property FileName : String Read GetFileName;
  End;

  IOTAProjectGroup = Interface
  ['{E49FA391-99E1-4B57-A9F6-C24F4AE54EA5}']
    Function GetFileName : String;
    Property FileName : String Read GetFileName;
  End;

  IOTAModule = Interface
  ['{46A12208-E8AE-44C4-BA13-08110B0DD23C}']
  End;

  IOTAProject = Interface(IOTAModule)
  ['{FCF17BAF-DF97-440F-A66B-1915515A0763}']
    Function GetFileName : String;
    Function GetModuleFileCount : Integer;
    Function GetModuleFileEditors(iIndex : Integer) : IOTAEditor;
    Property FileName : String Read GetFileName;
    Property ModuleFileCount : Integer Read GetModuleFileCount;
    Property ModuleFileEditors[iIndex : Integer] : IOTAEditor Read GetModuleFileEditors;
  End;

  IOTAModuleServices = Interface
  ['{70643581-3591-4282-963E-6254344EE724}']
    Function  GetModuleCount : Integer;
    Function  GetModules(iIndex : Integer) : IOTAModule;
    Property ModuleCount : Integer Read GetModuleCount;
    Property Modules[iIndex : Integer] : IOTAModule Read GetModules;
  End;

  TProjectStub = Class(TInterfacedObject, IOTAModule, IOTAProject)
    Function GetFileName : String;
    Function GetModuleFileCount: Integer;
    Function GetModuleFileEditors(iIndex : Integer) : IOTAEditor;
  End;

  TProjectGroupStub = Class(TInterfacedObject, IOTAModule, IOTAProjectGroup)
    Function GetFileName : String;
  End;

  TINIHelper = Class Helper For TCustomINIFile
  Strict Private
    Function GetText : String;
  Public
    Property Text : String Read GetText;
  End;

  IBorlandIDEServices = Interface
  End;

Var
  BorlandIDEServices : IBorlandIDEServices;

  Function  GetProjectINIFileName : String;
  Function  GetProjectName(Project : IOTAProject) : String;
  Procedure WriteINIFile(strText, strFileName : String);

Implementation

Uses
  SysUtils,
  Classes;

Type
  TBorlandIDEServices = Class(TInterfacedObject, IBorlandIDEServices, IOTAModuleServices)
  Strict Private
  Strict Protected
    Function GetModuleCount: Integer;
    Function GetModules(iIndex: Integer): IOTAModule;
  End;

Function GetProjectINIFileName : String;

Begin
  Result := ExtractFilePath(ParamStr(0)) + 'TestProjectOptionsProject.ITHELPER';
End;

Function GetProjectName(Project : IOTAProject) : String;

Begin
  Result := ExtractFileName(Project.FileName);
End;

Procedure WriteINIFile(strText, strFileName : String);

Var
  sl : TStringList;

Begin
  sl := TStringList.Create;
  Try
    sl.Text := strText;
    sl.SaveToFile(strFileName);
  Finally
    sl.Free;
  End;
End;

{ TProjectStub }

function TProjectStub.GetFileName: String;

begin
  Result := ExtractFilePath(ParamStr(0)) + 'TestProjectOptionsProject.dpr';
end;

Function TProjectStub.GetModuleFileCount: Integer;

Begin
  Result := 0;
End;

Function TProjectStub.GetModuleFileEditors(iIndex: Integer): IOTAEditor;

Begin
  Result := Nil;
End;

{ TProjectGroupStub }

function TProjectGroupStub.GetFileName: String;

begin
  Result := ExtractFilePath(ParamStr(0)) + 'TestGlobalOptionsGroup.groupproj';
end;

{ TINIHelper }

function TINIHelper.GetText: String;

Var
  sl : TStringList;
  i : Integer;

begin
  sl := TStringList.Create;
  Try
    ReadSections(sl);
    For i := 0 To sl.Count -1 Do
      Result := Result + Format('[%s]', [sl[i]]);
  Finally
    sl.Free;
  End;
end;

{ TBorlandIDEServices }

Function TBorlandIDEServices.GetModuleCount: Integer;

Begin
  Result := 1;
End;

Function TBorlandIDEServices.GetModules(iIndex: Integer): IOTAModule;

Begin
  Result := Nil;
  If iIndex = 0 Then
    Result := TProjectGroupStub.Create;
End;

initialization
  BorlandIDEServices := TBorlandIDEServices.Create;
end.
