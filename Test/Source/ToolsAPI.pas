(**

  THIS IS A STUB FOR TESTING THE OPEN TOOLS API.

  DUnit test for the Integrated Testing Helper.

  @Version 1.014
  @Author  David Hoyle
  @Date    21 Nov 2021

  @nodocumentation
  @nometrics Disabled metrics
  @nochecks Disbale checks

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

  IOTAProjectOptions = Interface
  ['{0FC4C69A-3BE8-4ADF-9F7A-4E5F5610A6D2}']
  End;

  IOTAProject = Interface(IOTAModule)
  ['{FCF17BAF-DF97-440F-A66B-1915515A0763}']
    Function  GetFileName : String;
    Function  GetModuleFileCount : Integer;
    Function  GetModuleFileEditors(iIndex : Integer) : IOTAEditor;
    Function  GetProjectOptions : IOTAProjectOptions;
    Property  FileName : String Read GetFileName;
    Property  ModuleFileCount : Integer Read GetModuleFileCount;
    Property  ModuleFileEditors[iIndex : Integer] : IOTAEditor Read GetModuleFileEditors;
    Property  ProjectOptions : IOTAProjectOptions Read GetProjectOptions;
  End;

  IOTAModuleServices = Interface
  ['{70643581-3591-4282-963E-6254344EE724}']
    Function  GetModuleCount : Integer;
    Function  GetModules(iIndex : Integer) : IOTAModule;
    Property  ModuleCount : Integer Read GetModuleCount;
    Property  Modules[iIndex : Integer] : IOTAModule Read GetModules;
  End;

  IOTAProjectOptionsConfigurations = Interface
  ['{F13D1130-0288-4228-AA50-0A3D08F2017D}']
    Function  GetActiveConfigurationName : String;
    Property  ActiveConfigurationName : String Read GetActiveConfigurationName;
  End;

  TProjectOptionsMock = Class(TInterfacedObject, IOTAProjectOptions, IOTAProjectOptionsConfigurations)
  Public
    Function GetActiveConfigurationName: string;
  End;

  TProjectStub = Class(TInterfacedObject, IOTAModule, IOTAProject)
  Public
    Function  GetFileName : String;
    Function  GetModuleFileCount: Integer;
    Function  GetModuleFileEditors(iIndex : Integer) : IOTAEditor;
    Function  GetProjectOptions : IOTAProjectOptions;
  End;

  TProjectGroupStub = Class(TInterfacedObject, IOTAModule, IOTAProjectGroup)
  Public
    Function  GetFileName : String;
  End;

  TINIHelper = Class Helper For TCustomINIFile
  Strict Private
    Function  GetText : String;
  Public
    Property Text : String Read GetText;
  End;

  IBorlandIDEServices = Interface
  End;

  TOTACompileMode = (cmOTAMake, cmOTABuild, cmOTACheck, cmOTAMakeUnit, cmClean, cmLink);
  TOTACompileResult = (crOTAFailed, crOTASucceeded, crOTABackground);

  TOTAProjectCompileInfo = Record
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

Function TProjectStub.GetProjectOptions: IOTAProjectOptions;

Begin
  Result := TProjectOptionsMock.Create;
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

{ TProjectOptionsMock }

Function TProjectOptionsMock.GetActiveConfigurationName: String;

Begin
  Result := 'DEBUG';
End;

initialization
  BorlandIDEServices := TBorlandIDEServices.Create;
end.
