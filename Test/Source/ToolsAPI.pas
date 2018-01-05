(**

  THIS IS A STUB FOR TESTING THE OPEN TOOLS API.

  @stopdocumentation

**)
unit ToolsAPI;

interface

Uses
  IniFiles;

Type
  IOTAProjectGroup = Interface
    Function GetFileName : String;
    Property FileName : String Read GetFileName;
  End;

  IOTAProject = Interface
    Function GetFileName : String;
    Property FileName : String Read GetFileName;
  End;

  TProjectStub = Class(TInterfacedObject, IOTAProject)
    Function GetFileName : String;
  End;

  TProjectGroupStub = Class(TInterfacedObject, IOTAProjectGroup)
    Function GetFileName : String;
  End;

  TINIHelper = Class Helper For TCustomINIFile
  Strict Private
    Function GetText : String;
  Public
    Property Text : String Read GetText;
  End;

  Function  ProjectGroup : IOTAProjectGroup;
  Function  GetProjectINIFileName : String;
  Function  GetProjectName(Project : IOTAProject) : String;
  Procedure WriteINIFile(strText, strFileName : String);

implementation

Uses
  SysUtils,
  Classes;

Function ProjectGroup : IOTAProjectGroup;

Begin
  Result := TProjectGroupStub.Create;;
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

end.
