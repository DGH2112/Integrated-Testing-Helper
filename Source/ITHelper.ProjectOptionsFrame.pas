(**
  
  This module contains the project options (version control, etc) in a frame so that it can be placed 
  in the project options dialgoue.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Jul 2018
  
**)
Unit ITHelper.ProjectOptionsFrame;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Buttons,
  ComCtrls,
  Grids,
  ValEdit,
  StdCtrls,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A frame to contain the project options. **)
  TframeProjectOptions = Class(TFrame, IITHOptionsFrame)
    lblResExts: TLabel;
    lblVersionInfo: TLabel;
    btnOpenEXE: TButton;
    chkIncrementBuildOnCompile: TCheckBox;
    edtResExts: TEdit;
    edtVersionInfo: TEdit;
    gbxVersionInfo: TGroupBox;
    lblMajor: TLabel;
    lblMinor: TLabel;
    lblRelease: TLabel;
    lblBuild: TLabel;
    lblResourceName: TLabel;
    vleVersionInfo: TValueListEditor;
    edtMajor: TEdit;
    upMajor: TUpDown;
    edtMinor: TEdit;
    upMinor: TUpDown;
    edtRelease: TEdit;
    upRelease: TUpDown;
    edtBuild: TEdit;
    upBuild: TUpDown;
    btnGetVersionInfo: TBitBtn;
    chkIncludeInProject: TCheckBox;
    chkCompileWithBRCC32: TCheckBox;
    edtResourceName: TEdit;
    chkEnabled: TCheckBox;
    dlgOpenEXE: TOpenDialog;
    Procedure btnOpenEXEClick(Sender: TObject);
    Procedure chkEnabledClick(Sender: TObject);
    Procedure BuildChange(Sender: TObject; Button: TUDBtnType);
    Procedure btnGetVersionInfoClick(Sender: TObject);
  Strict Private
    FProject : IOTAProject;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Function  IsValidated : Boolean;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  IniFiles,
  ITHelper.Constants;

(**

  This is an on click event handler for the GetvVersionInfo button.

  @precon  None.
  @postcon Extracts the version information from the IDE.

  @param   Sender as a TObject

**)
Procedure TframeProjectOptions.btnGetVersionInfoClick(Sender: TObject);

Const
  strMsg = 'Are you sure you want to replace the current version information with the ' +
    'version information from the IDE?';
  {$IFNDEF DXE20}
  strMajorVersion = 'MajorVersion';
  strMinorVersion = 'MinorVersion';
  strRelease = 'Release';
  strBuild = 'Build';
  strKeys = 'Keys';
  {$ENDIF}

Var
  S : TStrings;
  {$IFNDEF DXE20}
  PO : IOTAProjectOptions;
  i : Integer;
  sl : TStrings;
  {$ELSE}
  POC : IOTAProjectOptionsConfigurations;
  AC : IOTABuildConfiguration;
  {$ENDIF}

Begin
  If MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      {$IFDEF DXE20}
      If FProject.ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations, POC) = S_OK Then
        Begin
          AC := POC.ActiveConfiguration;
          upMajor.Position := AC.AsInteger[sVerInfo_MajorVer];
          upMinor.Position := AC.AsInteger[sVerInfo_MinorVer];
          upRelease.Position := AC.AsInteger[sVerInfo_Release];
          upBuild.Position := AC.AsInteger[sVerInfo_Build];
          S := vleVersionInfo.Strings;
          S.Text := StringReplace(AC.Value[sVerInfo_Keys], ';', #13#10, [rfReplaceAll]);
        End;
      {$ELSE}
      PO := FProject.ProjectOptions;
      upMajor.Position := PO.Values[strMajorVersion];
      upMinor.Position := PO.Values[strMinorVersion];
      upRelease.Position := PO.Values[strRelease];
      upBuild.Position := PO.Values[strBuild];
      i := VarAsType(PO.Values[strKeys], varInteger);
      sl := TStrings(i);
      S := vleVersionInfo.Strings;
      S.Assign(sl);
      S.Text := StringReplace(S.Text, ';', #13#10, [rfReplaceAll]);
      {$ENDIF}
    End;
End;

(**

  This is an on click event handler for the Browse Zip EXE button.

  @precon  None.
  @postcon Allows the user to select a zip executable.

  @param   Sender as a TObject

**)
Procedure TframeProjectOptions.btnOpenEXEClick(Sender: TObject);

Begin
  dlgOpenEXE.FileName := edtVersionInfo.Text;
  If dlgOpenEXE.Execute Then
    edtVersionInfo.Text := dlgOpenEXE.FileName;
End;

(**

  This is an on change event handler for the Build controls (Major, Minor, Release and
  Build) control.

  @precon  None.
  @postcon Updates the FileVersion member of the version information strings.

  @param   Sender as a TObject
  @param   Button as a TUDBtnType

**)
Procedure TframeProjectOptions.BuildChange(Sender: TObject; Button: TUDBtnType);

Const
  strFileVersion = 'FileVersion';
  strProductVersion = 'ProductVersion';

Var
  S : TStrings;

Begin
  S := vleVersionInfo.Strings;
  If S.IndexOfName(strFileVersion) > -1 Then
    S.Values[strFileVersion] := Format('%d.%d.%d.%d', [
      upMajor.Position,
      upMinor.Position,
      upRelease.Position,
      upBuild.Position]);
  If S.IndexOfName(strProductVersion) > -1 Then
    S.Values[strProductVersion] := Trim(Format('%d.%d%s', [
      upMajor.Position,
      upMinor.Position,
      strRevisions[Succ(upRelease.Position)]]));
End;

(**

  This is an on click event handler for the Enabled checkbox.

  @precon  None.
  @postcon Enables of disables the grouop box accordingly.

  @param   Sender as a TObject

**)
Procedure TframeProjectOptions.chkEnabledClick(Sender: TObject);

Begin
  gbxVersionInfo.Enabled := chkEnabled.Checked;
  lblMajor.Enabled := chkEnabled.Checked;
  edtMajor.Enabled := chkEnabled.Checked;
  upMajor.Enabled := chkEnabled.Checked;
  lblMinor.Enabled := chkEnabled.Checked;
  edtMinor.Enabled := chkEnabled.Checked;
  upMinor.Enabled := chkEnabled.Checked;
  lblRelease.Enabled := chkEnabled.Checked;
  edtRelease.Enabled := chkEnabled.Checked;
  upRelease.Enabled := chkEnabled.Checked;
  lblBuild.Enabled := chkEnabled.Checked;
  edtBuild.Enabled := chkEnabled.Checked;
  upBuild.Enabled := chkEnabled.Checked;
  vleVersionInfo.Enabled := chkEnabled.Checked;
  chkIncludeInProject.Enabled := chkEnabled.Checked;
  chkCompileWithBRCC32.Enabled := chkEnabled.Checked;
  lblResourceName.Enabled := chkEnabled.Checked;
  edtResourceName.Enabled := chkEnabled.Checked;
  btnGetVersionInfo.Enabled := chkEnabled.Checked;
End;

(**

  A constructor for the TframeProjectOptions class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner
  
  @param   AOwner as a TComponent

**)
Constructor TframeProjectOptions.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
End;

(**

  A destructor for the TframeProjectOptions class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TframeProjectOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method initialises the dialogue controls with information from the global options and the project 
  options.

  @precon  GlobalOps and Project must be valid instances.
  @postcon Initialises the dialogue controls with information from the global options and the project 
           options.

  @nohint  DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeProjectOptions.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject; Const DlgType : TITHDlgType); //FI:O804

Var
  ProjectOps: IITHProjectOptions;

Begin
  FProject := Project;
  ProjectOps := GlobalOps.ProjectOptions(Project);
  Try
    chkIncrementBuildOnCompile.Checked := ProjectOps.IncOnCompile;
    edtVersionInfo.Text          := ProjectOps.CopyVerInfo;
    edtResExts.Text              := ProjectOps.ResExtExc;
    chkEnabled.Checked           := ProjectOps.IncITHVerInfo;
    chkIncludeInProject.Checked  := ProjectOps.IncResInProj;
    chkCompileWithBRCC32.Checked := ProjectOps.CompileRes;
    upMajor.Position             := ProjectOps.Major;
    upMinor.Position             := ProjectOps.Minor;
    upRelease.Position           := ProjectOps.Release;
    upBuild.Position             := ProjectOps.Build;
    edtResourceName.Text         := ProjectOps.ResourceName;
    vleVersionInfo.Strings.Assign(ProjectOps.VerInfo);
  Finally
    ProjectOps := Nil;
  End;
  chkEnabledClick(Nil);
End;

(**

  This method implements the IsValidated interfac method for the frame.

  @precon  None.
  @postcon No validation is currently required to true is returned.

  @return  a Boolean

**)
Function TframeProjectOptions.IsValidated: Boolean;

Begin
  Result := True;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @nohint  DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeProjectOptions.SaveOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject; Const DlgType : TITHDlgType); //FI:O804

Var
  ProjectOps: IITHProjectOptions;

Begin
  ProjectOps := GlobalOps.ProjectOptions(Project);
  Try
    ProjectOps.IncOnCompile := chkIncrementBuildOnCompile.Checked;
    ProjectOps.CopyVerInfo := edtVersionInfo.Text;
    ProjectOps.ResExtExc := edtResExts.Text;
    ProjectOps.IncITHVerInfo := chkEnabled.Checked;
    ProjectOps.IncResInProj := chkIncludeInProject.Checked;
    ProjectOps.CompileRes := chkCompileWithBRCC32.Checked;
    ProjectOps.Major := upMajor.Position;
    ProjectOps.Minor := upMinor.Position;
    ProjectOps.Release := upRelease.Position;
    ProjectOps.Build := upBuild.Position;
    ProjectOps.ResourceName := edtResourceName.Text;
    ProjectOps.VerInfo.Assign(vleVersionInfo.Strings);
  Finally
    ProjectOps := Nil;
  End;
End;

End.

