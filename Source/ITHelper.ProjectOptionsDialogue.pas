(**

  This module contains a class which represents a form on which the project options for
  the active project can be edited.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Jul 2018

**)
Unit ITHelper.ProjectOptionsDialogue;

Interface

{$I 'CompilerDefinitions.inc'}

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
  StdCtrls,
  ToolsAPI,
  ITHelper.Interfaces,
  Grids,
  ValEdit,
  ComCtrls;

Type
  (** A class to represent the project options form. **)
  TfrmITHProjectOptionsDialogue = Class(TForm)
    lblResExts: TLabel;
    lblVersionInfo: TLabel;
    chkIncrementBuildOnCompile: TCheckBox;
    edtResExts: TEdit;
    edtVersionInfo: TEdit;
    btnOpenEXE: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgOpenEXE: TOpenDialog;
    vleVersionInfo: TValueListEditor;
    gbxVersionInfo: TGroupBox;
    chkEnabled: TCheckBox;
    lblMajor: TLabel;
    edtMajor: TEdit;
    upMajor: TUpDown;
    edtMinor: TEdit;
    upMinor: TUpDown;
    lblMinor: TLabel;
    edtRelease: TEdit;
    upRelease: TUpDown;
    lblRelease: TLabel;
    edtBuild: TEdit;
    upBuild: TUpDown;
    lblBuild: TLabel;
    btnGetVersionInfo: TBitBtn;
    chkIncludeInProject: TCheckBox;
    chkCompileWithBRCC32: TCheckBox;
    lblResourceName: TLabel;
    edtResourceName: TEdit;
    btnHelp: TBitBtn;
    Procedure btnOpenEXEClick(Sender: TObject);
    Procedure chkEnabledClick(Sender: TObject);
    Procedure BuildChange(Sender: TObject; Button: TUDBtnType);
    Procedure btnGetVersionInfoClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  Private
    { Private declarations }
    FProject : IOTAProject;
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);
  Public
    { Public declarations }
    Class Procedure Execute(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  ITHelper.TestingHelperUtils;

Const
  (** An INI Section name for the dialogue settings. **)
  strProjectDlgSection = 'Project Dlg';
  (** An INI key for the dialogue top. **)
  strTopKey = 'Top';
  (** An INI key for the dialogue left. **)
  strLeftKey = 'Left';
  (** An INI key for the dialogue height. **)
  strHeightKey = 'Height';
  (** An INI key for the dialogue width. **)
  strWidthKey = 'Width';

(**

  This is an on click event handler for the GetvVersionInfo button.

  @precon  None.
  @postcon Extracts the version information from the IDE.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOptionsDialogue.btnGetVersionInfoClick(Sender: TObject);

Const
  strMsg = 'Are you sure you want to replace the current version information with the ' +
    'version information from the IDE?';
  strMajorVersion = 'MajorVersion';
  strMinorVersion = 'MinorVersion';
  strRelease = 'Release';
  strBuild = 'Build';
  strKeys = 'Keys';

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
  If MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes Then
    Exit;
  {$IFDEF DXE20}
  If FProject.ProjectOptions.QueryInterface(IOTAProjectOptionsConfigurations,
    POC) = S_OK Then
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

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Project Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOptionsDialogue.btnHelpClick(Sender: TObject);

Const
  strProjectOptions = 'ProjectOptions';

Begin
  HTMLHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strProjectOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the Browse Zip EXE button.

  @precon  None.
  @postcon Allows the user to select a zip executable.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOptionsDialogue.btnOpenEXEClick(Sender: TObject);

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
Procedure TfrmITHProjectOptionsDialogue.BuildChange(Sender: TObject; Button: TUDBtnType);

Const
  strFileVersion = 'FileVersion';

Var
  S : TStrings;

Begin
  S := vleVersionInfo.Strings;
  If S.IndexOfName(strFileVersion) > -1 Then
    S.Values[strFileVersion] :=
      Format('%d.%d.%d.%d', [upMajor.Position, upMinor.Position, upRelease.Position,
        upBuild.Position]);
End;

(**

  This is an on click event handler for the Enabled checkbox.

  @precon  None.
  @postcon Enables of disables the grouop box accordingly.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOptionsDialogue.chkEnabledClick(Sender: TObject);

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

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOps as a IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant

**)
Class Procedure TfrmITHProjectOptionsDialogue.Execute(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject);

ResourceString
  strProjectOptionsFor = 'Project Options for %s';

Var
  frm: TfrmITHProjectOptionsDialogue;

Begin
  frm := TfrmITHProjectOptionsDialogue.Create(Nil);
  Try
    frm.Caption := Format(strProjectOptionsFor, [TITHToolsAPIFunctions.GetProjectName(Project)]);
    frm.FProject := Project;
    frm.InitialiseOptions(GlobalOps, Project);
    frm.chkEnabledClick(Nil);
    If frm.ShowModal = mrOK Then
      frm.SaveOptions(GlobalOps, Project);
  Finally
    frm.Free;
  End;
End;

(**

  This method initialises the dialogue controls with information from the global options and the project 
  options.

  @precon  GlobalOps and Project must be valid instances.
  @postcon Initialises the dialogue controls with information from the global options and the project 
           options.

  @param   GlobalOps as a IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant

**)
Procedure TfrmITHProjectOptionsDialogue.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    Top    := iniFile.ReadInteger(strProjectDlgSection, strTopKey, (Screen.Height - Height) Div 2);
    Left   := iniFile.ReadInteger(strProjectDlgSection, strLeftKey, (Screen.Width - Width) Div 2);
    Height := iniFile.ReadInteger(strProjectDlgSection, strHeightKey, Height);
    Width  := iniFile.ReadInteger(strProjectDlgSection, strWidthKey, Width);
  Finally
    iniFile.Free;
  End;
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
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as a IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant

**)
Procedure TfrmITHProjectOptionsDialogue.SaveOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    iniFile.WriteInteger(strProjectDlgSection, strTopKey, Top);
    iniFile.WriteInteger(strProjectDlgSection, strLeftKey, Left);
    iniFile.WriteInteger(strProjectDlgSection, strHeightKey, Height);
    iniFile.WriteInteger(strProjectDlgSection, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
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
