(**

  This module contains a class which represents a form on which the project options for
  the active project can be edited.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Jul 2012

**)
Unit ProjectOptionsDialogue;

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
  GlobalOptions,
  Grids,
  ValEdit,
  ComCtrls;

Type
  (** A class to represent the project options form. **)
  TfrmProjectOptionsDialogue = Class(TForm)
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
    Procedure InitialiseOptions(GlobalOps: TGlobalOptions; Project: IOTAProject);
    Procedure SaveOptions(GlobalOps: TGlobalOptions; Project: IOTAProject);
  Public
    { Public declarations }
    Class Procedure Execute(GlobalOps: TGlobalOptions; Project: IOTAProject);
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  TestingHelperUtils;

(**

  This is an on click event handler for the GetvVersionInfo button.

  @precon  None.
  @postcon Extracts the version information from the IDE.

  @param   Sender as a TObject

**)
Procedure TfrmProjectOptionsDialogue.btnGetVersionInfoClick(Sender: TObject);

Const
  strMsg = 'Are you sure you want to replace the current version information with the ' +
    'version information from the IDE?';

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
    upMajor.Position := PO.Values['MajorVersion'];
    upMinor.Position := PO.Values['MinorVersion'];
    upRelease.Position := PO.Values['Release'];
    upBuild.Position := PO.Values['Build'];
    i := VarAsType(PO.Values['Keys'], varInteger);
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
Procedure TfrmProjectOptionsDialogue.btnHelpClick(Sender: TObject);

Begin
  HTMLHelp(0, PChar(ITHHTMLHelpFile('ProjectOptions')), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the Browse Zip EXE button.

  @precon  None.
  @postcon Allows the user to select a zip executable.

  @param   Sender as a TObject

**)
Procedure TfrmProjectOptionsDialogue.btnOpenEXEClick(Sender: TObject);

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
Procedure TfrmProjectOptionsDialogue.BuildChange(Sender: TObject; Button: TUDBtnType);

Var
  S : TStrings;

Begin
  S := vleVersionInfo.Strings;
  If S.IndexOfName('FileVersion') > -1 Then
    S.Values['FileVersion'] :=
      Format('%d.%d.%d.%d', [upMajor.Position, upMinor.Position, upRelease.Position,
        upBuild.Position]);
End;

(**

  This is an on click event handler for the Enabled checkbox.

  @precon  None.
  @postcon Enables of disables the grouop box accordingly.

  @param   Sender as a TObject

**)
Procedure TfrmProjectOptionsDialogue.chkEnabledClick(Sender: TObject);

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

  @param   GlobalOps as a TGlobalOptions
  @param   Project   as an IOTAProject

**)
Class Procedure TfrmProjectOptionsDialogue.Execute(GlobalOps: TGlobalOptions;
  Project: IOTAProject);

Begin
  With TfrmProjectOptionsDialogue.Create(Nil) Do
    Try
      Caption := Format('Project Options for %s', [GetProjectName(Project)]);
      FProject := Project;
      InitialiseOptions(GlobalOps, Project);
      chkEnabledClick(Nil);
      If ShowModal = mrOK Then
        SaveOptions(GlobalOps, Project);
    Finally
      Free;
    End;
End;

(**

  This method initialises the dialogue controls with information from the global options
  and the project options.

  @precon  GlobalOps and Project must be valid instances.
  @postcon Initialises the dialogue controls with information from the global options
           and the project options.

  @param   GlobalOps as a TGlobalOptions
  @param   Project   as an IOTAProject

**)
Procedure TfrmProjectOptionsDialogue.InitialiseOptions(GlobalOps: TGlobalOptions;
  Project: IOTAProject);

Begin
  With TMemIniFile.Create(GlobalOps.INIFileName) Do
    Try
      Top    := ReadInteger('Project Dlg', 'Top', (Screen.Height - Height) Div 2);
      Left   := ReadInteger('Project Dlg', 'Left', (Screen.Width - Width) Div 2);
      Height := ReadInteger('Project Dlg', 'Height', Height);
      Width  := ReadInteger('Project Dlg', 'Width', Width);
    Finally
      Free;
    End;
  With GlobalOps.ProjectOptions(Project) Do
    Try
      chkIncrementBuildOnCompile.Checked := IncOnCompile;
      edtVersionInfo.Text          := CopyVerInfo;
      edtResExts.Text              := ResExtExc;
      chkEnabled.Checked           := IncITHVerInfo;
      chkIncludeInProject.Checked  := IncResInProj;
      chkCompileWithBRCC32.Checked := CompileRes;
      upMajor.Position             := Major;
      upMinor.Position             := Minor;
      upRelease.Position           := Release;
      upBuild.Position             := Build;
      edtResourceName.Text         := ResourceName;
      vleVersionInfo.Strings.Assign(VerInfo);
    Finally
      Free;
    End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as a TGlobalOptions
  @param   Project   as an IOTAProject

**)
Procedure TfrmProjectOptionsDialogue.SaveOptions(GlobalOps: TGlobalOptions;
  Project: IOTAProject);

Begin
  With TMemIniFile.Create(GlobalOps.INIFileName) Do
    Try
      WriteInteger('Project Dlg', 'Top', Top);
      WriteInteger('Project Dlg', 'Left', Left);
      WriteInteger('Project Dlg', 'Height', Height);
      WriteInteger('Project Dlg', 'Width', Width);
      UpdateFile;
    Finally
      Free;
    End;
  With GlobalOps.ProjectOptions(Project) Do
    Try
      IncOnCompile := chkIncrementBuildOnCompile.Checked;
      CopyVerInfo := edtVersionInfo.Text;
      ResExtExc := edtResExts.Text;
      IncITHVerInfo := chkEnabled.Checked;
      IncResInProj := chkIncludeInProject.Checked;
      CompileRes := chkCompileWithBRCC32.Checked;
      Major := upMajor.Position;
      Minor := upMinor.Position;
      Release := upRelease.Position;
      Build := upBuild.Position;
      ResourceName := edtResourceName.Text;
      VerInfo.Assign(vleVersionInfo.Strings);
    Finally
      Free;
    End;
End;

End.
