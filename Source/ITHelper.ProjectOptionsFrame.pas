(**
  
  This module contains the project options (version control, etc) in a frame so that it can be placed 
  in the project options dialgoue.

  @Author  David Hoyle
  @Version 1.193
  @Date    28 Mar 2020
  
  @license

    Integrated Testing helper is a RAD Studio plug-in for running pre and post
    build processes.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)

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
  ExtCtrls,
  ToolsAPI,
  Generics.Collections,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A frame to contain the project options. **)
  TframeProjectOptions = Class(TFrame, IITHOptionsFrame)
    lblResExts: TLabel;
    lblVersionInfo: TLabel;
    btnOpenEXE: TButton;
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
    chkIncludeInProject: TCheckBox;
    chkCompileWithBRCC32: TCheckBox;
    edtResourceName: TEdit;
    chkEnabled: TCheckBox;
    dlgOpenEXE: TOpenDialog;
    gpnlVerInfo: TGridPanel;
    gpnlResource: TGridPanel;
    gpnlCopyVerInfo: TGridPanel;
    lvIncrementOnCompileMode: TListView;
    lblIncrementOnCompileMode: TLabel;
    btnGetVersionInfo: TButton;
    Procedure btnOpenEXEClick(Sender: TObject);
    Procedure chkEnabledClick(Sender: TObject);
    Procedure BuildChange(Sender: TObject; Button: TUDBtnType);
    Procedure btnGetVersionInfoClick(Sender: TObject);
    procedure lvIncrementOnCompileModeCustomDrawSubItem(Sender: TCustomListView;
        Item: TListItem; SubItem: Integer; State: TCustomDrawState; var
        DefaultDraw: Boolean);
    procedure lvIncrementOnCompileModeMouseUp(Sender: TObject; Button:
        TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lvIncrementOnCompileModeResize(Sender: TObject);
  Strict Private
    Type
      (** A record to describe the information store for the list of compile mode configuations. **)
      TITHConfigCompileModes = Record
        FConfigName  : String;
        FCompileMode : Array[TOTACompileMode] Of Boolean;
      End;
    (** An enumerate to define the columns of the increment on compile mode list view. **)
    TITHCompileModeField = (cmfConfigName, cmfMake, cmfBuild, cmfCheck, cmfMakeUnit);
  Strict Private
    FProject : IOTAProject;
    FCompileModes : TList<TITHConfigCompileModes>;
  {$IFDEF D2010} Strict {$ENDIF D2010} Protected
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Function  IsValidated : Boolean;
    Procedure InitialiseCompileModes(Const ProjectOps : IITHProjectOptions);
    Procedure FinaliseCompileModes(Const ProjectOps : IITHProjectOptions);
    Procedure PopulateIncOnCompileModes;
    Function  FindConfig(Const strConfigName : String) : Integer;
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
  FCompileModes := TList<TITHConfigCompileModes>.Create;
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
  FCompileModes.Free;
  Inherited;
End;

(**

  This method saves the increment on compile mode settings to the ITHelper INI file.

  @precon  ProjectOps must be a valid instance.
  @postcon The increment on compile mode settings are saved.

  @param   ProjectOps as an IITHProjectOptions as a constant

**)
Procedure TframeProjectOptions.FinaliseCompileModes(Const ProjectOps: IITHProjectOptions);

Var
  iConfig: Integer;
  R : TITHConfigCompileModes;
  
Begin
  For iConfig := 0 To FCompileModes.Count - 1 Do
    Begin
      R := FCompileModes[iConfig];
      ProjectOps.IncOnCompile[cmOTAMake, R.FConfigName] := R.FCompileMode[cmOTAMake];
      ProjectOps.IncOnCompile[cmOTABuild, R.FConfigName] := R.FCompileMode[cmOTABuild];
      ProjectOps.IncOnCompile[cmOTACheck, R.FConfigName] := R.FCompileMode[cmOTACheck];
      ProjectOps.IncOnCompile[cmOTAMakeUnit, R.FConfigName] := R.FCompileMode[cmOTAMakeUnit];
    End;
End;

(**

  This method returns the index of the named configuration in the compile mode list.

  @precon  None.
  @postcon Returns the index of the named configuration else returns -1 if not found.

  @param   strConfigName as a String as a constant
  @return  an Integer

**)
Function TframeProjectOptions.FindConfig(Const strConfigName : String) : Integer;

Var
  iConfig : Integer;

Begin
  Result := -1;
  For iConfig := 0 To FCompileModes.Count - 1 Do
    If CompareText(FCompileModes[iConfig].FConfigName, strConfigName) = 0 Then
      Begin
        Result := iConfig;
        Break;
      End;
End;

(**

  This method creates the list of increment on compile mode settings for each configuration.

  @precon  ProjectOps must be a valid instances.
  @postcon The list of compile modes for each configuration is created.

  @param   ProjectOps as an IITHProjectOptions as a constant

**)
Procedure TframeProjectOptions.InitialiseCompileModes(Const ProjectOps: IITHProjectOptions);

Var
  POC : IOTAProjectOptionsConfigurations;
  iConfig: Integer;
  R : TITHConfigCompileModes;
  
Begin
  If Supports(FProject.ProjectOptions, IOTAProjectOptionsConfigurations, POC) Then
    For iConfig := 0 To POC.ConfigurationCount - 1 Do
      Begin
        R.FConfigName := POC.Configurations[iConfig].Name;
        R.FCompileMode[cmOTAMake] := ProjectOps.IncOnCompile[cmOTAMake, R.FConfigName];
        R.FCompileMode[cmOTABuild] := ProjectOps.IncOnCompile[cmOTABuild, R.FConfigName];
        R.FCompileMode[cmOTACheck] := ProjectOps.IncOnCompile[cmOTACheck, R.FConfigName];
        R.FCompileMode[cmOTAMakeUnit] := ProjectOps.IncOnCompile[cmOTAMakeUnit, R.FConfigName];
        FCompileModes.Add(R);
      End;
  PopulateIncOnCompileModes;
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
  ITS : IOTAIDEThemingServices;

Begin
  FProject := Project;
  ProjectOps := GlobalOps.ProjectOptions(Project);
  Try
    InitialiseCompileModes(ProjectOps);
    edtVersionInfo.Text                := ProjectOps.CopyVerInfo;
    edtResExts.Text                    := ProjectOps.ResExtExc;
    chkEnabled.Checked                 := ProjectOps.IncITHVerInfo;
    chkIncludeInProject.Checked        := ProjectOps.IncResInProj;
    chkCompileWithBRCC32.Checked       := ProjectOps.CompileRes;
    upMajor.Position                   := ProjectOps.Major;
    upMinor.Position                   := ProjectOps.Minor;
    upRelease.Position                 := ProjectOps.Release;
    upBuild.Position                   := ProjectOps.Build;
    edtResourceName.Text               := ProjectOps.ResourceName;
    vleVersionInfo.Strings.Assign(ProjectOps.VerInfo);
  Finally
    ProjectOps := Nil;
  End;
  chkEnabledClick(Nil);
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    Begin 
      vleVersionInfo.Ctl3D := False;
      vleVersionInfo.DrawingStyle := gdsGradient;
      vleVersionInfo.FixedColor := ITS.StyleServices.GetSystemColor(clBtnFace);
      vleVersionInfo.Color := ITS.StyleServices.GetSystemColor(clWindow);
      vleVersionInfo.GradientStartColor := ITS.StyleServices.GetSystemColor(clBtnFace);
      vleVersionInfo.GradientEndColor := ITS.StyleServices.GetSystemColor(clBtnFace);
      vleVersionInfo.Font.Color := ITS.StyleServices.GetSystemColor(clWindowText);
    End;
  {$ENDIF DXE102}
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

  This is an on custom draw sub item event handler for the conpile mode list view.

  @precon  None.
  @postcon Colours No red and Yes green.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   SubItem     as an Integer
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TframeProjectOptions.lvIncrementOnCompileModeCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState; Var DefaultDraw: Boolean);

Const
  iLightRed = $8080FF;
  iLightGreen = $80FF80;

Var
  iConfig: Integer;
  ITS : IOTAIDEThemingServices;

Begin
  Sender.Canvas.Font.Color := clWindowText;
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    If cdsSelected In State Then
      Sender.Canvas.Font.Color := ITS.StyleServices.GetSystemColor(clHighlightText)
    Else
      Sender.Canvas.Font.Color := ITS.StyleServices.GetSystemColor(clWindowText);
  If Not Item.Selected Then
    Sender.Canvas.Font.Color := clBlack;
  {$ENDIF DXE102}
  Sender.Canvas.Brush.Color := iLightRed;
  iConfig := FindConfig(Item.Caption);
  If iConfig > -1 Then
    If FCompileModes[iConfig].FCompileMode[TOTACompileMode(Byte(Pred(SubItem)))] Then
      Sender.Canvas.Brush.Color := iLightGreen;
End;

(**

  This is an on mouse up event handler for the increment on compile mode list view.

  @precon  None.
  @postcon Toggles the enabled / disabled setting of a compile mode on a configuration.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TframeProjectOptions.lvIncrementOnCompileModeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  (**

    This method returns the field that was clicked over.

    @precon  None.
    @postcon Returns the field that was clicked over.

    @param   X as an Integer as a constant
    @return  a TITHCompileModeField

  **)
  Function ColumnClicked(Const X : Integer) : TITHCompileModeField;

  Var
    iPosition: TWidth;
    eCompileMode: TITHCompileModeField;

  Begin
    Result := cmfConfigName;
    iPosition := 0;
    If X > iPosition Then
      For eCompileMode := Low(TITHCompileModeField) To High(TITHCompileModeField) Do
        Begin
          Inc(iPosition, lvIncrementOnCompileMode.Columns[Integer(eCompileMode)].Width);
          If X < iPosition Then
            Begin
              Result := eCompileMode;
              Break;
            End;
        End;
  End;

Var
  Item : TListItem;
  iConfig : Integer;
  R: TITHConfigCompileModes;
  eCompileMode: TITHCompileModeField;

Begin
  Item := lvIncrementOnCompileMode.GetItemAt(X, Y);
  If Assigned(Item) Then
    Begin
      eCompileMode := ColumnClicked(X);
      If eCompileMode In [cmfMake..cmfMakeUnit] Then
        Begin
          iConfig := FindConfig(Item.Caption);
          If iConfig > -1 Then
            Begin
              R := FCompileModes[iConfig];
              Case eCompileMode Of
                cmfMake:     R.FCompileMode[cmOTAMake]     := Not R.FCompileMode[cmOTAMake];
                cmfBuild:    R.FCompileMode[cmOTABuild]    := Not R.FCompileMode[cmOTABuild];
                cmfCheck:    R.FCompileMode[cmOTACheck]    := Not R.FCompileMode[cmOTACheck];
                cmfMakeUnit: R.FCompileMode[cmOTAMakeUnit] := Not R.FCompileMode[cmOTAMakeUnit];
              End;
              FCompileModes[iConfig] := R;
            End;
        End;
      PopulateIncOnCompileModes;
    End;  
End;

(**

  This is an on resise event handler for the Increment on Compile Mode listview.

  @precon  None.
  @postcon The caption columns is resized to fit all columns on the screen.

  @param   Sender as a TObject

**)
Procedure TframeProjectOptions.lvIncrementOnCompileModeResize(Sender: TObject);

Const
  iColumnWidth = 100;
  iScrollbarWidth = 22;
  iMultiplier = 4;

Begin
  lvIncrementOnCompileMode.Columns[Integer(cmfMake)].Width := iColumnWidth;
  lvIncrementOnCompileMode.Columns[Integer(cmfBuild)].Width := iColumnWidth;
  lvIncrementOnCompileMode.Columns[Integer(cmfCheck)].Width := iColumnWidth;
  lvIncrementOnCompileMode.Columns[Integer(cmfMakeUnit)].Width := iColumnWidth;
  lvIncrementOnCompileMode.Columns[Integer(cmfConfigName)].Width :=
    lvIncrementOnCompileMode.Width - iColumnWidth * iMultiplier - iScrollbarWidth;
End;

(**

  This method renders the list of increment of compile modes.

  @precon  None.
  @postcon The increment on compile mode list view is populated with configurations and settings.

**)
Procedure TframeProjectOptions.PopulateIncOnCompileModes;

Const
  astrBoolean : Array[False..True] Of String = ('No', 'Yes');

Var
  Item: TListItem;
  Config : TITHConfigCompileModes;
  eCompileMode: TOTACompileMode;
  strSelected : String;

Begin
  lvIncrementOnCompileMode.Items.BeginUpdate;
  Try
    If Assigned(lvIncrementOnCompileMode.Selected) Then
      strSelected := lvIncrementOnCompileMode.Selected.Caption;
    lvIncrementOnCompileMode.Clear;
    For Config In FCompileModes Do
      Begin
        Item := lvIncrementOnCompileMode.Items.Add;
        Item.Caption := Config.FConfigName;
        For eCompileMode := Low(TOTACompileMode) To High(TOTACompileMode) Do
          Item.SubItems.Add(astrBoolean[Config.FCompileMode[eCompileMode]]);
        If Item.Caption = strSelected Then
          item.Selected := True;
      End;
  Finally
    lvIncrementOnCompileMode.Items.EndUpdate;
  End;
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
    FinaliseCompileModes(ProjectOps);
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
