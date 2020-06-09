(**

  This module contains a class which represents a form on which the project options for
  the active project can be edited.

  @Author  David Hoyle
  @Version 1.312
  @Date    09 Jun 2020

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
Unit ITHelper.ProjectOpsDlg;

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
  ComCtrls,
  ITHelper.ProjectOptionsFrame,
  ITHelper.ExternalToolsFrame,
  ITHelper.ZIPFrame, System.ImageList, Vcl.ImgList;

Type
  (** An enumerate to define the page to display on opening. **)
  TITHProjectOptionType = (potProjectOptions, potBeforeCompile, potAfterCompile, potZipping);

  (** An enumerate to describe the pages of the options dialogue. **)
  TITHOptionsPage = (opProjectOptions, opBeforeCompileTools, opAfterCompileTools, opZipping);
  
  (** A class to represent the project options form. **)
  TfrmITHProjectOpsDlg = Class(TForm)
    pgcProjectOptions: TPageControl;
    tabProjectOptions: TTabSheet;
    tabBeforeCompileTools: TTabSheet;
    tabAfterCompileTools: TTabSheet;
    tabZipping: TTabSheet;
    btnHelp: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ilButtons: TImageList;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  Strict Private
    FProjectOptions          : IITHOptionsFrame;
    FBeforeCompileTools      : IITHOptionsFrame;
    FAfterCompileTools       : IITHOptionsFrame;
    FZipping                 : IITHOptionsFrame;
  Strict Protected
    Procedure LoadSettings(Const GlobalOps: IITHGlobalOptions);
    Procedure SaveSettings(Const GlobalOps: IITHGlobalOptions);
  Public
    { Public declarations }
    Class Procedure Execute(Const ProjectOptionType : TITHProjectOptionType;
      Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  IniFiles,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  ITHelper.Types,
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

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Project Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOpsDlg.btnHelpClick(Sender: TObject);

Const
  strProjectOptions = 'ProjectOptions';
  strCompilationTools = 'CompilationTools';
  strZIPOptions = 'ZIPOptions';

Var
  strTopic : String;

Begin
  Case TITHOptionsPage(pgcProjectOptions.ActivePageIndex) Of
    opProjectOptions:                          strTopic := strProjectOptions;
    opBeforeCompileTools, opAfterCompileTools: strTopic := strCompilationTools;
    opZipping:                                 strTopic := strZIPOptions;  
  End;
  HtmlHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strTopic)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Validates the frames in the form.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOpsDlg.btnOKClick(Sender: TObject);

Begin
  If Not (
    FProjectOptions.IsValidated And
    FBeforeCompileTools.IsValidated And
    FAfterCompileTools.IsValidated And
    FZipping.Isvalidated) Then
    ModalResult := mrNone;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   ProjectOptionType as a TITHProjectOptionType as a constant
  @param   GlobalOps         as an IITHGlobalOptions as a constant
  @param   Project           as an IOTAProject as a constant

**)
Class Procedure TfrmITHProjectOpsDlg.Execute(Const ProjectOptionType : TITHProjectOptionType;
  Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);

ResourceString
  strProjectOptionsFor = 'Project Options for %s';

Var
  frm: TfrmITHProjectOpsDlg;

Begin
  frm := TfrmITHProjectOpsDlg.Create(Nil);
  Try
    TITHToolsAPIFunctions.RegisterFormClassForTheming(TfrmITHProjectOpsDlg, frm);
    frm.Caption := Format(strProjectOptionsFor, [TITHToolsAPIFunctions.GetProjectName(Project)]);
    frm.LoadSettings(GlobalOps);
    frm.FProjectOptions.InitialiseOptions(GlobalOps, Project);
    frm.FBeforeCompileTools.InitialiseOptions(GlobalOps, Project, dtBefore);
    frm.FAfterCompileTools.InitialiseOptions(GlobalOps, Project, dtAfter);
    frm.FZipping.InitialiseOptions(GlobalOps, Project);
    Case ProjectOptionType Of
      potProjectOptions: frm.pgcProjectOptions.ActivePage := frm.tabProjectOptions;
      potBeforeCompile: frm.pgcProjectOptions.ActivePage := frm.tabBeforeCompileTools;
      potAfterCompile: frm.pgcProjectOptions.ActivePage := frm.tabAfterCompileTools;
      potZipping: frm.pgcProjectOptions.ActivePage := frm.tabZipping;
    End;
    If frm.ShowModal = mrOK Then
      Begin
        frm.SaveSettings(GlobalOps);
        frm.FProjectOptions.SaveOptions(GlobalOps, Project);
        frm.FBeforeCompileTools.SaveOptions(GlobalOps, Project, dtBefore);
        frm.FAfterCompileTools.SaveOptions(GlobalOps, Project, dtAfter);
        frm.FZipping.SaveOptions(GlobalOps, Project);
      End;
  Finally
    frm.Free;
  End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmITHProjectOptionsDialogue class.

  @precon  None.
  @postcon Creates the options frame dynamically (so any changes in the frames take effect in this dlg).

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOpsDlg.FormCreate(Sender: TObject);

ResourceString
  strIITHOptionsFrameNotSupported = 'IITHOptionsFrame not supported!';

Const
  strProjectOptionsFrameName = 'frameProjectOptionsFrame';
  strBeforeCompileToolsName = 'frameBeforeCompileTools';
  strAfterCompileToolsName = 'frameAfterCompileTools';
  strZippingName = 'frameZipping';

Var
  F: Tframe;
  {$IFDEF DXE102}
  ITS : IOTAIDEThemingServices;
  {$ENDIF DEX102}

Begin
  F := TframeProjectOptions.Create(Self);
  F.Name := strProjectOptionsFrameName;
  F.Parent := tabProjectOptions;
  F.Align := alClient;
  {$IFDEF DXE102}
  F.ParentBackground := False;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    F.Color := ITS.StyleServices.GetSystemColor(clBtnFace);
  {$ENDIF DXE102}
  If Not Supports(F, IITHOptionsFrame, FProjectOptions) Then
    Raise Exception.Create(strIITHOptionsFrameNotSupported);
  F := TframeExternalTools.Create(Self);
  F.Name := strBeforeCompileToolsName;
  F.Parent := tabBeforeCompileTools;
  F.Align := alClient;
  {$IFDEF DXE102}
  F.ParentBackground := False;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    F.Color := ITS.StyleServices.GetSystemColor(clBtnFace);
  {$ENDIF DXE102}
  If Not Supports(F, IITHOptionsFrame, FBeforeCompileTools) Then
    Raise Exception.Create(strIITHOptionsFrameNotsupported);
  F := TframeExternalTools.Create(Self);
  F.Name := strAfterCompileToolsName;
  F.Parent := tabAfterCompileTools;
  F.Align := alClient;
  {$IFDEF DXE102}
  F.ParentBackground := False;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    F.Color := ITS.StyleServices.GetSystemColor(clBtnFace);
  {$ENDIF DXE102}
  If Not Supports(F, IITHOptionsFrame, FAfterCompileTools) Then
    Raise Exception.Create(strIITHOptionsFrameNotSupported);
  F := TframeZipping.Create(Self);
  F.Name := strZippingName;
  F.Parent := tabZipping;
  F.Align := alClient;
  {$IFDEF DXE102}
  F.ParentBackground := False;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) And ITS.IDEThemingEnabled Then
    F.Color := ITS.StyleServices.GetSystemColor(clBtnFace);
  {$ENDIF DXE102}
  If Not Supports(F, IITHOptionsFrame, FZipping) Then
    Raise Exception.Create(strIITHOptionsFrameNotSupported);
End;

(**

  This method loads the position settings for the options dialogue from the ini file.

  @precon  None.
  @postcon The dialogue is positioned on the screen where it was last time.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Procedure TfrmITHProjectOpsDlg.LoadSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

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
End;

(**

  This method saves the current position of the options dialogue to the ini file.

  @precon  None.
  @postcon The options dialogue position and size is saved to the ini file.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Procedure TfrmITHProjectOpsDlg.SaveSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

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
End;

End.
