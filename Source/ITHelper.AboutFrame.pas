(**

  This module contains a frame for the root node of the BADI Options frame in the IDE.

  @Author  David Hoyle
  @Version 1.002
  @Date    21 Nov 2021

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
Unit ITHelper.AboutFrame;

Interface

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
  StdCtrls,
  ExtCtrls,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A class to represent the options frame. **)
  TframeAboutITHelper = Class(TFrame, IITHOptionsFrame)
    lblITHelper: TLabel;
    lblAuthor: TLabel;
    lblBuild: TLabel;
    lblPleaseSelect: TLabel;
    lblEurekaLog: TLabel;
    lblBuildDate: TLabel;
    pnlFudgePanel: TPanel;
    mmoInformation: TMemo;
  Strict Private
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IITHOptionsFrame
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
    Function IsValidated: Boolean;
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  {$ENDIF}
  ITHelper.CommonFunctions,
  ITHelper.Constants;

{$R *.dfm}

(**

  A constructor for the TframeAboutITHelper class.

  @precon  None.
  @postcon Does nothing but used for code site tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner

  @param   AOwner as a TComponent

**)
Constructor TframeAboutITHelper.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
End;

(**

  A destructor for the TframeAboutITHelper class.

  @precon  None.
  @postcon Does nothing but used for code site tracing.

**)
Destructor TframeAboutITHelper.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method initialises the options frame.

  @precon  None.
  @postcon Updates the labels with the current build information.

  @nohint  GlobalOps Project, DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeAboutITHelper.InitialiseOptions(Const GlobalOps: IITHGlobalOptions; //FI:O804
  Const Project: IOTAProject; Const DlgType: TITHDlgType); //FI:O804

ResourceString
  strLicense =
    'A RAD Studio IDE plug-in to help you manage the application build process:'#13#10 +
    ' * Manage Version Control;'#13#10 +
    ' * Run tools before or after a compilation;'#13#10 +
    ' * ZIP the project for backups, uploads and archiving.'#13#10 +
    ''#13#10 +
    'Integrated Testing helper is a RAD Studio plug-in for running pre and post build processes.'#13#10 +
    ''#13#10 +
    'Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)'#13#10 +
    ''#13#10 +
    'This program is free software: you can redistribute it and/or modify it under the ' +
    'terms of the GNU General Public License as published by the Free Software ' +
    'Foundation, either version 3 of the License, or (at your option) any later version.'#13#10 +
    ''#13#10 +
    'This program is distributed in the hope that it will be useful, but WITHOUT ANY ' +
    'WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS ' +
    'FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.'#13#10 +
    ''#13#10 +
    'You should have received a copy of the GNU General Public License along with this ' +
    'program. If not, see <https://www.gnu.org/licenses/>.';
  strBrowseAndDocIt = 'Integrated Testing Helper %d.%d%s';
  {$IFDEF DEBUG}
  strBuild = 'DEBUG Build %d.%d.%d.%d';
  {$ELSE}
  strBuild = 'Build %d.%d.%d.%d';
  {$ENDIF}
  strBuildDate = 'Build Date: %s';
  {$IFDEF EUREKALOG_VER7}
  strEurekaLogStatus = 'EurekaLog is compiled into this version:'#13#10 +
    '  Installed:'#9'%s'#13#10 +
    '  Active:'#9#9'%s';
  {$ELSE}
  strEurekaLogStatus = 'EurekaLog is NOT compiled into this version.';
  {$ENDIF}

Const
  strBuildDateFmt = 'ddd dd/mmm/yyyy hh:nn';

Var
  strModuleName : String;
  recVersionInfo : TITHVersionInfo;
  dtDate : TDateTime;
  iSize : Integer;

Begin
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  BuildNumber(strModuleName, recVersionInfo);
  lblITHelper.Caption := Format(strBrowseAndDocIt, [recVersionInfo.FMajor, recVersionInfo.FMinor,
    strRevisions[Succ(recVersionInfo.FBugFix)]]);
  lblBuild.Caption := Format(strBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor,
    recVersionInfo.FBugFix, recVersionInfo.FBuild]);
  FileAge(strModuleName, dtDate);
  lblBuildDate.Caption := Format(strBuildDate, [FormatDateTime(strBuildDateFmt, dtDate)]);
  mmoInformation.lines.Text := strLicense;
  {$IFDEF EUREKALOG_VER7}
  lblEurekaLog.Caption := Format(strEurekaLogStatus, [
    BoolToStr(ExceptionLog7.IsEurekaLogInstalled, True),
    BoolToStr(ExceptionLog7.IsEurekaLogActive, True)
  ]);
  lblEurekaLog.Font.Color := clGreen;
  {$ELSE}
  lblEurekaLog.Caption := strEurekaLogStatus;
  lblEurekaLog.Font.Color := clRed;
  {$ENDIF}
End;

(**

  This method validates the options frame.

  @precon  None.
  @postcon Nothing to validate here.

  @return  a Boolean

**)
Function TframeAboutITHelper.IsValidated: Boolean;

Begin
  Result := True;
End;

(**

  This method saves the options frame.

  @precon  None.
  @postcon Nothing to save here.

  @nocheck EmptyMethod
  @nohint  GlobalOps Project, DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeAboutITHelper.SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject;
  Const DlgType: TITHDlgType);

Begin //FI:W519
End;

End.

