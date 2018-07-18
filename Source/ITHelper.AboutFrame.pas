(**

  This modulel contains a frame for the root node of the BADI Options frame in the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Jul 2018

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
    lblInformation: TLabel;
    lblEurekaLog: TLabel;
    lblBuildDate: TLabel;
  Strict Private
  Strict Protected
    // IITHOptionsFrame
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
    Function IsValidated: Boolean;
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType: TITHDlgType = dtNA);
  Public
  End;

Implementation

Uses
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7;
  {$ENDIF}
  ITHelper.CommonFunctions, ITHelper.Constants;

{$R *.dfm}

(**

  This method initialises the options frame.

  @precon  None.
  @postcon Upudates the labels with the current build information.

  @nohint  GlobalOps Project, DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeAboutITHelper.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject; Const DlgType: TITHDlgType);

Const
  strBrowseAndDocIt = 'Integrated Testing Helper %d.%d%s';
  {$IFDEF DEBUG}
  strDEBUGBuild = 'DEBUG Build %d.%d.%d.%d';
  {$ELSE}
  strBuild = 'Build %d.%d.%d.%d';
  {$ENDIF}
  strBuildDateFmt = 'ddd dd/mmm/yyyy hh:nn';
  strBuildDate = 'Build Date: %s';
  {$IFDEF EUREKALOG_VER7}
  strEurekaLogStatus = 'EurekaLog is compiled into this version:'#13#10 +
    '  Installed:'#9'%s'#13#10 +
    '  Active:'#9#9'%s';
  {$ELSE}
  strEurekaLogStatus = 'EurekaLog is NOT compiled into this version.';
  {$ENDIF}

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
  {$IFDEF DEBUG}
  lblBuild.Caption := Format(strDEBUGBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor,
    recVersionInfo.FBugFix, recVersionInfo.FBuild]);
  lblBuild.Font.Color := clRed;
  {$ELSE}
  lblBuild.Caption := Format(strBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor,
    recVersionInfo.FBugFix, recVersionInfo.FBuild]);
  {$ENDIF}
  FileAge(strModuleName, dtDate);
  lblBuildDate.Caption := Format(strBuildDate, [FormatDateTime(strBuildDateFmt, dtDate)]);
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

  This method saves the optins frame.

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

Begin
End;

End.
