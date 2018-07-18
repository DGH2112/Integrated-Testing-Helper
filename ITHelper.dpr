(**

  This module define a DLL which can be loaded in the BDS 2010 IDE and
  implement an IDE wizard.

  @Author  David Hoyle
  @Date    18 Jul 2018
  @Version 2.0

  @nocheck EmptyBEGINEND

  @todo    Create a wrapper for the ziplibrary and use that instead of an external zip programme.
  @todo    Consider adding a Project Notifier to handle rename / save as for the project .ITHelper file.
  @todo    Create a command-line app ITHelper.exe which can perform version control, zipping, etc from a
           .DPR (and implied .ITHelper) file.

**)
Library ITHelper;

{$R 'SplashScreenIcon.res' 'SplashScreenIcon.RC'}
{$R 'ITHelperVersionInfo.res' 'ITHelperVersionInfo.RC'}
{$R 'ITHelperMenuIcons.res' 'ITHelperMenuIcons.rc'}

{$INCLUDE 'Source\CompilerDefinitions.inc'}
{$INCLUDE 'Source\LibrarySuffixes.inc'}

uses
  ShareMem,
  SysUtils,
  Classes,
  ITHelper.Wizard in 'Source\ITHelper.Wizard.pas',
  ITHelper.TestingHelperUtils in 'Source\ITHelper.TestingHelperUtils.pas',
  ITHelper.AdditionalZipFilesForm in 'Source\ITHelper.AdditionalZipFilesForm.pas' {frmITHAdditionalZipFiles},
  ITHelper.EnabledOptions in 'Source\ITHelper.EnabledOptions.pas' {frmITHEnabledOptions},
  ITHelper.OTAInterfaces in 'Source\ITHelper.OTAInterfaces.pas',
  ITHelper.ProjectManagerMenuInterface in 'Source\ITHelper.ProjectManagerMenuInterface.pas',
  ITHelper.IDENotifierInterface in 'Source\ITHelper.IDENotifierInterface.pas',
  ITHelper.GlobalOptions in 'Source\ITHelper.GlobalOptions.pas',
  ITHelper.ProjectOptionsDialogue in 'Source\ITHelper.ProjectOptionsDialogue.pas' {frmITHProjectOptionsDialogue},
  ITHelper.SplashScreen in 'Source\ITHelper.SplashScreen.pas',
  ITHelper.CommonFunctions in 'Source\ITHelper.CommonFunctions.pas',
  ITHelper.Constants in 'Source\ITHelper.Constants.pas',
  ITHelper.ResourceStrings in 'Source\ITHelper.ResourceStrings.pas',
  ITHelper.AboutBox in 'Source\ITHelper.AboutBox.pas',
  ITHelper.ExternalProcessInfo in 'Source\ITHelper.ExternalProcessInfo.pas',
  ITHelper.ProcessingForm in 'Source\ITHelper.ProcessingForm.pas' {frmITHProcessing},
  ITHelper.ProgrammeInfoForm in 'Source\ITHelper.ProgrammeInfoForm.pas' {frmITHProgrammeInfo},
  ITHelper.CustomMessages in 'Source\ITHelper.CustomMessages.pas',
  ITHelper.PascalParsing in 'Source\ITHelper.PascalParsing.pas',
  ITHelper.Interfaces in 'Source\ITHelper.Interfaces.pas',
  ITHelper.Types in 'Source\ITHelper.Types.pas',
  ITHelper.ProjectOptions in 'Source\ITHelper.ProjectOptions.pas',
  ITHelper.ZIPManager in 'Source\ITHelper.ZIPManager.pas',
  ITHelper.ResponseFile in 'Source\ITHelper.ResponseFile.pas',
  ITHelper.MessageManager in 'Source\ITHelper.MessageManager.pas',
  ITHelper.VersionManager in 'Source\ITHelper.VersionManager.pas',
  ITHelper.ProjectOptionsFrame in 'Source\ITHelper.ProjectOptionsFrame.pas' {frameProjectOptions: TFrame},
  ITHelper.ExternalToolsFrame in 'Source\ITHelper.ExternalToolsFrame.pas' {frameExternalTools: TFrame},
  ITHelper.ZIPFrame in 'Source\ITHelper.ZIPFrame.pas' {frameZipping: TFrame},
  ITHelper.GlobalOptionsFrame in 'Source\ITHelper.GlobalOptionsFrame.pas' {frameGlobalOptions: TFrame},
  ITHelper.AddInOptions in 'Source\ITHelper.AddInOptions.pas',
  ITHelper.AboutFrame in 'Source\ITHelper.AboutFrame.pas' {frameAboutITHelper: TFrame},
  ITHelper.FontFrame in 'Source\ITHelper.FontFrame.pas' {frameFonts: TFrame};

{$R *.res}

Begin
End.

