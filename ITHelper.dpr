(**

  This module define a DLL which can be loaded in the BDS 2010 IDE and
  implement an IDE wizard.

  @Author  David Hoyle
  @Date    02 Mar 2018
  @Version 2.0

  @nocheck EmptyBEGINEND

  @todo    Change ALL (BorlandIDEServices As Xxxx) for Supports()
  @todo    Change DGHCreateProcess to take an IITHMessageManager interface so all process messages are 
           handled by the interface.
  @todo    Put the project options back into a single tabbed dialogue (remember NOT to use listviews
           to store information).
  @todo    Put global options into the IDEs Options dialogue.
  @todo    Create a wrapper for the ziplibrary and use that instead of an external zip programme.

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
  ITHelper.ConfigurationForm in 'Source\ITHelper.ConfigurationForm.pas' {frmITHConfigureDlg},
  ITHelper.TestingHelperUtils in 'Source\ITHelper.TestingHelperUtils.pas',
  ITHelper.AdditionalZipFilesForm in 'Source\ITHelper.AdditionalZipFilesForm.pas' {frmITHAdditionalZipFiles},
  ITHelper.EnabledOptions in 'Source\ITHelper.EnabledOptions.pas' {frmITHEnabledOptions},
  ITHelper.OTAInterfaces in 'Source\ITHelper.OTAInterfaces.pas',
  ITHelper.ProjectManagerMenuInterface in 'Source\ITHelper.ProjectManagerMenuInterface.pas',
  ITHelper.IDENotifierInterface in 'Source\ITHelper.IDENotifierInterface.pas',
  ITHelper.GlobalOptions in 'Source\ITHelper.GlobalOptions.pas',
  ITHelper.FontDialogue in 'Source\ITHelper.FontDialogue.pas' {frmITHFontDialogue},
  ITHelper.ZIPDialogue in 'Source\ITHelper.ZIPDialogue.pas' {frmITHZIPDialogue},
  ITHelper.GlobalOptionsDialogue in 'Source\ITHelper.GlobalOptionsDialogue.pas' {frmITHGlobalOptionsDialogue},
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
  ITHelper.VersionManager in 'Source\ITHelper.VersionManager.pas';

{$R *.res}

Begin
End.

