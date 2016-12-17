(**
  
  This module define a DLL which can be loaded in the BDS 2009 IDE and
  implement an IDE wizard.

  @Author  David Hoyle
  @Date    06 Aug 2011
  @Version 1.0

**)
library ITHelper2009;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R 'SplashScreenIcon.res' '..\SplashScreenIcon.RC'}
{$R 'ITHelperVersionInfo.res' 'ITHelperVersionInfo.RC'}
{$R 'ITHelperMenuIcons.res' '..\ITHelperMenuIcons.rc'}

uses
  ShareMem,
  ExceptionLog,
  SysUtils,
  Classes,
  TestingHelperWizard in '..\Source\TestingHelperWizard.pas',
  ConfigurationForm in '..\Source\ConfigurationForm.pas' {frmConfigure},
  TestingHelperUtils in '..\Source\TestingHelperUtils.pas',
  ProcessingForm in '..\Source\ProcessingForm.pas' {frmProcessing},
  AdditionalZipFilesForm in '..\Source\AdditionalZipFilesForm.pas' {Form1},
  ProgrammeInfoForm in '..\..\..\Library\ProgrammeInfoForm.pas' {frmProgrammeInfo},
  dghlibrary in '..\..\..\Library\dghlibrary.pas',
  ExternalProcessInfo in '..\..\..\Library\ExternalProcessInfo.pas',
  MSXML2_TLB in '..\..\..\LIBRARY\MSXML2_TLB.pas',
  checkforupdates in '..\..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  EnabledOptions in '..\Source\EnabledOptions.pas' {frmEnabledOptions},
  CheckForUpdatesOptionsForm in '..\..\..\Library\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions},
  ITHInitialiseOTAInterfaces in '..\Source\ITHInitialiseOTAInterfaces.pas',
  ProjectManagerMenuInterface in '..\Source\ProjectManagerMenuInterface.pas',
  IDENotifierInterface in '..\Source\IDENotifierInterface.pas',
  GlobalOptions in '..\Source\GlobalOptions.pas',
  FontDialogue in '..\Source\FontDialogue.pas' {frmFontDialogue},
  ZIPDialogue in '..\Source\ZIPDialogue.pas' {frmZIPDialogue},
  GlobalOptionsDialogue in '..\Source\GlobalOptionsDialogue.pas' {frmGlobalOptionsDialogue},
  ProjectOptionsDialogue in '..\Source\ProjectOptionsDialogue.pas' {frmProjectOptionsDialogue},
  About in '..\..\..\Library\About.pas' {frmAbout},
  DGHSpectrum in '..\..\..\Components\Source\DGHSpectrum.pas';

{$R *.res}

begin
end.


