(**

  This module contains a form for configuring the parts of the process that
  are to be enabled.

  @Author  David Hoyle
  @Version 1.0
  @Date    09 Jul 2012

**)
Unit EnabledOptions;

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
  Buttons,
  GlobalOptions;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

Type
  (** A form to represent the form interface. **)
  TfrmEnabledOptions = Class(TForm)
    chkEnable: TCheckBox;
    gbxOptions: TGroupBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    chkBefore: TCheckBox;
    chkAfter: TCheckBox;
    chkZip: TCheckBox;
    chkIncBuild: TCheckBox;
    chkBuildRes: TCheckBox;
    chkCopyVerInfo: TCheckBox;
    btnHelp: TBitBtn;
    Procedure EnabledClick(Sender: TObject);
    Procedure btnHelpClick(Sender: TObject);
  Private
  Public
    { Public declarations }
    Class Function Execute(strProjectGroup: String; Var Options: TEnabledOptions)
      : Boolean;
  End;

Implementation

{$R *.dfm}

uses TestingHelperUtils;
{ TForm2 }

(**

  This is an on click event handler for the Enabled check box.

  @precon  None.
  @postcon Enabled or disables the

  @param   Sender as a TObject

**)
Procedure TfrmEnabledOptions.EnabledClick(Sender: TObject);

Begin
  gbxOptions.Enabled     := chkEnable.Checked;
  chkBefore.Enabled      := chkEnable.Checked;
  chkAfter.Enabled       := chkEnable.Checked;
  chkZip.Enabled         := chkEnable.Checked;
  chkIncBuild.Enabled    := chkEnable.Checked;
  chkCopyVerInfo.Enabled := chkEnable.Checked;
  chkBuildRes.Enabled    := chkEnable.Checked;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Enabled Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmEnabledOptions.btnHelpClick(Sender: TObject);

Begin
  HtmlHelp(0, PChar(ITHHTMLHelpFile('EnabledOptions')), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is the forms main interface method for invoking the dialogue.

  @precon  None.
  @postcon Invokes the dialogue with parameters passed as var parameters.

  @param   strProjectGroup as a String
  @param   Options         as a TEnabledOptions as a reference
  @return  a Boolean

**)
Class Function TfrmEnabledOptions.Execute(strProjectGroup: String;
  Var Options: TEnabledOptions): Boolean;

Begin
  Result := False;
  With TfrmEnabledOptions.Create(Nil) Do
    Try
      Caption           := strProjectGroup + ' Options';
      chkEnable.Checked := eoGroupEnabled In Options;
      EnabledClick(Nil);
      chkBefore.Checked      := eoBefore In Options;
      chkAfter.Checked       := eoAfter In Options;
      chkZip.Checked         := eoZip In Options;
      chkIncBuild.Checked    := eoIncrementBuild In Options;
      chkCopyVerInfo.Checked := eoCopyVersionInfo In Options;
      chkBuildRes.Checked    := eoBuildVersionResource In Options;
      If ShowModal = mrOK Then
        Begin
          Options := [];
          If chkEnable.Checked Then
            Include(Options, eoGroupEnabled);
          If chkBefore.Checked Then
            Include(Options, eoBefore);
          If chkAfter.Checked Then
            Include(Options, eoAfter);
          If chkZip.Checked Then
            Include(Options, eoZip);
          If chkIncBuild.Checked Then
            Include(Options, eoIncrementBuild);
          If chkBuildRes.Checked Then
            Include(Options, eoBuildVersionResource);
          If chkCopyVerInfo.Checked Then
            Include(Options, eoCopyVersionInfo);
          Result := True;
        End;
    Finally
      Free;
    End;
End;

End.
