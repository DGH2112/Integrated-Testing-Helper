(**

  This module contains a form for configuring the parts of the process that
  are to be enabled.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jan 2018

**)
Unit ITHelper.EnabledOptions;

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
  ITHelper.GlobalOptions;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** A form to represent the form interface. **)
  TfrmITHEnabledOptions = Class(TForm)
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
    Class Function Execute(Const strProjectGroup: String; Var Options: TITHEnabledOptions) : Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  ITHelper.TestingHelperUtils;
{ TForm2 }

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Enabled Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHEnabledOptions.btnHelpClick(Sender: TObject);

Const
  strEnabledOptions = 'EnabledOptions';

Begin
  HtmlHelp(0, PChar(ITHHTMLHelpFile(strEnabledOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the Enabled check box.

  @precon  None.
  @postcon Enabled or disables the

  @param   Sender as a TObject

**)
Procedure TfrmITHEnabledOptions.EnabledClick(Sender: TObject);

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

  This is the forms main interface method for invoking the dialogue.

  @precon  None.
  @postcon Invokes the dialogue with parameters passed as var parameters.

  @param   strProjectGroup as a String as a constant
  @param   Options         as a TITHEnabledOptions as a reference
  @return  a Boolean

**)
Class Function TfrmITHEnabledOptions.Execute(Const strProjectGroup: String;
  Var Options: TITHEnabledOptions): Boolean;

Const
  strOptions = ' Options';

var
  frm: TfrmITHEnabledOptions;

Begin
  Result := False;
  frm := TfrmITHEnabledOptions.Create(Nil);
  Try
    frm.Caption           := strProjectGroup + strOptions;
    frm.chkEnable.Checked := eoGroupEnabled In Options;
    frm.EnabledClick(Nil);
    frm.chkBefore.Checked      := eoBefore In Options;
    frm.chkAfter.Checked       := eoAfter In Options;
    frm.chkZip.Checked         := eoZip In Options;
    frm.chkIncBuild.Checked    := eoIncrementBuild In Options;
    frm.chkCopyVerInfo.Checked := eoCopyVersionInfo In Options;
    frm.chkBuildRes.Checked    := eoBuildVersionResource In Options;
    If frm.ShowModal = mrOK Then
      Begin
        Options := [];
        If frm.chkEnable.Checked Then
          Include(Options, eoGroupEnabled);
        If frm.chkBefore.Checked Then
          Include(Options, eoBefore);
        If frm.chkAfter.Checked Then
          Include(Options, eoAfter);
        If frm.chkZip.Checked Then
          Include(Options, eoZip);
        If frm.chkIncBuild.Checked Then
          Include(Options, eoIncrementBuild);
        If frm.chkBuildRes.Checked Then
          Include(Options, eoBuildVersionResource);
        If frm.chkCopyVerInfo.Checked Then
          Include(Options, eoCopyVersionInfo);
        Result := True;
      End;
  Finally
    frm.Free;
  End;
End;

End.
