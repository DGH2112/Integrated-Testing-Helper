(**

  This module contains a form for configuring the parts of the process that
  are to be enabled.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Sep 2019

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
Unit ITHelper.EnabledOptions;

Interface

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
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
  ITHelper.GlobalOptions, 
  ITHelper.Types;

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
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
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
  HtmlHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strEnabledOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  A constructor for the TfrmITHEnabledOptions class.

  @precon  None.
  @postcon Does nothing but used for codesite tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner
  
  @param   AOwner as a TComponent

**)
Constructor TfrmITHEnabledOptions.Create(AOwner: TComponent);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming); {$ENDIF}
  Inherited Create(AOwner);
End;

(**

  A destructor for the TfrmITHEnabledOptions class.

  @precon  None.
  @postcon Does nothing but used for codesite tracing.

**)
Destructor TfrmITHEnabledOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming); {$ENDIF}
  Inherited;
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
