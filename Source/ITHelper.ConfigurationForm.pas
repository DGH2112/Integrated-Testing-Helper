(**

  This module contains a class which represents a form for editing the lists of
  external applications which need to be run before and after the compilation
  of the currently active project.

  @Version 1.0
  @Author  David Hoyle
  @Date    16 Jul 2018

**)
Unit ITHelper.ConfigurationForm;

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
  ComCtrls,
  ToolsAPI,
  ImgList,
  ExtCtrls,
  ITHelper.ExternalProcessInfo,
  ITHelper.Interfaces;

Type
  (** A class to represent the editing interface. **)
  TfrmITHConfigureDlg = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    procedure btnHelpClick(Sender: TObject);
  Strict Private
  Strict Protected
  Public
    Class Function Execute(Const Project: IOTAProject; Const GlobalOps: IITHGlobalOptions): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  FileCtrl,
  ITHelper.ProgrammeInfoForm,
  ITHelper.TestingHelperUtils;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Display the Compilation Tools help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnHelpClick(Sender: TObject);

Const
  strCompilationTools = 'CompilationTools';

Begin
  HTMLHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strCompilationTools)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is the classes main interface method for invoking the dialogue.

  @precon  None.
  @postcon The classes main interface method for invoking the dialogue.

  @param   Project   as an IOTAProject as a constant
  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   DlgType   as a TITHDlgType as a constant
  @return  a Boolean

**)
Class Function TfrmITHConfigureDlg.Execute(Const Project: IOTAProject;
  Const GlobalOps: IITHGlobalOptions): Boolean;

Var
  frm: TfrmITHConfigureDlg;

Begin
  Result := False;
  frm := TfrmITHConfigureDlg.Create(Nil);
  Try
    If frm.ShowModal = mrOK Then
      Begin
      End;
  Finally
    frm.Free;
  End;
End;

End.
