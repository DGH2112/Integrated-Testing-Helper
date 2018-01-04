(**

  This module defines a class which represents a form for entering a path and
  wildcards for additional files to be included in the zipping process.

  @Version 1.0
  @Date    04 Jan 2018
  @Author  David Hoyle

**)
Unit ITHelper.AdditionalZipFilesForm;

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
  Buttons,
  StdCtrls,
  ToolsAPI;

Type
  (** This is a class to represents the form interface. **)
  TfrmITHAdditionalZipFiles = Class(TForm)
    lblWildcard: TLabel;
    edtWildcard: TEdit;
    btnBrowse: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Procedure btnBrowseClick(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  Private
    { Private declarations }
    FProject: IOTAProject;
  Public
    { Public declarations }
    Class Function Execute(Const Project: IOTAProject; Var strWildcard: String): Boolean;
  End;

Implementation

{$R *.dfm}


Uses
  FileCtrl,
  ITHelper.TestingHelperUtils;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Allows the users to browse for a directory.

  @param   Sender as a TObject

**)
Procedure TfrmITHAdditionalZipFiles.btnBrowseClick(Sender: TObject);

ResourceString
  strZipFilePath = 'Zip File Path';

Var
  strDir: String;

Begin
  strDir := ExtractFilePath(edtWildcard.Text);
  If SelectDirectory(strZipFilePath, '', strDir{$IFDEF D2005},
    [sdNewFolder, sdShowShares, sdNewUI, sdValidateDir] {$ENDIF} ) Then
    edtWildcard.Text := strDir + '\' + ExtractFileName(edtWildcard.Text);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Ebsures that the directory exists.

  @param   Sender as a TObject

**)
Procedure TfrmITHAdditionalZipFiles.btnOKClick(Sender: TObject);

ResourceString
  strDirectoryDoesNotExists = 'The directory "%s" does not exists.';

Var
  strDir: String;

Begin
  strDir := ExtractFilePath(edtWildcard.Text);
  If Not SysUtils.DirectoryExists(ExpandMacro(strDir, FProject)) Then
    Begin
      ModalResult := mrNone;
      MessageDlg(Format(strDirectoryDoesNotExists, [strDir]),
        mtError, [mbOK], 0);
    End;
End;

(**

  This is the classes main interface method for invoking the dialogue.

  @precon  None.
  @postcon Displays the dialogue. On confirmation the wildcard is passed back via the var parameter.

  @param   Project     as an IOTAProject as a constant
  @param   strWildcard as a String as a reference
  @return  a Boolean

**)
Class Function TfrmITHAdditionalZipFiles.Execute(Const Project: IOTAProject;
  Var strWildcard: String): Boolean;

Var
  frm: TfrmITHAdditionalZipFiles;

Begin
  Result := False;
  frm := TfrmITHAdditionalZipFiles.Create(Nil);
  Try
    frm.FProject := Project;
    frm.edtWildcard.Text := strWildcard;
    If frm.ShowModal = mrOK Then
      Begin
        strWildcard := frm.edtWildcard.Text;
        Result := True;
      End;
  Finally
    frm.Free;
  End;
End;

End.
