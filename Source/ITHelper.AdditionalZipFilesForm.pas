(**

  This module defines a class which represents a form for entering a path and
  wildcards for additional files to be included in the zipping process.

  @Version 1.015
  @Date    28 Mar 2020
  @Author  David Hoyle

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
  ToolsAPI, System.ImageList, Vcl.ImgList;

Type
  (** This is a class to represents the form interface. **)
  TfrmITHAdditionalZipFiles = Class(TForm)
    lblWildcard: TLabel;
    edtWildcard: TEdit;
    btnBrowse: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ilButtons: TImageList;
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
  If Not SysUtils.DirectoryExists(TITHToolsAPIFunctions.ExpandMacro(strDir, FProject.FileName)) Then
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
    TITHToolsAPIFunctions.RegisterFormClassForTheming(TfrmITHAdditionalZipFiles, frm);
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
