(**
  
  This module contains a frame to contain the zipping configuration information.

  @Author  David Hoyle
  @Version 1.010
  @Date    12 Jun 2020
  
  @license

    Integrated Testing helper is a RAD Studio plug-in for running pre and post
    build processes.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Integrated-Testing-Helper)

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
Unit ITHelper.ZIPFrame;

Interface

Uses
  Windows,
  Messages,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces;

Type
  (** A frame to represent the zipping configuration. **)
  TframeZipping = Class(TFrame, IITHOptionsFrame)
    chkModifiedFiles: TCheckBox;
    dlgOpenZIP: TOpenDialog;
    mmoExclusionPatterns: TMemo;
    btnBrowseBasePath: TButton;
    edtBasePath: TEdit;
    btnBrowseZip: TButton;
    edtZipName: TEdit;
    lbAdditionalWildcards: TListBox;
    cbxEnabledZipping: TCheckBox;
    lblZIPBasePath: TLabel;
    lblFilePatternsToExclude: TLabel;
    lblAdditionalFiles: TLabel;
    lblZIPName: TLabel;
    lblZipInfo: TLabel;
    ilButtons: TImageList;
    btnDeleteZip: TButton;
    btnEditZip: TButton;
    btnAddZip: TButton;
    Procedure btnAddZipClick(Sender: TObject);
    Procedure btnBrowseBasePathClick(Sender: TObject);
    Procedure btnBrowseZipClick(Sender: TObject);
    Procedure btnDeleteZipClick(Sender: TObject);
    Procedure btnEditZipClick(Sender: TObject);
    Procedure cbxEnabledZippingClick(Sender: TObject);
    Procedure edtZipEXEExit(Sender: TObject);
  Strict Private
    FProject : IOTAProject;
    FFileName: String;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Function  IsValidated : Boolean;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  FileCtrl,
  ITHelper.AdditionalZipFilesForm,
  ITHelper.TestingHelperUtils,
  ITHelper.CommonFunctions;

(**

  This is an on click event handler for the Add ZIP button.

  @precon  None.
  @postcon Allows the user to add

  @param   Sender as a TObject

**)
Procedure TframeZipping.btnAddZipClick(Sender: TObject);

Var
  strWildcard: String;

Begin
  strWildcard := edtBasePath.Text;
  If strWildcard = '' Then
    strWildcard := ExtractFilePath(edtZipName.Text);
  If TfrmITHAdditionalZipFiles.Execute(FProject, strWildcard) Then
    lbAdditionalWildcards.Items.Add(strWildcard);
End;

(**

  This method is an on click event handler for the Browse Base Path button.

  @precon  None.
  @postcon Allows the user to select a directory to be the base path for
           relative directories in the zip process.

  @param   Sender as a TObject

**)
Procedure TframeZipping.btnBrowseBasePathClick(Sender: TObject);

ResourceString
  strZipBaseDirectory = 'Zip Base Directory';

Var
  strDir: String;

Begin
  strDir := edtBasePath.Text;
  If strDir = '' Then
    strDir := TITHToolsAPIFunctions.ExpandMacro(ExtractFilePath(edtZipName.Text), FProject.FileName);
  If SelectDirectory(strZipBaseDirectory, '', strDir {$IFDEF D2005},
    [sdNewFolder, sdShowShares, sdNewUI, sdValidateDir] {$ENDIF}) Then
    edtBasePath.Text := strDir + '\';
End;

(**

  This method allows the user to browse for a zip file.

  @precon  None.
  @postcon If the dialogue is confirmed the zip file name is updated with the
           selected file.

  @param   Sender as a TObject

**)
Procedure TframeZipping.btnBrowseZipClick(Sender: TObject);

Begin
  dlgOpenZIP.FileName := ExtractFileName(TITHToolsAPIFunctions.ExpandMacro(edtZipName.Text, FProject.FileName));
  dlgOpenZIP.InitialDir := ExtractFilePath(TITHToolsAPIFunctions.ExpandMacro(edtZipName.Text, FProject.FileName));
  If dlgOpenZIP.Execute Then
    edtZipName.Text := dlgOpenZIP.FileName;
End;

(**

  This is an on click event handler for the Delete Zip button.

  @precon  None.
  @postcon Deletes the selected items from the additional wildcards list box.

  @param   Sender as a TObject

**)
Procedure TframeZipping.btnDeleteZipClick(Sender: TObject);

Begin
  If lbAdditionalWildcards.ItemIndex > -1 Then
    lbAdditionalWildcards.DeleteSelected;
End;

(**

  This is an on click event handler for the Edit Zip button.

  @precon  None.
  @postcon Displays the selected additional wildcard so that it can be edited.

  @param   Sender as a TObject

**)
Procedure TframeZipping.btnEditZipClick(Sender: TObject);

Var
  strWildcard: String;
  iIndex     : Integer;

Begin
  iIndex := lbAdditionalWildcards.ItemIndex;
  If iIndex > -1 Then
    Begin
      strWildcard := lbAdditionalWildcards.Items[iIndex];
      If TfrmITHAdditionalZipFiles.Execute(FProject, strWildcard) Then
        lbAdditionalWildcards.Items[iIndex] := strWildcard;
    End;
End;

(**

  This method enabled or disabled the zip controls depend on whether the user
  wishes to use this facility.

  @precon  None.
  @postcon Enabled or disabled the zip controls depend on whether the user
           wishes to use this facility.

  @param   Sender as a TObject

**)
Procedure TframeZipping.cbxEnabledZippingClick(Sender: TObject);

Begin
  lblZIPName.Enabled               := cbxEnabledZipping.Checked;
  edtZipName.Enabled               := cbxEnabledZipping.Checked;
  lbAdditionalWildcards.Enabled    := cbxEnabledZipping.Checked;
  btnAddZip.Enabled                := cbxEnabledZipping.Checked;
  btnBrowseZip.Enabled             := cbxEnabledZipping.Checked;
  btnEditZip.Enabled               := cbxEnabledZipping.Checked;
  btnDeleteZip.Enabled             := cbxEnabledZipping.Checked;
  lblZipInfo.Enabled               := cbxEnabledZipping.Checked;
  edtBasePath.Enabled              := cbxEnabledZipping.Checked;
  btnBrowseBasePath.Enabled        := cbxEnabledZipping.Checked;
  lblAdditionalFiles.Enabled       := cbxEnabledZipping.Checked;
  lblFilePatternsToExclude.Enabled := cbxEnabledZipping.Checked;
  mmoExclusionPatterns.Enabled     := cbxEnabledZipping.Checked;
  chkModifiedFiles.Enabled         := cbxEnabledZipping.Checked;
End;

(**

  A constructor for the TframeZipping class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner

  @param   AOwner as a TComponent

**)
Constructor TframeZipping.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
End;

(**

  A destructor for the TframeZipping class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TframeZipping.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This is an on exit event handler for the ZIPEXE control.

  @precon  None.
  @postcon Updates the ZIP EXE status on exiting the zip exe control.

  @param   Sender as a TObject

**)
Procedure TframeZipping.edtZipEXEExit(Sender: TObject);

ResourceString
  strCorrect = 'The zip executable "%s" has been found!';
  strInCorrect = 'The zip executable "%s" CAN NOT be found!';

Begin
  If FileExists(FFileName) Or DGHFindOnPath(FFileName, '') Then
    Begin
      lblZipInfo.Caption    := Format(strCorrect, [FFileName]);
      lblZipInfo.Font.Color := clGreen;
    End
  Else
    Begin
      lblZipInfo.Caption    := Format(strInCorrect, [FFileName]);
      lblZipInfo.Font.Color := clRed;
    End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @nohint  DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeZipping.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project : IOTAProject; Const DlgType : TITHDlgType); //FI:O804

Var
  ProjectOps: IITHProjectOptions;

Begin
  FProject := Project;
  FFileName := GlobalOps.ZipEXE;
  ProjectOps := GlobalOps.ProjectOptions(FProject);
  Try
    cbxEnabledZipping.Checked := ProjectOps.EnableZipping;
    edtZipName.Text           := ProjectOps.ZipName;
    edtBasePath.Text          := ProjectOps.BasePath;
    mmoExclusionPatterns.Text := ProjectOps.ExcPatterns;
    lbAdditionalWildcards.Items.Assign(ProjectOps.AddZipFiles);
    chkModifiedFiles.Checked  := ProjectOps.SaveModifiedFiles;
  Finally
    ProjectOps := Nil;
  End;
  cbxEnabledZippingClick(Nil);
  edtZipEXEExit(Nil);
End;

(**

  This method returns true if the frame information is valid.

  @precon  None.
  @postcon Returns true if the frame information is valid.

  @return  a Boolean

**)
Function TframeZipping.IsValidated: Boolean;

ResourceString
  strZipFileBaseDirectoryDoesNotExist = 'The zip file base directory "%s" does not exist.';
  strZipFilePathDirectoryDoesNotExist = 'The zip file path directory "%s" does not exist.';

Begin
  Result := True;
  If cbxEnabledZipping.Checked And Not
    SysUtils.DirectoryExists(TITHToolsAPIFunctions.ExpandMacro(edtBasePath.Text, FProject.FileName)) Then
    Begin
      MessageDlg(Format(strZipFileBaseDirectoryDoesNotExist,
          [TITHToolsAPIFunctions.ExpandMacro(edtBasePath.Text, FProject.FileName)]), mtError, [mbOK], 0);
      Result := False;
    End;
  If cbxEnabledZipping.Checked And Not
    SysUtils.DirectoryExists(ExtractFilePath(TITHToolsAPIFunctions.ExpandMacro(edtZipName.Text,
    FProject.FileName))) Then
    Begin
      MessageDlg(Format(strZipFilePathDirectoryDoesNotExist,
          [TITHToolsAPIFunctions.ExpandMacro(ExtractFilePath(edtZipName.Text), FProject.FileName)]),
          mtError, [mbOK], 0);
      Result := False;
    End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @nohint  DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeZipping.SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject;
  Const DlgType : TITHDlgType); //FI:O804

Var
  ProjectOps: IITHProjectOptions;

Begin
  ProjectOps := GlobalOps.ProjectOptions(Project);
  Try
    ProjectOps.EnableZipping := cbxEnabledZipping.Checked;
    ProjectOps.ZipName := edtZipName.Text;
    ProjectOps.BasePath := edtBasePath.Text;
    ProjectOps.ExcPatterns := mmoExclusionPatterns.Text;
    ProjectOps.AddZipFiles.Assign(lbAdditionalWildcards.Items);
    ProjectOps.SaveModifiedFiles := chkModifiedFiles.Checked;
  Finally
    ProjectOps := Nil;
  End;
End;

End.

