(**

  This module contains code which represents a form for configuring the zipping of
  projects during the compilation cycle.

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Jan 2018

**)
Unit ITHelper.ZIPDialogue;

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
  ToolsAPI,
  ITHelper.Interfaces;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** A class to represent the form. **)
  TfrmITHZIPDialogue = Class(TForm)
    lblZipInfo: TLabel;
    lblZIPName: TLabel;
    lblAdditionalFiles: TLabel;
    lblFilePatternsToExclude: TLabel;
    cbxEnabledZipping: TCheckBox;
    lbAdditionalWildcards: TListBox;
    btnAddZip: TBitBtn;
    btnEditZip: TBitBtn;
    btnDeleteZip: TBitBtn;
    edtZipName: TEdit;
    btnBrowseZip: TButton;
    edtBasePath: TEdit;
    btnBrowseBasePath: TButton;
    mmoExclusionPatterns: TMemo;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgOpenZIP: TOpenDialog;
    btnHelp: TBitBtn;
    Procedure btnAddZipClick(Sender: TObject);
    Procedure btnBrowseBasePathClick(Sender: TObject);
    Procedure btnBrowseZipClick(Sender: TObject);
    Procedure btnDeleteZipClick(Sender: TObject);
    Procedure btnEditZipClick(Sender: TObject);
    Procedure cbxEnabledZippingClick(Sender: TObject);
    Procedure edtZipEXEExit(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions);
    procedure btnHelpClick(Sender: TObject);
  Strict Private
    { Private declarations }
    FProject : IOTAProject;
    FFileName: String;
  Public
    { Public declarations }
    Class Procedure Execute(Const Project: IOTAProject; Const GlobalOps: IITHGlobalOptions);
  End;

Implementation

{$R *.dfm}

Uses
  ITHelper.AdditionalZipFilesForm,
  FileCtrl,
  ITHelper.TestingHelperUtils,
  IniFiles, 
  ITHelper.CommonFunctions;

Const
  (** An INI Section for the dialogues size and position. **)
  strZIPDlgSection = 'ZIP Dlg';
  (** An INI Key for the top **)
  strTopKey = 'Top';
  (** An INI Key for the left **)
  strLeftLey = 'Left';
  (** An INI Key for the height **)
  strHeightKey = 'Height';
  (** An INI Key for the width **)
  strWidthKey = 'Width';

(**

  This is an on click event handler for the Add ZIP button.

  @precon  None.
  @postcon Allows the user to add

  @param   Sender as a TObject

**)
Procedure TfrmITHZIPDialogue.btnAddZipClick(Sender: TObject);

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
Procedure TfrmITHZIPDialogue.btnBrowseBasePathClick(Sender: TObject);

ResourceString
  strZipBaseDirectory = 'Zip Base Directory';

Var
  strDir: String;

Begin
  strDir := edtBasePath.Text;
  If strDir = '' Then
    strDir := ExpandMacro(ExtractFilePath(edtZipName.Text), FProject.FileName);
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
Procedure TfrmITHZIPDialogue.btnBrowseZipClick(Sender: TObject);

Begin
  dlgOpenZIP.FileName := ExtractFileName(ExpandMacro(edtZipName.Text, FProject.FileName));
  dlgOpenZIP.InitialDir := ExtractFilePath(ExpandMacro(edtZipName.Text, FProject.FileName));
  If dlgOpenZIP.Execute Then
    edtZipName.Text := dlgOpenZIP.FileName;
End;

(**

  This is an on click event handler for the Delete Zip button.

  @precon  None.
  @postcon Deletes the selected items from the additional wildcards list box.

  @param   Sender as a TObject

**)
Procedure TfrmITHZIPDialogue.btnDeleteZipClick(Sender: TObject);

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
Procedure TfrmITHZIPDialogue.btnEditZipClick(Sender: TObject);

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

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the ZIP Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHZIPDialogue.btnHelpClick(Sender: TObject);

Const
  strZIPOptions = 'ZIPOptions';

Begin
  HtmlHelp(0, PChar(ITHHTMLHelpFile(strZIPOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Checks the paths of the zip file and base directory and then confirms the
           dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmITHZIPDialogue.btnOKClick(Sender: TObject);

ResourceString
  strZipFileBaseDirectoryDoesNotExist = 'The zip file base directory "%s" does not exist.';
  strZipFilePathDirectoryDoesNotExist = 'The zip file path directory "%s" does not exist.';

Begin
  If cbxEnabledZipping.Checked And Not SysUtils.DirectoryExists(ExpandMacro(edtBasePath.Text,
      FProject.FileName)) Then
    Begin
      MessageDlg(Format(strZipFileBaseDirectoryDoesNotExist,
          [ExpandMacro(edtBasePath.Text, FProject.FileName)]), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
  If cbxEnabledZipping.Checked And Not SysUtils.DirectoryExists(ExtractFilePath(ExpandMacro(
    edtZipName.Text, FProject.FileName))) Then
    Begin
      MessageDlg(Format(strZipFilePathDirectoryDoesNotExist,
          [ExpandMacro(ExtractFilePath(edtZipName.Text), FProject.FileName)]), mtError, [mbOK], 0);
      ModalResult := mrNone;
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
Procedure TfrmITHZIPDialogue.cbxEnabledZippingClick(Sender: TObject);

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
End;

(**

  This is an on exit event handler for the ZIPEXE control.

  @precon  None.
  @postcon Updates the ZIP EXE status on exiting the zip exe control.

  @param   Sender as a TObject

**)
Procedure TfrmITHZIPDialogue.edtZipEXEExit(Sender: TObject);

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

  This is the forms main interface method for editing a projects zipping opions..

  @precon  Project and GlobalOps must be valid instances.
  @postcon Dislays the dialogue.

  @param   Project   as an IOTAProject as a constant
  @param   GlobalOps as a IITHGlobalOptions as a constant

**)
Class Procedure TfrmITHZIPDialogue.Execute(Const Project: IOTAProject;
  Const GlobalOps: IITHGlobalOptions);

ResourceString
  strZIPOptionsFor = 'ZIP Options for %s';

Var
  frm: TfrmITHZIPDialogue;

Begin
  frm := TfrmITHZIPDialogue.Create(Nil);
  Try
    frm.FProject := Project;
    frm.InitialiseOptions(GlobalOps);
    frm.cbxEnabledZippingClick(Nil);
    frm.edtZipEXEExit(Nil);
    frm.Caption := Format(strZIPOptionsFor, [GetProjectName(Project)]);
    If frm.ShowModal = mrOK Then
      frm.SaveOptions(GlobalOps);
  Finally
    frm.Free;
  End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOps as a IITHGlobalOptions as a constant

**)
Procedure TfrmITHZIPDialogue.InitialiseOptions(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    Top    := iniFile.ReadInteger(strZIPDlgSection, strTopKey, (Screen.Height - Height) Div 2);
    Left   := iniFile.ReadInteger(strZIPDlgSection, strLeftLey, (Screen.Width -  Width) Div 2);
    Height := iniFile.ReadInteger(strZIPDlgSection, strHeightKey, Height);
    Width  := iniFile.ReadInteger(strZIPDlgSection, strWidthKey, Width);
  Finally
    iniFile.Free;
  End;
  FFileName := GlobalOps.ZipEXE;
  ProjectOps := GlobalOps.ProjectOptions(FProject);
  Try
    cbxEnabledZipping.Checked := ProjectOps.EnableZipping;
    edtZipName.Text           := ProjectOps.ZipName;
    edtBasePath.Text          := ProjectOps.BasePath;
    mmoExclusionPatterns.Text := ProjectOps.ExcPatterns;
    lbAdditionalWildcards.Items.Assign(ProjectOps.AddZipFiles);
  Finally
    ProjectOps := Nil;
  End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as a IITHGlobalOptions as a constant

**)
Procedure TfrmITHZIPDialogue.SaveOptions(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    iniFile.WriteInteger(strZIPDlgSection, strTopKey, Top);
    iniFile.WriteInteger(strZIPDlgSection, strLeftLey, Left);
    iniFile.WriteInteger(strZIPDlgSection, strHeightKey, Height);
    iniFile.WriteInteger(strZIPDlgSection, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
  ProjectOps := GlobalOps.ProjectOptions(FProject);
  Try
    ProjectOps.EnableZipping := cbxEnabledZipping.Checked;
    ProjectOps.ZipName := edtZipName.Text;
    ProjectOps.BasePath := edtBasePath.Text;
    ProjectOps.ExcPatterns := mmoExclusionPatterns.Text;
    ProjectOps.AddZipFiles.Assign(lbAdditionalWildcards.Items);
  Finally
    ProjectOps := Nil;
  End;
End;

End.
