(**

  This module contains code which represents a form for configuring the zipping of
  projects during the compilation cycle.

  @Author  David Hoyle
  @Version 1.0
  @Date    08 Jul 2012

**)
Unit ZIPDialogue;

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
  GlobalOptions;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

Type
  (** A class to represent the form. **)
  TfrmZIPDialogue = Class(TForm)
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
    Procedure InitialiseOptions(GlobalOps: TGlobalOptions);
    Procedure SaveOptions(GlobalOps: TGlobalOptions);
    procedure btnHelpClick(Sender: TObject);
  Private
    { Private declarations }
    FProject : IOTAProject;
    FFileName: String;
  Public
    { Public declarations }
    Class Procedure Execute(Project: IOTAProject; GlobalOps: TGlobalOptions);
  End;

Implementation

{$R *.dfm}

Uses
  AdditionalZipFilesForm,
  FileCtrl,
  TestingHelperUtils,
  dghlibrary,
  IniFiles;

(**

  This is an on click event handler for the Add ZIP button.

  @precon  None.
  @postcon Allows the user to add

  @param   Sender as a TObject

**)
Procedure TfrmZIPDialogue.btnAddZipClick(Sender: TObject);

Var
  strWildcard: String;

Begin
  strWildcard := edtBasePath.Text;
  If strWildcard = '' Then
    strWildcard := ExtractFilePath(edtZipName.Text);
  If TfrmAdditionalZipFiles.Execute(FProject, strWildcard) Then
    lbAdditionalWildcards.Items.Add(strWildcard);
End;

(**

  This method is an on click event handler for the Browse Base Path button.

  @precon  None.
  @postcon Allows the user to select a directory to be the base path for
           relative directories in the zip process.

  @param   Sender as a TObject

**)
Procedure TfrmZIPDialogue.btnBrowseBasePathClick(Sender: TObject);

Var
  strDir: String;

Begin
  strDir := edtBasePath.Text;
  If strDir = '' Then
    strDir := ExpandMacro(ExtractFilePath(edtZipName.Text), FProject);
  If SelectDirectory('Zip Base Directory', '', strDir {$IFDEF D2005},
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
Procedure TfrmZIPDialogue.btnBrowseZipClick(Sender: TObject);

Begin
  dlgOpenZIP.FileName := ExtractFileName(ExpandMacro(edtZipName.Text, FProject));
  dlgOpenZIP.InitialDir := ExtractFilePath(ExpandMacro(edtZipName.Text, FProject));
  If dlgOpenZIP.Execute Then
    edtZipName.Text := dlgOpenZIP.FileName;
End;

(**

  This is an on click event handler for the Delete Zip button.

  @precon  None.
  @postcon Deletes the selected items from the additional wildcards list box.

  @param   Sender as a TObject

**)
Procedure TfrmZIPDialogue.btnDeleteZipClick(Sender: TObject);

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
Procedure TfrmZIPDialogue.btnEditZipClick(Sender: TObject);

Var
  strWildcard: String;
  iIndex     : Integer;

Begin
  iIndex := lbAdditionalWildcards.ItemIndex;
  If iIndex > -1 Then
    Begin
      strWildcard := lbAdditionalWildcards.Items[iIndex];
      If TfrmAdditionalZipFiles.Execute(FProject, strWildcard) Then
        lbAdditionalWildcards.Items[iIndex] := strWildcard;
    End;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the ZIP Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmZIPDialogue.btnHelpClick(Sender: TObject);

Begin
  HtmlHelp(0, PChar(ITHHTMLHelpFile('ZIPOptions')), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Checks the paths of the zip file and base directory and then confirms the
           dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmZIPDialogue.btnOKClick(Sender: TObject);

Begin
  If cbxEnabledZipping.Checked And Not
  {$IFDEF Dxe20}System.{$ENDIF}SysUtils.DirectoryExists(ExpandMacro(edtBasePath.Text,
      FProject)) Then
    Begin
      MessageDlg(Format('The zip file base directory "%s" does not exist.',
          [ExpandMacro(edtBasePath.Text, FProject)]), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
  If cbxEnabledZipping.Checked And Not
  {$IFDEF Dxe20}System.{$ENDIF}SysUtils.DirectoryExists(ExtractFilePath(ExpandMacro(
    edtZipName.Text, FProject))) Then
    Begin
      MessageDlg(Format('The zip file path directory "%s" does not exist.',
          [ExpandMacro(ExtractFilePath(edtZipName.Text), FProject)]), mtError, [mbOK], 0);
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
Procedure TfrmZIPDialogue.cbxEnabledZippingClick(Sender: TObject);

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
Procedure TfrmZIPDialogue.edtZipEXEExit(Sender: TObject);

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

  @param   Project   as an IOTAProject
  @param   GlobalOps as a TGlobalOptions

**)
Class Procedure TfrmZIPDialogue.Execute(Project: IOTAProject; GlobalOps: TGlobalOptions);

Begin
  With TfrmZIPDialogue.Create(Nil) Do
    Try
      FProject := Project;
      InitialiseOptions(GlobalOps);
      cbxEnabledZippingClick(Nil);
      edtZipEXEExit(Nil);
      Caption := Format('ZIP Options for %s', [GetProjectName(Project)]);
      If ShowModal = mrOK Then
        SaveOptions(GlobalOps);
    Finally
      Free;
    End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOps as a TGlobalOptions

**)
Procedure TfrmZIPDialogue.InitialiseOptions(GlobalOps: TGlobalOptions);

Begin
  With TMemIniFile.Create(GlobalOps.INIFileName) Do
    Try
      Top    := ReadInteger('ZIP Dlg', 'Top', (Screen.Height - Height) Div 2);
      Left   := ReadInteger('ZIP Dlg', 'Left', (Screen.Width -  Width) Div 2);
      Height := ReadInteger('ZIP Dlg', 'Height', Height);
      Width  := ReadInteger('ZIP Dlg', 'Width', Width);
    Finally
      Free;
    End;
  FFileName                 := GlobalOps.ZipEXE;
  With GlobalOps.ProjectOptions(FProject) Do
    Try
      cbxEnabledZipping.Checked := EnableZipping;
      edtZipName.Text           := ZipName;
      edtBasePath.Text          := BasePath;
      mmoExclusionPatterns.Text := ExcPatterns;
      lbAdditionalWildcards.Items.Assign(AddZipFiles);
    Finally
      Free;
    End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as a TGlobalOptions

**)
Procedure TfrmZIPDialogue.SaveOptions(GlobalOps: TGlobalOptions);

Begin
  With TMemIniFile.Create(GlobalOps.INIFileName) Do
    Try
      WriteInteger('ZIP Dlg', 'Top', Top);
      WriteInteger('ZIP Dlg', 'Left', Left);
      WriteInteger('ZIP Dlg', 'Height', Height);
      WriteInteger('ZIP Dlg', 'Width', Width);
      UpdateFile;
    Finally
      Free;
    End;
  With GlobalOps.ProjectOptions(FProject) Do
    Try
      EnableZipping := cbxEnabledZipping.Checked;
      ZipName := edtZipName.Text;
      BasePath := edtBasePath.Text;
      ExcPatterns := mmoExclusionPatterns.Text;
      AddZipFiles.Assign(lbAdditionalWildcards.Items);
    Finally
      Free;
    End;
End;

End.
