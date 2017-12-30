(**

  This modules contains a form for specifying the EXE, Params and Working
  Directory for external tools.

  @Version 1.0
  @Author  David Hoyle
  @Date    29 Dec 2017

**)
Unit ITHelper.ProgrammeInfoForm;

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
  ExtCtrls;

Type
  (** An enumerate for the starting window state of the application. **)
  TStartingWindowState = (
    swsDisabled = 0,
    swsNormal = SW_NORMAL,
    swsMinimised = SW_SHOWMINIMIZED,
    swsMaximised = SW_SHOWMAXIMIZED
  );

  (** A class to represent the form. **)
  TfrmProgrammeInfo = Class(TForm)
    lblProgramme: TLabel;
    edtProgramme: TEdit;
    lblParameters: TLabel;
    edtParameters: TEdit;
    lblWorkingDirectrory: TLabel;
    edtWorkingDirectory: TEdit;
    btnEXE: TButton;
    btnDirectory: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgOpen: TOpenDialog;
    rgpStartingWindowState: TRadioGroup;
    edtTitle: TEdit;
    lblTitle: TLabel;
    Procedure btnEXEClick(Sender: TObject);
    Procedure btnDirectoryClick(Sender: TObject);
  Strict Private
  Strict Protected
    Procedure LoadPosition(Const strINIFile: String);
    Procedure SavePosition(Const strINIFile: String);
  Public
    Class Function Execute(Var strTitle, strEXE, strParam, strDir: String;
      Const strINIFile: String; Var State: TStartingWindowState): Boolean;
  End;

Implementation

{$R *.dfm}


Uses
  FileCtrl,
  INIFiles;

Const
  (** An INI section name for the dialogue settings. **)
  strProgrammeInfoDlgSection = 'ProgrammeInfoDlg';
  (** An INI key for the dialogue top. **)
  strTopKey = 'Top';
  (** An INI key for the dialogue left. **)
  strLeftKey = 'Left';
  (** An INI key for the dialogue height. **)
  strHeightKey = 'Height';
  (** An INI key for the dialogue width. **)
  strWidthKey = 'Width';

(**

  This is an on click event handler for the Directory browse button.

  @precon  None.
  @postcon Allows the user to browse for a directory.

  @param   Sender as a TObject

**)
Procedure TfrmProgrammeInfo.btnDirectoryClick(Sender: TObject);

ResourceString
  strSelectWorkingDirectory = 'Select a Working Directory';

Var
  strDirectory: String;

Begin
  strDirectory := edtWorkingDirectory.Text;
  If strDirectory = '' Then
    strDirectory := ExtractFilePath(edtProgramme.Text);
  If SelectDirectory(strSelectWorkingDirectory, '', strDirectory,
    [sdNewFolder, sdShowShares, sdNewUI, sdValidateDir]) Then
    edtWorkingDirectory.Text := strDirectory + '\';
End;

(**

  This is an on click event handler for the EXE browse button.

  @precon  None.
  @postcon Allows the user to browse for EXE, BAT anmd BTM files.

  @param   Sender as a TObject

**)
Procedure TfrmProgrammeInfo.btnEXEClick(Sender: TObject);

Begin
  If dlgOpen.Execute Then
    Begin
      edtProgramme.Text := dlgOpen.Filename;
      If edtWorkingDirectory.Text = '' Then
        edtWorkingDirectory.Text := ExtractFilePath(dlgOpen.Filename);
    End;
End;

(**

  This is the classes interface Singleton method. If the dialogue is confirmed the information is
  returned in the var parameters.

  @precon  None.
  @postcon If the dialogue is confirmed the information is returned in the var parameters .

  @param   strTitle   as a String as a reference
  @param   strEXE     as a String as a reference
  @param   strParam   as a String as a reference
  @param   strDir     as a String as a reference
  @param   strINIFile as a String as a constant
  @param   State      as a TStartingWindowState as a reference
  @return  a Boolean

**)
Class Function TfrmProgrammeInfo.Execute(Var strTitle, strEXE, strParam, strDir: String;
  Const strINIFile: String; Var State: TStartingWindowState): Boolean;

Var
  frm: TfrmProgrammeInfo;

Begin
  frm := TfrmProgrammeInfo.Create(Nil);
  Try
    Result := False;
    frm.edtTitle.Text := strTitle;
    frm.edtProgramme.Text := strEXE;
    frm.edtParameters.Text := strParam;
    frm.edtWorkingDirectory.Text := strDir;
    Case State Of
      swsNormal:    frm.rgpStartingWindowState.ItemIndex := 0;
      swsMinimised: frm.rgpStartingWindowState.ItemIndex := 1;
      swsMaximised: frm.rgpStartingWindowState.ItemIndex := 2;
    Else
      frm.rgpStartingWindowState.Enabled := False;
    End;
    frm.LoadPosition(strINIFile);
    If frm.ShowModal = mrOK Then
      Begin
        strTitle := frm.edtTitle.Text;
        strEXE := frm.edtProgramme.Text;
        strParam := frm.edtParameters.Text;
        strDir := frm.edtWorkingDirectory.Text;
        Case frm.rgpStartingWindowState.ItemIndex Of
          1: State := swsMinimised;
          2: State := swsMaximised;
        Else
          State := swsNormal;
        End;
        frm.SavePosition(strINIFile);
        Result := True;
      End;
  Finally
    frm.Free;
  End;
End;

(**

  This method loads the position of the dialogue from the pased INI File.

  @precon  None.
  @postcon Loads the position of the dialogue from the pased INI File.

  @param   strINIFile as a String as a constant

**)
Procedure TfrmProgrammeInfo.LoadPosition(Const strINIFile: String);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(strINIFile);
  Try
    Top := iniFile.ReadInteger(strProgrammeInfoDlgSection, strTopKey, Application.MainForm.Top +
      Application.MainForm.Height Div 2 - Height Div 2);
    Left := iniFile.ReadInteger(strProgrammeInfoDlgSection, strLeftKey, Application.MainForm.Left
      + Application.MainForm.Width Div 2 - Width Div 2);
    Height := iniFile.ReadInteger(strProgrammeInfoDlgSection, strHeightKey, Height);
    Width := iniFile.ReadInteger(strProgrammeInfoDlgSection, strWidthKey, Width);
  Finally
    iniFile.Free;
  End;
End;

(**

  This method saves the position of the dialogue to the passed INI file.

  @precon  None.
  @postcon Saves the position of the dialogue to the passed INI file.

  @param   strINIFile as a String as a constant

**)
Procedure TfrmProgrammeInfo.SavePosition(Const strINIFile: String);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(strINIFile);
  Try
    iniFile.WriteInteger(strProgrammeInfoDlgSection, strTopKey, Top);
    iniFile.WriteInteger(strProgrammeInfoDlgSection, strLeftKey, Left);
    iniFile.WriteInteger(strProgrammeInfoDlgSection, strHeightKey, Height);
    iniFile.WriteInteger(strProgrammeInfoDlgSection, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

End.
