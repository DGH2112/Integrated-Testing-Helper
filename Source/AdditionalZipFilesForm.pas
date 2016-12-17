(**

  This module defines a class which represents a form for entering a path and
  wildcards for additional files to be included in the zipping process.

  @Version 1.0
  @Date    12 Jun 2012
  @Author  David Hoyle

**)
unit AdditionalZipFilesForm;

interface

uses
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

type
  (** This is a class to represents the form interface. **)
  TfrmAdditionalZipFiles = class(TForm)
    lblWildcard: TLabel;
    edtWildcard: TEdit;
    btnBrowse: TButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    FProject : IOTAProject;
  public
    { Public declarations }
    Class Function Execute(Project : IOTAProject; var strWildcard : String) : Boolean;
  end;

implementation

{$R *.dfm}

Uses
  FileCtrl,
  TestingHelperUtils;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Allows the users to browse for a directory.

  @param   Sender as a TObject

**)
procedure TfrmAdditionalZipFiles.btnBrowseClick(Sender: TObject);

Var
  strDir : String;

begin
  strDir := ExtractFilePath(edtWildcard.Text);
  If SelectDirectory('Zip File Path', '', strDir{$IFDEF D2005},
    [sdNewFolder, sdShowShares, sdNewUI, sdValidateDir] {$ENDIF} ) Then
    edtWildcard.Text := strDir + '\' + ExtractFileName(edtWildcard.Text);
end;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Ebsures that the directory exists.

  @param   Sender as a TObject

**)
procedure TfrmAdditionalZipFiles.btnOKClick(Sender: TObject);

Var
  strDir : String;

begin
  strDir := ExtractFilePath(edtWildcard.Text);
  If Not SysUtils.DirectoryExists(ExpandMacro(strDir, FProject)) Then
    Begin
      ModalResult := mrNone;
      MessageDlg(Format('The directory "%s" does not exists.', [strDir]),
        mtError, [mbOK], 0);
    End;
end;

(**

  This is the classes main interface method for invoking the dialogue.

  @precon  None.
  @postcon Displays the dialogue. On confirmation the wildcard is passed back via the var
           parameter.

  @param   Project     as an IOTAProject
  @param   strWildcard as a String as a reference
  @return  a Boolean

**)
class function TfrmAdditionalZipFiles.Execute(Project : IOTAProject;
  var strWildcard: String): Boolean;

begin
  Result := False;
  With TfrmAdditionalZipFiles.Create(Nil) Do
    Try
      FProject := Project;
      edtWildcard.Text := strWildcard;
      If ShowModal = mrOK Then
        Begin
          strWildcard := edtWildcard.Text;
          Result := True;
        End;
    Finally
      Free;
    End;
end;

end.

