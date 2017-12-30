(**

  This module contains a class to represent a form for editing the applications
  global options.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Dec 2017

**)
Unit ITHelper.GlobalOptionsDialogue;

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
  ComCtrls,
  StdCtrls,
  ITHelper.GlobalOptions;

Type
  (** A class which represents a form **)
  TfrmITHGlobalOptionsDialogue = Class(TForm)
    lblZIPEXE: TLabel;
    lblZipParams: TLabel;
    chkGroupMessages: TCheckBox;
    chkAutoScrollMessages: TCheckBox;
    edtZipEXE: TEdit;
    btnBrowseZipEXE: TButton;
    edtZipParams: TEdit;
    chkSwitchToMessages: TCheckBox;
    lblClearMessagesAfter: TLabel;
    edtClearMessages: TEdit;
    udClearMessages: TUpDown;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnCheckForUpdates: TBitBtn;
    dlgOpenEXE: TOpenDialog;
    lblShortcuts: TLabel;
    hkShortcut: THotKey;
    btnAssign: TBitBtn;
    lvShortcuts: TListView;
    btnHelp: TBitBtn;
    Procedure btnBrowseZipEXEClick(Sender: TObject);
    procedure lvShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAssignClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  Private
    { Private declarations }
    FGlobalOps: TITHGlobalOptions;
    Procedure InitialiseOptions(Const GlobalOps: TITHGlobalOptions);
    Procedure SaveOptions(Const GlobalOps: TITHGlobalOptions);
  Public
    { Public declarations }
    Class Procedure Execute(Const GlobalOps: TITHGlobalOptions);
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  ActnList,
  ITHelper.TestingHelperUtils,
  Menus;

{ TfrmGlobalOptionsDialogue }

(**

  This is an on click event handler for the Assign button.

  @precon  None.
  @postcon Assigns the shortcut to the selected action.

  @param   Sender as a TObject

**)
procedure TfrmITHGlobalOptionsDialogue.btnAssignClick(Sender: TObject);

begin
  If lvShortcuts.ItemIndex >  -1 Then
    lvShortcuts.Selected.SubItems[0] := ShortCutToText(hkShortcut.HotKey);
end;

(**

  This is an on click event handler for the Browse Version From EXE button.

  @precon  None.
  @postcon Allows the user to select an executable from which to get version
           information.

  @param   Sender as a TObject

**)
Procedure TfrmITHGlobalOptionsDialogue.btnBrowseZipEXEClick(Sender: TObject);

Begin
  dlgOpenEXE.InitialDir := ExtractFilePath(edtZipEXE.Text);
  dlgOpenEXE.FileName   := ExtractFileName(edtZipEXE.Text);
  If dlgOpenEXE.Execute Then
    edtZipEXE.Text := dlgOpenEXE.FileName;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the HTML Help Global Options page.

  @param   Sender as a TObject

**)
Procedure TfrmITHGlobalOptionsDialogue.btnHelpClick(Sender: TObject);

Const
  strGlobalOptions = 'GlobalOptions';

Begin
  HTMLHelp(0, PChar(ITHHTMLHelpFile(strGlobalOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is the forms main interface method for displaying the global options.

  @precon  GlobalOps must be a valid instance.
  @postcon Displays the dialogue

  @param   GlobalOps as a TITHGlobalOptions as a constant

**)
Class Procedure TfrmITHGlobalOptionsDialogue.Execute(Const GlobalOps: TITHGlobalOptions);

Var
  frm: TfrmITHGlobalOptionsDialogue;

Begin
  frm := TfrmITHGlobalOptionsDialogue.Create(Nil);
  Try
    frm.FGlobalOps := GlobalOps;
    frm.InitialiseOptions(GlobalOps);
    If frm.ShowModal = mrOK Then
      frm.SaveOptions(GlobalOps);
  Finally
    frm.Free;
  End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  GlobalOps must be a valid instance.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOps as a TITHGlobalOptions as a constant

**)
Procedure TfrmITHGlobalOptionsDialogue.InitialiseOptions(Const GlobalOps: TITHGlobalOptions);

Var
  i: Integer;
  A : Taction;
  Item : TListItem;

Begin
  chkGroupMessages.Checked      := GlobalOps.GroupMessages;
  chkAutoScrollMessages.Checked := GlobalOps.AutoScrollMessages;
  udClearMessages.Position      := GlobalOps.ClearMessages;
  edtZipEXE.Text                := GlobalOps.ZipEXE;
  edtZipParams.Text             := GlobalOps.ZipParameters;
  chkSwitchToMessages.Checked   := GlobalOps.SwitchToMessages;
  For i := 0 To Actions.Count - 1 Do
    If Actions[i] Is TAction Then
      Begin
        A := Actions[i] As TAction;
        Item := lvShortcuts.Items.Add;
        Item.Caption := A.Name;
        Item.SubItems.Add(ShortCutToText(A.ShortCut));
      End;
  hkShortcut.HotKey := 0;
End;

(**

  This is an on select item event handler for the list view.

  @precon  None.
  @postcon Assigns the actions short cut to the Hot Key control.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
procedure TfrmITHGlobalOptionsDialogue.lvShortcutsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

begin
  If Selected Then
    hkShortcut.HotKey := TextToShortCut(Item.SubItems[0]);
end;

(**

  This method saves the project options to the ini file.

  @precon  GlobalOps must be a valid instance.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as a TITHGlobalOptions as a constant

**)
Procedure TfrmITHGlobalOptionsDialogue.SaveOptions(Const GlobalOps: TITHGlobalOptions);

Var
  i: Integer;
  A: TAction;

Begin
  GlobalOps.GroupMessages      := chkGroupMessages.Checked;
  GlobalOps.AutoScrollMessages := chkAutoScrollMessages.Checked;
  GlobalOps.ClearMessages      := udClearMessages.Position;
  GlobalOps.ZipEXE             := edtZipEXE.Text;
  GlobalOps.ZipParameters      := edtZipParams.Text;
  GlobalOps.SwitchToMessages   := chkSwitchToMessages.Checked;
  For i := 0 To Actions.Count - 1 Do
    If Actions[i] Is TAction Then
      Begin
        A := Actions[i] As TAction;
        A.ShortCut := TextToShortCut(lvShortcuts.Items[i].SubItems[0]);
      End;
End;

End.
