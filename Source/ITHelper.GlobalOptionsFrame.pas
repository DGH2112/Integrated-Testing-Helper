(**
  
  This module contains a frame for the Global Options for ITHelper.

  @Version 1.0
  @Author  David Hoyle
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
Unit ITHelper.GlobalOptionsFrame;

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
  ComCtrls,
  Buttons,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces, Vcl.ExtCtrls;

Type
  (** A frame to hold the global options. **)
  TframeGlobalOptions = Class(TFrame, IITHOptionsFrame)
    btnAssign: TBitBtn;
    dlgOpenEXE: TOpenDialog;
    lvShortcuts: TListView;
    hkShortcut: THotKey;
    edtZipParams: TEdit;
    edtZipEXE: TEdit;
    chkSwitchToMessages: TCheckBox;
    chkGroupMessages: TCheckBox;
    chkAutoScrollMessages: TCheckBox;
    btnBrowseZipEXE: TButton;
    udClearMessages: TUpDown;
    edtClearMessages: TEdit;
    lblShortcuts: TLabel;
    lblZipParams: TLabel;
    lblZIPEXE: TLabel;
    lblClearMessagesAfter: TLabel;
    pnlFudgePanel: TPanel;
    Procedure btnBrowseZipEXEClick(Sender: TObject);
    Procedure lvShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure btnAssignClick(Sender: TObject);
  Strict Private
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject;
      Const DlgType : TITHDlgType);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project : IOTAProject;
      Const DlgType : TITHDlgType);
    Function IsValidated: Boolean;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  Menus,
  ActnList,
  ITHelper.TestingHelperUtils;

{$R *.dfm}

(**

  This is an on click event handler for the Assign button.

  @precon  None.
  @postcon Assigns the shortcut to the selected action.

  @param   Sender as a TObject

**)
procedure TframeGlobalOptions.btnAssignClick(Sender: TObject);

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
Procedure TframeGlobalOptions.btnBrowseZipEXEClick(Sender: TObject);

Begin
  dlgOpenEXE.InitialDir := ExtractFilePath(edtZipEXE.Text);
  dlgOpenEXE.FileName   := ExtractFileName(edtZipEXE.Text);
  If dlgOpenEXE.Execute Then
    edtZipEXE.Text := dlgOpenEXE.FileName;
End;

(**

  A destructor for the TframeGlobalOptions class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

  @nocheck MissingCONSTInParam
  @nohint  AOwner

  @param   AOwner as a TComponent

**)
Constructor TframeGlobalOptions.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
End;

(**

  A destructor for the TframeGlobalOptions class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TframeGlobalOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  GlobalOps must be a valid instance.
  @postcon Initialises the project options in the dialogue.

  @nohint  Project DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeGlobalOptions.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project : IOTAProject; Const DlgType : TITHDlgType); //FI:O804

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
  For i := 0 To TITHToolsAPIFunctions.Actions.Count - 1 Do
    If TITHToolsAPIFunctions.Actions[i] Is TAction Then
      Begin
        A := TITHToolsAPIFunctions.Actions[i] As TAction;
        Item := lvShortcuts.Items.Add;
        Item.Caption := A.Name;
        Item.SubItems.Add(ShortCutToText(A.ShortCut));
      End;
  hkShortcut.HotKey := 0;
End;

(**

  This method validates the dialogue.

  @precon  None.
  @postcon There is no validation currently needed.

  @return  a Boolean

**)
Function TframeGlobalOptions.IsValidated: Boolean;

Begin
  Result := True;
End;

(**

  This is an on select item event handler for the list view.

  @precon  None.
  @postcon Assigns the actions short cut to the Hot Key control.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
procedure TframeGlobalOptions.lvShortcutsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

begin
  btnAssign.Enabled := Selected;
  If Selected Then
    hkShortcut.HotKey := TextToShortCut(Item.SubItems[0]);
end;

(**

  This method saves the project options to the ini file.

  @precon  GlobalOps must be a valid instance.
  @postcon Saves the project options to the ini file.

  @nohint  Project DlgType

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeGlobalOptions.SaveOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project : IOTAProject; Const DlgType : TITHDlgType); //FI:O804

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
  For i := 0 To TITHToolsAPIFunctions.Actions.Count - 1 Do
    If TITHToolsAPIFunctions.Actions[i] Is TAction Then
      Begin
        A := TITHToolsAPIFunctions.Actions[i] As TAction;
        A.ShortCut := TextToShortCut(lvShortcuts.Items[i].SubItems[0]);
      End;
End;

End.

