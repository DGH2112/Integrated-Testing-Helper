(**
  
  This module contains a frame for displaying the external tools to be run before or after compilation.

  @Author  David Hoyle
  @Version 1.005
  @Date    28 Mar 2020
  
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
Unit ITHelper.ExternalToolsFrame;

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
  ImgList,
  ComCtrls,
  StdCtrls,
  Buttons,
  ToolsAPI,
  ITHelper.Types,
  ITHelper.Interfaces,
  ITHelper.ExternalProcessInfo;

Type
  (** This is a frame for representing the before or after compile tools to be run. **)
  TframeExternalTools = Class(TFrame, IITHOptionsFrame)
    chkWarn: TCheckBox;
    lblCompile: TLabel;
    lvCompile: TListView;
    ilStatus: TImageList;
    ilButtons: TImageList;
    btnDown: TButton;
    btnUp: TButton;
    btnCopy: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    Procedure btnAddClick(Sender: TObject);
    Procedure lvResize(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnUpClick(Sender: TObject);
    Procedure btnDownClick(Sender: TObject);
    Procedure lvCompileDblClick(Sender: TObject);
    Procedure lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure btnCopyClick(Sender: TObject);
    Procedure lvCompileSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvCompileChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  Strict Private
    FGlobalOps: IITHGlobalOptions;
    FProcesses : TITHProcessCollection;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    Procedure AddListItem(Const Item: TListItem; Const Process: TITHProcessInfo);
    Procedure AddProcesses(Const strSection: String; Const Project: IOTAProject);
    Procedure SaveProcesses(Const strSection: String; Const Project: IOTAProject);
    Procedure PopulateListView;
    Procedure InitialiseOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
      Const DlgType : TITHDlgType = dtNA);
    Procedure SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject = Nil;
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
  ITHelper.ProgrammeInfoForm;

Type
  (** An enumerate to define the columns of the dialogue listview. @nohints **)
  TITHColumn = (coTitle, coEXE, coParams, coDir);

Const
  (** A constant arrant to distinguish between before and after compilation tools. **)
  strSection : Array[Low(TITHDlgType)..High(TITHDlgType)] Of String = (
    'Not Applicable', 'Pre-Compilation', 'Post-Compilation');
  (** A constant array of string representing the data type the dialogue is to work
      with. **)
  DlgTypes : Array[Low(TITHDlgType)..High(TITHDlgType)] of String = ('NA', 'Before', 'After');

(**

  This procedure configures the item in the list view.

  @precon  Item must be a valid.
  @postcon Configures the item in the list view.

  @param   Item    as a TListItem as a constant
  @param   Process as a TITHProcessInfo as a constant

**)
Procedure TframeExternalTools.AddListItem(Const Item: TListItem; Const Process: TITHProcessInfo);

Begin
  Item.Checked := Process.FEnabled;
  Item.Caption := Process.FTitle;
  Item.SubItems.Add(Process.FEXE);
  Item.SubItems.Add(Process.FParams);
  Item.SubItems.Add(Process.FDir);
End;

(**

  This procedure adds processes to the passed list view.

  @precon  Processes and ListView must be valid instances.
  @postcon Adds processes to the passed list view.

  @param   strSection as a String as a constant
  @param   Project    as an IOTAProject as a constant

**)
Procedure TframeExternalTools.AddProcesses(Const strSection: String; Const Project: IOTAProject);

Var
  ProjectOps: IITHProjectOptions;

Begin
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    FProcesses.Clear;
    FProcesses.LoadFromINI(ProjectOps.INIFile, strSection);
  Finally
    ProjectOps := Nil;
  End;
End;

(**

  This is an on click event handler for the Add Before button.

  @precon  None.
  @postcon Adds an external programme to the Before Compile list view.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnAddClick(Sender: TObject);

Var
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;

Begin
  If TfrmITHProgrammeInfo.Execute(strTitle, strProgramme, strParameters, strWorkingDirectory,
    FGlobalOps.INIFileName) Then
    Begin
      FProcesses.AddProcessInfo(True, strProgramme, strParameters, strWorkingDirectory, strTitle);
      PopulateListView;
    End;
End;

(**

  This is an on click event handler for the Copy Before button.

  @precon  None.
  @postcon Copies to selected item to a new entry.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnCopyClick(Sender: TObject);

Var
  iIndex: Integer;
  P : TITHProcessInfo;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      P := FProcesses.Process[iIndex];
      FProcesses.AddProcessInfo(P.FEnabled, P.FEXE, P.FParams, P.FDir, P.FTitle);
      PopulateListView;
    End;
End;

(**

  This is an on click event handler for the Delete Before button.

  @precon  None.
  @postcon Deletes the selected item from the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnDeleteClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      FProcesses.Delete(iIndex);
      PopulateListView;
    End;
End;

(**

  This is an on click event handler for the Down Before button.

  @precon  None.
  @postcon Moves the selected item down the Before Compile list view.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnDownClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If (iIndex > -1) And (iIndex < lvCompile.Items.Count - 1) Then
    Begin
      FProcesses.SwapItems(iIndex, iIndex + 1);
      PopulateListView;
      lvCompile.ItemIndex := iIndex + 1;
    End;
End;

(**

  This is an on click event handler for the Edit Before button.

  @precon  None.
  @postcon Allows the user to edit the selected Before Compile List View Item.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnEditClick(Sender: TObject);

Var
  iIndex: Integer;
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  P : TITHProcessInfo;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      P := FProcesses[iIndex];
      strTitle := P.FTitle;
      strProgramme := P.FEXE;
      strParameters := P.FParams;
      strWorkingDirectory := P.FDir;
      If TfrmITHProgrammeInfo.Execute(strTitle, strProgramme, strParameters, strWorkingDirectory,
        FGlobalOps.INIFileName) Then
        Begin
          P.FTitle := strTitle;
          P.FEXE := strProgramme;
          P.FParams := strParameters;
          P.FDir := strWorkingDirectory;
          FProcesses[iIndex] := P;
          PopulateListView;
        End;
    End;
End;

(**

  This is an on click event handler for the Up Before button.

  @precon  None.
  @postcon Moves the selected item up the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.btnUpClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > 0 Then
    Begin
      FProcesses.SwapItems(iIndex, iIndex - 1);
      PopulateListView;
      lvCompile.ItemIndex := iIndex - 1;
    End;
End;

(**

  A constructor for the TframeExternalTools class.

  @precon  None.
  @postcon Creates a list for processes to be stored in.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeExternalTools.Create(AOwner: TComponent);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create(AOwner);
  FProcesses := TITHProcessCollection.Create;
End;

(**

  A destructor for the TframeExternalTools class.

  @precon  None.
  @postcon Frees the memory used by the process list.

**)
Destructor TframeExternalTools.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FProcesses.Free;
  Inherited Destroy;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeExternalTools.InitialiseOptions(Const GlobalOps: IITHGlobalOptions;
  Const Project: IOTAProject; Const DlgType : TITHDlgType);

Var
  ProjectOps: IITHProjectOptions;

Begin
  FGlobalOps := GlobalOps;
  lblCompile.Caption := Format(lblCompile.Caption, [DlgTypes[DlgType]]);
  chkWarn.Caption := Format(chkWarn.Caption, [DlgTypes[DlgType]]);
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    If DlgType = dtBefore Then
      chkWarn.Checked := ProjectOps.WarnBefore
    Else
      chkWarn.Checked := ProjectOps.WarnAfter;
  Finally
    ProjectOps := Nil;
  End;
  AddProcesses(strSection[DlgType], Project);
  PopulateListView;
  lvCompileSelectItem(Nil, Nil, False);
End;

(**

  This is an implementation of the frame intertfaces IsValidated method.

  @precon  None.
  @postcon No validation is required here at the moment so True is returned.

  @return  a Boolean

**)
Function TframeExternalTools.IsValidated: Boolean;

Begin
  Result := True;
End;

(**

  This is an on change event handler for the Compile Listview control.

  @precon  None.
  @postcon Synchronises the enabled property of the process with the check status of the item.

  @param   Sender as a TObject
  @param   Item   as a TListItem
  @param   Change as a TItemChange

**)
Procedure TframeExternalTools.lvCompileChange(Sender: TObject; Item: TListItem; Change: TItemChange);

Var
  P : TITHProcessInfo;
  
Begin
  If Item.Index > -1 Then
    Begin
      P := FProcesses.Process[Item.Index];
      P.FEnabled := Item.Checked;
      FProcesses.Process[Item.Index] := P;
    End;
End;

(**

  This is an on double click event handler for the Before Compile List View.

  @precon  None.
  @postcon Edits the selected item.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.lvCompileDblClick(Sender: TObject);

Begin
  btnEditClick(Sender);
End;

(**

  This method updates the buttons assoviated with the before compile programmes and enables
  and disables them depending upon the selection of items in the list.

  @precon  None.
  @postcon Updates the buttons assoviated with the before compile programmes and enables
           and disables them depending upon the selection of items in the list.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TframeExternalTools.lvCompileSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);

Begin
  btnAdd.Enabled    := True;
  btnEdit.Enabled   := lvCompile.Selected <> Nil;
  btnDelete.Enabled := lvCompile.Selected <> Nil;
  btnUp.Enabled     := (lvCompile.Selected <> Nil) And (lvCompile.ItemIndex > 0);
  btnDown.Enabled := (lvCompile.Selected <> Nil) And (lvCompile.ItemIndex < lvCompile.Items.Count - 1);
  btnCopy.Enabled := lvCompile.Selected <> Nil;
End;

(**

  This is an CustomDrawItem event for the list view.

  @precon  None.
  @postcon Draw readonly items in the list with a light red background.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TframeExternalTools.lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Var DefaultDraw: Boolean);

Const
  iSmallPadding = 2;
  iLargePadding = 6;

Var
  cstrBuffer : Array[0..MAX_PATH] Of Char;
  
  (**

    This function returns display rectangle for the given indexed sub item.

    @precon  iIndex must be a valid SubItem index..
    @postcon Returns display rectangle for the given indexed sub item.

    @param   iIndex as an Integer as a constant
    @return  a TRect

  **)
  Function GetSubItemRect(Const iIndex: Integer): TRect;

  Var
    j: Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j  := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
    Inc(Result.Top, iSmallPadding); 
    Inc(Result.Bottom, iSmallPadding);
    Inc(Result.Left, iLargePadding);
    Dec(Result.Right, iLargePadding);
  End;

  (**

    This method draws the sub items in the listview.

    @precon  None.
    @postcon The subitem is drawn in the listview.

  **)
  Procedure DrawSubItems;

  Var
    i : Integer;
    R: TRect;
    Ops : Integer;
  
  Begin
    For i := Pred(Integer(coEXE)) To Pred(Integer(coDir)) Do
      Begin
        Case i Of
          Pred(Integer(coEXE)), Pred(Integer(coDir)):
            Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
          Pred(Integer(coParams)):
            Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_END_ELLIPSIS Or DT_NOPREFIX;
        Else
          Ops := DT_LEFT;
        End;
        R := GetSubItemRect(i);
        Sender.Canvas.Brush.Color := clWindow;
        If Item.Selected Then
          Sender.Canvas.Brush.Color := clHighlight;
        Sender.Canvas.Refresh;
        StrPCopy(cstrBuffer, Item.SubItems[i]);
        DrawText(Sender.Canvas.Handle, cstrBuffer, Length(Item.SubItems[i]), R, Ops);
        R.Left := R.Right;
      End;
  End;
  
Var
  R, ItemR: TRect;
  Ops : Integer;

Begin
  DefaultDraw := False;
  // Set Left Background
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color  := clHighlightText;
    End;
  ItemR := Item.DisplayRect(drBounds);
  Sender.Canvas.FillRect(ItemR);
  // Draw Status Icon
  R := Item.DisplayRect(drBounds);
  Sender.Canvas.FillRect(R);
  ilStatus.Draw(Sender.Canvas, R.Left, R.Top, Integer(Item.Checked), True);
  // Draw Caption
  R := Item.DisplayRect(drLabel);
  Inc(R.Top, iSmallPadding);
  Inc(R.Bottom, iSmallPadding);
  Inc(R.Left, iSmallPadding);
  Dec(R.Right, iSmallPadding);
  Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_END_ELLIPSIS Or DT_NOPREFIX;
  StrPCopy(cstrBuffer, Item.Caption);
  DrawText(Sender.Canvas.Handle, cstrBuffer, Length(Item.Caption), R, Ops);
  DrawSubItems;
End;

(**

  This is an on resize event handler for both list view controls.

  @precon  None.
  @postcon Ensures the columns of the list views are wide enough to display in
           the dialogue.

  @param   Sender as a TObject

**)
Procedure TframeExternalTools.lvResize(Sender: TObject);

Const
  dblTitlePctWidth = 0.15;
  dblEXEPctWidth = 0.35;
  dblParamsPctWidth = 0.20;
  dblDirPctWidth = 0.30;

Var
  i: Integer;
  LV: TListView;

Begin
  LV := Sender As TListView;
  i := LV.ClientWidth;
  LV.Columns[Integer(coTitle)].Width := Trunc(i * dblTitlePctWidth);
  LV.Columns[Integer(coExe)].Width := Trunc(i * dblEXEPctWidth);
  LV.Columns[Integer(coParams)].Width := Trunc(i * dblParamsPctWidth);
  LV.Columns[Integer(coDir)].Width := Trunc(i * dblDirPctWidth);
End;

(**

  This method renders the list of processes into the listview.

  @precon  None.
  @postcon The list of processes is added to the listview.

**)
Procedure TframeExternalTools.PopulateListView;

Var
  iItemIndex : Integer;
  iProcess : Integer;
  Item : TListItem;
  
Begin
  iItemIndex := lvCompile.ItemIndex;
  lvCompile.OnChange := Nil;
  lvCompile.Items.BeginUpdate;
  Try
    lvCompile.Clear;
    For iProcess := 0 To FProcesses.Count - 1 Do
      Begin
        Item := lvCompile.Items.Add;
        AddListItem(Item, FProcesses[iProcess]);
      End;
    If iItemIndex > lvCompile.Items.Count - 1 Then
      iItemIndex := lvCompile.Items.Count - 1;
    lvCompile.ItemIndex := iItemIndex;
  Finally
    lvCompile.Items.EndUpdate;
    lvCompile.OnChange := lvCompileChange;
  End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   Project   as an IOTAProject as a constant
  @param   DlgType   as a TITHDlgType as a constant

**)
Procedure TframeExternalTools.SaveOptions(Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject;
  Const DlgType : TITHDlgType);

Var
  ProjectOps: IITHProjectOptions;

Begin
  ProjectOps := GlobalOps.ProjectOptions(Project);
  Try
    If DlgType = dtBefore Then
      ProjectOps.WarnBefore := chkWarn.Checked
    Else
      ProjectOps.WarnAfter := chkWarn.Checked;
  Finally
    ProjectOps := Nil;
  End;
  SaveProcesses(strSection[DlgType], Project);
End;

(**

  This procedure saves the list view items to an INI file.

  @precon  Processes and ListView must be valid instances.
  @postcon Saves the list view items to an INI file.

  @param   strSection as a String as a constant
  @param   Project    as an IOTAProject as a constant

**)
Procedure TframeExternalTools.SaveProcesses(Const strSection: String; Const Project: IOTAProject);

Begin
  FProcesses.SaveToINI(FGlobalOps.ProjectOptions(Project).INIFile, strSection);
End;

End.

