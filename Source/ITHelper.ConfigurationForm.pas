(**

  This module contains a class which represents a form for editing the lists of
  external applications which need to be run before and after the compilation
  of the currently active project.

  @Version 1.0
  @Author  David Hoyle
  @Date    14 Jul 2018

**)
Unit ITHelper.ConfigurationForm;

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
  ComCtrls,
  ToolsAPI,
  ImgList,
  ExtCtrls,
  ITHelper.ExternalProcessInfo,
  ITHelper.Interfaces, System.ImageList;

Type
  (** An enumerate to define which set of data the dialogue is to work with. **)
  TITHDlgType = (dtBefore, dtAfter);

  (** A class to represent the editing interface. **)
  TfrmITHConfigureDlg = Class(TForm)
    lblCompile: TLabel;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lvCompile: TListView;
    btnUp: TBitBtn;
    btnDown: TBitBtn;
    ilStatus: TImageList;
    btnCopy: TBitBtn;
    chkWarn: TCheckBox;
    btnHelp: TBitBtn;
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
    Procedure lvCompileSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnHelpClick(Sender: TObject);
  Strict Private
    { Private declarations }
    FGlobalOps: IITHGlobalOptions;
    FProject  : IOTAProject;
    FDlgType  : TITHDlgType;
  Strict Protected
    Procedure SwapItems(Const Item1, Item2: TListItem);
    Procedure AddListItem(Const Item: TListItem; Const Process: TITHProcessInfo);
    Procedure AddProcesses(Const Processes: TITHProcessCollection; Const strSection: String;
      Const ListView: TListView; Const Project: IOTAProject);
    Procedure SaveProcesses(Const Processes: TITHProcessCollection; Const ListView: TListView;
      Const strSection: String; Const Project: IOTAProject);
    Procedure InitialiseOptions(Const Project: IOTAProject);
    Procedure SaveOptions(Const Project: IOTAProject);
  Public
    { Public declarations }
    Class Function Execute(Const Project: IOTAProject; Const GlobalOps: IITHGlobalOptions;
      Const DlgType : TITHDlgType): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  FileCtrl,
  ITHelper.ProgrammeInfoForm,
  ITHelper.TestingHelperUtils;

Type
  (** An enumerate to define the columns of the dialogue listview. @nohints **)
  TITHColumn = (coTitle, coEXE, coParams, coDir);
  (** A record helper for the above enumerate to translate the enumerate to a subimte or column reference
      index. @nohints **)
  TITHColumnHelper = Record Helper For TITHColumn
    Function ColumnIndex : Integer;
    Function SubItemIndex : Integer;
  End;
  
Const
  (** A constant array of string representing the data type the dialogue is to work
      with. **)
  DlgTypes : Array[Low(TITHDlgType)..High(TITHDlgType)] of String = ('Before', 'After');
  (** An INI Section Suffix for the dialogue settings. **)
  strDlg = ' Dlg';
  (** An INI Key name for the top position of the dialogue. **)
  strTopKey = 'Top';
  (** An INI Key name for the left position of the dialogue. **)
  strLeftKey = 'Left';
  (** An INI Key name for the height of the dialogue. **)
  strHeightKey = 'Height';
  (** An INI Key name for the width of the dialogue. **)
  strWidthKey = 'Width';

{ TITHColumnHelper }

(**

  This method returns the enumerate as a column index.

  @precon  None.
  @postcon Returns the enumerate as a column index.

  @return  an Integer

**)
Function TITHColumnHelper.ColumnIndex: Integer;

Begin
  Result := Ord(Self);
End;

(**

  This method returns the enumerate as a subitem index.

  @precon  None.
  @postcon Returns the enumerate as a subitem index.

  @return  an Integer

**)
Function TITHColumnHelper.SubItemIndex: Integer;

Begin
  Result := Pred(Ord(Self));
End;

(**

  This procedure configures the item in the list view.

  @precon  Item must be a valid.
  @postcon Configures the item in the list view.

  @param   Item    as a TListItem as a constant
  @param   Process as a TITHProcessInfo as a constant

**)
Procedure TfrmITHConfigureDlg.AddListItem(Const Item: TListItem; Const Process: TITHProcessInfo);

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

  @param   Processes  as a TITHProcessCollection as a constant
  @param   strSection as a String as a constant
  @param   ListView   as a TListView as a constant
  @param   Project    as an IOTAProject as a constant

**)
Procedure TfrmITHConfigureDlg.AddProcesses(Const Processes: TITHProcessCollection;
  Const strSection: String; Const ListView: TListView; Const Project: IOTAProject);

Var
  i      : Integer;
  Item   : TListItem;
  ProjectOps: IITHProjectOptions;

Begin
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    Processes.LoadFromINI(ProjectOps.INIFile, strSection);
    For i := 0 To Processes.Count - 1 Do
      Begin
        Item := ListView.Items.Add;
        AddListItem(Item, Processes[i]);
      End;
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
Procedure TfrmITHConfigureDlg.btnAddClick(Sender: TObject);

Var
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  Item: TListItem;

Begin
  If TfrmITHProgrammeInfo.Execute(strTitle, strProgramme, strParameters, strWorkingDirectory,
    FGlobalOps.INIFileName) Then
    Begin
      Item         := lvCompile.Items.Add;
      Item.Caption := strTitle;
      Item.SubItems.Add(strProgramme);
      Item.SubItems.Add(strParameters);
      Item.SubItems.Add(strWorkingDirectory);
      Item.Checked := True;
    End;
End;

(**

  This is an on click event handler for the Copy Before button.

  @precon  None.
  @postcon Copies to selected item to a new entry.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnCopyClick(Sender: TObject);

Var
  iIndex: Integer;
  Item  : TListItem;
  P : TITHProcessInfo;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      Item := lvCompile.Items[iIndex];
      P.FTitle := Item.Caption;
      P.FEXE := Item.SubItems[coEXE.SubItemIndex];
      P.FParams := Item.SubItems[coParams.SubItemIndex];
      P.FDir := Item.SubItems[coDir.SubItemIndex];
      P.FEnabled := Item.Checked;
      Item := lvCompile.Items.Add;
      Item.Caption := P.FTitle;
      Item.SubItems.Add(P.FEXE);
      Item.SubItems.Add(P.FParams);
      Item.SubItems.Add(P.FDir);
      Item.Checked := P.FEnabled;
    End;
End;

(**

  This is an on click event handler for the Delete Before button.

  @precon  None.
  @postcon Deletes the selected item from the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnDeleteClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    lvCompile.Items.Delete(iIndex);
End;

(**

  This is an on click event handler for the Down Before button.

  @precon  None.
  @postcon Moves the selected item down the Before Compile list view.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnDownClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If (iIndex > -1) And (iIndex < lvCompile.Items.Count - 1) Then
    Begin
      SwapItems(lvCompile.Items[iIndex], lvCompile.Items[iIndex + 1]);
      lvCompile.ItemIndex := iIndex + 1;
    End;
End;

(**

  This is an on click event handler for the Edit Before button.

  @precon  None.
  @postcon Allows the user to edit the selected Before Compile List View Item.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnEditClick(Sender: TObject);

Var
  iIndex: Integer;
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  Item    : TListItem;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      Item                := lvCompile.Items[iIndex];
      strTitle            := Item.Caption;
      strProgramme        := Item.SubItems[coEXE.SubItemIndex];
      strParameters       := Item.SubItems[coParams.SubItemIndex];
      strWorkingDirectory := Item.SubItems[coDir.SubItemIndex];
      If TfrmITHProgrammeInfo.Execute(strTitle, strProgramme, strParameters, strWorkingDirectory,
        FGlobalOps.INIFileName) Then
        Begin
          Item.Caption     := strTitle;
          Item.SubItems[coEXE.SubItemIndex] := strProgramme;
          Item.SubItems[coParams.SubItemIndex] := strParameters;
          Item.SubItems[coDir.SubItemIndex] := strWorkingDirectory;
        End;
    End;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Display the Compilation Tools help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnHelpClick(Sender: TObject);

Const
  strCompilationTools = 'CompilationTools';

Begin
  HTMLHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strCompilationTools)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the Up Before button.

  @precon  None.
  @postcon Moves the selected item up the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.btnUpClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > 0 Then
    Begin
      SwapItems(lvCompile.Items[iIndex], lvCompile.Items[iIndex - 1]);
      lvCompile.ItemIndex := iIndex - 1;
    End;
End;

(**

  This is the classes main interface method for invoking the dialogue.

  @precon  None.
  @postcon The classes main interface method for invoking the dialogue.

  @param   Project   as an IOTAProject as a constant
  @param   GlobalOps as an IITHGlobalOptions as a constant
  @param   DlgType   as a TITHDlgType as a constant
  @return  a Boolean

**)
Class Function TfrmITHConfigureDlg.Execute(Const Project: IOTAProject;
  Const GlobalOps: IITHGlobalOptions; Const DlgType : TITHDlgType): Boolean;

Const
  strSection : Array[Low(TITHDlgType)..High(TITHDlgType)] Of String = (
    'Pre-Compilation', 'Post-Compilation');

ResourceString
  strProgrammesToExecuteCompilation = 'Programmes to execute %s compilation: %s';

Var
  Processes: TITHProcessCollection;
  frm: TfrmITHConfigureDlg;

Begin
  Result := False;
  frm := TfrmITHConfigureDlg.Create(Nil);
  Try
    frm.FProject   := Project;
    frm.FGlobalOps := GlobalOps;
    frm.FDlgType := DlgType;
    frm.Caption    := Format(strProgrammesToExecuteCompilation, [DlgTypes[DlgType],
      TITHToolsAPIFunctions.GetProjectName(Project)]);
    frm.lblCompile.Caption := Format(frm.lblCompile.Caption, [DlgTypes[DlgType]]);
    frm.chkWarn.Caption := Format(frm.chkWarn.Caption, [DlgTypes[DlgType]]);
    frm.InitialiseOptions(Project);
    Processes := TITHProcessCollection.Create;
    Try
      frm.AddProcesses(Processes, strSection[DlgType], frm.lvCompile, Project);
    Finally
      Processes.Free;
    End;
    frm.lvCompileSelectItem(Nil, Nil, False);
    If frm.ShowModal = mrOK Then
      Begin
        frm.SaveOptions(Project);
        Processes := TITHProcessCollection.Create;
        Try
          frm.SaveProcesses(Processes, frm.lvCompile, strSection[DlgType], Project);
        Finally
          Processes.Free;
        End;
        Result := True;
      End;
  Finally
    frm.Free;
  End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   Project as an IOTAProject as a constant

**)
Procedure TfrmITHConfigureDlg.InitialiseOptions(Const Project: IOTAProject);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile :=TMemIniFile.Create(FGlobalOps.INIFileName);
  Try
    Top    := iniFile.ReadInteger(DlgTypes[FDlgType] + strDlg, strTopKey, (Screen.Height - Height) Div 2);
    Left   := iniFile.ReadInteger(DlgTypes[FDlgType] + strDlg, strLeftKey, (Screen.Width - Width) Div 2);
    Height := iniFile.ReadInteger(DlgTypes[FDlgType] + strDlg, strHeightKey, Height);
    Width  := iniFile.ReadInteger(DlgTypes[FDlgType] + strDlg, strWidthKey, Width);
  Finally
    iniFile.Free;
  End;
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    If FDlgType = dtBefore Then
      chkWarn.Checked := ProjectOps.WarnBefore
    Else
      chkWarn.Checked := ProjectOps.WarnAfter;
  Finally
    ProjectOps := Nil;
  End;
End;

(**

  This is an on double click event handler for the Before Compile List View.

  @precon  None.
  @postcon Edits the selected item.

  @param   Sender as a TObject

**)
Procedure TfrmITHConfigureDlg.lvCompileDblClick(Sender: TObject);

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
Procedure TfrmITHConfigureDlg.lvCompileSelectItem(Sender: TObject; Item: TListItem;
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
Procedure TfrmITHConfigureDlg.lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
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
Procedure TfrmITHConfigureDlg.lvResize(Sender: TObject);

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
  LV.Columns[coTitle.ColumnIndex].Width := Trunc(i * dblTitlePctWidth);
  LV.Columns[coExe.ColumnIndex].Width := Trunc(i * dblEXEPctWidth);
  LV.Columns[coParams.ColumnIndex].Width := Trunc(i * dblParamsPctWidth);
  LV.Columns[coDir.ColumnIndex].Width := Trunc(i * dblDirPctWidth);
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   Project as an IOTAProject as a constant

**)
Procedure TfrmITHConfigureDlg.SaveOptions(Const Project: IOTAProject);

Var
  iniFile: TMemIniFile;
  ProjectOps: IITHProjectOptions;

Begin
  iniFile := TMemIniFile.Create(FGlobalOps.INIFileName);
  Try
    iniFile.WriteInteger(DlgTypes[FDlgType] + strDlg, strTopKey, Top);
    iniFile.WriteInteger(DlgTypes[FDlgType] + strDlg, strLeftKey, Left);
    iniFile.WriteInteger(DlgTypes[FDlgType] + strDlg, strHeightKey, Height);
    iniFile.WriteInteger(DlgTypes[FDlgType] + strDlg, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
  ProjectOps := FGlobalOps.ProjectOptions(Project);
  Try
    If FDlgType = dtBefore Then
      ProjectOps.WarnBefore := chkWarn.Checked
    Else
      ProjectOps.WarnAfter := chkWarn.Checked;
  Finally
    ProjectOps := Nil;
  End;
End;

(**

  This procedure saves the list view items to an INI file.

  @precon  Processes and ListView must be valid instances.
  @postcon Saves the list view items to an INI file.

  @param   Processes  as a TITHProcessCollection as a constant
  @param   ListView   as a TListView as a constant
  @param   strSection as a String as a constant
  @param   Project    as an IOTAProject as a constant

**)
Procedure TfrmITHConfigureDlg.SaveProcesses(Const Processes: TITHProcessCollection;
  Const ListView: TListView; Const strSection: String; Const Project: IOTAProject);

Var
  i: Integer;

Begin
  For i := 0 To ListView.Items.Count - 1 Do
    Processes.AddProcessInfo(
      ListView.Items[i].Checked,
      ListView.Items[i].SubItems[coEXE.SubItemIndex],
      ListView.Items[i].SubItems[coParams.SubItemIndex],
      ListView.Items[i].SubItems[coDir.SubItemIndex],
      ListView.Items[i].Caption);
  Processes.SaveToINI(FGlobalOps.ProjectOptions(Project).INIFile, strSection);
End;

(**

  This method swaps the information in the 2 list view items.

  @precon  None.
  @postcon Swaps the information in the 2 list view items.

  @param   Item1 as a TListItem as a constant
  @param   Item2 as a TListItem as a constant

**)
Procedure TfrmITHConfigureDlg.SwapItems(Const Item1, Item2: TListItem);

Var
  strTitle, strEXE, strParam, strDir: String;
  boolEnabled: Boolean;

Begin
  strTitle := Item1.Caption;
  strEXE := Item1.SubItems[coEXE.SubItemIndex];
  strParam := Item1.SubItems[coParams.SubItemIndex];
  strDir := Item1.SubItems[coDir.SubItemIndex];
  boolEnabled := Item1.Checked;
  Item1.Caption := Item2.Caption;
  Item1.SubItems[coEXE.SubItemIndex] := Item2.SubItems[coEXE.SubItemIndex];
  Item1.SubItems[coParams.SubItemIndex] := Item2.SubItems[coParams.SubItemIndex];
  Item1.SubItems[coDir.SubItemIndex] := Item2.SubItems[coDir.SubItemIndex];
  Item1.Checked := Item2.Checked;
  Item2.Caption := strTitle;
  Item2.SubItems[coEXE.SubItemIndex] := strEXE;
  Item2.SubItems[coParams.SubItemIndex] := strParam;
  Item2.SubItems[coDir.SubItemIndex] := strDir;
  Item2.Checked := boolEnabled;
End;

End.
