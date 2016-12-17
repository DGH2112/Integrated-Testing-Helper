(**

  This module contains a class which represents a form for editing the lists of
  external applications which need to be run before and after the compilation
  of the currently active project.

  @Version 1.0
  @Author  David Hoyle
  @Date    08 Jul 2012

**)
Unit ConfigurationForm;

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
  dghlibrary,
  ExternalProcessInfo,
  ExtCtrls,
  GlobalOptions;

Type
  (** An enumerate to define which set of data the dialogue is to work with. **)
  TDlgType = (dtBefore, dtAfter);

  (** A class to represent the editing interface. **)
  TfrmConfigure = Class(TForm)
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
  Private
    { Private declarations }
    FGlobalOps: TGlobalOptions;
    FProject  : IOTAProject;
    FDlgType  : TDlgType;
    Procedure SwapItems(Item1, Item2: TListItem);
    Procedure AddListItem(Item: TListItem; Process: TProcessInfo);
    Procedure AddProcesses(Processes: TProcessCollection; strSection: String;
      ListView: TListView; Project: IOTAProject);
    Procedure SaveProcesses(Processes: TProcessCollection; ListView: TListView;
      strSection: String; Project: IOTAProject);
    Procedure InitialiseOptions(Project: IOTAProject);
    Procedure SaveOptions(Project: IOTAProject);
  Public
    { Public declarations }
    Class Function Execute(Project: IOTAProject; GlobalOps: TGlobalOptions;
      DlgType : TDlgType): Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  ProgrammeInfoForm,
  TestingHelperUtils,
  FileCtrl;

Const
  (** A constant array of string representing the data type the dialogue is to work
      with. **)
  DlgTypes : Array[Low(TDlgType)..High(TDlgType)] of String = ('Before', 'After');

(**

  This is an on click event handler for the Add Before button.

  @precon  None.
  @postcon Adds an external programme to the Before Compile list view.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnAddClick(Sender: TObject);

Var
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  Item: TListItem;
  Disabled: TStartingWindowState;

Begin
  Disabled := swsDisabled;
  If TfrmProgrammeInfo.Execute(strTitle, strProgramme, strParameters, strWorkingDirectory,
    FGlobalOps.INIFileName, Disabled) Then
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
Procedure TfrmConfigure.btnCopyClick(Sender: TObject);

Var
  iIndex: Integer;
  Item  : TListItem;
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  boolEnabled: Boolean;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      Item                := lvCompile.Items[iIndex];
      strTitle            := Item.Caption;
      strProgramme        := Item.SubItems[0];
      strParameters       := Item.SubItems[1];
      strWorkingDirectory := Item.SubItems[2];
      boolEnabled         := Item.Checked;
      Item                := lvCompile.Items.Add;
      Item.Caption        := strTitle;
      Item.SubItems.Add(strProgramme);
      Item.SubItems.Add(strParameters);
      Item.SubItems.Add(strWorkingDirectory);
      Item.Checked := boolEnabled;
    End;
End;

(**

  This is an on click event handler for the Down Before button.

  @precon  None.
  @postcon Moves the selected item down the Before Compile list view.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnDownClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If (iIndex > -1) And (iIndex < lvCompile.Items.Count - 1) Then
    Begin
      With lvCompile Do
        SwapItems(Items[iIndex], Items[iIndex + 1]);
      lvCompile.ItemIndex := iIndex + 1;
    End;
End;

(**

  This is an on click event handler for the Up Before button.

  @precon  None.
  @postcon Moves the selected item up the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnUpClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > 0 Then
    Begin
      With lvCompile Do
        SwapItems(Items[iIndex], Items[iIndex - 1]);
      lvCompile.ItemIndex := iIndex - 1;
    End;
End;

(**

  This is an on click event handler for the Delete Before button.

  @precon  None.
  @postcon Deletes the selected item from the Before Compile List View.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnDeleteClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    lvCompile.Items.Delete(iIndex);
End;

(**

  This is an on click event handler for the Edit Before button.

  @precon  None.
  @postcon Allows the user to edit the selected Before Compile List View Item.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnEditClick(Sender: TObject);

Var
  iIndex: Integer;
  strTitle, strProgramme, strParameters, strWorkingDirectory: String;
  Item    : TListItem;
  Disabled: TStartingWindowState;

Begin
  iIndex := lvCompile.ItemIndex;
  If iIndex > -1 Then
    Begin
      Item                := lvCompile.Items[iIndex];
      strTitle            := Item.Caption;
      strProgramme        := Item.SubItems[0];
      strParameters       := Item.SubItems[1];
      strWorkingDirectory := Item.SubItems[2];
      Disabled            := swsDisabled;
      If TfrmProgrammeInfo.Execute(strTitle, strProgramme, strParameters,
        strWorkingDirectory, FGlobalOps.INIFileName, Disabled) Then
        Begin
          Item.Caption     := strTitle;
          Item.SubItems[0] := strProgramme;
          Item.SubItems[1] := strParameters;
          Item.SubItems[2] := strWorkingDirectory;
        End;
    End;
End;

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Display the Compilation Tools help page.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.btnHelpClick(Sender: TObject);

Begin
  HTMLHelp(0, PChar(ITHHTMLHelpFile('CompilationTools')), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is the classes main interface method for invoking the dialogue.

  @precon  None.
  @postcon The classes main interface method for invoking the dialogue.

  @param   Project   as an IOTAProject
  @param   GlobalOps as a TGlobalOptions
  @param   DlgType   as a TDlgType
  @return  a Boolean

**)
Class Function TfrmConfigure.Execute(Project: IOTAProject;
  GlobalOps: TGlobalOptions; DlgType : TDlgType): Boolean;

Const
  strSection : Array[Low(TDlgType)..High(TDlgType)] Of String = (
    'Pre-Compilation', 'Post-Compilation');

Var
  Processes: TProcessCollection;

Begin
  Result := False;
  With TfrmConfigure.Create(Nil) Do
    Try
      FProject   := Project;
      FGlobalOps := GlobalOps;
      FDlgType := DlgType;
      Caption    := Format('Programmes to execute %s compilation: %s', [DlgTypes[DlgType],
        GetProjectName(Project)]);
      lblCompile.Caption := Format(lblCompile.Caption, [DlgTypes[DlgType]]);
      chkWarn.Caption := Format(chkWarn.Caption, [DlgTypes[DlgType]]);
      InitialiseOptions(Project);
      Processes := TProcessCollection.Create;
      Try
        AddProcesses(Processes, strSection[DlgType], lvCompile, Project);
      Finally
        Processes.Free;
      End;
      lvCompileSelectItem(Nil, Nil, False);
      If ShowModal = mrOK Then
        Begin
          SaveOptions(Project);
          Processes := TProcessCollection.Create;
          Try
            SaveProcesses(Processes, lvCompile, strSection[DlgType], Project);
          Finally
            Processes.Free;
          End;
          Result                     := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   Project as an IOTAProject

**)
Procedure TfrmConfigure.InitialiseOptions(Project: IOTAProject);

Begin
  With TMemIniFile.Create(FGlobalOps.INIFileName) Do
    Try
      Top    := ReadInteger(DlgTypes[FDlgType] + ' Dlg', 'Top', (Screen.Height - Height) Div 2);
      Left   := ReadInteger(DlgTypes[FDlgType] + ' Dlg', 'Left', (Screen.Width - Width) Div 2);
      Height := ReadInteger(DlgTypes[FDlgType] + ' Dlg', 'Height', Height);
      Width  := ReadInteger(DlgTypes[FDlgType] + ' Dlg', 'Width', Width);
    Finally
      Free;
    End;
  With FGlobalOps.ProjectOptions(Project) Do
    Try
      If FDlgType = dtBefore Then
        chkWarn.Checked := WarnBefore
      Else
        chkWarn.Checked := WarnAfter;
    Finally
      Free;
    End;
End;

(**

  This procedure configures the item in the list view.

  @precon  Item must be a valid.
  @postcon Configures the item in the list view.

  @param   Item    as a TListItem
  @param   Process as a TProcessInfo

**)
Procedure TfrmConfigure.AddListItem(Item: TListItem; Process: TProcessInfo);

Begin
  Item.Checked := Process.boolEnabled;
  Item.Caption := Process.strTitle;
  Item.SubItems.Add(Process.strEXE);
  Item.SubItems.Add(Process.strParams);
  Item.SubItems.Add(Process.strDir);
End;

(**

  This procedure adds processes to the passed list view.

  @precon  Processes and ListView must be valid instances.
  @postcon Adds processes to the passed list view.

  @param   Processes  as a TProcessCollection
  @param   strSection as a String
  @param   ListView   as a TListView
  @param   Project    as an IOTAProject

**)
Procedure TfrmConfigure.AddProcesses(Processes: TProcessCollection; strSection: String;
  ListView: TListView; Project: IOTAProject);

Var
  i      : Integer;
  Item   : TListItem;
  ProjectOps: TProjectOptions;

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
    ProjectOps.Free;
  End;
End;

(**

  This method saves the project options to the ini file.

  @precon  None.
  @postcon Saves the project options to the ini file.

  @param   Project as an IOTAProject

**)
Procedure TfrmConfigure.SaveOptions(Project: IOTAProject);

Begin
  With TMemIniFile.Create(FGlobalOps.INIFileName) Do
    Try
      WriteInteger(DlgTypes[FDlgType] + ' Dlg', 'Top', Top);
      WriteInteger(DlgTypes[FDlgType] + ' Dlg', 'Left', Left);
      WriteInteger(DlgTypes[FDlgType] + ' Dlg', 'Height', Height);
      WriteInteger(DlgTypes[FDlgType] + ' Dlg', 'Width', Width);
      UpdateFile;
    Finally
      Free;
    End;
  With FGlobalOps.ProjectOptions(Project) Do
    Try
      If FDlgType = dtBefore Then
        WarnBefore := chkWarn.Checked
      Else
        WarnAfter := chkWarn.Checked;
    Finally
      Free;
    End;
End;

(**

  This procedure saves the list view items to an INI file.

  @precon  Processes and ListView must be valid instances.
  @postcon Saves the list view items to an INI file.

  @param   Processes  as a TProcessCollection
  @param   ListView   as a TListView
  @param   strSection as a String
  @param   Project    as an IOTAProject

**)
Procedure TfrmConfigure.SaveProcesses(Processes: TProcessCollection; ListView: TListView;
  strSection: String; Project: IOTAProject);

Var
  i: Integer;

Begin
  With ListView Do
    For i := 0 To Items.Count - 1 Do
      Processes.AddProcessInfo(Items[i].Checked, Items[i].SubItems[0],
        Items[i].SubItems[1], Items[i].SubItems[2], Items[i].Caption);
  Processes.SaveToINI(FGlobalOps.ProjectOptions(Project).INIFile, strSection);
End;

(**

  This is an on double click event handler for the Before Compile List View.

  @precon  None.
  @postcon Edits the selected item.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.lvCompileDblClick(Sender: TObject);
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
Procedure TfrmConfigure.lvCompileSelectItem(Sender: TObject; Item: TListItem;
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

  This is an on resize event handler for both list view controls.

  @precon  None.
  @postcon Ensures the columns of the list views are wide enough to display in
           the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmConfigure.lvResize(Sender: TObject);

Var
  i: Integer;

Begin
  With Sender As TListView Do
    Begin
      i                := ClientWidth;
      Columns[0].Width := Trunc(i * 0.15);
      Columns[1].Width := Trunc(i * 0.35);
      Columns[2].Width := Trunc(i * 0.2);
      Columns[3].Width := Trunc(i * 0.3);
    End;
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
Procedure TfrmConfigure.lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Var DefaultDraw: Boolean);

Var
  i       : Integer;
  R, ItemR: TRect;
  Buffer  : Array [0 .. 2048] Of Char;
  Ops     : Integer;

  (**

    This function returns display rectangle for the given indexed sub item.

    @precon  iIndex must be a valid SubItem index..
    @postcon Returns display rectangle for the given indexed sub item.

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(iIndex: Integer): TRect;

  Var
    j: Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j  := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
    Inc(Result.Top, 2);    // Padding / Margin
    Inc(Result.Bottom, 2); // Padding / Margin
    Inc(Result.Left, 6);   // Padding / Margin
    Dec(Result.Right, 6);  // Padding / Margin
  End;

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
  Inc(R.Top, 2);    // Padding / Margin
  Inc(R.Bottom, 2); // Padding / Margin
  Inc(R.Left, 2);   // Padding / Margin
  Dec(R.Right, 2);  // Padding / Margin
  Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_END_ELLIPSIS Or DT_NOPREFIX;
  StrPCopy(Buffer, Item.Caption);
  DrawText(Sender.Canvas.Handle, Buffer, Length(Item.Caption), R, Ops);
  // Draw Sub Items
  For i := 0 To 2 Do
    Begin
      Case i Of
        0, 2:
          Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_PATH_ELLIPSIS Or DT_NOPREFIX;
        1:
          Ops := DT_LEFT Or DT_MODIFYSTRING Or DT_END_ELLIPSIS Or DT_NOPREFIX;
      Else
        Ops := DT_LEFT;
      End;
      R := GetSubItemRect(i);
      StrPCopy(Buffer, Item.SubItems[i]);
      Sender.Canvas.Brush.Color := clWindow;
      If Item.Selected Then
        Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Refresh;
      DrawText(Sender.Canvas.Handle, Buffer, Length(Item.SubItems[i]), R, Ops);
      R.Left := R.Right;
    End;
End;

(**

  This method swaps the information in the 2 list view items.

  @precon  None.
  @postcon Swaps the information in the 2 list view items.

  @param   Item1 as a TListItem
  @param   Item2 as a TListItem

**)
Procedure TfrmConfigure.SwapItems(Item1, Item2: TListItem);

Var
  strTitle, strEXE, strParam, strDir: String;
  boolEnabled                       : Boolean;

Begin
  strTitle          := Item1.Caption;
  strEXE            := Item1.SubItems[0];
  strParam          := Item1.SubItems[1];
  strDir            := Item1.SubItems[2];
  boolEnabled       := Item1.Checked;
  Item1.Caption     := Item2.Caption;
  Item1.SubItems[0] := Item2.SubItems[0];
  Item1.SubItems[1] := Item2.SubItems[1];
  Item1.SubItems[2] := Item2.SubItems[2];
  Item1.Checked     := Item2.Checked;
  Item2.Caption     := strTitle;
  Item2.SubItems[0] := strEXE;
  Item2.SubItems[1] := strParam;
  Item2.SubItems[2] := strDir;
  Item2.Checked     := boolEnabled;
End;

End.
