(**

  This module contains often used code for use through out this application.

  @Version 1.113
  @Author  David Hoyle
  @Date    21 Nov 2021

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
unit ITHelper.TestingHelperUtils;

interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  Graphics,
  Classes,
  Contnrs,
  Menus,
  Forms,
  ITHelper.Interfaces,
  ITHelper.Types;

Type
  (** An enumerate to define the type of information to be returned from GetProcInfo. **)
  TProcInfoType = (pitEXE, pitParam, pitDir);

  (** An enumerate to define which message should be cleared from the IDE
      message window. **)
  TClearMessage = (cmCompiler, cmSearch, cmTool, cmGroup);
  (** A set of the above message types. **)
  TClearMessages = Set of TClearMessage;

  (** A record to hold version information. **)
  TVersionInfo = Record
    iMajor  : Integer;
    iMinor  : Integer;
    iBugfix : Integer;
    iBuild  : Integer;
  End;

  (** This is a record to encapsulate all the Tools API utility code. **)
  TITHToolsAPIFunctions = Record
  Strict Private
    {$IFNDEF CONSOLE_TESTRUNNER}
    Class Procedure SaveModule(Const MessageMgr : IITHMessageManager; Const Module : IOTAModule); Static;
    {$ENDIF}
  Public
    {$IFNDEF CONSOLE_TESTRUNNER}
    Class Procedure SaveAllModifiedFiles(Const MessageMgr : IITHMessageManager); Static;
    Class Procedure SaveProjectModifiedFiles(Const MessageMgr : IITHMessageManager;
      Const Project : IOTAProject); Static;
    Class Function  ActiveProject : IOTAProject; Static;
    Class Function  ExpandMacro(Const strPath : String; Const strFileName : String) : String; Static;
    Class Function  CreateMenuItem(Const strName, strCaption, strParentMenu : String;
      Const ClickProc, UpdateProc : TNotifyEvent; Const boolBefore, boolChildMenu : Boolean;
      Const strShortCut : String; Const iMaskColour : TColor = clLime) : TMenuItem; Static;
    Class Procedure ClearMessages(Const Msg : TClearMessages); Static;
    Class Procedure ShowHelperMessages(Const boolITHGroup : Boolean); Static;
    Class Procedure RegisterFormClassForTheming(Const AFormClass : TCustomFormClass;
      Const Component : TComponent = Nil); Static;
    Class Procedure ApplyTheming(Const Component : TComponent); Static;
    {$ENDIF}
    Class Function  ProjectGroup: IOTAProjectGroup; Static;
    Class Function  GetProjectName(Const Project : IOTAProject) : String; Static;
    Class Function  ResolvePath(Const strFName, strPath : String) : String; Static;
    Class Function  Actions : TObjectList; Static;
    Class Function  ITHHTMLHelpFile(Const strContext : String = '') : String; Static;
  End;


Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  Controls,
  ActnList,
  {$IFNDEF CONSOLE_TESTRUNNER} 
  ITHelper.CustomMessages,
  {$ENDIF}
  ComCtrls, 
  ITHelper.ResourceStrings;

{$IFNDEF CONSOLE_TESTRUNNER}
ResourceString
  (** A resource string for the save modified files message. **)
  strZippingSaved = '[Zipping] %s: Saved!';

Const
  (** A constant string for the build message window page. **)
  strBuild = 'Build';
  (** A constant name suffix for images. **)
  strImageResNameSuffix = 'Image';
{$ENDIF}

Var
  (** A private variable to is used to hold action reference so that they
      can be removed from the IDE. **)
  FOTAActions : TObjectList;

{$IFNDEF CONSOLE_TESTRUNNER}
Function FindMenuItem(Const strParentMenu: String): TMenuItem; Forward;
{$ENDIF}

{$IFNDEF CONSOLE_TESTRUNNER}
(**

  This method adds an image from the projects resource (bitmap) to the IDEs image list. The image name in
  the resource must end in Image as this is appended to the given name. An integer for the position of 
  that image in the IDEs image list is returned.

  @precon  None.
  @postcon The named image is loaded from the projects resource and put into the IDEs image list and its
           index returned.

  @Note    Different technicals are used for the different IDE version.
  @Note    The way described in Delphi 2010s ToolsAPI file causes an exception and there is not used.

  @param   strImageName as a String as a constant
  @param   iMaskColour  as a TColor as a constant
  @return  an Integer

**)
Function AddImageToIDE(Const strImageName : String; Const iMaskColour : TColor) : Integer;

Var
  NS : INTAServices;
  ilImages : TImageList;
  BM : TBitMap;

begin
  Result := -1;
  If FindResource(hInstance, PChar(strImageName + strImageResNameSuffix), RT_BITMAP) > 0 Then
    If Supports(BorlandIDEServices, INTAServices, NS) Then
      Begin
        ilImages := TImageList.Create(Nil);
        Try
          BM := TBitMap.Create;
          Try
            BM.LoadFromResourceName(hInstance, strImageName + strImageResNameSuffix);
            {$IFDEF D2005}
            {$IFDEF RS110}
            {$ELSE}
            ilImages.AddMasked(BM, iMaskColour);
            // EXCEPTION: Operation not allowed on sorted list
            // Result := NTAS.AddImages(ilImages, 'OTATemplateImages');
            Result := NS.AddImages(ilImages);
            {$ENDIF RS110}
            Result := NS.AddImage(strImageName + strImageResNameSuffix, [BM]);
            {$ELSE}
            Result := NS.AddMasked(BM, iMaskColour);
            {$ENDIF D2005}
          Finally
            BM.Free;
          End;
        Finally
          ilImages.Free;
        End;
      End;
end;

(**

  This method finds and returns a reference to the named menu item in the IDE else returns nil.

  @precon  None.
  @postcon Finds and returns a reference to the named menu item in the IDE else returns nil.

  @param   strParentMenu as a String as a constant
  @return  a TMenuItem

**)
function FindMenuItem(Const strParentMenu : String): TMenuItem;

  (**

    This method iterates the sub-menus of a main menu item.

    @precon  Menu must be a valid menu item.
    @postcon Iterates the sub-menus of a main menu item.

    @param   Menu as a TMenuItem as a constant
    @return  a TMenuItem

  **)
  Function IterateSubMenus(Const Menu : TMenuItem) : TMenuItem;

  Var
    iSubMenu : Integer;

  Begin
    Result := Nil;
    For iSubMenu := 0 To Menu.Count - 1 Do
      Begin
        If CompareText(strParentMenu, Menu[iSubMenu].Name) = 0 Then
          Result := Menu[iSubMenu]
        Else
          Result := IterateSubMenus(Menu[iSubMenu]);
        If Result <> Nil Then
          Break;
      End;
  End;

Var
  iMenu : Integer;
  NS : INTAServices;
  Items : TMenuItem;

begin
  Result := Nil;
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    For iMenu := 0 To NS.MainMenu.Items.Count - 1 Do
      Begin
        Items := NS.MainMenu.Items;
        If CompareText(strParentMenu, Items[iMenu].Name) = 0 Then
          Result := Items[iMenu]
        Else
          Result := IterateSubMenus(Items);
        If Result <> Nil Then
          Break;
      End;
end;

(**

  This method removes any tool bar buttons that correspond to actions from this
  expert so that there are no access violations once it is removed.

  @precon  None.
  @postcon Removes any tool bar buttons that correspond to actions from this
           expert so that there are no access violations once it is removed.

**)
Procedure RemoveToolbarButtonsAssociatedWithActions;

  (**

    This function checks to see whether the given action is in our action list and returns true if it is.

    @precon  None.
    @postcon Checks to see whether the given action is in our action list and returns true if it is.

    @param   Action as a TBasicAction as a constant
    @return  a Boolean

  **)
  Function IsCustomAction(Const Action : TBasicAction) : Boolean;

  Var
    i: Integer;

  Begin
    Result := False;
    For i := 0 To FOTAActions.Count - 1 Do
      If Action = FOTAActions[i] Then
        Begin
          Result := True;
          Break;
        End;
  End;

  (**

    This method iterates over the buttons on a toolbar and removed the button if its action corresponds 
    to an action from this expert.

    @precon  None.
    @postcon Iterates over the buttons on a toolbar and removed the button if its action corresponds to 
             an action from this expert.

    @param   TB as a TToolbar as a constant

  **)
  Procedure RemoveAction(Const TB : TToolbar);

  Var
    i: Integer;

  Begin
    If TB <> Nil Then
      For i := TB.ButtonCount - 1 DownTo 0 Do
        Begin
          If IsCustomAction(TB.Buttons[i].Action) Then
            TB.RemoveControl(TB.Buttons[i]);
        End;
  End;

Var
  NS : INTAServices;

Begin
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    Begin
      RemoveAction(NS.ToolBar[sCustomToolBar]);
      RemoveAction(NS.Toolbar[sStandardToolBar]);
      RemoveAction(NS.Toolbar[sDebugToolBar]);
      RemoveAction(NS.Toolbar[sViewToolBar]);
      RemoveAction(NS.Toolbar[sDesktopToolBar]);
      RemoveAction(NS.Toolbar[sInternetToolBar]);
      RemoveAction(NS.Toolbar[sCORBAToolBar]);
      RemoveAction(NS.Toolbar[sAlignToolbar]);
      RemoveAction(NS.Toolbar[sBrowserToolbar]);
      RemoveAction(NS.Toolbar[sHTMLDesignToolbar]);
      RemoveAction(NS.Toolbar[sHTMLFormatToolbar]);
      RemoveAction(NS.Toolbar[sHTMLTableToolbar]);
      RemoveAction(NS.Toolbar[sPersonalityToolBar]);
      RemoveAction(NS.Toolbar[sPositionToolbar]);
      RemoveAction(NS.Toolbar[sSpacingToolbar]);
    End;
End;
{$ENDIF}

(**

  This function returns a reference to the Experts actions.

  @precon  None.
  @postcon Returns a reference to the Experts actions.

  @return  a TObjectList

**)
Class Function TITHToolsAPIFunctions.Actions : TObjectList;

Begin
  Result := FOTAActions;
End;

{$IFNDEF CONSOLE_TESTRUNNER}
(**

  This method returns the active project in the IDE else returns Nil if there is
  no active project.

  @precon  None.
  @postcon Returns the active project in the IDE else returns Nil if there is
           no active project.

  @return  an IOTAProject

**)
Class Function TITHToolsAPIFunctions.ActiveProject : IOTAProject;

var
  G : IOTAProjectGroup;

Begin
  Result := Nil;
  G := ProjectGroup;
  If G <> Nil Then
    Result := G.ActiveProject;
End;

{$IFNDEF CONSOLE_TESTRUNNER}
(**

  This method wraps the Tools API Apply Theme method from the IDE Theming Services so that components that
  do not theme can be handled here on one place.

  @precon  Component must be a valid root component to be themed along with its children.
  @postcon The component and its children are themed.

  @param   Component as a TComponent as a constant

**)
Class Procedure TITHToolsAPIFunctions.ApplyTheming(Const Component: TComponent);

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices250;
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.ApplyTheme(Component);
  {$ENDIF RS110}
End;
{$ENDIF CONSOLE_TESTRUNNER}

(**

  This method clears the IDE message window of the given message types.

  @precon  None.
  @postcon Clears the IDE message window of the given message types.

  @param   Msg as a TClearMessages as a constant

**)
Class Procedure TITHToolsAPIFunctions.ClearMessages(Const Msg : TClearMessages);

Var
  MS : IOTAMessageServices;
  Group : IOTAMessageGroup;
  
Begin
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    Begin
      If cmCompiler In Msg Then
        MS.ClearCompilerMessages;
      If cmSearch In Msg Then
        MS.ClearSearchMessages;
      If cmTool In Msg Then
        MS.ClearToolMessages;
      If cmGroup In Msg Then
        Begin
          Group := MS.GetGroup(strITHelperGroup);
          If Assigned(Group) Then
            Begin
              MS.ClearMessageGroup(Group);
              MS.ClearToolMessages(Group);
            End;
        End;
    End;
End;

(**

  This method does the following: Adds an image to the IDE if found in the project resource, creates a 
  menu item, creates an action in the IDE for the menu if one is required, associated the action with the
  menu and adds the menu to the IDE as a sibling or underneath the parent item as required.

  @precon  None.
  @postcon Returns a reference to the menu.

  @note    You should always keep a reference to the Main menu item you create so you can remove you 
           menus from the IDE.
  @nometric LongParameterList Toxicity

  @param   strName       as a String as a constant
  @param   strCaption    as a String as a constant
  @param   strParentMenu as a String as a constant
  @param   ClickProc     as a TNotifyEvent as a constant
  @param   UpdateProc    as a TNotifyEvent as a constant
  @param   boolBefore    as a Boolean as a constant
  @param   boolChildMenu as a Boolean as a constant
  @param   strShortCut   as a String as a constant
  @param   iMaskColour   as a TColor as a constant
  @return  a TMenuItem

**)
Class Function TITHToolsAPIFunctions.CreateMenuItem( //FI:C102
                 Const strName, strCaption, strParentMenu : String;
                 Const ClickProc, UpdateProc : TNotifyEvent;
                 Const boolBefore, boolChildMenu : Boolean;
                 Const strShortCut : String;
                 Const iMaskColour : TColor = clLime
               ) : TMenuItem;

Const
  strActionNameSuffix = 'Action';
  strITHelperMenuCategory = 'ITHelperMenus';
  strMenuNameSuffix = 'Menu';

Var
  NS : INTAServices;
  CA : TAction;
  //{$IFNDEF D2005}
  miMenuItem : TMenuItem;
  //{$ENDIF}
  iImageIndex : Integer;

begin //FI:C101
  Result := Nil;
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    Begin
      // Add Image to IDE
      iImageIndex := AddImageToIDE(strName, iMaskColour);
      // Create the IDE action (cached for removal later)
      CA := Nil;
      Result := TMenuItem.Create(NS.MainMenu);
      If Assigned(ClickProc) Then
        Begin
          CA := TAction.Create(NS.ActionList);
          CA.ActionList := NS.ActionList;
          CA.Name := strName + strActionNameSuffix;
          CA.Caption := strCaption;
          CA.OnExecute := ClickProc;
          CA.OnUpdate := UpdateProc;
          CA.ShortCut := TextToShortCut(strShortCut);
          CA.Tag := TextToShortCut(strShortCut);
          {$IFDEF RS110}
          CA.ImageName := strName + strImageResNameSuffix;
          {$ELSE}
          CA.ImageIndex := iImageIndex;
          {$ENDIF RS110}
          CA.Category := strITHelperMenuCategory;
          FOTAActions.Add(CA);
        End Else
      If strCaption <> '' Then
        Begin
          Result.Caption := strCaption;
          Result.ShortCut := TextToShortCut(strShortCut);
          Result.ImageIndex := iImageIndex;
        End Else
          Result.Caption := '-';
      // Create menu (removed through parent menu)
      Result.Action := CA;
      Result.Name := strName + strMenuNameSuffix;
      // Create Action and Menu.
      //{$IFDEF D2005}
      // This is the new way to do it BUT doesn't create icons for the menu.
      //NTAS.AddActionMenu(strParentMenu + 'Menu', CA, Result, boolBefore, boolChildMenu);
      //{$ELSE}
      miMenuItem := FindMenuItem(strParentMenu + strMenuNameSuffix);
      If miMenuItem <> Nil Then
        Begin
          If Not boolChildMenu Then
            Begin
              If boolBefore Then
                miMenuItem.Parent.Insert(miMenuItem.MenuIndex, Result)
              Else
                miMenuItem.Parent.Insert(miMenuItem.MenuIndex + 1, Result);
            End Else
              miMenuItem.Add(Result);
        End;
      //{$ENDIF}
    End;
end;

(**

  This function returns the passed path / filename with any of the below macros expanded. {$PROJPATH$} 
  The project path including the trailing backslash {$PROJDRIVE$} The project drive including the colon.

  @precon  Project must be a valid instance.
  @postcon Returns the passed path / filename with any macros expanded.

  @param   strPath     as a String as a constant
  @param   strFileName as a String as a constant
  @return  a String

**)
Class Function TITHToolsAPIFunctions.ExpandMacro(Const strPath : String;
  Const strFileName : String) : String;

Const
  strPROJPATHMacroName = '{$PROJPATH$}';
  strPROJDRIVEMacroName = '{$PROJDRIVE$}';

Begin
  Result := strPath;
  Result := StringReplace(Result, strPROJPATHMacroName, ExtractFilePath(strFileName),
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, strPROJDRIVEMacroName, ExtractFileDrive(strFileName),
    [rfReplaceAll, rfIgnoreCase]);
End;
{$ENDIF}

(**

  This function returns a project name with the DPRP or DPK extension.

  @precon  Project must be a valid instance of a IOTAProject interface.
  @postcon Returns a project name with the DPRP or DPK extension.

  @param   Project as an IOTAProject as a constant
  @return  a String

**)
Class Function TITHToolsAPIFunctions.GetProjectName(Const Project : IOTAProject) : String;

Const
  strDPRExt = '.dpr';
  strDPKExt = '.dpk';

Var
  i : Integer;
  strExt: String;

Begin
  Result := ExtractFileName(Project.FileName);
  For i := 0 To Project.ModuleFileCount - 1 Do
    Begin
      strExt := LowerCase(ExtractFileExt(Project.ModuleFileEditors[i].FileName));
      If (strExt = strDPRExt) Or (strExt = strDPKExt) Then
        Begin
          Result := ChangeFileExt(Result, strExt);
          Break;
        End;
    End;
End;

(**

  This function returns the ITHelper`s HTML Help file with an optional page reference.

  @precon  None.
  @postcon Returns the ITHelper`s HTML Help file with an optional page reference.

  @param   strContext as a String as a constant
  @return  a String

**)
Class Function TITHToolsAPIFunctions.ITHHTMLHelpFile(Const strContext : String = '') : String;

Const
  strITHelperCHMFileName = 'ITHelper.chm';
  strHTMLContext = '::/%s.html';

Var
  iSize: Cardinal;

Begin
  SetLength(Result, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(Result), MAX_PATH);
  SetLength(Result, iSize);
  Result := ExtractFilePath(Result) + strITHelperCHMFileName;
  If strContext <> '' Then
    Result := Result + Format(strHTMLContext, [strContext]);
End;

(**

  This method returns the current project group reference or nil if there is no
  project group open.

  @precon  None.
  @postcon Returns the current project group reference or nil if there is no
           project group open.

  @return  an IOTAProjectGroup

**)
Class Function TITHToolsAPIFunctions.ProjectGroup: IOTAProjectGroup;

Var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProjectGroup: IOTAProjectGroup;

Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) Then
    Begin
      For i := 0 To AModuleServices.ModuleCount - 1 Do
        Begin
          AModule := AModuleServices.Modules[i];
          If (AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK) Then
            Break;
        End;
      Result := AProjectGroup;
    End;
end;

{$IFNDEF CONSOLE_TESTRUNNER}
(**

  This method wraps the IDE Theming Service Register Form Class method to simplify its implementation.

  @precon  None.
  @postcon The form class passed is registered for theming.

  @param   AFormClass as a TCustomFormClass as a constant
  @param   Component  as a TComponent as a constant

**)
Class Procedure TITHToolsAPIFunctions.RegisterFormClassForTheming(Const AFormClass : TCustomFormClass;
  Const Component : TComponent = Nil);

{$IFDEF RS102}
Var
  {$IFDEF RS104}
  ITS : IOTAIDEThemingServices;
  {$ELSE}
  ITS : IOTAIDEThemingServices250;
  {$ENDIF RS104}
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  {$IFDEF RS104}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
  {$ELSE}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
  {$ENDIF RS104}
    If ITS.IDEThemingEnabled Then
      Begin
        ITS.RegisterFormClass(AFormClass);
        If Assigned(Component) Then
          ITS.ApplyTheme(Component);
      End;
  {$ENDIF RS102}
End;
{$ENDIF CONSOLE_TESTRUNNER}

(**

  This function resolve the path of the file name with respect to the
  path given to produce a full path to the file name.

  @precon  None.
  @postcon Resolve the path of the file name with respect to the
           path given to produce a full path to the file name

  @param   strFName as a String as a constant
  @param   strPath  as a String as a constant
  @return  a String

**)
Class Function TITHToolsAPIFunctions.ResolvePath(Const strFName, strPath : String) : String;

Const
  strSingleDot = '.\';
  strDoubleDot = '..\';

Var
  strFileName : String;
  strPathName :String;

Begin
  strFileName := strFName;
  strPathName := strPath;
  If strFileName[1] = '.' Then
    Begin
      Repeat
        If Copy(strFileName, 1, Length(strSingleDot)) = strSingleDot Then
          strFileName := Copy(strFileName, Length(strSingleDot) + 1,
            Length(strFileName) - Length(strSingleDot));
        If Copy(strFileName, 1, Length(strDoubleDot)) = strDoubleDot Then
          Begin
            strFileName := Copy(strFileName, Length(strDoubleDot) + 1,
              Length(strFileName) - Length(strDoubleDot));
            strPathName := ExtractFilePath(Copy(strPathName, 1, Length(strPathName) - 1));
          End;
      Until strFileName[1] <> '.';
      Result := strPathName + strFileName;
    End Else
    Begin
      If ExtractFilePath(strFileName) = '' Then
        Result := strPathName + strFileName
      Else
        Result := strFileName;
    End;
End;

{$IFNDEF CONSOLE_TESTRUNNER}
(**

  This method iterates the editor buffer checking for modified files. If one is modified it save the 
  file. If Prompt is true then you are prompted to save the file else it is automatically saved.

  @precon  None.
  @postcon Iterates the files open in the IDE and if they are modified saves the files.

  @param   MessageMgr as an IITHMessageManager as a constant

**)
Class Procedure TITHToolsAPIFunctions.SaveAllModifiedFiles(Const MessageMgr : IITHMessageManager);

Var
  ES : IOTAEditorServices;
  Iterator: IOTAEditBufferIterator;
  i: Integer;
  E: IOTAEditBuffer;

Begin
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    Begin
      If ES.GetEditBufferIterator(Iterator) Then
        Begin
          For i := 0 To Iterator.Count - 1 Do
            Begin
              E := Iterator.EditBuffers[i];
              If E.IsModified Then
                Begin
                  E.Module.Save(False, True);
                  MessageMgr.AddMsg(Format(strZippingSaved, [ExtractFileName(E.Module.FileName)]),
                    fnTools, ithfDefault);
                End;
            End;
        End;
    End;
End;

(**

  This method attempts to save the given module is if the module is valid, its current editor are valid 
  and the editor is modified.

  @precon  None.
  @postcon If the module has been modified it is saved.

  @param   MessageMgr as an IITHMessageManager as a constant
  @param   Module     as an IOTAModule as a constant

**)
Class Procedure TITHToolsAPIFunctions.SaveModule(Const MessageMgr : IITHMessageManager;
  Const Module : IOTAModule);

Begin
  If Assigned(Module) And Assigned(Module.CurrentEditor) And Module.CurrentEditor.Modified Then
    Begin
      Module.Save(False, True);
      MessageMgr.AddMsg(Format(strZippingSaved, [ExtractFileName(Module.FileName)]), fnHeader,
        ithfDefault);
    End;
End;

(**

  This method iterate through the files in the project and saves any files that have been modified.

  @precon  None.
  @postcon An files in the project which have been modified are saved.

  @param   MessageMgr as an IITHMessageManager as a constant
  @param   Project    as an IOTAProject as a constant

**)
Class Procedure TITHToolsAPIFunctions.SaveProjectModifiedFiles(Const MessageMgr : IITHMessageManager;
  Const Project : IOTAProject);

Var
  iModule: Integer;
  MI: IOTAModuleInfo;
  M: IOTAModule;
  MS: IOTAModuleServices;

Begin
  If Assigned(Project) Then
    If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
      Begin
        For iModule := 0 To Project.GetModuleCount - 1 Do
          Begin
            MI := Project.GetModule(iModule);
            M := MS.FindModule(MI.FileName);
            If Assigned(M) Then
              SaveModule(MessageMgr, M);
          End;
        SaveModule(MessageMgr, Project);
      End;
End;

(**

  This method displays the package`s message tab.

  @precon  None.
  @postcon Displays the package`s message tab.

  @param   boolITHGroup as a Boolean as a constant

**)
Class Procedure TITHToolsAPIFunctions.ShowHelperMessages(Const boolITHGroup : Boolean);

Var
  MS : IOTAMessageServices;
  G : IOTAMessageGroup;

Begin
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    Begin
      G := Nil;
      If boolITHGroup Then
        G := MS.GetGroup(strITHelperGroup)
      Else
        G := MS.GetGroup(strBuild);
      If Application.MainForm.Visible Then
        MS.ShowMessageView(G);
    End;
End;
{$ENDIF}

(** Creates an object list for storing all action reference so they can be
    removed from the IDE. **)
Initialization
  FOTAActions := TObjectList.Create(True);
(** Frees the actions and in doing so removes them from the IDE. **)
Finalization
  {$IFNDEF CONSOLE_TESTRUNNER}
  RemoveToolbarButtonsAssociatedWithActions;
  {$ENDIF}
  FOTAActions.Free;
end.
