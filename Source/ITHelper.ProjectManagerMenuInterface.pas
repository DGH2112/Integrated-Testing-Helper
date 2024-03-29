(**

  This module contains the interfaces for creating menus in the IDEs project manager.
  There are two interfaces, one for D2005 to 2009 and another for D2010 onwards.

  @Author  David Hoyle
  @Version 1.003
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
Unit ITHelper.ProjectManagerMenuInterface;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

{$IFDEF D2005}
Uses
  ToolsAPI,
  ITHelper.Wizard,
  Classes,
  Menus;

Type
  (** A class to handle the creation of a menu for the project manager. **)
  TITHProjectManagerMenu = Class(TNotifierObject, IUnknown, IOTANotifier,
    {$IFDEF D2010} IOTAProjectMenuItemCreatorNotifier {$ELSE} INTAProjectMenuCreatorNotifier {$ENDIF})
  Strict Private
    FWizard: TITHWizard;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IOTANotifier
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    {$IFDEF D2010}
    // IOTAProjectMenuItemCreatorNotifier
    Procedure AddMenu(Const Project: IOTAProject; Const IdentList: TStrings;
      Const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    {$ELSE}
    // INTAProjectMenuCreatorNotifier
    Function AddMenu(Const Ident: String): TMenuItem;
    Function CanHandle(Const Ident: String): Boolean;
    {$ENDIF}
    // General Methods
    Procedure OptionsClick(Sender: TObject);
  Public
    Constructor Create(Const Wizard: TITHWizard);
    Destructor Destroy; Override;
  End;

{$IFDEF D2010}
(** A class to define a Delphi 2010 Project Menu Item. **)
  TITHelperProjectMenu = Class(TNotifierObject, IUnknown, IOTANotifier, IOTALocalMenu,
    IOTAProjectManagerMenu)
  Strict Private
    FWizard  : TITHWizard;
    FProject : IOTAProject;
    FPosition: Integer;
    FCaption : String;
    FName    : String;
    FVerb    : String;
    FParent  : String;
    FSetting : TSetting;
  Strict Protected
    // IOTALocalMenu
    Function GetCaption: String;
    Function GetChecked: Boolean;
    Function GetEnabled: Boolean;
    Function GetHelpContext: Integer;
    Function GetName: String;
    Function GetParent: String;
    Function GetPosition: Integer;
    Function GetVerb: String;
    Procedure SetCaption(Const Value: String);
    Procedure SetChecked(Value: Boolean);
    Procedure SetEnabled(Value: Boolean);
    Procedure SetHelpContext(Value: Integer);
    Procedure SetName(Const Value: String);
    Procedure SetParent(Const Value: String);
    Procedure SetPosition(Value: Integer);
    Procedure SetVerb(Const Value: String);
    // IOTAProjectManagerMenu
    Function GetIsMultiSelectable: Boolean;
    Procedure SetIsMultiSelectable(Value: Boolean);
    Procedure Execute(Const MenuContextList: IInterfaceList); Overload;
    Function PreExecute(Const MenuContextList: IInterfaceList): Boolean;
    Function PostExecute(Const MenuContextList: IInterfaceList): Boolean;
    // General Methods
  Public
    Constructor Create(Const Wizard: TITHWizard; Const Project: IOTAProject; Const strCaption, strName,
      strVerb, strParent: String; Const iPosition: Integer; Const Setting: TSetting);
    Destructor Destroy; Override;
  End;
{$ENDIF}
{$ENDIF}

Implementation

{$IFDEF D2005}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  {$IFNDEF D2010}
  ITHelper.CommonFunctions,
  {$ENDIF}
  ITHelper.TestingHelperUtils;

ResourceString
  (** A resource string for plug-ins main menu. **)
  strMainCaption = 'Integrated Testing Helper';
  (** A resource string for Project options menu. **)
  strProjectCaption = 'Project Options';
  (** A resource string for Before Compilation menu. **)
  strBeforeCaption = 'Before Compilation';
  (** A resource string for After Compilation Menu. **)
  strAfterCaption = 'After Compilation';
  (** A resource string for ZIP Options menu. **)
  strZIPCaption = 'ZIP Options';

Const
  (** A constant string identifier for the main menu. **)
  strMainName = 'ITHelperMainMenu';
  (** A constant string identifier for Project Options. **)
  strProjectName = 'ITHProjMgrProjectOptions';
  (** A constant string identifier for Before Options. **)
  strBeforeName = 'ITHProjMgrBefore';
  (** A constant string identifier for After Options. **)
  strAfterName = 'ITHProjMgrAfter';
  (** A constant string identifier for ZIP Options. **)
  strZIPName = 'ITHProjMgrZIPOptions';

{ TProjectManagerMenu }

{$IFDEF D2010}
(**

  This method create a menu to be displayed in the project manager for handling the configuration of 
  Integrated Testing Helper Options.

  @precon  None.
  @postcon Create a menu to be displayed in the project manager for handling the configuration of 
           Integrated Testing Helper Options.

  @nocheck MissingCONSTInParam
  @nohints

  @param   Project                as an IOTAProject as a constant
  @param   IdentList              as a TStrings as a constant
  @param   ProjectManagerMenuList as an IInterfaceList as a constant
  @param   IsMultiSelect          as a Boolean

**)
Procedure TITHProjectManagerMenu.AddMenu(Const Project: IOTAProject; Const IdentList: TStrings;
  Const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);

Const
  strOptions = 'Options';

Var
  i, j     : Integer;
  iPosition: Integer;
  M        : IOTAProjectManagerMenu;

Begin
  For i := 0 To IdentList.Count - 1 Do
    If sProjectContainer = IdentList[i] Then
      Begin
        iPosition := 0;
        For j     := 0 To ProjectManagerMenuList.Count - 1 Do
          Begin
            M := ProjectManagerMenuList.Items[j] As IOTAProjectManagerMenu;
            If CompareText(M.Verb, strOptions) = 0 Then
              Begin
                iPosition := M.Position + 1;
                Break;
              End;
          End;
        ProjectManagerMenuList.Add(TITHelperProjectMenu.Create(FWizard, Project,
          strMainCaption, strMainName, strMainName, '', iPosition, seProject));
        ProjectManagerMenuList.Add(TITHelperProjectMenu.Create(FWizard, Project,
          strProjectCaption, strProjectName, strProjectName, strMainName, iPosition + 1,
          seProject));
        ProjectManagerMenuList.Add(TITHelperProjectMenu.Create(FWizard, Project,
          strBeforeCaption, strBeforeName, strBeforeName, strMainName, iPosition + 2,
          seBefore));
        ProjectManagerMenuList.Add(TITHelperProjectMenu.Create(FWizard, Project,
          strAfterCaption, strAfterName, strAfterName, strMainName, iPosition + 3,
          seAfter));
        ProjectManagerMenuList.Add(TITHelperProjectMenu.Create(FWizard, Project,
          strZIPCaption, strZIPName, strZIPName, strMainName, iPosition + 4, seZIP));
      End;
End;
{$ELSE}
(**

  This method create a menu to be displayed in the project manager for handling
  the configuration of Integrated Testing Helper Options.

  @precon  None.
  @postcon Create a menu to be displayed in the project manager for handling
           the configuration of Integrated Testing Helper Options.

  @param   Ident as a string as a constant
  @return  a TMenuItem

**)
Function TITHProjectManagerMenu.AddMenu(Const Ident: String): TMenuItem;

Var
  SM: TMenuItem;

Begin
  Result := Nil;
  If Like(sProjectContainer, Ident) Then
    Begin
      Result         := TMenuItem.Create(Nil);
      Result.Caption := strMainCaption;
      SM             := TMenuItem.Create(Nil);
      SM.Caption     := strProjectCaption;
      SM.Name        := strProjectName;
      SM.OnClick     := OptionsClick;
      Result.Add(SM);
      SM         := TMenuItem.Create(Nil);
      SM.Caption := strBeforeCaption;
      SM.Name    := strBeforeName;
      SM.OnClick := OptionsClick;
      Result.Add(SM);
      SM         := TMenuItem.Create(Nil);
      SM.Caption := strAfterCaption;
      SM.Name    := strAfterName;
      SM.OnClick := OptionsClick;
      Result.Add(SM);
      SM         := TMenuItem.Create(Nil);
      SM.Caption := strZIPCaption;
      SM.Name    := strZIPName;
      SM.OnClick := OptionsClick;
      Result.Add(SM);
    End;
End;
{$ENDIF}

(**

  This method is not implemented.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TITHProjectManagerMenu.AfterSave;
Begin
End;

(**

  This method is not implemented.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TITHProjectManagerMenu.BeforeSave;
Begin
End;

{$IFNDEF D2010}
(**

  This method is called for the selected node in the project manager asking if
  a menu should be created for the given ID. We return true for the identifier
  "ProjectContainer".

  @precon  None.
  @postcon Returns true for the identifier "ProjectContainer".

  @param   Ident as a string as a constant
  @return  a Boolean

**)
Function TITHProjectManagerMenu.CanHandle(Const Ident: String): Boolean;

Begin
  Result := sProjectContainer = Ident;
End;
{$ENDIF}

(**

  This is a constructor for the TITHProjectManagerMenu class.

  @precon  None.
  @postcon Holds a reference to the main wizard for configuring options.

  @param   Wizard as a TITHWizard as a constant

**)
Constructor TITHProjectManagerMenu.Create(Const Wizard: TITHWizard);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FWizard := Wizard;
End;

(**

  A destructor for the TITHProjectManagerMenu class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TITHProjectManagerMenu.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
End;

(**

  This method is not implemented.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TITHProjectManagerMenu.Destroyed;
Begin
End;

(**

  This method is not implemented.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TITHProjectManagerMenu.Modified;
Begin
End;

(**

  This is an on click event handler for the project manager menu items.

  @precon  None.
  @postcon Invokes the main wizards various option dialogues for the selected project.

  @param   Sender as a TObject

**)
Procedure TITHProjectManagerMenu.OptionsClick(Sender: TObject);

Var
  Project : IOTAProject;
  strIdent: String;
  PM : IOTAProjectManager;

Begin
  If Supports(BorlandIDEServices, IOTAProjectManager, PM) Then
    Begin
      Project := PM.GetCurrentSelection(strIdent);
      If Sender Is TMenuItem Then
        If (Sender As TMenuItem).Name = strMainCaption Then
          FWizard.ConfigureOptions(Project, seProject)
        Else If (Sender As TMenuItem).Name = strProjectName Then
          FWizard.ConfigureOptions(Project, seProject)
        Else If (Sender As TMenuItem).Name = strBeforeName Then
          FWizard.ConfigureOptions(Project, seBefore)
        Else If (Sender As TMenuItem).Name = strAfterName Then
          FWizard.ConfigureOptions(Project, seAfter)
        Else If (Sender As TMenuItem).Name = strZIPName Then
          FWizard.ConfigureOptions(Project, seZIP);
    End;
End;

{ TITHelperProjectMenu }

{$IFDEF D2010}

(**

  This is a constructor method for the TITHelperProjectMenu class.

  @precon  None.
  @postcon Initialises the class with reference to the project and the wizard.

  @param   Wizard     as a TITHWizard as a constant
  @param   Project    as an IOTAProject as a constant
  @param   strCaption as a String as a constant
  @param   strName    as a String as a constant
  @param   strVerb    as a String as a constant
  @param   strParent  as a String as a constant
  @param   iPosition  as an Integer as a constant
  @param   Setting    as a TSetting as a constant

**)
Constructor TITHelperProjectMenu.Create(Const Wizard: TITHWizard; Const Project: IOTAProject;
  Const strCaption, strName, strVerb, strParent: String; Const iPosition: Integer;
  Const Setting: TSetting);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TITHelperProjectMenu.Create', tmoTiming);{$ENDIF}
  FWizard   := Wizard;
  FProject  := Project;
  FPosition := iPosition;
  FCaption  := strCaption;
  FName     := strName;
  FVerb     := strVerb;
  FParent   := strParent;
  FSetting  := Setting;
End;

(**

  A destructor for the TITHelperProjectMenu class.

  @precon  None.
  @postcon Does nothing but is used for code site tracing.

**)
Destructor TITHelperProjectMenu.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TITHelperProjectMenu.Destroy', tmoTiming);{$ENDIF}
End;

(**

  This is an execute method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Displays the Configuration Options dialogue for the current project.

  @nohints

  @param   MenuContextList as an IInterfaceList as a constant

**)
Procedure TITHelperProjectMenu.Execute(Const MenuContextList: IInterfaceList);

Begin
  FWizard.ConfigureOptions(FProject, FSetting);
End;

(**

  This is an GetCaption method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns the menu caption to be displayed.

  @return  a String

**)
Function TITHelperProjectMenu.GetCaption: String;

Begin
  Result := FCaption;
End;

(**

  This is an GetChecked method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns false for the menu item to not be checked.

  @return  a Boolean

**)
Function TITHelperProjectMenu.GetChecked: Boolean;

Begin
  Result := False;
End;

(**

  This is an GetEnabled method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns true for the menu item to be enabled.

  @return  a Boolean

**)
Function TITHelperProjectMenu.GetEnabled: Boolean;

Begin
  Result := True;
End;

(**

  This is an GetHelpContext method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns zero for no context help.

  @return  a Integer

**)
Function TITHelperProjectMenu.GetHelpContext: Integer;

Begin
  Result := 0;
End;

(**

  This is an GetIsMultiSelectable method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Returns false for not being a multi-selectable menu item.

  @return  a Boolean

**)
Function TITHelperProjectMenu.GetIsMultiSelectable: Boolean;

Begin
  Result := False;
End;

(**

  This is an GetName method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns a name for the menu item.

  @return  a String

**)
Function TITHelperProjectMenu.GetName: String;

Begin
  Result := FName;
End;

(**

  This is an GetParent method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns an empty string since this menu item does not have a parent menu.

  @return  a String

**)
Function TITHelperProjectMenu.GetParent: String;

Begin
  Result := FParent;
End;

(**

  This is an GetPosition method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns the position passed to the menu items constructor.

  @return  a Integer

**)
Function TITHelperProjectMenu.GetPosition: Integer;

Begin
  Result := FPosition;
End;

(**

  This is an GetVerb method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Returns an identifier for the menu item.

  @return  a String

**)
Function TITHelperProjectMenu.GetVerb: String;

Begin
  Result := FVerb;
End;

(**

  This is a post execute method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Does nothing and returns false.

  @nohints

  @param   MenuContextList as an IInterfaceList as a constant
  @return  a Boolean

**)
Function TITHelperProjectMenu.PostExecute(Const MenuContextList: IInterfaceList): Boolean;

Begin
  Result := False;
End;

(**

  This is a pre execute method implementation for the IOTAProjectManageMenu interface.

  @precon  None.
  @postcon Does nothing and returns false.

  @nohints

  @param   MenuContextList as an IInterfaceList as a constant
  @return  a Boolean

**)
Function TITHelperProjectMenu.PreExecute(Const MenuContextList: IInterfaceList): Boolean;

Begin
  Result := False;
End;

(**

  This is an SetCaption method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod
  @nohints

  @param   Value as a String as a Constant

**)
Procedure TITHelperProjectMenu.SetCaption(Const Value: String);

Begin
  // Do nothing.
End;

(**

  This is an SetChecked method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohints

  @param   Value as a Boolean

**)
Procedure TITHelperProjectMenu.SetChecked(Value: Boolean);

Begin
  // Do nothing.
End;

(**

  This is an SetEnabled method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohints

  @param   Value as a Boolean

**)
Procedure TITHelperProjectMenu.SetEnabled(Value: Boolean);

Begin
  // Do nothing.
End;

(**

  This is an SetHelpContext method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohints

  @param   Value as a Integer

**)
Procedure TITHelperProjectMenu.SetHelpContext(Value: Integer);

Begin
  // Do nothing.
End;

(**

  This is an SetIsMultiSelectable method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohints

  @param   Value as a Boolean

**)
Procedure TITHelperProjectMenu.SetIsMultiSelectable(Value: Boolean);

Begin
  // Do nothing.
End;

(**

  This is an SetName method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod
  @nohints

  @param   Value as a String as a Constant

**)
Procedure TITHelperProjectMenu.SetName(Const Value: String);

Begin
  // Do nothing.
End;

(**

  This is an SetParent method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod
  @nohints

  @param   Value as a String as a Constant

**)
Procedure TITHelperProjectMenu.SetParent(Const Value: String);

Begin
  // Do nothing.
End;

(**

  This is an SetPosition method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohints

  @param   Value as a Integer

**)
Procedure TITHelperProjectMenu.SetPosition(Value: Integer);

Begin
  // Do nothing.
End;

(**

  This is an SetVerb method implementation for the IOTAProjectManageMenu
  interface.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod
  @nohints

  @param   Value as a String as a Constant

**)
Procedure TITHelperProjectMenu.SetVerb(Const Value: String);

Begin
  // Do nothing.
End;

{$ENDIF}
{$ENDIF}

End.


