(**
  
  This module contains a class / interface for managaing message in the application.

  @Author  David Hoyle
  @Version 1.001
  @Date    05 Jun 2020
  
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
Unit ITHelper.MessageManager;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  Classes,
  ITHelper.Interfaces, 
  ITHelper.Types;

Type
  (** A class which implements the IITHMessageManager interface for managing messages in the plug-in. **)
  TITHMessageManager = Class(TInterfacedObject, IITHMessageManager)
  Strict Private
    FGlobalOps   : IITHGlobalOptions;
    FMsgs        : TInterfaceList;
    FParentMsg   : IITHCustomMessage;
    FLastMessage : Int64;
    FMessaging   : Boolean;
  {$IFDEF D2010} Strict {$ENDIF} Protected
    // IITHMessageManager
    Function  GetCount : Integer;
    Function  GetItem(Const iIndex : Integer) : IITHCustomMessage;
    Function  GetLastMessage : Int64;
    Function  GetParentMsg : IITHCustomMessage;
    Procedure SetParentMsg(Const ParentMsg : IITHCustomMessage);
    Procedure Clear;
    Function  AddMsg(Const strText: String; Const eFontName : TITHFontNames;
      Const eFont : TITHFonts; Const ptrParentMsg : Pointer = Nil): IITHCustomMessage;
    Procedure DisableMessaging;
  Public
    Constructor Create(Const GlobalOps : IITHGlobalOptions);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  ToolsAPI, 
  ITHelper.CustomMessages, 
  ITHelper.ResourceStrings;

(**

  This method adds a custom message to the IDE and returns a POINTER to that message.

  @precon  ptrParent must be a POINTER to another message not a reference.
  @postcon Adds a custom message to the IDE and returns a POINTER to that message.

  @param   strText      as a String as a constant
  @param   eFontName    as a TITHFontNames as a constant
  @param   eFont        as a TITHFonts as a constant
  @param   ptrParentMsg as a Pointer as a constant
  @return  an IITHCustomMessage

**)
Function TITHMessageManager.AddMsg(Const strText: String; Const eFontName : TITHFontNames;
  Const eFont : TITHFonts; Const ptrParentMsg : Pointer = Nil): IITHCustomMessage;

Var
  MS : IOTAMessageServices;
  G: IOTAMessageGroup;

Begin
  Result := Nil;
  If FMessaging And Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    Begin
      Result := TITHCustomMessage.Create(strText, FGlobalOps.FontName[eFontName],
        FGlobalOps.FontColour[eFont], FGlobalOps.FontStyles[eFont]);
      FMsgs.Add(Result);
      If Not Assigned(ptrParentMsg) Then
        Begin
          G := Nil;
          If FGlobalOps.GroupMessages Then
            G := MS.AddMessageGroup(strITHelperGroup)
          Else
            G := MS.GetMessageGroup(0);
          If FGlobalOps.AutoScrollMessages <> G.AutoScroll Then
            G.AutoScroll := FGlobalOps.AutoScrollMessages;
          Result.MessagePntr := MS.AddCustomMessagePtr(Result, G);
        End
      Else
        MS.AddCustomMessage(Result, ptrParentMsg);
      FLastMessage := GetTickCount;
    End;
End;

(**

  This method clears all the messages from the manager.

  @precon  None.
  @postcon All the messages are cleared from the managers internal collection (they still remain in the
           message view).

**)
Procedure TITHMessageManager.Clear;

Begin
  FMsgs.Clear;
End;

(**

  A constructor for the TITHMessageManager class.

  @precon  None.
  @postcon Stores references to the Global Options and creates a message list.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Constructor TITHMessageManager.Create(Const GlobalOps: IITHGlobalOptions);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FMessaging := True;
  FGlobalOps := GlobalOps;
  FMsgs := TInterfaceList.Create;
End;

(**

  A destructor for the TITHMessageManager class.

  @precon  None.
  @postcon Frees the message list.

**)
Destructor TITHMessageManager.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FMsgs.Free;
  Inherited Destroy;
End;

(**

  This method disables messages from being created and sent to the messages view.

  @precon  None.
  @postcon No messages will be sent to the message view.

**)
Procedure TITHMessageManager.DisableMessaging;

Begin
  FMessaging := False;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the internal message list.

  @return  an Integer

**)
Function TITHMessageManager.GetCount: Integer;

Begin
  Result := FMsgs.Count;
End;

(**

  This is a getter method for the Item property.

  @precon  None.
  @postcon Returns the indexed message from the internal list.

  @param   iIndex as an Integer as a constant
  @return  an IITHCustomMessage

**)
Function TITHMessageManager.GetItem(Const iIndex: Integer): IITHCustomMessage;

Begin
  Result := FMsgs[iIndex] As IITHCustomMessage;
End;

(**

  This is a getter method for the LastMessage property.

  @precon  None.
  @postcon Returns the tickcount of the last message created.

  @return  an Int64

**)
Function TITHMessageManager.GetLastMessage: Int64;

Begin
  Result := FLastMessage;
End;

(**

  This is a getter method for the ParentMsg property.

  @precon  None.
  @postcon Returns the current parent message.

  @return  an IITHCustomMessage

**)
Function TITHMessageManager.GetParentMsg: IITHCustomMessage;

Begin
  Result := FParentMsg;
End;

(**

  This is a setter method for the ParentMsg property.

  @precon  None.
  @postcon Sets the current ParentMsg referernce.

  @param   ParentMsg as an IITHCustomMessage as a constant

**)
Procedure TITHMessageManager.SetParentMsg(Const ParentMsg: IITHCustomMessage);

Begin
  If FParentMsg <> ParentMsg Then
    FParentMsg := ParentMsg;
End;

End.


