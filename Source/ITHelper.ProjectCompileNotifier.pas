(**
  
  This module contains a class which implements the IOTAProjectCompileNotifier. This is currently the
  only way that the actual compile config asnd platform can be found out for Build Groups.

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
Unit ITHelper.ProjectCompileNotifier;

Interface

{$INCLUDE CompilerDefinitions.inc}

{$IFDEF DXE00}
Uses
  ToolsAPI,
  ITHelper.Interfaces;

Type
  (** A class which implements the IOTAProjectCompileNotifier interface. **)
  TITHProjectCompileNotifier = Class(TNotifierObject, IOTAProjectCompileNotifier)
  Strict Private
    FCompileInformation : TITHSetCompileInformation;
  Strict Protected
    Procedure AfterCompile(Var CompileInfo: TOTAProjectCompileInfo);
    Procedure BeforeCompile(Var CompileInfo: TOTAProjectCompileInfo);
  Public
    Constructor Create(Const CompileInformation : TITHSetCompileInformation);
    Destructor Destroy; Override;
  End;
{$ENDIF DXE00}

Implementation

{$IFDEF DXE00}
Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  ITHelper.Constants;

(**

  This method is called after the project has been compiled.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  CompileInfo

  @param   CompileInfo as a TOTAProjectCompileInfo as a reference

**)
Procedure TITHProjectCompileNotifier.AfterCompile(Var CompileInfo: TOTAProjectCompileInfo);

Begin
End;

(**

  This method is called before the project is compiled.

  @precon  None.
  @postcon The compile settings are returned to the IDE Compile Notifier.

  @param   CompileInfo as a TOTAProjectCompileInfo as a reference

**)
Procedure TITHProjectCompileNotifier.BeforeCompile(Var CompileInfo: TOTAProjectCompileInfo);

Begin
  If Assigned(FCompileInformation) Then
    FCompileInformation(CompileInfo);
End;

(**

  A constructor for the TITHProjectCompileNotifier class.

  @precon  None.
  @postcon Saves a reference to the callback interface.

  @param   CompileInformation as an TITHSetCompileInformation as a constant

**)
Constructor TITHProjectCompileNotifier.Create(Const CompileInformation : TITHSetCompileInformation);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FCompileInformation := CompileInformation;
End;

(**

  A destructor for the TITHProjectCompileNotifier class.

  @precon  None.
  @postcon Not Used.

**)
Destructor TITHProjectCompileNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;
{$ENDIF DXE00}

End.

