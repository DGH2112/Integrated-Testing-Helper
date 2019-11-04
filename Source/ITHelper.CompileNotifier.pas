(**
  
  This module contains a class which implements the IOTACompileNotifier in order to find out what mode
  the compilation is under (TOTACompileMode).

  @Author  David Hoyle
  @Version 1.0
  @Date    04 Nov 2019
  
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
Unit ITHelper.CompileNotifier;

Interface

Uses
  ToolsAPI,
  Generics.Collections,
  ITHelper.Interfaces;

{$INCLUDE 'CompilerDefinitions.inc'}

{$IFDEF D2010}
Type
  {$IFDEF DXE00}
  (** A descendant interface for the IOTACompileNotifier to provide an extra method to hook the
      Compile Information Interface. **)
  IITHCompileNotifier = Interface(IOTACompileNotifier)
    Procedure HookCompileInformation(Const CompileInformation : IITHCompileInformation);
  End;
  {$ENDIF DXE00}

  (** A class which implements the IOTACompileNotifier interface. **)
  TITHCompileNotifier = Class(TNotifierObject, IOTACompileNotifier
    {$IFDEF DXE00} , IITHCompileNotifier {$ENDIF DXE00} )
  Strict Private
    {$IFDEF DXE00}
    FCompileInformation : IITHCompileInformation;
    {$ENDIF DXE00}
    FMessageMgr         : IITHMessageManager;
  Strict Protected
    // IOTACompileNotifier
    Procedure ProjectCompileFinished(Const Project: IOTAProject; Result: TOTACompileResult);
    Procedure ProjectCompileStarted(Const Project: IOTAProject; Mode: TOTACompileMode);
    Procedure ProjectGroupCompileFinished(Result: TOTACompileResult);
    Procedure ProjectGroupCompileStarted(Mode: TOTACompileMode);
    {$IFDEF DXE00}
    // IITHCompileNotifier
    Procedure HookCompileInformation(Const CompileInformation : IITHCompileInformation);
    {$ENDIF DXE00}
  Public
    Constructor Create(Const MessageMgr : IITHMessageManager);
    Destructor Destroy; Override;
  End;
{$ENDIF D2010}

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils,
  Variants,
  ITHelper.Types,
  ITHelper.Constants,
  ITHelper.TestingHelperUtils,
  ITHelper.ProjectCompileNotifier;

{$IFDEF D2010}
Const
  (** A constant array of fonts aligned with the compile results. **)
  aFont : Array[TOTACompileResult] Of TITHFonts = (ithfFailure, ithfSuccess, ithfWarning);

(**

  A constructor for the TITHCompileNotifier class.

  @precon  None.
  @postcon Does nothing - used for code site tracing to check for memory leaks due to coupling.

  @param   MessageMgr         as an IITHMessageManager as a constant

**)
Constructor TITHCompileNotifier.Create(Const MessageMgr : IITHMessageManager);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create;
  {$IFDEF DXE00}
  FCompileInformation := Nil;
  {$ENDIF DXE00}
  FMessageMgr := MessageMgr;
End;

(**

  A destructor for the TITHCompileNotifier class.

  @precon  None.
  @postcon Does nothing - used for code site tracing to check for memory leaks due to coupling.

**)
Destructor TITHCompileNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  {$IFDEF DXE00}
  FCompileInformation := Nil;
  {$ENDIF DXE00}
  Inherited Destroy;
End;

{$IFDEF DXE00}
(**

  This method hooks the IITHCompileInformation interface to the class so it can read and write the
  compile information.

  @precon  None.
  @postcon The CompileInformation interface is set.

  @param   CompileInformation as an IITHCompileInformation as a constant

**)
Procedure TITHCompileNotifier.HookCompileInformation(Const CompileInformation : IITHCompileInformation);

Begin
  FCompileInformation := CompileInformation;
End;
{$ENDIF DXE00}

(**

  This method is called when an individual project has finished compiling.

  @precon  None.
  @postcon Not used.

  @nohint  Project Result
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Project as an IOTAProject as a constant
  @param   Result  as a TOTACompileResult

**)
Procedure TITHCompileNotifier.ProjectCompileFinished(Const Project: IOTAProject;
  Result: TOTACompileResult);

ResourceString
  strMsg = 'Project "%s" finished compiling (Result: %s).';

Begin
  FMessageMgr.AddMsg(
    Format(strMsg, [
      ExtractFileName(Project.FileName),
      astrCompileResult[Result]
    ]),
    fnHeader,
    aFont[Result]
  );
End;

(**

  This method is called when an individual project starts compiling.

  @precon  None.
  @postcon Captures the compile mode.

  @nohint  Project
  @nocheck MissingCONSTInParam

  @param   Project as an IOTAProject as a constant
  @param   Mode    as a TOTACompileMode

**)
Procedure TITHCompileNotifier.ProjectCompileStarted(Const Project: IOTAProject; Mode: TOTACompileMode);

ResourceString
  strMsg = 'Project "%s" started compiling (Mode: %s, Config: %s, Platform: %s).';

{$IFNDEF DXE00}
Const
  strWin32 = 'Win32';
  strBaseConfig = 'Base';
{$ENDIF DXE00}

Var
  Msg: IITHCustomMessage;
  {$IFDEF DXE00}
  CompileInfo : TOTAProjectCompileInfo;
  {$ENDIF DXE00}

Begin
  {$IFDEF DXE00} If Assigned(FCompileInformation) Then {$ENDIF DXE00}
    Begin
      {$IFDEF DXE00}
      CompileInfo := FCompileInformation.CompileInformation;
      CompileInfo.Mode := Mode;
      {$ENDIF DXE00}
      Msg := FMessageMgr.AddMsg(
        Format(strMsg, [
          ExtractFileName(Project.FileName),
          astrCompileMode[ {$IFDEF DXE00} CompileInfo.Mode {$ELSE} Mode {$ENDIF DXE00} ],
          {$IFDEF DXE00} CompileInfo.Configuration {$ELSE} strBaseConfig {$ENDIF DXE00},
          {$IFDEF DXE00} CompileInfo.Platform {$ELSE} strWin32 {$ENDIF DXE00}
        ]),
        fnHeader,
        ithfDefault
      );
    End;
End;

(**

  This method is called when the compile group is finished.

  @precon  None.
  @postcon Not used.

  @nohint  Result
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Result as a TOTACompileResult

**)
Procedure TITHCompileNotifier.ProjectGroupCompileFinished(Result: TOTACompileResult);

ResourceString
  strMsg = 'Project Group finished compiling (Result: %s).';

Begin
  FMessageMgr.AddMsg(
    Format(strMsg, [
      astrCompileResult[Result]
    ]),
    fnHeader,
    aFont[Result]
  );
End;

(**

  This method is called when the compile group is started.

  @precon  None.
  @postcon Not used.

  @nohint  Mode
  @nocheck MissingCONSTInParam EmptyMethod

  @param   Mode as a TOTACompileMode

**)
Procedure TITHCompileNotifier.ProjectGroupCompileStarted(Mode: TOTACompileMode);

ResourceString
  strMsg = 'Project Group started compiling (Mode: %s).';

Begin
  FMessageMgr.AddMsg(
    Format(strMsg, [
      astrCompileMode[Mode]
    ]),
    fnHeader,
    ithfDefault
  )
End;
{$ENDIF D2010}

End.

