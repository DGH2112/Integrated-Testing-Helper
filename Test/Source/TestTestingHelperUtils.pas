(**

  DUnit test for the Integrated Testing Helper.

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
unit TestTestingHelperUtils;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, StdCtrls, Controls, Buttons, ComCtrls, Classes, Dialogs, Windows, Variants,
  Forms, SysUtils, ITHelper.TestingHelperUtils, Graphics, Messages;

Type
  TestApplicationFunctions = Class(TTestCase)
  Published
    Procedure TestResolvePath;
  End;

implementation

{ TestApplicationFunctions }

procedure TestApplicationFunctions.TestResolvePath;

Var
  strFName, strPath, strResult : String;

begin
  strFName  := '.\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy\Athingy.exe';
  CheckEquals(TITHToolsAPIFunctions.ResolvePath(strFName, strPath), strResult, 'Test 1');
  strFName  := '.\New Folder\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy\New Folder\Athingy.exe';
  CheckEquals(TITHToolsAPIFunctions.ResolvePath(strFName, strPath), strResult, 'Test 2');
  strFName  := '..\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy.exe';
  CheckEquals(TITHToolsAPIFunctions.ResolvePath(strFName, strPath), strResult, 'Test 3');
  strFName  := '..\..\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Athingy.exe';
  CheckEquals(TITHToolsAPIFunctions.ResolvePath(strFName, strPath), strResult, 'Test 4');
  strFName  := 'C:\Borland Studio Projects\Application\Athingy\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy\Athingy.exe';
  CheckEquals(TITHToolsAPIFunctions.ResolvePath(strFName, strPath), strResult, 'Test 5');
end;

initialization
  RegisterTest('Functions', TestApplicationFunctions.Suite);
end.


