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
  CheckEquals(ResolvePath(strFName, strPath), strResult, 'Test 1');
  strFName  := '.\New Folder\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy\New Folder\Athingy.exe';
  CheckEquals(ResolvePath(strFName, strPath), strResult, 'Test 2');
  strFName  := '..\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy.exe';
  CheckEquals(ResolvePath(strFName, strPath), strResult, 'Test 3');
  strFName  := '..\..\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Athingy.exe';
  CheckEquals(ResolvePath(strFName, strPath), strResult, 'Test 4');
  strFName  := 'C:\Borland Studio Projects\Application\Athingy\Athingy.exe';
  strPath   := 'C:\Borland Studio Projects\Application\Athingy\';
  strResult := 'C:\Borland Studio Projects\Application\Athingy\Athingy.exe';
  CheckEquals(ResolvePath(strFName, strPath), strResult, 'Test 5');
end;

initialization
  RegisterTest('Functions', TestApplicationFunctions.Suite);
end.

