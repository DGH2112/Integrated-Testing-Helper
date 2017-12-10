(**

  This module contains a modeless form for displaying the progress of the
  before or after compile information.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Apr 2011

**)
unit ProcessingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

{$INCLUDE 'CompilerDefinitions.inc'}

type
  (** A class to represent the modeless form. **)
  TfrmProcessing = class(TForm)
  private
    { Private declarations }
    FForm: TPanel;
    FInfo: TPanel;
    FFileName: TPanel;
    FMsg: TTimer;
    FCanHide : Boolean;
    Function TextWidth(AFont : TFont; strMsg : String):  Integer;
    Procedure InitialiseControls;
    procedure FMsgTimer(Sender: TObject);
  public
    { Public declarations }
    Class Procedure ShowProcessing(strMsg : String; iColour : TColor = clBlue;
      boolWait : Boolean = False);
    Class Procedure HideProcessing;
    Class procedure ProcessFileName(strFileName : String);
    (**
      This property determines if the form can be closed.
      @precon  None.
      @postcon Determines if the form can be closed.
      @return  a Boolean
    **)
    Property CanHide : Boolean Read FCanHide Write FCanHide;
  end;

implementation

{$R *.dfm}

Var
  (** A private varaiable to hold the Singleton reference to the form. **)
  frm : TfrmProcessing;

(**

  This method returns the width of the text on the Canvas.

  @precon  None.
  @postcon Returns the width of the text on the Canvas.

  @param   AFont  as a TFont
  @param   strMsg as a String
  @return  an Integer

**)
function TfrmProcessing.TextWidth(AFont : TFont; strMsg: String): Integer;
begin
  Canvas.Font.Assign(AFont);
  Result := Canvas.TextWidth(strMsg);
end;

{ TfrmProcessing }

(**

  This method signifies that the form can be closed by the timer event handler.

  @precon  None.
  @postcon Signifies that the form can be closed by the timer event handler.

**)
class procedure TfrmProcessing.HideProcessing;
begin
  frm.CanHide := True;
  If Not frm.FMsg.Enabled Then
    frm.Hide;
end;

(**

  TYhis method creates all the controls on the form. For Delphi 7 backward
  compatibility.

  @precon  None.
  @postcon Creates all the controls on the form. For Delphi 7 backward
           compatibility.

**)
procedure TfrmProcessing.InitialiseControls;


Begin
  FForm := TPanel.Create(Self);
  FInfo := TPanel.Create(Self);
  FFileName := TPanel.Create(Self);
  FMsg := TTimer.Create(Self);
  With FForm Do
    Begin
      Name := 'pnlForm';
      Parent := Self;
      Align := alClient;
      TabOrder := 0;
      Caption := '';
    End;
  With FInfo Do
    Begin
      Name := 'pnlInfo';
      Parent := FForm;
      Height := 28;
      Align := alTop;
      BevelOuter := bvNone;
      Caption := '';
      ParentFont := False;
      Font.Name := 'Tahoma';
      Font.Size := 10;
      Font.Style := [fsBold];
      TabOrder := 0;
      {$IFDEF D2005}
      VerticalAlignment := taAlignBottom;
      {$ENDIF}
    End;
  With FFileName Do
    Begin
      Name := 'pnlFileName';
      Parent := FForm;
      Align := alClient;
      BevelOuter := bvNone;
      ParentFont := False;
      TabOrder := 1;
      Caption := '';
      Font.Color := clGreen;
      {$IFDEF D2005}
      VerticalAlignment := taAlignTop;
      {$ENDIF}
    End;
  With FMsg Do
    Begin
      Name := 'tmMsg';
      Enabled := False;
      Interval := 2500;
      OnTimer := FMsgTimer;
    End;
end;

(**

  This method updates the filename panel on the form.

  @precon  None.
  @postcon Updates the filename panel on the form.

  @param   strFileName as a String

**)
class procedure TfrmProcessing.ProcessFileName(strFileName : String);
begin
  frm.FFileName.Caption := strFileName;
  Application.ProcessMessages;
end;

(**

  This method displays the modeless form with the message and starts the timer
  so that the for is displayed for a minimum of 2.5 seconds.

  @precon  None.
  @postcon Displays the modeless form with the message and starts the timer
           so that the for is displayed for a minimum of 2.5 seconds.

  @param   strMsg   as a String
  @param   iColour  as a TColor
  @param   boolWait as a Boolean

**)
class procedure TfrmProcessing.ShowProcessing(strMsg : String;
  iColour : TColor = clBlue; boolWait : Boolean = False);

begin
  frm.CanHide := False;
  frm.FInfo.Caption := strMsg;
  frm.FInfo.Font.Color := iColour;
  frm.Width := 50 + frm.TextWidth(frm.FInfo.Font, strMsg);
  frm.Left := Screen.Width Div 2 - frm.Width Div 2;
  If Not frm.Visible Then frm.Show;
  frm.FFileName.Caption := '';
  Application.ProcessMessages;
  If boolWait Then
    frm.FMsg.Enabled := True;
end;

(**

  This is an on timer event handler for the form.

  @precon  None.
  @postcon Disables the timer and hides the form if it can else shortens the
           timer interval.

  @param   Sender as a TObject

**)
procedure TfrmProcessing.FMsgTimer(Sender: TObject);
begin
  If frm.CanHide Then
    Begin
      FMsg.Enabled := False;
      Hide;
    End Else
      FMsg.Interval := 250;
end;

{ TDGHPanelHelper }

(** Creates an instance of the form for use in the application. **)
Initialization
  frm := TfrmProcessing.Create(Nil);
  frm.InitialiseControls;
(** Frees the form at unloading. **)
Finalization
  frm.Free;
end.



