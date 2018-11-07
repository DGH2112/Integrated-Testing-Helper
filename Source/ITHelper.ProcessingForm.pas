(**

  This module contains a modeless form for displaying the progress of the
  before or after compile information.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Sep 2018

**)
Unit ITHelper.ProcessingForm;

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
  ExtCtrls;

{$INCLUDE 'CompilerDefinitions.inc'}


Type
  (** A class to represent the modeless form. **)
  TfrmITHProcessing = Class(TForm)
  Strict Private
    FForm     : TPanel;
    FInfo     : TPanel;
    FFileName : TPanel;
    FMsg      : TTimer;
    FCanHide  : Boolean;
    FMessage  : String;
    FFName    : String;
  Strict Protected
    Function  TextWidth(Const AFont: TFont; Const strMsg: String): Integer;
    Procedure MsgTimer(Sender: TObject);
    Class Procedure CheckWidth;
  Protected
    Procedure InitialiseControls;
  Public
    Class Procedure ShowProcessing(Const strMsg: String; Const iColour: TColor = clBlue;
      Const boolWait: Boolean = False);
    Class Procedure HideProcessing;
    Class Procedure ProcessFileName(Const strFileName: String);
    (**
      This property determines if the form can be closed.
      @precon  None.
      @postcon Determines if the form can be closed.
      @return  a Boolean
    **)
    Property CanHide: Boolean Read FCanHide Write FCanHide;
  End;

Implementation

{$R *.dfm}

Uses
  Math, ITHelper.TestingHelperUtils;


Var
  (** A private varaiable to hold the Singleton reference to the form. **)
  FormInstance: TfrmITHProcessing;

(**

  This class method calculates the minimum width for the progress form based on the filename and message
  text.

  @precon  None.
  @postcon Updates the width and position of the progress for based on the message and filename strings.

**)
Class Procedure TfrmITHProcessing.CheckWidth;

Const
  iTextPadding = 50;
var
  iWidth: Integer;

Begin
  iWidth := Max(
    FormInstance.TextWidth(FormInstance.FInfo.Font, FormInstance.FMessage),
    FormInstance.TextWidth(FormInstance.FInfo.Font, FormInstance.FFName)
  );
  FormInstance.Width := iTextPadding + iWidth;
  FormInstance.Left := Screen.Width Div 2 - FormInstance.Width Div 2;
End;

(**

  This method signifies that the form can be closed by the timer event handler.

  @precon  None.
  @postcon Signifies that the form can be closed by the timer event handler.

**)
Class Procedure TfrmITHProcessing.HideProcessing;

Begin
  FormInstance.CanHide := True;
  If Not FormInstance.FMsg.Enabled Then
    FormInstance.Hide;
End;

(**

  TYhis method creates all the controls on the form. For Delphi 7 backward
  compatibility.

  @precon  None.
  @postcon Creates all the controls on the form. For Delphi 7 backward
           compatibility.

**)
Procedure TfrmITHProcessing.InitialiseControls;

Const
  strPnlFormName = 'pnlForm';
  strPnlInfoName = 'pnlInfo';
  iInfoHeight = 28;
  strFontName = 'Tahoma';
  iFontSize = 10;
  strPnlFileName = 'pnlFileName';
  strTmMsgName = 'tmMsg';
  iTimerInterval = 2500;

Begin
  FForm := TPanel.Create(Self);
  FForm.Name := strPnlFormName;
  FForm.Parent := Self;
  FForm.Align := alClient;
  FForm.TabOrder := 0;
  FForm.Caption := '';
  FForm.Font.Name := strFontName;
  FForm.Font.Size := iFontSize;
  FInfo := TPanel.Create(Self);
  FInfo.Name := strPnlInfoName;
  FInfo.Parent := FForm;
  FInfo.ParentFont := True;
  FInfo.Height := iInfoHeight;
  FInfo.Align := alTop;
  FInfo.BevelOuter := bvNone;
  FInfo.Caption := '';
  FInfo.ParentFont := False;
  FInfo.Font.Style := [fsBold];
  FInfo.TabOrder := 0;
  FInfo.VerticalAlignment := taAlignBottom;
  FFileName := TPanel.Create(Self);
  FFileName.Name := strPnlFileName;
  FFileName.Parent := FForm;
  FFileName.ParentFont := True;
  FFileName.Align := alClient;
  FFileName.BevelOuter := bvNone;
  FFileName.ParentFont := False;
  FFileName.TabOrder := 1;
  FFileName.Caption := '';
  FFileName.Font.Color := clGreen;
  FFileName.VerticalAlignment := taAlignTop;
  FMsg := TTimer.Create(Self);
  FMsg.Name := strTmMsgName;
  FMsg.Enabled := False;
  FMsg.Interval := iTimerInterval;
  FMsg.OnTimer := MsgTimer;
End;

(**

  This is an on timer event handler for the form.

  @precon  None.
  @postcon Disables the timer and hides the form if it can else shortens the
           timer interval.

  @param   Sender as a TObject

**)
Procedure TfrmITHProcessing.MsgTimer(Sender: TObject);

Const
  iMsgTimerInterval = 250;

Begin
  If FormInstance.CanHide Then
    Begin
      FMsg.Enabled := False;
      Hide;
    End
  Else
    FMsg.Interval := iMsgTimerInterval;
End;

(**

  This method updates the filename panel on the form.

  @precon  None.
  @postcon Updates the filename panel on the form.

  @param   strFileName as a String as a constant

**)
Class Procedure TfrmITHProcessing.ProcessFileName(Const strFileName: String);

Begin
  FormInstance.FFName := strFileName;
  FormInstance.FFileName.Caption := FormInstance.FFName;
  CheckWidth;
  Application.ProcessMessages;
End;

(**

  This method displays the modeless form with the message and starts the timer so that the for is 
  displayed for a minimum of 2.5 seconds.

  @precon  None.
  @postcon Displays the modeless form with the message and starts the timer so that the for is displayed
           for a minimum of 2.5 seconds.

  @param   strMsg   as a String as a constant
  @param   iColour  as a TColor as a constant
  @param   boolWait as a Boolean as a constant

**)
Class Procedure TfrmITHProcessing.ShowProcessing(Const strMsg: String;
  Const iColour: TColor = clBlue; Const boolWait: Boolean = False);

Begin
  FormInstance.CanHide := False;
  FormInstance.FInfo.Caption := strMsg;
  FormInstance.FInfo.Font.Color := iColour;
  FormInstance.FMessage := strMsg;
  CheckWidth;
  TITHToolsAPIFunctions.ApplyTheming(FormInstance);
  If Not FormInstance.Visible Then
    FormInstance.Show;
  FormInstance.FFileName.Caption := '';
  Application.ProcessMessages;
  If boolWait Then
    FormInstance.FMsg.Enabled := True;
End;

(**

  This method returns the width of the text on the Canvas.

  @precon  None.
  @postcon Returns the width of the text on the Canvas.

  @param   AFont  as a TFont as a constant
  @param   strMsg as a String as a constant
  @return  an Integer

**)
Function TfrmITHProcessing.TextWidth(Const AFont: TFont; Const strMsg: String): Integer;

Begin
  Canvas.Font.Assign(AFont);
  Result := Canvas.TextWidth(strMsg);
End;

(** Creates an instance of the form for use in the application. **)
Initialization
  //: @bug TITHToolsAPIFunctions.RegisterFormClassForTheming(TfrmITHProcessing);
  FormInstance := TfrmITHProcessing.Create(Nil);
  FormInstance.InitialiseControls;
  TITHToolsAPIFunctions.ApplyTheming(FormInstance);
(** Frees the form at unloading. **)
Finalization
  FormInstance.Free;
End.
