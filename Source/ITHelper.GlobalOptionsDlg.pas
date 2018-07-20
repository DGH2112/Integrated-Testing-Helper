(**

  This module contains a class which represents a form on which the project options for
  the active project can be edited.

  @Author  David Hoyle
  @Version 1.0
  @Date    20 Jul 2018

**)
Unit ITHelper.GlobalOptionsDlg;

Interface

{$I 'CompilerDefinitions.inc'}

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
  Buttons,
  StdCtrls,
  ToolsAPI,
  ITHelper.Interfaces,
  Grids,
  ValEdit,
  ComCtrls,
  ITHelper.GlobalOptionsFrame,
  ITHelper.FontFrame,
  ITHelper.AboutFrame;

Type
  (** An enumerate to define the page to display on opening. **)
  TITHGlobalOptionType = (gotGlobalOptions, gotFonts, gotAbout);

  (** An enumerate to describe the pages of the options dialogue. **)
  TITHGlobalOptionsPage = (gopGlobalOptions, gopFonts, gopAbout);
  
  (** A class to represent the project options form. **)
  TfrmITHGlobalOptionsDlg = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    pgcProjectOptions: TPageControl;
    tabGlobalOptions: TTabSheet;
    tabFonts: TTabSheet;
    tabAbout: TTabSheet;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  Strict Private
    FGlobalOptions : IITHOptionsFrame;
    FFonts         : IITHOptionsFrame;
    FAbout         : IITHOptionsFrame;
  Strict Protected
    Procedure LoadSettings(Const GlobalOps: IITHGlobalOptions);
    Procedure SaveSettings(Const GlobalOps: IITHGlobalOptions);
  Public
    { Public declarations }
    Class Procedure Execute(Const GlobalOptionType : TITHGlobalOptionType;
      Const GlobalOps: IITHGlobalOptions);
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  ITHelper.Types,
  ITHelper.TestingHelperUtils;

Const
  (** An INI Section name for the dialogue settings. **)
  strGlobalDlgSection = 'Global Dlg';
  (** An INI key for the dialogue top. **)
  strTopKey = 'Top';
  (** An INI key for the dialogue left. **)
  strLeftKey = 'Left';
  (** An INI key for the dialogue height. **)
  strHeightKey = 'Height';
  (** An INI key for the dialogue width. **)
  strWidthKey = 'Width';

(**

  This is an on click event handler for the Help button.

  @precon  None.
  @postcon Displays the Project Options help page.

  @param   Sender as a TObject

**)
Procedure TfrmITHGlobalOptionsDlg.btnHelpClick(Sender: TObject);

Const
  //: @todo HTML Help needs checking
  strProjectOptions = 'GlobalOptions';
  strCompilationTools = 'Fonts';
  strZIPOptions = 'About';

Var
  strTopic : String;

Begin
  Case TITHGlobalOptionsPage(pgcProjectOptions.ActivePageIndex) Of
    gopGlobalOptions: strTopic := strProjectOptions;
    gopFonts:         strTopic := strCompilationTools;
    gopAbout:         strTopic := strZIPOptions;  
  End;
  HtmlHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strTopic)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Validates the frames in the form.

  @param   Sender as a TObject

**)
Procedure TfrmITHGlobalOptionsDlg.btnOKClick(Sender: TObject);

Begin
  If Not (
    FGlobalOptions.IsValidated And
    FFonts.IsValidated And
    FAbout.IsValidated) Then
    ModalResult := mrNone;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   GlobalOptionType as a TITHGlobalOptionType as a constant
  @param   GlobalOps        as an IITHGlobalOptions as a constant

**)
Class Procedure TfrmITHGlobalOptionsDlg.Execute(Const GlobalOptionType : TITHGlobalOptionType;
  Const GlobalOps: IITHGlobalOptions);

Var
  frm: TfrmITHGlobalOptionsDlg;

Begin
  frm := TfrmITHGlobalOptionsDlg.Create(Nil);
  Try
    frm.LoadSettings(GlobalOps);
    frm.FGlobalOptions.InitialiseOptions(GlobalOps, Nil);
    frm.FFonts.InitialiseOptions(GlobalOps, Nil, dtBefore);
    frm.FAbout.InitialiseOptions(GlobalOps, Nil);
    Case GlobalOptionType Of
      gotGlobalOptions: frm.pgcProjectOptions.ActivePage := frm.tabGlobalOptions;
      gotFonts:         frm.pgcProjectOptions.ActivePage := frm.tabFonts;
      gotAbout:         frm.pgcProjectOptions.ActivePage := frm.tabAbout;
    End;
    If frm.ShowModal = mrOK Then
      Begin
        frm.SaveSettings(GlobalOps);
        frm.FGlobalOptions.SaveOptions(GlobalOps, Nil);
        frm.FFonts.SaveOptions(GlobalOps, Nil, dtBefore);
        frm.FAbout.SaveOptions(GlobalOps, Nil);
      End;
  Finally
    frm.Free;
  End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmITHProjectOptionsDialogue class.

  @precon  None.
  @postcon Creates the options frame dynamically (so any changes in the frames take effect in this dlg).

  @param   Sender as a TObject

**)
Procedure TfrmITHGlobalOptionsDlg.FormCreate(Sender: TObject);

ResourceString
  strIITHOptionsFrameNotSupported = 'IITHOptionsFrame not supported!';

Const
  strGlobalOptionsFrameName = 'frameGlobalOptions';
  strFontsName = 'frameFonts';
  strAboutName = 'frameAbout';

Var
  F: Tframe;

Begin
  F := TframeGlobalOptions.Create(Self);
  F.Name := strGlobalOptionsFrameName;
  F.Parent := tabGlobalOptions;
  F.Align := alClient;
  If Not Supports(F, IITHOptionsFrame, FGlobalOptions) Then
    Raise Exception.Create(strIITHOptionsFrameNotSupported);
  F := TframeFonts.Create(Self);
  F.Name := strFontsName;
  F.Parent := tabFonts;
  F.Align := alClient;
  If Not Supports(F, IITHOptionsFrame, FFonts) Then
    Raise Exception.Create(strIITHOptionsFrameNotsupported);
  F := TframeAboutITHelper.Create(Self);
  F.Name := strAboutName;
  F.Parent := tabAbout;
  F.Align := alClient;
  If Not Supports(F, IITHOptionsFrame, FAbout) Then
    Raise Exception.Create(strIITHOptionsFrameNotSupported);
End;

(**

  This method loads the position settings for the options dialogue from the ini file.

  @precon  None.
  @postcon The dialogue is positioned on the screen where it was last time.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Procedure TfrmITHGlobalOptionsDlg.LoadSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    Top    := iniFile.ReadInteger(strGlobalDlgSection, strTopKey, (Screen.Height - Height) Div 2);
    Left   := iniFile.ReadInteger(strGlobalDlgSection, strLeftKey, (Screen.Width - Width) Div 2);
    Height := iniFile.ReadInteger(strGlobalDlgSection, strHeightKey, Height);
    Width  := iniFile.ReadInteger(strGlobalDlgSection, strWidthKey, Width);
  Finally
    iniFile.Free;
  End;
End;

(**

  This method saves the current position of the options dialogue to the ini file.

  @precon  None.
  @postcon The options dialogue position and size is saved to the ini file.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Procedure TfrmITHGlobalOptionsDlg.SaveSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    iniFile.WriteInteger(strGlobalDlgSection, strTopKey, Top);
    iniFile.WriteInteger(strGlobalDlgSection, strLeftKey, Left);
    iniFile.WriteInteger(strGlobalDlgSection, strHeightKey, Height);
    iniFile.WriteInteger(strGlobalDlgSection, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

End.
