(**

  This module contains a class which represents a form on which the project options for
  the active project can be edited.

  @Author  David Hoyle
  @Version 1.0
  @Date    16 Jul 2018

**)
Unit ITHelper.ProjectOptionsDialogue;

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
  ITHelper.ProjectOptionsFrame,
  ITHelper.ExternalToolsFrame,
  ITHelper.ZIPFrame;

Type
  (** An enumerate to define the page to display on opening. **)
  TITHProjectOptionType = (potProjectOptions, potBeforeCompile, potAfterCompile, potZipping);

  (** A class to represent the project options form. **)
  TfrmITHProjectOptionsDialogue = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    pgcProjectOptions: TPageControl;
    tabProjectOptions: TTabSheet;
    tabBeforeCompileTools: TTabSheet;
    tabAfterCompileTools: TTabSheet;
    tabZipping: TTabSheet;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  Strict Private
    FProjectOptions     : TframeProjectOptions;
    FBeforeCompileTools : TframeExternalTools;
    FAfterCompileTools  : TframeExternalTools;
    FZipping            : TframeZipping;
  Strict Protected
    Procedure LoadSettings(Const GlobalOps: IITHGlobalOptions);
    Procedure SaveSettings(Const GlobalOps: IITHGlobalOptions);
  Public
    { Public declarations }
    Class Procedure Execute(Const ProjectOptionType : TITHProjectOptionType;
      Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);
  End;

Implementation

{$R *.dfm}

Uses
  IniFiles,
  {$IFDEF DXE20}
  CommonOptionStrs,
  {$ENDIF}
  ITHelper.TestingHelperUtils;

Const
  (** An INI Section name for the dialogue settings. **)
  strProjectDlgSection = 'Project Dlg';
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
Procedure TfrmITHProjectOptionsDialogue.btnHelpClick(Sender: TObject);

Const
  strProjectOptions = 'ProjectOptions'; //: @todo Fix Help for the active tab.
  strZIPOptions = 'ZIPOptions';

Begin
  HTMLHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strProjectOptions)), HH_DISPLAY_TOPIC, 0);
  HtmlHelp(0, PChar(TITHToolsAPIFunctions.ITHHTMLHelpFile(strZIPOptions)), HH_DISPLAY_TOPIC, 0);
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Validates the frames in the form.

  @param   Sender as a TObject

**)
Procedure TfrmITHProjectOptionsDialogue.btnOKClick(Sender: TObject);

Begin
  If Not FZipping.Isvalidated Then //: @todo Validate ALL frames!
    ModalResult := mrNone;
End;

(**

  This method initialises the project options in the dialogue.

  @precon  None.
  @postcon Initialises the project options in the dialogue.

  @param   ProjectOptionType as a TITHProjectOptionType as a constant
  @param   GlobalOps         as an IITHGlobalOptions as a constant
  @param   Project           as an IOTAProject as a constant

**)
Class Procedure TfrmITHProjectOptionsDialogue.Execute(Const ProjectOptionType : TITHProjectOptionType;
  Const GlobalOps: IITHGlobalOptions; Const Project: IOTAProject);

ResourceString
  strProjectOptionsFor = 'Project Options for %s';

Var
  frm: TfrmITHProjectOptionsDialogue;

Begin
  frm := TfrmITHProjectOptionsDialogue.Create(Nil);
  Try
    frm.Caption := Format(strProjectOptionsFor, [TITHToolsAPIFunctions.GetProjectName(Project)]);
    frm.LoadSettings(GlobalOps);
    frm.FProjectOptions.InitialiseOptions(GlobalOps, Project);
    frm.FBeforeCompileTools.InitialiseOptions(GlobalOps, Project, dtBefore);
    frm.FAfterCompileTools.InitialiseOptions(GlobalOps, Project, dtAfter);
    frm.FZipping.InitialiseOptions(GlobalOps, Project);
    Case ProjectOptionType Of
      potProjectOptions: frm.pgcProjectOptions.ActivePage := frm.tabProjectOptions;
      potBeforeCompile: frm.pgcProjectOptions.ActivePage := frm.tabBeforeCompileTools;
      potAfterCompile: frm.pgcProjectOptions.ActivePage := frm.tabAfterCompileTools;
      potZipping: frm.pgcProjectOptions.ActivePage := frm.tabZipping;
    End;
    If frm.ShowModal = mrOK Then
      Begin
        frm.SaveSettings(GlobalOps);
        frm.FProjectOptions.SaveOptions(GlobalOps, Project);
        frm.FBeforeCompileTools.SaveOptions(Project, dtBefore);
        frm.FAfterCompileTools.SaveOptions(Project, dtAfter);
        frm.FZipping.SaveOptions(GlobalOps);
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
Procedure TfrmITHProjectOptionsDialogue.FormCreate(Sender: TObject);

Const
  strProjectOptionsFrameName = 'frameProjectOptionsFrame';
  strBeforeCompileToolsName = 'frameBeforeCompileTools';
  strAfterCompileToolsName = 'frameAfterCompileTools';
  strZippingName = 'frameZipping';

Begin
  FProjectOptions := TframeProjectOptions.Create(Self);
  FProjectOptions.Name := strProjectOptionsFrameName;
  FProjectOptions.Parent := tabProjectOptions;
  FProjectOptions.Align := alClient;
  FBeforeCompileTools := TframeExternalTools.Create(Self);
  FBeforeCompileTools.Name := strBeforeCompileToolsName;
  FBeforeCompileTools.Parent := tabBeforeCompileTools;
  FBeforeCompileTools.Align := alClient;
  FAfterCompileTools := TframeExternalTools.Create(Self);
  FAfterCompileTools.Name := strAfterCompileToolsName;
  FAfterCompileTools.Parent := tabAfterCompileTools;
  FAfterCompileTools.Align := alClient;
  FZipping := TframeZipping.Create(Self);
  FZipping.Name := strZippingName;
  FZipping.Parent := tabZipping;
  FZipping.Align := alClient;
End;

(**

  This method loads the position settings for the options dialogue from the ini file.

  @precon  None.
  @postcon The dialogue is positioned on the screen where it was last time.

  @param   GlobalOps as an IITHGlobalOptions as a constant

**)
Procedure TfrmITHProjectOptionsDialogue.LoadSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    Top    := iniFile.ReadInteger(strProjectDlgSection, strTopKey, (Screen.Height - Height) Div 2);
    Left   := iniFile.ReadInteger(strProjectDlgSection, strLeftKey, (Screen.Width - Width) Div 2);
    Height := iniFile.ReadInteger(strProjectDlgSection, strHeightKey, Height);
    Width  := iniFile.ReadInteger(strProjectDlgSection, strWidthKey, Width);
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
Procedure TfrmITHProjectOptionsDialogue.SaveSettings(Const GlobalOps: IITHGlobalOptions);

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(GlobalOps.INIFileName);
  Try
    iniFile.WriteInteger(strProjectDlgSection, strTopKey, Top);
    iniFile.WriteInteger(strProjectDlgSection, strLeftKey, Left);
    iniFile.WriteInteger(strProjectDlgSection, strHeightKey, Height);
    iniFile.WriteInteger(strProjectDlgSection, strWidthKey, Width);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

End.
