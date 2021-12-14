{*********************************************************}
{                                                         }
{       Dialog for sending HTML Email using TRichView     }
{       editor and Clever Components                      }
{                                                         }
{       TRichView (c) Sergey Tkachenko                    }
{       http://www.trichview.com                          }
{       Clever Components (c) Clever Components Team      }
{       http://www.clevercomponents.com                   }
{                                                         }
{*********************************************************}

{
  How to use:

  Use the following function:
  function ComposeAndSendHTMLEmail(Smtp: TclSmtp; Msg: TclMailMessage;
    HTMLKind: THTMLKind = htmlAdvanced): Boolean;
  It shows a dialog for editing email and sends email using Smtp and Msg.
  "From", "To", "Subject" of e-mail are initialized using the corresponding
  properties of Msg.
  If HTMLKind = htmlSimple, a basic HTML is used.
  If HTMLKind = htmlAdvanced (recommended), HTML with CSS is used.
}

unit clRVSendDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ToolWin, ComCtrls, ImgList, ActnList,
  RichViewActions, RVFontCombos, RVStyle, RVScroll, RichView, RVEdit, Menus,
  RVALocalize, clRVSendMail, clMailMessage, clSMTP, CRVData, RVTypes, RVItem,
  clTcpClient, clHttp, clTcpClientTls, RVADownloadInterface, RVStrFuncs,
  RVAClDownloadInterface;



type
  TfrmComposeHTMLEMail = class(TForm)
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    ImageList1: TImageList;
    cmbFont: TRVFontComboBox;
    cmbFontSize: TRVFontSizeComboBox;
    ToolButton1: TToolButton;
    rvActionFontBold1: TrvActionFontBold;
    rvActionFontItalic1: TrvActionFontItalic;
    rvActionFontUnderline1: TrvActionFontUnderline;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton6: TToolButton;
    rvActionFontEx1: TrvActionFontEx;
    rvActionFontColor1: TrvActionFontColor;
    rvActionFontBackColor1: TrvActionFontBackColor;
    rvActionParagraph1: TrvActionParagraph;
    rvActionAlignLeft1: TrvActionAlignLeft;
    rvActionAlignRight1: TrvActionAlignRight;
    rvActionAlignCenter1: TrvActionAlignCenter;
    rvActionAlignJustify1: TrvActionAlignJustify;
    rvActionParaBullets1: TrvActionParaBullets;
    rvActionParaNumbering1: TrvActionParaNumbering;
    rvActionParaNumbering2: TrvActionParaNumbering;
    rvActionInsertPicture1: TrvActionInsertPicture;
    rvActionFillColor1: TrvActionFillColor;
    rvActionInsertHLine1: TrvActionInsertHLine;
    rvActionInsertTable1: TrvActionInsertTable;
    rvActionTableInsertRowsBelow1: TrvActionTableInsertRowsBelow;
    rvActionTableInsertRowsAbove1: TrvActionTableInsertRowsAbove;
    rvActionTableInsertColLeft1: TrvActionTableInsertColLeft;
    rvActionTableInsertColRight1: TrvActionTableInsertColRight;
    rvActionTableDeleteRows1: TrvActionTableDeleteRows;
    rvActionTableDeleteCols1: TrvActionTableDeleteCols;
    rvActionTableMergeCells1: TrvActionTableMergeCells;
    rvActionTableSplitCells1: TrvActionTableSplitCells;
    rvActionTableProperties1: TrvActionTableProperties;
    btnFontColor: TToolButton;
    btnFontBackColor: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton5: TToolButton;
    ToolButton11: TToolButton;
    rvActionIndentInc1: TrvActionIndentInc;
    rvActionIndentDec1: TrvActionIndentDec;
    RichViewEdit1: TRichViewEdit;
    RVStyle1: TRVStyle;
    RVAPopupMenu1: TRVAPopupMenu;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    rvActionInsertHyperlink1: TrvActionInsertHyperlink;
    btnInsertTable: TToolButton;
    rvActionCut1: TrvActionCut;
    rvActionCopy1: TrvActionCopy;
    rvActionPaste1: TrvActionPaste;
    rvActionUndo1: TrvActionUndo;
    rvActionRedo1: TrvActionRedo;
    rvActionSelectAll1: TrvActionSelectAll;
    Panel3: TPanel;
    rvActionItemProperties1: TrvActionItemProperties;
    MainMenu1: TMainMenu;
    mitFile: TMenuItem;
    mitEdit: TMenuItem;
    mitPriority: TMenuItem;
    mitAttachment: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll1: TMenuItem;
    mitAddFile: TMenuItem;
    mitDeleteFile: TMenuItem;
    mitHigh: TMenuItem;
    mitMedium: TMenuItem;
    mitLow: TMenuItem;
    ilPriority: TImageList;
    mitSend: TMenuItem;
    mitClose: TMenuItem;
    mitSaveFile: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    lblTo: TLabel;
    lblFrom: TLabel;
    lblSubject: TLabel;
    lblCC: TLabel;
    lblBCC: TLabel;
    txtSubject: TEdit;
    txtFrom: TEdit;
    txtTo: TEdit;
    btnShowCC: TButton;
    txtCC: TEdit;
    txtBCC: TEdit;
    lvFiles: TListView;
    Splitter1: TSplitter;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    pmFiles: TPopupMenu;
    mitAddFile2: TMenuItem;
    mitDeleteFile2: TMenuItem;
    mitSaveFile2: TMenuItem;
    mitSave: TMenuItem;
    RVAControlPanel1: TRVAControlPanel;
    clHttp1: TclHttp;
    RVAClDownloadInterface1: TRVAClDownloadInterface;
    procedure FormCreate(Sender: TObject);
    procedure RichViewEdit1Enter(Sender: TObject);
    procedure EditsEnter(Sender: TObject);
    procedure rvActionColor1ShowColorPicker(Sender: TObject);
    procedure rvActionColor1HideColorPicker(Sender: TObject);
    procedure btnInsertTableClick(Sender: TObject);
    procedure mitPriorityClick(Sender: TObject);
    procedure btnShowCCClick(Sender: TObject);
    procedure mitAddFileClick(Sender: TObject);
    procedure mitDeleteFileClick(Sender: TObject);
    procedure lvFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure mitSaveFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mitSendClick(Sender: TObject);
    procedure mitCloseClick(Sender: TObject);
    procedure txtChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mitSaveClick(Sender: TObject);
    procedure RichViewEdit1ReadHyperlink(Sender: TCustomRichView;
      const Target, Extras: TRVUnicodeString; DocFormat: TRVLoadFormat;
      var StyleNo: Integer; var ItemTag: TRVTag;
      var ItemName: TRVUnicodeString);
    procedure RichViewEdit1ItemHint(Sender: TCustomRichView;
      RVData: TCustomRVData; ItemNo: Integer; var HintText: TRVUnicodeString);
    procedure RichViewEdit1Jump(Sender: TObject; id: Integer);
    procedure RichViewEdit1KeyPress(Sender: TObject; var Key: Char);
    procedure RichViewEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FReadOnly: Boolean;
    FUnusedIDs: TStringList;
    procedure DisableEnableUI(Enable: Boolean);
    procedure ShowCopies;
    procedure HideCopies;
    procedure ShowFiles;
    procedure HideFiles;
    function AddAttach(ABody: TclAttachmentBody): TListItem;
    procedure SetReadOnly(const Value: Boolean);
    procedure DoSaveAttachment(Sender: TObject; ABody: TclAttachmentBody;
    var AFileName: string; var AData: TStream; var Handled: Boolean);
    procedure AssignControlPanel;
  public
    { Public declarations }
    procedure AdjustFocus;
    procedure FromMessage(Msg: TclMailMessage; MailSource: TStrings;
      UnusedIDs: TStringList);
    procedure ToMessage(Msg: TclMailMessage; var Attachments: TStrings);
    procedure DeleteTempFiles;
    procedure AllowSaving;
    procedure BeforeLoadingBody;
    procedure AfterLoadingBody;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    class function GetMessageTempDir: string;
    class function GetUniqueTempFileName(const FileName: String): string;
    class function GetAttachFileName(ABody: TclAttachmentBody): string;    
  end;

function ComposeAndSendHTMLEmail(Smtp: TclSmtp; Msg: TclMailMessage;
  HTMLKind: THTMLKind = htmlAdvanced): Boolean;



implementation

{$R *.dfm}
{------------------------------------------------------------------------------}
function ComposeAndSendHTMLEmail(Smtp: TclSmtp; Msg: TclMailMessage;
  HTMLKind: THTMLKind = htmlAdvanced): Boolean;
var frm: TfrmComposeHTMLEMail;
    Attachments: TStrings;
begin
  frm := TfrmComposeHTMLEMail.Create(Application);
  Attachments := nil;
  try
    frm.FromMessage(Msg, nil, nil);
    Result := frm.ShowModal=mrOk;
    if Result then begin
      frm.ToMessage(Msg, Attachments);
      frm.RichViewEdit1.DeleteUnusedStyles(True, True, True);
      Screen.Cursor := crHourGlass;
      SendHTMLEmail(frm.RichViewEdit1, Smtp, Msg, HTMLKind, Attachments);
    end;
  finally
    frm.DeleteTempFiles;
    frm.Free;
    Attachments.Free;
    Screen.Cursor := crDefault;
  end;
end;
{==================== Initialization ==========================================}
procedure TfrmComposeHTMLEMail.FormCreate(Sender: TObject);
begin
  DisableEnableUI(False);
  //mitSend.Caption := RVA_GetS(rvam_btn_OK);
  mitClose.Caption := RVA_GetS(rvam_btn_Cancel);
  btnInsertTable.Hint := RVA_GetS(rvam_act_InsertTableH);
  HideCopies;
  HideFiles;
  AssignControlPanel;
  RVA_LocalizeForm(Self);
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.AssignControlPanel;
var i: Integer;
begin
  for i := 0 to ActionList1.ActionCount-1 do
    if ActionList1.Actions[i] is TrvCustomAction then
      TrvCustomAction(ActionList1.Actions[i]).ControlPanel := RVAControlPanel1;
  RVAPopupMenu1.ControlPanel := RVAControlPanel1;
end;
{========================== General UI ========================================}
procedure TfrmComposeHTMLEMail.DisableEnableUI(Enable: Boolean);
begin
  if ReadOnly then
    Enable := False;
  RVAControlPanel1.ActionsEnabled := Enable;
  cmbFont.Enabled := Enable;
  cmbFontSize.Enabled := Enable;
  btnInsertTable.Enabled := Enable;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.FormShow(Sender: TObject);
begin
  if (txtCC.Text<>'') or (txtBCC.Text<>'') then
    ShowCopies;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.RichViewEdit1Enter(Sender: TObject);
begin
  DisableEnableUI(True);
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.EditsEnter(Sender: TObject);
begin
  DisableEnableUI(False);
end;
{------------------------------------------------------------------------------}
// Hide CC and BCC
procedure TfrmComposeHTMLEMail.HideCopies;
var h: Integer;
begin
  lblCC.Visible := False;
  txtCC.Visible := False;
  lblBCC.Visible := False;
  txtBCC.Visible := False;
  h := Panel2.Height - txtSubject.Top;
  txtSubject.Top := txtCC.Top;
  lblSubject.Top := lblCC.Top;
  Panel1.Height := txtSubject.Top + h;
  Panel2.Height := txtSubject.Top + h;
  btnShowCC.Caption := '+';
end;
{------------------------------------------------------------------------------}
// Show CC and BCC
procedure TfrmComposeHTMLEMail.ShowCopies;
var h: Integer;
begin
  lblCC.Visible := True;
  txtCC.Visible := True;
  lblBCC.Visible := True;
  txtBCC.Visible := True;
  h := Panel2.Height - txtSubject.Top;
  txtSubject.Top := txtBCC.Top + (txtBCC.Top-txtCC.Top);
  lblSubject.Top := lblBCC.Top + (lblBCC.Top-lblCC.Top);
  Panel1.Height := txtSubject.Top + h;
  Panel2.Height := txtSubject.Top + h;
  btnShowCC.Caption := '-';
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.btnShowCCClick(Sender: TObject);
begin
  if not txtCC.Visible then
    ShowCopies
  else
    HideCopies;
end;
{------------------------------------------------------------------------------}
// Hide list of attachments
procedure TfrmComposeHTMLEMail.HideFiles;
begin
  lvFiles.Width := 0;
end;
{------------------------------------------------------------------------------}
// Show list of attachments
procedure TfrmComposeHTMLEMail.ShowFiles;
begin
  if lvFiles.Width = 0 then
    lvFiles.Width := Splitter1.MinSize;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.AdjustFocus;
begin
  if txtFrom.Text='' then
    ActiveControl := txtFrom
  else if txtTo.Text='' then
    ActiveControl := txtTo
  else if txtSubject.Text='' then
    ActiveControl := txtSubject
  else
    ActiveControl := RichViewEdit1;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.SetReadOnly(const Value: Boolean);
var Color: TColor;
begin
  FReadOnly := Value;
  DisableEnableUI(RichViewEdit1.Focused);
  RichViewEdit1.ReadOnly := Value;
  txtSubject.ReadOnly := Value;
  txtFrom.ReadOnly := Value;
  txtTo.ReadOnly := Value;
  txtCC.ReadOnly := Value;
  txtBCC.ReadOnly := Value;
  mitAddFile.Enabled := not ReadOnly;
  mitAddFile2.Enabled := not ReadOnly;
  mitHigh.Enabled := not ReadOnly;
  mitLow.Enabled := not ReadOnly;
  mitMedium.Enabled := not ReadOnly;
  mitDeleteFile.Enabled := (lvFiles.Selected<>nil) and not ReadOnly;
  mitDeleteFile2.Enabled := mitDeleteFile.Enabled;
  mitSaveFile.Enabled := (lvFiles.Selected<>nil);
  mitSaveFile2.Enabled := mitSaveFile.Enabled;
  if ReadOnly then
    Color := clBtnFace
  else
    Color := clWindow;
  txtFrom.Color := Color;
  txtTo.Color := Color;
  txtCC.Color := Color;
  txtBCC.Color := Color;
  txtSubject.Color := Color;
  if ReadOnly then begin
    mitClose.Caption := RVA_GetS(rvam_btn_Close);
    Caption := 'View E-Mail';
    end
  else begin
    mitClose.Caption := RVA_GetS(rvam_btn_Cancel);
    Caption := 'Compose E-Mail';
  end;
  mitSend.Visible := not ReadOnly;
  ToolBar1.Visible := not ReadOnly;

end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.txtChange(Sender: TObject);
begin
  RichViewEdit1.Modified := True;
end;
{=========================== Editing rich text ================================}
procedure TfrmComposeHTMLEMail.rvActionColor1ShowColorPicker(
  Sender: TObject);
begin
  if (Sender as TAction).ActionComponent is TToolButton then
    TToolButton(TAction(Sender).ActionComponent).Down := True;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.rvActionColor1HideColorPicker(
  Sender: TObject);
begin
  if (Sender as TAction).ActionComponent is TToolButton then
    TToolButton(TAction(Sender).ActionComponent).Down := False;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.btnInsertTableClick(Sender: TObject);
begin
  rvActionInsertTable1.ShowTableSizeDialog(RichViewEdit1,
    btnInsertTable);
end;
{============================== Priority ======================================}
procedure TfrmComposeHTMLEMail.mitPriorityClick(Sender: TObject);
begin
  ilPriority.GetIcon(TMenuItem(Sender).Tag, Icon);
  RichViewEdit1.Modified := True;
end;
{============================ Attached files ==================================}
// Get temp dir for attachments
class function TfrmComposeHTMLEMail.GetMessageTempDir: string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
  l: Integer;
begin
  l := GetTempPath(MAX_PATH, Buffer);
  SetString(Result, Buffer, l);
  if (Result <> '') and (Result[Length(Result)] <> '\') then
  begin
    Result := Result + '\';
  end;
  Result := Result + 'CleverMailClient\';
  ForceDirectories(Result);
end;
{------------------------------------------------------------------------------}
// Get unique temporal file name for FileName
class function TfrmComposeHTMLEMail.GetUniqueTempFileName(const FileName: String): string;
var Dir, Name, Ext: String;
    i: Integer;
begin
  Dir := GetMessageTempDir;
  if not FileExists(Dir+FileName) then begin
    Result := FileName;
    exit;
  end;
  Ext := ExtractFileExt(FileName);
  Name := Copy(FileName, 1, Length(FileName)-Length(Ext));
  Name := Name + '(';
  Ext  := ')' + Ext;
  i := 1;
  while FileExists(Dir+Name+IntToStr(i)+Ext) do
    inc(i);
  Result := Name+IntToStr(i)+Ext;
end;
{------------------------------------------------------------------------------}
// Add attachment
procedure TfrmComposeHTMLEMail.mitAddFileClick(Sender: TObject);
var
  Item: TListItem;
  NewName: string;
begin
  if OpenDialog.Execute() then
  begin
    NewName := GetMessageTempDir() +
      GetUniqueTempFileName(ExtractFileName(OpenDialog.FileName));
    CopyFile(PChar(OpenDialog.FileName), PChar(NewName), False);
    Item := lvFiles.Items.Add();
    Item.Caption := ExtractFileName(NewName);
    Item.ImageIndex := 37;
    ShowFiles;
    RichViewEdit1.Modified := True;
  end;
end;
{------------------------------------------------------------------------------}
// Add attachment from message
function TfrmComposeHTMLEMail.AddAttach(ABody: TclAttachmentBody): TListItem;
begin
  Result := lvFiles.Items.Add();
  Result.Caption := GetUniqueTempFileName(GetAttachFileName(ABody));
  Result.ImageIndex := 37;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.DoSaveAttachment(Sender: TObject; ABody: TclAttachmentBody;
    var AFileName: string; var AData: TStream; var Handled: Boolean);
var Item: TListItem;
    Idx: Integer;
begin
  if ABody is TclImageBody then begin
    if (FUnusedIDs=nil) or not FUnusedIDs.Find('cid:'+ABody.ContentID, Idx) then
      exit;
  end;
  Item := AddAttach(ABody);
  AData := TFileStream.Create(GetMessageTempDir() + Item.Caption, fmCreate);
  Handled := True;
end;
{------------------------------------------------------------------------------}
// Remove attachment
procedure TfrmComposeHTMLEMail.mitDeleteFileClick(Sender: TObject);
var Index: Integer;
begin
  if lvFiles.Selected=nil then
    exit;
  DeleteFile(GetMessageTempDir() + lvFiles.Selected.Caption);
  Index := lvFiles.Selected.Index;
  lvFiles.Selected.Delete();
  if Index>=lvFiles.Items.Count then
    dec(Index);
  if Index>=0 then
   lvFiles.Selected := lvFiles.Items[Index];
  RichViewEdit1.Modified := True;   
end;
{------------------------------------------------------------------------------}
// Save attachment
procedure TfrmComposeHTMLEMail.mitSaveFileClick(Sender: TObject);
begin
  if lvFiles.Selected=nil then
    exit;
  SaveDialog.FileName := lvFiles.Selected.Caption;
  if SaveDialog.Execute then
    CopyFile(PChar(GetMessageTempDir() + lvFiles.Selected.Caption), PChar(SaveDialog.FileName), False);
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.lvFilesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  mitDeleteFile.Enabled := (lvFiles.Selected<>nil) and not ReadOnly;
  mitDeleteFile2.Enabled := mitDeleteFile.Enabled;
  mitSaveFile.Enabled := (lvFiles.Selected<>nil);
  mitSaveFile2.Enabled := mitSaveFile.Enabled;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.DeleteTempFiles;
var i: Integer;
begin
  try
    for i := 0 to lvFiles.Items.Count-1 do
      DeleteFile(GetMessageTempDir() + lvFiles.Items[i].Caption);
    RemoveDir(GetMessageTempDir());
  except
  end;
end;
{------------------------------------------------------------------------------}
class function TfrmComposeHTMLEMail.GetAttachFileName(ABody: TclAttachmentBody): string;
var
  i: Integer;
  c: Char;
begin
  Result := ExtractFileName(ABody.FileName);
  if (Result = '') then
  begin
    for i := 1 to Length(ABody.ContentID) do begin
      c := ABody.ContentID[i];
      if (c in ['0'..'9', 'a'..'z', 'A'..'Z', '.']) then
        Result := Result + ABody.ContentID[i];
    end;
    if (Result = '') then
      Result := 'noname.dat';
  end;
end;
{========================== File functions ====================================}
procedure TfrmComposeHTMLEMail.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ReadOnly or not RichViewEdit1.Modified then
    exit;
  if mitSave.Visible then
    case Application.MessageBox('Save changes to the message?', 'Message was changed',
      MB_YESNOCANCEL or MB_ICONQUESTION) of
      IDYES:
         ModalResult := mrYes;
      IDNO: ;
      IDCANCEL:
        CanClose := False;
    end
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.AllowSaving;
begin
  mitSave.Visible := True;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.mitSaveClick(Sender: TObject);
begin
  RichViewEdit1.Modified := False;
  ModalResult := mrYes;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.mitSendClick(Sender: TObject);
begin
  RichViewEdit1.Modified := False;
  ModalResult := mrOk;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.mitCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{==============================================================================}
procedure TfrmComposeHTMLEMail.FromMessage(Msg: TclMailMessage;
  MailSource: TStrings; UnusedIDs: TStringList);
var Msg2: TclMailMessage;
begin
  txtSubject.Text := Msg.Subject;
  txtFrom.Text := Msg.From.FullAddress;
  txtTo.Text := Msg.ToList.EmailAddresses;
  txtCC.Text := Msg.CCList.EmailAddresses;
  txtBCC.Text := Msg.BCCList.EmailAddresses;
  FUnusedIDs := UnusedIDs;
  case Msg.Priority of
    mpLow:    begin mitLow.Checked := True; mitPriorityClick(mitLow); end;
    mpNormal: begin mitMedium.Checked := True; mitPriorityClick(mitMedium); end;
    mpHigh:   begin mitHigh.Checked := True; mitPriorityClick(mitHigh); end;
  end;

  if (Msg.Attachments.Count>0) or ((UnusedIDs<>nil) and (UnusedIDs.Count>0)) then begin
    Msg2 := TclMailMessage.Create(nil);
    try
      Msg2.OnSaveAttachment := DoSaveAttachment;
      if MailSource<>nil then
        Msg2.MessageSource := MailSource
      else
        Msg2.MessageSource := Msg.MessageSource;
    finally
      Msg2.Free;
    end;
  end;
 if lvFiles.Items.Count>0 then
   ShowFiles;
  RichViewEdit1.Modified := False;
  AdjustFocus;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.ToMessage(Msg: TclMailMessage;
  var Attachments: TStrings);
var i: Integer;
begin
  Attachments := nil;
  Msg.Subject := txtSubject.Text;
  Msg.From.FullAddress := txtFrom.Text;
  Msg.ToList.EmailAddresses := txtTo.Text;
  Msg.CCList.EmailAddresses := txtCC.Text;
  Msg.BCCList.EmailAddresses := txtBCC.Text;
  if mitLow.Checked then
    Msg.Priority := mpLow
  else if mitHigh.Checked then
    Msg.Priority := mpHigh
  else
    Msg.Priority := mpNormal;
  if lvFiles.Items.Count>0 then
  begin
    Attachments := TStringList.Create;
    for i := 0 to lvFiles.Items.Count-1 do
      Attachments.Add(GetMessageTempDir() + lvFiles.Items[i].Caption);
  end;
end;
{========================== Hyperlinks ========================================}
procedure TfrmComposeHTMLEMail.RichViewEdit1ReadHyperlink(
  Sender: TCustomRichView; const Target, Extras: TRVUnicodeString;
  DocFormat: TRVLoadFormat; var StyleNo: Integer;
  var ItemTag: TRVTag; var ItemName: TRVUnicodeString);
var
  URL: TRVUnicodeString;
begin
  if DocFormat=rvlfURL then
    StyleNo :=
      rvActionInsertHyperlink1.GetHyperlinkStyleNo(RichViewEdit1);
  URL := rvActionInsertHyperlink1.EncodeTarget(Target);
  ItemTag := URL;
end;

procedure TfrmComposeHTMLEMail.RichViewEdit1ItemHint(
  Sender: TCustomRichView; RVData: TCustomRVData; ItemNo: Integer;
  var HintText: TRVUnicodeString);

begin
  if RVData.GetItem(ItemNo).GetBoolValueEx(rvbpJump, Sender.Style) then
  begin
    if HintText<>'' then
      HintText := HintText+'; ';
    HintText := HintText+PChar(RVData.GetItemTag(ItemNo));
  end;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.RichViewEdit1Jump(Sender: TObject;
  id: Integer);
begin
  rvActionInsertHyperlink1.GoToLink(RichViewEdit1, id);
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.RichViewEdit1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key in [#9, ' ', ',', ';'] then begin
    rvActionInsertHyperlink1.DetectURL(RichViewEdit1);
    rvActionInsertHyperlink1.TerminateHyperlink(RichViewEdit1);
  end;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.RichViewEdit1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_RETURN) then begin
    rvActionInsertHyperlink1.DetectURL(RichViewEdit1);
    rvActionInsertHyperlink1.TerminateHyperlink(RichViewEdit1);
  end;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.FormActivate(Sender: TObject);
begin
  cmbFont.UpdateSelection;
  cmbFontSize.UpdateSelection;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.BeforeLoadingBody;
begin
  RVAControlPanel1.InitImportPictures(nil, nil);
  RichViewEdit1.OnImportPicture := RVAControlPanel1.DoImportPicture;
end;
{------------------------------------------------------------------------------}
procedure TfrmComposeHTMLEMail.AfterLoadingBody;
begin
  RVAControlPanel1.DoneImportPictures;
  RichViewEdit1.OnImportPicture := nil;
end;


end.
