unit main;

interface

{$I RV_Defs.inc}

uses
  // Delphi units
  Classes, Graphics, Controls, Forms, ExtCtrls, ComCtrls, ImgList, Menus,
  Dialogs, ActnList, Actions, ImageList,
  {$IFDEF RICHVIEWDEFXE2}
  UITypes,
  {$ENDIF}
  // CleverComponents units
  clPOP3, clMC, clSMTP, clMailMessage, clEncoder,
  clTcpClient, clCertificate,
  clTcpClientTls, clTcpCommandClient, clEmailAddress,
  // TRichView units
  {$IFDEF RICHVIEWDEF2007}
  RVGifAnimate2007,
  {$ENDIF}
  // TRichView + CleverComponents units
  clRVSendDialog, clRVSendDialogEx,
  // Demo units
  MessagePersister, Progress,
  // Email scanner
  antivirus;

type
  TMainForm = class(TForm)
    tvFolders: TTreeView;
    Splitter1: TSplitter;
    lvMessages: TListView;
    FolderImages: TImageList;
    MessageImages: TImageList;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    NewMessage1: TMenuItem;
    Properties1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    DeleteMessage1: TMenuItem;
    MarkasRead1: TMenuItem;
    MarkasUnread1: TMenuItem;
    ools1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Accounts1: TMenuItem;
    clSMTP: TclSMTP;
    clPOP3: TclPOP3;
    Send1: TMenuItem;
    Receive1: TMenuItem;
    MessageParser: TclMailMessage;
    PopupMenu: TPopupMenu;
    DeleteMessage2: TMenuItem;
    N1: TMenuItem;
    Properties2: TMenuItem;
    LoadMessage1: TMenuItem;
    SaveMessage1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N3: TMenuItem;
    OpenMessage2: TMenuItem;
    ActionList1: TActionList;
    actNewMessage: TAction;
    actLoadMessage: TAction;
    actSaveMessage: TAction;
    actShowMessageSource: TAction;
    actOpenMessage: TAction;
    OpenMessage1: TMenuItem;
    actDeleteMessage: TAction;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    actMarkMessageAsRead: TAction;
    actMarkMessageAsUnread: TAction;
    MarkasRead2: TMenuItem;
    MarkasUnread2: TMenuItem;
    actReplyToSender: TAction;
    actReplyToAll: TAction;
    actForwardMessage: TAction;
    Forward1: TMenuItem;
    Reply1: TMenuItem;
    ReplytoAll1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Reply2: TMenuItem;
    ReplytoAll2: TMenuItem;
    Forward2: TMenuItem;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure Exit1Click(Sender: TObject);
    procedure lvMessagesDblClick(Sender: TObject);
    procedure Receive1Click(Sender: TObject);
    procedure Send1Click(Sender: TObject);
    procedure Accounts1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure lvMessagesCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvMessagesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure clPOP3VerifyServer(Sender: TObject;
      ACertificate: TclCertificate; const AStatusText: String;
      AStatusCode: Integer; var AVerified: Boolean);
    procedure clSMTPVerifyServer(Sender: TObject;
      ACertificate: TclCertificate; const AStatusText: String;
      AStatusCode: Integer; var AVerified: Boolean);
    procedure actNewMessageExecute(Sender: TObject);
    procedure actLoadMessageExecute(Sender: TObject);
    procedure actSaveMessageExecute(Sender: TObject);
    procedure actShowMessageSourceExecute(Sender: TObject);
    procedure actOpenMessageExecute(Sender: TObject);
    procedure actDeleteMessageExecute(Sender: TObject);
    procedure actMarkMessageAsReadExecute(Sender: TObject);
    procedure actMarkMessageAsUnreadExecute(Sender: TObject);
    procedure actReplyToSenderExecute(Sender: TObject);
    procedure actReplyToAllExecute(Sender: TObject);
    procedure actForwardMessageExecute(Sender: TObject);
    procedure lvMessagesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FAccounts: TclMailAccounts;
    FMessageList: TclMailMessageList;
    FLoading: Boolean;
    FProgress: TfrmProgress;
    FIsStop: Boolean;
    FEditMode: Boolean;
    FPopVerified: Boolean; 
    FSmtpVerified: Boolean;
    FAVScanner: TclAVScanner;

    procedure AddListItem(AMessage: TclMailMessageItem);
    procedure LoadListView(AStatus: TclMailMessageStatus);
    procedure LoadFolders;
    procedure LoadMessageList;
    procedure StoreMessageList;
    procedure LoadMessageStatuses;
    procedure CreateNewMessage;
    procedure DoOnMessageListChanged(Sender: TObject);
    function GetSelectedMessage: TclMailMessageItem;
    function GetSelectedStatus: TclMailMessageStatus;
    procedure LoadAccounts;
    procedure StoreAccounts;
    procedure ReceiveMessages;
    procedure SendMessages;
    procedure SendMessage(AMessage: TclMailMessageItem);
    function GetMessageCount(AStatus: TclMailMessageStatus): Integer;
    function GetMessageFileName: string;
    function GetAccountFileName: string;
    procedure DoStopProcess(Sender: TObject);
    procedure OpenCurrentMessage(HowToOpen: TMessageReplyKind);
    procedure UpdateUI;
    procedure BeforeLoadMessage;
    procedure AfterLoadMessage;
  end;

const
  cTlsMode: array[Boolean] of TclClientTlsMode = (ctNone, ctAutomatic);
  
var
  MainForm: TMainForm;

implementation

uses
  AccountsForm, MessageSourceForm, SysUtils, CommCtrl, Windows,
  Types;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FProgress := TfrmProgress.Create(nil);
  FProgress.OnStop := DoStopProcess;
  FAccounts := TclMailAccounts.Create(nil);
  FMessageList := TclMailMessageList.Create();
  FMessageList.OnMessageChanged := DoOnMessageListChanged;

  FAVScanner := TclClamAVScanner.Create(ExtractFilePath(ParamStr(0))); //TODO move it to options dialog of MailClient
  FAVScanner.TempDir := TfrmComposeHTMLEMail.GetMessageTempDir();

  LoadMessageList();
  LoadMessageStatuses();
  LoadAccounts();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StoreAccounts();
  StoreMessageList();
  FAVScanner.Free();
  FMessageList.Free();
  FAccounts.Free();
  FProgress.Free();
  clSMTP.Close();
  clPOP3.Close();
end;

procedure TMainForm.LoadMessageStatuses();
begin
  tvFolders.Items[0].Data := Pointer(msInbox);
  tvFolders.Items[1].Data := Pointer(msOutbox);
  tvFolders.Items[2].Data := Pointer(msSent);
  tvFolders.Items[3].Data := Pointer(msDeleted);
  tvFolders.Items[4].Data := Pointer(msDraft);
  tvFolders.Items[5].Data := Pointer(msInfected);
end;

procedure TMainForm.AddListItem(AMessage: TclMailMessageItem);
const
  PiorityMap: array[TclMessagePriority] of Integer = (2, -1, 1);
  
var
  Item: TListItem;
begin
  Item := lvMessages.Items.Add();
  Item.Caption := '';
  Item.SubItems.Add(AMessage.From);
  Item.SubItems.Add(AMessage.Subject);
  Item.SubItems.Add(DateTimeToStr(AMessage.Date));
  Item.Data := AMessage;
  Item.ImageIndex := PiorityMap[AMessage.Priority];
end;

function TMainForm.GetMessageCount(AStatus: TclMailMessageStatus): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FMessageList.Count - 1 do
  begin
    if (FMessageList[i].Status = AStatus) then
    begin
      Inc(Result);
    end;
  end;
end;

procedure TMainForm.LoadFolders;
const
  FolderCaptions: array[TclMailMessageStatus] of string = (
    'Drafts', 'Outbox', 'Sent Items', 'Inbox', 'Deleted Items', 'Infected Items'
  );
var
  i, MessageCount: Integer;
  TreeNode: TTreeNode;
  Status: TclMailMessageStatus;
begin
  tvFolders.Items.BeginUpdate();
  try
    for i := 0 to tvFolders.Items.Count - 1 do
    begin
      TreeNode := tvFolders.Items[i];
      Status := TclMailMessageStatus(TreeNode.Data);
      MessageCount := GetMessageCount(Status);
      if MessageCount > 0 then
      begin
        TreeNode.Text := Format('%s (%d)', [FolderCaptions[Status], MessageCount]);
      end else
      begin
        TreeNode.Text := FolderCaptions[Status];
      end;
    end;
  finally
    tvFolders.Items.EndUpdate();
  end;
end;

procedure TMainForm.LoadListView(AStatus: TclMailMessageStatus);
var
  i: Integer;
begin
  lvMessages.Items.BeginUpdate();
  try
    lvMessages.Clear();
    for i := 0 to FMessageList.Count - 1 do
    begin
      if (FMessageList[i].Status = AStatus) then
      begin
        AddListItem(FMessageList[i]);
      end;
    end;
  finally
    lvMessages.Items.EndUpdate();
  end;
end;

function TMainForm.GetMessageFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if (Result <> '') and (Result[Length(Result)] <> '\') then
  begin
    Result := Result + '\';
  end;
  Result := Result + 'messages.dat';
end;

function TMainForm.GetAccountFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if (Result <> '') and (Result[Length(Result)] <> '\') then
  begin
    Result := Result + '\';
  end;
  Result := Result + 'accounts.dat';
end;

procedure TMainForm.LoadMessageList();
begin
  FLoading := True;
  try
    FMessageList.Load(GetMessageFileName());
  finally
    FLoading := False;
  end;
end;

procedure TMainForm.StoreMessageList();
begin
  FMessageList.Store(GetMessageFileName());
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  tvFolders.Items[0].Selected := True;
end;

procedure TMainForm.tvFoldersChange(Sender: TObject; Node: TTreeNode);
begin
  LoadFolders();
  LoadListView(GetSelectedStatus());
  UpdateUI();  
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.DoOnMessageListChanged(Sender: TObject);
begin
  if (not FLoading) then
  begin
    LoadFolders();
    LoadListView(GetSelectedStatus());
    Application.ProcessMessages();
  end;
end;

procedure TMainForm.OpenCurrentMessage(HowToOpen: TMessageReplyKind);
var
  Msg, Msg2: TclMailMessageItem;

  function CanEdit: Boolean;
  var AName, AEmail: string;
  begin
    Result := Msg.Status in [msDraft, msOutbox];
    if Result then
      exit;
    if Msg.Status=msDeleted then begin
      GetEmailAddressParts(Msg.From, AName, AEmail);
      Result := LowerCase(AEmail)=LowerCase(FAccounts.EMail);
    end;
  end;


begin
  if FEditMode then Exit;
  FEditMode := True;
  BeforeLoadMessage;
  try
    Msg := GetSelectedMessage();
    if (Msg <> nil) then begin
      if HowToOpen<>mekEdit then begin
        Msg2 := Msg;
        Msg := FMessageList.Add();
        Msg.Assign(Msg2);
        Msg.Status := msDraft;
      end;
      if (HowToOpen<>mekEdit) or CanEdit then
        case ReplyToHTMLEmail(Msg.MailMessage, HowToOpen,
          GetCompleteEmailAddress(FAccounts.Name, FAccounts.EMail)) of
          meSend:
            begin
              SendMessage(Msg);
            end;
          meSave:
            begin
              Msg.Status := msDraft;
              Msg.MarkedAsRead := False;
            end;
          meCancel:
            begin
              if HowToOpen<>mekEdit then
                Msg.Free()
              else
                Msg.MarkedAsRead := True;
            end;
        end
      else begin
        ViewHTMLEmail(Msg.MailMessage);
        Msg.MarkedAsRead := True;
      end;
    end;
  finally
    AfterLoadMessage;
    FEditMode := False;
  end;
end;

procedure TMainForm.CreateNewMessage();
var
  Msg: TclMailMessageItem;
begin
  if FEditMode then Exit;
  FEditMode := True;
  try
    Msg := FMessageList.Add();
    MessageParser.Clear;
    MessageParser.From.Name := FAccounts.Name;
    MessageParser.From.Email := FAccounts.EMail;
    Msg.MailMessage := MessageParser.MessageSource;
    case EditHTMLEmail(Msg.MailMessage) of
      meSend:
        begin
          SendMessage(Msg);
        end;
      meSave:
        begin
          tvFolders.Items[4].Selected := True;
        end;
    end
  finally
    FEditMode := False;
  end;
end;

procedure TMainForm.lvMessagesDblClick(Sender: TObject);
begin
  OpenCurrentMessage(mekEdit);
end;

procedure TMainForm.lvMessagesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    OpenCurrentMessage(mekEdit);
end;

function TMainForm.GetSelectedMessage: TclMailMessageItem;
begin
  if (lvMessages.Selected <> nil) then
  begin
    Result := TclMailMessageItem(lvMessages.Selected.Data);
  end else
  begin
    Result := nil;
  end;
end;

function TMainForm.GetSelectedStatus: TclMailMessageStatus;
begin
  Assert(tvFolders.Selected <> nil);
  Result := TclMailMessageStatus(tvFolders.Selected.Data);
end;

procedure TMainForm.Receive1Click(Sender: TObject);
var
  oldCur: TCursor;
begin
  oldCur := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ReceiveMessages();
  finally
    Screen.Cursor := oldCur;
  end;
end;

procedure TMainForm.Send1Click(Sender: TObject);
var
  oldCur: TCursor;
begin
  oldCur := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SendMessages();
  finally
    Screen.Cursor := oldCur;
  end;
end;

procedure TMainForm.ReceiveMessages;
var
  i: Integer;
  MsgItem: TclMailMessageItem;
  UIDList: TStrings;
begin
  if clPOP3.Active then Exit;
  try
    EnableWindow(Handle, False);
    FProgress.RetrProgress(1);
    FIsStop := False;
    clPOP3.Server := FAccounts.POP3Server;
    clPOP3.UserName := FAccounts.POP3User;
    clPOP3.Password := FAccounts.POP3Password;
    clPOP3.Port := FAccounts.POP3Port;
    clPOP3.UseTLS := cTlsMode[FAccounts.POP3UseSSL];
    clPOP3.UseSasl := FAccounts.POP3Sasl;
    clPOP3.Open();
    UIDList := TStringList.Create();
    try
      clPOP3.GetUIDList(UIDList);
      FProgress.SetupProgress(clPOP3.MessageCount);
      for i := 0 to clPOP3.MessageCount - 1 do
      begin
        if FIsStop then Break;
        if (FMessageList.Find(UIDList[i]) = nil) then
        begin
          MsgItem := FMessageList.Add();
          MsgItem.Status := msInbox;
          MsgItem.UID := UIDList[i];
          clPOP3.Retrieve(i+1);
          MsgItem.MailMessage.Assign(clPOP3.Response);

          if not FAVScanner.ScanMessage(MsgItem.MailMessage) then
          begin
            MsgItem.Status := msInfected;
          end;

          if not FAccounts.LeaveMessage then
          begin
            clPOP3.Delete(i+1);
          end;
        end else
        if not FAccounts.LeaveMessage then
        begin
          clPOP3.Delete(i+1);
        end;
        FProgress.StepProgress();
      end;
    finally
      UIDList.Free();
      clPOP3.Close();
    end;
  finally
    FProgress.Hide();
    EnableWindow(Handle, True);
  end;
end;

procedure TMainForm.SendMessages;
var
  i: Integer;
begin
  if clSMTP.Active then Exit;
  try
    EnableWindow(Handle, False);
    FProgress.SendProgress(GetMessageCount(msOutbox));
    FIsStop := False;
    clSMTP.Server := FAccounts.SMTPServer;
    clSMTP.UserName := FAccounts.SMTPUser;
    clSMTP.Password := FAccounts.SMTPPassword;
    clSMTP.Port := FAccounts.SMTPPort;
    clSMTP.UseTLS := cTlsMode[FAccounts.SMTPUseSSL];
    clSMTP.UseSasl := FAccounts.SMTPSasl;
    clSMTP.Open();
    try
      for i := 0 to FMessageList.Count - 1 do
      begin
        if FIsStop then Break;
        if (FMessageList[i].Status = msOutbox) then
        begin
          clSMTP.Send(FMessageList[i].MailMessage);
          FMessageList[i].Status := msSent;
          FProgress.StepProgress();
        end;
      end;
    finally
      clSMTP.Close();
    end;
  finally
    FProgress.Hide();
    EnableWindow(Handle, True);
  end;
end;

procedure TMainForm.SendMessage(AMessage: TclMailMessageItem);
var
  oldCur: TCursor;
begin
  if clSMTP.Active or (AMessage = nil) then Exit;
  AMessage.Status := msOutbox;
  if not FAccounts.SendImmediately then Exit;
  oldCur := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    EnableWindow(Handle, False);
    FProgress.SendProgress(1);
    FIsStop := False;
    clSMTP.Server := FAccounts.SMTPServer;
    clSMTP.UserName := FAccounts.SMTPUser;
    clSMTP.Password := FAccounts.SMTPPassword;
    clSMTP.Port := FAccounts.SMTPPort;
    clSMTP.UseTLS := cTlsMode[FAccounts.SMTPUseSSL];
    clSMTP.UseSasl := FAccounts.SMTPSasl;
    clSMTP.Open();
    try
      clSMTP.Send(AMessage.MailMessage);
      AMessage.Status := msSent;
      FProgress.StepProgress();
    finally
      clSMTP.Close();
    end;
  finally
    FProgress.Hide();
    EnableWindow(Handle, True);
    Screen.Cursor := oldCur;
  end;
end;

procedure TMainForm.Accounts1Click(Sender: TObject);
begin
  TfrmAccounts.ShowAccounts(FAccounts);
  FPopVerified := False;
  FSmtpVerified := False;
  StoreAccounts();
end;

procedure TMainForm.LoadAccounts();
begin
  FAccounts.Load(GetAccountFileName());
end;

procedure TMainForm.StoreAccounts();
begin
  FAccounts.Store(GetAccountFileName());
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  ShowMessage('Clever Internet Suite'#$D#$A'www.CleverComponents.com'#$D#$A'TRichView'#$D#$A'www.trichview.com');
end;

procedure TMainForm.lvMessagesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  i: Integer;
  R, R1: TRect;
begin
  if TclMailMessageItem(Item.Data).MarkedAsRead then
  begin
    Sender.Canvas.Font.Style := [];
  end else
  begin
    Sender.Canvas.Font.Style := [fsBold];
  end;
  if Item.Selected then
  begin
    if Sender.Focused then
    begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clHighlightText;
    end else
    begin
      Sender.Canvas.Brush.Color := clBtnFace;
      Sender.Canvas.Font.Color := clWindowText;
    end;
  end else
  begin
    Sender.Canvas.Brush.Color := clWindow;
    Sender.Canvas.Font.Color := clWindowText;
  end;
  ListView_GetItemRect(Item.Handle, Item.Index, R, LVIR_SELECTBOUNDS);
  MessageImages.Draw(Sender.Canvas, R.Left, R.Top, Item.ImageIndex);
  ListView_GetItemRect(Item.Handle, Item.Index, R1, LVIR_ICON);
  R.Left := R.Left + R1.Right - R1.Left;
  Sender.Canvas.FillRect(R);
  for i := 0 to Item.SubItems.Count - 1 do
  begin
    ListView_GetSubItemRect(Item.Handle, Item.Index, i + 1, LVIR_LABEL, @R);
    R.Top := R.Top + ((R.Bottom - R.Top) - Sender.Canvas.TextHeight('W')) div 2;
    R.Bottom := R.Bottom + ((R.Bottom - R.Top) - Sender.Canvas.TextHeight('W')) div 2;
    DrawText(Sender.Canvas.Handle, PChar(Item.SubItems[i]),
      Length(Item.SubItems[i]), R, DT_END_ELLIPSIS);
  end;
  DefaultDraw := False;
end;

procedure TMainForm.DoStopProcess(Sender: TObject);
begin
  FIsStop := True;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not (clSMTP.Active or clPOP3.Active);
  if clSMTP.Active then
    MessageBox(0, 'Mail sending is in progress', 'Warning', MB_OK);
  if clPOP3.Active then
    MessageBox(0, 'Mail retrieving is in progress', 'Warning', MB_OK);
end;

procedure TMainForm.clPOP3VerifyServer(Sender: TObject;
  ACertificate: TclCertificate; const AStatusText: String;
  AStatusCode: Integer; var AVerified: Boolean);
begin
  if not AVerified then
  begin
    AVerified := FPopVerified;
  end;
  if not AVerified and (MessageDlg(AStatusText + #13#10' Do you wish to proceed ?',
    mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    AVerified := True;
    FPopVerified := True;
  end;
end;

procedure TMainForm.clSMTPVerifyServer(Sender: TObject;
  ACertificate: TclCertificate; const AStatusText: String;
  AStatusCode: Integer; var AVerified: Boolean);
begin
  if not AVerified then
  begin
    AVerified := FSmtpVerified;
  end;
  if not AVerified and (MessageDlg(AStatusText + #13#10' Do you wish to proceed ?',
    mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    AVerified := True;
    FSmtpVerified := True;
  end;
end;

procedure TMainForm.actNewMessageExecute(Sender: TObject);
begin
  CreateNewMessage();
end;

procedure TMainForm.actLoadMessageExecute(Sender: TObject);
var
  Msg: TclMailMessageItem;
begin
  if OpenDialog1.Execute() then
  begin
    Msg := FMessageList.Add();
    Msg.Status := msInbox;
    Msg.MailMessage.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TMainForm.actSaveMessageExecute(Sender: TObject);
var
  Msg: TclMailMessageItem;
begin
  Msg := GetSelectedMessage();
  if (Msg <> nil) then
  begin
    SaveDialog1.FileName := Msg.Subject + '.' + SaveDialog1.DefaultExt;
    if SaveDialog1.Execute() then
    begin
      Msg.MailMessage.SaveToFile(SaveDialog1.FileName);
    end;
  end;
end;

procedure TMainForm.actShowMessageSourceExecute(Sender: TObject);
begin
  if (GetSelectedMessage() <> nil) then
  begin
    TfrmMessageSource.ShowMessageSource(GetSelectedMessage().MailMessage);
  end;
end;

procedure TMainForm.actOpenMessageExecute(Sender: TObject);
begin
  OpenCurrentMessage(mekEdit);
end;

procedure TMainForm.actDeleteMessageExecute(Sender: TObject);
var
  Msg: TclMailMessageItem;
begin
  Msg := GetSelectedMessage();
  if (Msg <> nil) then
  begin
    if (Msg.Status = msDeleted) then
    begin
      Msg.Free();
    end else
    begin
      Msg.Status := msDeleted;
    end;
  end;
end;

procedure TMainForm.actMarkMessageAsReadExecute(Sender: TObject);
begin
  if (GetSelectedMessage() <> nil) then
  begin
    GetSelectedMessage().MarkedAsRead := True;
  end;
end;

procedure TMainForm.actMarkMessageAsUnreadExecute(Sender: TObject);
begin
  if (GetSelectedMessage() <> nil) then
  begin
    GetSelectedMessage().MarkedAsRead := False;
  end;
end;

procedure TMainForm.actReplyToSenderExecute(Sender: TObject);
begin
  OpenCurrentMessage(mekReply);
end;

procedure TMainForm.actReplyToAllExecute(Sender: TObject);
begin
  OpenCurrentMessage(mekReplyToAll);
end;

procedure TMainForm.actForwardMessageExecute(Sender: TObject);
begin
  OpenCurrentMessage(mekForward);
end;

procedure TMainForm.UpdateUI;
var Msg: TclMailMessageItem;
begin
  Msg := GetSelectedMessage();
  actSaveMessage.Enabled := Msg<>nil;
  actShowMessageSource.Enabled := Msg<>nil;
  actOpenMessage.Enabled := Msg<>nil;
  actDeleteMessage.Enabled := Msg<>nil;
  actMarkMessageAsRead.Enabled := (Msg<>nil) and not Msg.MarkedAsRead;
  actMarkMessageAsUnread.Enabled := (Msg<>nil) and Msg.MarkedAsRead;
  actReplyToSender.Enabled := (Msg<>nil) and (Msg.Status=msInbox);
  actReplyToAll.Enabled := actReplyToSender.Enabled;
  actForwardMessage.Enabled := (Msg<>nil) and (Msg.Status<>msOutbox);
end;

procedure TMainForm.lvMessagesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateUI;
end;

procedure TMainForm.BeforeLoadMessage;
begin
  Panel1.Left := lvMessages.Left + (lvMessages.Width - Panel1.Width) div 2;
  Panel1.Top := lvMessages.Top + (lvMessages.Height - Panel1.Height) div 2;
  Panel1.Visible := True;
  Panel1.Repaint;
end;

procedure TMainForm.AfterLoadMessage;
begin
  Panel1.Visible := False;
end;

end.
