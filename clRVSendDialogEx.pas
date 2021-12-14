{*********************************************************}
{                                                         }
{       Procedure sending HTML Email using TRichView      }
{       editor and Clever Components                      }
{                                                         }
{       TRichView (c) Sergey Tkachenko                    }
{       http://www.trichview.com                          }
{       Clever Components (c) Clever Components Team      }
{       http://www.clevercomponents.com                   }
{                                                         }
{*********************************************************}

{
  ViewHTMLEmail() displays the specified email.
  EditHTMLEmail() opens a window for editing the specified email.
  ReplyToHTMLEmail() opens a window for editing the specified email, or
    forwarding it, or replying to it, depending on HowToReply parameter.

  Parameters:
    MailSource - email source, this is clMailMessage.MessageSource.
    MaxLineLength is used to break lines when quoting text for replying or
    forwarding.
    If HTMLKind = htmlSimple, a basic HTML is used.
    If HTMLKind = htmlAdvanced (recommended), HTML with CSS is used.
}

{$I RV_Defs.inc}

unit clRVSendDialogEx;

interface

uses Classes, Controls, Graphics, SysUtils, Forms, clRVSendDialog,
     HTMLView, HtmlGlobals, HtmlSubs,
     rvHtmlViewImport, clMailMessage, RVClasses, CRVData, RVRVData,
     RVGrHandler,
     RichView, RVEdit, RVFuncs, RVQuoteText, RVTypes, clRVSendMail;

type
  TMessageReplyKind  = (mekEdit, mekReply, mekReplyToAll, mekForward);
  TMessageEditResult = (meCancel, meSave, meSend);


procedure ViewHTMLEmail(MailSource: TStrings);
function EditHTMLEmail(MailSource: TStrings;
  HTMLKind: THTMLKind = htmlAdvanced): TMessageEditResult;
function ReplyToHTMLEmail(MailSource: TStrings; HowToReply: TMessageReplyKind;
  const From: String; HTMLKind: THTMLKind = htmlAdvanced;
  MaxLineLength: Integer = 80): TMessageEditResult;

implementation

type

  TMailImageItem = class (TCollectionItem)
    public
      ID, FileName: TRVUnicodeString;
      Used: Boolean;
      destructor Destroy; override;
  end;


  TLoadHelper = class
  public
    Images: TCollection;
    OldImportPicture: TRVImportPictureEvent;
    constructor Create;
    destructor Destroy; override;
    function MakeListOfUnusedIDs: TStringList;
    procedure DoMailMessageSaveAttachment(Sender: TObject; ABody: TclAttachmentBody;
    var AFileName: string; var AData: TStream; var Handled: Boolean);
    procedure DoImportPicture(Sender: TCustomRichView;
      const Location: TRVUnicodeString; Width, Height: Integer;
      var Graphic: TGraphic);
    procedure DoImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream); { // change to the declaration below if you use
    // the original THTMLViewer:
    procedure DoImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
    }

  end;
{------------------------------------------------------------------------------}
constructor TLoadHelper.Create;
begin
  inherited Create;
  Images := TCollection.Create(TMailImageItem);
end;
{------------------------------------------------------------------------------}
destructor TLoadHelper.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TLoadHelper.DoMailMessageSaveAttachment(Sender: TObject; ABody: TclAttachmentBody;
    var AFileName: string; var AData: TStream; var Handled: Boolean);
var FileName: String;
    p: Integer;
    Item: TMailImageItem;
begin
  if not (ABody is TclImageBody) then
    exit;
  FileName := TfrmComposeHTMLEMail.GetMessageTempDir() + TfrmComposeHTMLEMail.GetUniqueTempFileName(AFileName);
  p := Pos('@', FileName);
  if p<>0 then
    FileName := Copy(FileName, 1, p-1);
  AData := TFileStream.Create(FileName, fmCreate);
  Item := Images.Add as TMailImageItem;
  Item.ID := 'cid:'+ABody.ContentID;
  Item.FileName := FileName;
end;
{------------------------------------------------------------------------------}
function TLoadHelper.MakeListOfUnusedIDs: TStringList;
var i: Integer;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  for i := 0 to Images.Count-1 do
    if not TMailImageItem(Images.Items[i]).Used then
      Result.Add(TMailImageItem(Images.Items[i]).ID);
end;
{------------------------------------------------------------------------------}
procedure TLoadHelper.DoImportPicture(Sender: TCustomRichView;
  const Location: TRVUnicodeString; Width, Height: Integer;
  var Graphic: TGraphic);
var i: Integer;
    FileName: TRVUnicodeString;
begin
  Graphic := nil;
  for i := 0 to Images.Count-1 do
    if TMailImageItem(Images.Items[i]).ID=Location then
    begin
      FileName := TMailImageItem(Images.Items[i]).FileName;
      TMailImageItem(Images.Items[i]).Used := True;
      Graphic := RVGraphicHandler.LoadFromFile(FileName);
      exit;
    end;
  if Assigned(OldImportPicture) then
    OldImportPicture(Sender, Location, Width, Height, Graphic);
end;

{------------------------------------------------------------------------------}
procedure TLoadHelper.DoImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
{procedure TLoadHelper.DoImageRequest(Sender: TObject; const SRC: string;
  var Stream: TMemoryStream);}
begin
  Stream := WaitStream;
end;
{==============================================================================}

procedure LoadBody(Msg: TclMailMessage; rve: TCustomRichViewEdit; MailSource: TStrings;
  BackColor: TColor; var UnusedImageIds: TStringList);
var HTMLViewer: THTMLViewer;
    Importer: TRVHTMLViewImporter;
    Helper: TLoadHelper;
    Stream: TRVMemoryStream;
begin
  UnusedImageIds := nil;
  rve.Clear;
  if Msg.Html<>nil then begin
    HTMLViewer := THTMLViewer.Create(nil);
    HTMLViewer.Visible := False;
    HTMLViewer.Parent := rve.Parent;
    HTMLViewer.DefBackground := BackColor;
    Importer := TRVHTMLViewImporter.Create(nil);
    Helper := TLoadHelper.Create;
    try
      Helper.OldImportPicture := rve.OnImportPicture;
      rve.OnImportPicture := Helper.DoImportPicture;
      Msg.OnSaveAttachment := Helper.DoMailMessageSaveAttachment;
      Msg.MessageSource := MailSource;
      HTMLViewer.OnImageRequest := Helper.DoImageRequest;
      Stream := TRVMemoryStream.Create;
      try
        Msg.Html.Strings.SaveToStream(Stream {$IFDEF RVUNICODESTR}, TEncoding.Unicode{$ENDIF});
        Stream.Position := 0;
        HTMLViewer.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
      Importer.ImportHtmlViewer(HTMLViewer, rve);
      UnusedImageIds := Helper.MakeListOfUnusedIDs;
    finally
      rve.OnImportPicture := Helper.OldImportPicture;
      Helper.Free;
      Importer.Free;
      HTMLViewer.Free;
    end;
    end
  else if Msg.Text<>nil then
    rve.AddTextNL(Msg.Text.Strings.Text, 0, 0, 0);
  rve.Format;
end;
{------------------------------------------------------------------------------}
procedure ViewHTMLEmail(MailSource: TStrings);
var frm : TfrmComposeHTMLEMail;
    Msg: TclMailMessage;
    UnusedImageIds: TStringList;
begin
  Msg := TclMailMessage.Create(Application);
  frm := TfrmComposeHTMLEMail.Create(Application);
  try
    Msg.MessageSource := MailSource;
    frm.BeforeLoadingBody;
    try
      LoadBody(Msg, frm.RichViewEdit1, MailSource, clBtnFace, UnusedImageIds);
    finally
      frm.AfterLoadingBody;
    end;
    try
      frm.FromMessage(Msg, MailSource, UnusedImageIds);
    finally
      UnusedImageIds.Free;
    end;
    frm.ReadOnly := True;
    frm.ShowModal;
  finally
    frm.DeleteTempFiles;
    frm.Free;
    msg.Free;
  end;
end;

function EditHTMLEmail(MailSource: TStrings; HTMLKind: THTMLKind = htmlAdvanced): TMessageEditResult;
begin
  Result := ReplyToHTMLEmail(MailSource, mekEdit, '', HTMLKind, 0);
end;
{------------------------------------------------------------------------------}
function ReplyToHTMLEmail(MailSource: TStrings; HowToReply: TMessageReplyKind;
  const From: String; HTMLKind: THTMLKind = htmlAdvanced; 
  MaxLineLength: Integer = 80): TMessageEditResult;
var frm : TfrmComposeHTMLEMail;
    Attachments: TStrings;
    Res: TModalResult;
    Msg: TclMailMessage;
    UnusedImageIds: TStringList;
begin
  Attachments := nil;
  Msg := TclMailMessage.Create(Application);
  frm := TfrmComposeHTMLEMail.Create(Application);
  try
    Msg.MessageSource := MailSource;
    frm.AllowSaving;
    case HowToReply of
      mekForward:
        begin
          Msg.From.FullAddress := From;
          Msg.ToList.Clear;
          Msg.CCList.Clear;
          Msg.BCCList.Clear;
          Msg.Subject := 'Fw: '+Msg.Subject;
        end;
      mekReply:
        begin
          Msg.ToList.Clear;
          Msg.ToList.Add(Msg.From.FullAddress);
          Msg.From.FullAddress := From;
          Msg.CCList.Clear;
          Msg.BCCList.Clear;
          if LowerCase(Copy(Msg.Subject, 1, 4))<>'re: ' then
            Msg.Subject := 'Re: '+Msg.Subject;
        end;
      mekReplyToAll:
        begin
          Msg.ToList.Add(Msg.From.FullAddress);
          Msg.From.FullAddress := From;
          Msg.BCCList.Clear;
          if LowerCase(Copy(Msg.Subject, 1, 4))<>'re: ' then
            Msg.Subject := 'Re: '+Msg.Subject;
        end;
    end;
    frm.BeforeLoadingBody;
    try
      LoadBody(Msg, frm.RichViewEdit1, MailSource, clWindow, UnusedImageIds);
    finally
      frm.AfterLoadingBody;
    end;
    try
      frm.FromMessage(Msg, MailSource, UnusedImageIds);
    finally
      UnusedImageIds.Free;
    end;
    if HowToReply<>mekEdit then
      QuoteText( '>', MaxLineLength, frm.RichViewEdit1);
    Res := frm.ShowModal;
    if Res in [mrOk, mrYes] then begin
      frm.ToMessage(Msg, Attachments);
      frm.RichViewEdit1.SetSelectionBounds(0, frm.RichViewEdit1.GetOffsBeforeItem(0),
        0, frm.RichViewEdit1.GetOffsBeforeItem(0));
      frm.RichViewEdit1.DeleteUnusedStyles(True, True, True);
      CopyHTMLEmailFromRVToMsg(frm.RichViewEdit1, Msg, HTMLKind, Attachments);
      MailSource.Assign(Msg.MessageSource);
    end;
    case Res of
      mrOk:
        Result := meSend;
      mrYes:
        Result := meSave;
      else  Result := meCancel;
    end;
  finally
    frm.DeleteTempFiles;
    frm.Free;
    Attachments.Free;
    Msg.Free;
  end;
end;


{================================== TRVImageItem ==============================}
destructor TMailImageItem.Destroy;
begin
  DeleteFile(FileName);
  inherited;
end;

end.
