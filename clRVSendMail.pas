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
  How to use:

  Use the following procedure:
  procedure SendHTMLEmail(RV: TCustomRichView; Smtp: TclSmtp; Msg: TclMailMessage;
    HTMLKind: THTMLKind = htmlAdvanced; Attachments: TStrings = nil);
  If HTMLKind = htmlSimple, a basic HTML is used.
  If HTMLKind = htmlAdvanced (recommended), HTML with CSS is used.
}

unit clRVSendMail;

interface

{$I RV_Defs.inc}

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Jpeg,
  Dialogs, StdCtrls,
  clMailMessage, clSMTP, clTcpClient,
  RVStyle, RVScroll, RichView, RVEdit, CRVData, RVTypes, RVGrHandler, RVStrFuncs;

type
  THTMLKind = (htmlSimple, htmlAdvanced);

procedure SendHTMLEmail(RV: TCustomRichView; Smtp: TclSmtp; Msg: TclMailMessage;
  HTMLKind: THTMLKind = htmlAdvanced; Attachments: TStrings = nil);
procedure CopyHTMLEmailFromRVToMsg(RV: TCustomRichView; Msg: TclMailMessage;
  HTMLKind: THTMLKind = htmlAdvanced; Attachments: TStrings = nil);

implementation


{------------------------------------------------------------------------------}
type
  TSendHelper = class
  public
    Images: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure DoSaveImage2(Sender: TCustomRichView;
     Graphic: TGraphic; SaveFormat: TRVSaveFormat; const Path,
     ImagePrefix: TRVUnicodeString; var ImageSaveNo: Integer;
     var Location: TRVUnicodeString; var DoDefault: Boolean);
    procedure DoMailMessageLoadAttachment(Sender: TObject;
      ABody: TclAttachmentBody; var AFileName: String; var AData: TStream;
      var Handled: Boolean);
  end;
{========================== TSendHelper =======================================}
constructor TSendHelper.Create;
begin
  inherited Create;
  Images := TStringList.Create;
end;
{------------------------------------------------------------------------------}
destructor TSendHelper.Destroy;
var i: Integer;
begin
  for i := 0 to Images.Count-1 do
    Images.Objects[i].Free;
  Images.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TSendHelper.DoSaveImage2(Sender: TCustomRichView;
  Graphic: TGraphic; SaveFormat: TRVSaveFormat; const Path,
  ImagePrefix: TRVUnicodeString; var ImageSaveNo: Integer;
  var Location: TRVUnicodeString; var DoDefault: Boolean);
var gr: TGraphic;
    bmp: TBitmap;
    Stream: TMemoryStream;
begin
  if SaveFormat<>rvsfHTML then
    exit;
  if not (Graphic is TJPEGImage) and not RVGraphicHandler.IsHTMLGraphic(Graphic) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Assign(Graphic);
    except
      bmp.Width := Graphic.Width;
      bmp.Height := Graphic.Height;
      bmp.Canvas.Draw(0,0, Graphic);
    end;
    gr := TJPEGImage.Create;
    gr.Assign(bmp);
    bmp.Free;
  end
  else
    gr := Graphic;

  Location := RVFormatW('image%d.%s',
    [ImageSaveNo, GraphicExtension(TGraphicClass(gr.ClassType))]);
  inc(ImageSaveNo);
  Stream := TMemoryStream.Create;
  gr.SaveToStream(Stream);
  Images.AddObject(Location, Stream);
  DoDefault := False;
  if gr<>Graphic then
    gr.Free;
end;
{------------------------------------------------------------------------------}
procedure TSendHelper.DoMailMessageLoadAttachment(Sender: TObject;
  ABody: TclAttachmentBody; var AFileName: String;
  var AData: TStream; var Handled: Boolean);
var Index: Integer;
begin
  Index :=  Images.IndexOf(AFileName);
  Handled := Index>=0;
  if Handled then
  begin
    AData := TMemoryStream.Create;
    TStream(Images.Objects[Index]).Position := 0;
    AData.CopyFrom(TStream(Images.Objects[Index]), 0);
    AData.Position := 0;
    exit;
  end;
end;
{------------------------------------------------------------------------------}
// Returns HTML
function DoGetHTML(rv: TCustomRichView; HTMLKind: THTMLKind; UTF8: Boolean): TRVRawByteString;
var Stream: TMemoryStream;
    Options: TRVSaveOptions;
begin
  Options := [rvsoNoHypertextImageBorders, rvsoImageSizes,
    rvsoUseCheckpointsNames];
  if UTF8 then
    Include(Options, rvsoUTF8);
  Stream := TMemoryStream.Create;
  case HTMLKind of
    htmlSimple:
      rv.SaveHTMLToStream(Stream, '', '', '', Options);
    htmlAdvanced:
      rv.SaveHTMLToStreamEx(Stream, '', '', '', '', '', '', Options);
  end;
  Stream.Position := 0;
  SetLength(Result, Stream.Size);
  Stream.ReadBuffer(PRVAnsiChar(Result)^, Length(Result));
  Stream.Free;
end;
// Returns HTML in UTF-16 (needed in Delphi 2009+ or newer)
{$IFDEF RVUNICODESTR}
function GetHTMLW(rv: TCustomRichView; HTMLKind: THTMLKind): TRVUnicodeString;
begin
  Result := UTF8ToUnicodeString(DoGetHTML(rv, HTMLKind, True));
end;
{$ENDIF}
// Returns HTML
function GetHTML(rv: TCustomRichView;HTMLKind: THTMLKind): String;
begin
   {$IFDEF RVUNICODESTR}
   // Delphi 2009 or newer
   Result := GetHTMLW(rv, HTMLKind);
   {$ELSE}
   // Delphi 2007 or older
   Result := DoGetHTML(rv, HTMLKind, False);
   {$ENDIF}
end;
{------------------------------------------------------------------------------}
{$IFDEF RVUNICODESTR}
// Returns plain text in UTF-16
function GetTextW(rv: TCustomRichView): TRVUnicodeString;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  rv.SaveTextToStreamW('', Stream, 80, False, False);
  Stream.Position := 0;
  SetLength(Result, Stream.Size div 2);
  Stream.ReadBuffer(PRVUnicodeChar(Result)^, Length(Result)*2);
  Stream.Free;
end;
{$ELSE}
// Returns plain text in ANSI
function GetTextA(rv: TCustomRichView): TRVAnsiString;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  rv.SaveTextToStream('', Stream, 80, False, False);
  Stream.Position := 0;
  SetLength(Result, Stream.Size);
  Stream.ReadBuffer(PRVAnsiChar(Result)^, Length(Result));
  Stream.Free;
end;
{$ENDIF}
// Returns text
function GetText(rv: TCustomRichView): String;
begin
   {$IFDEF RVUNICODESTR}
   // Delphi 2009 or newer
   Result := GetTextW(rv);
   {$ELSE}
   // Delphi 2007 or older
   Result := GetTextA(rv);
   {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure AssignMsg(Src, Dst: TclMailMessage);
begin
  Dst.Priority := Src.Priority;
  Dst.Newsgroups.Assign(Src.Newsgroups);
  Dst.From.Assign(Src.From);
  Dst.ToList.Assign(Src.ToList);  
  Dst.CCList.Assign(Src.CCList);
  Dst.BCCList.Assign(Src.BCCList);
  Dst.ReadReceiptTo := Src.ReadReceiptTo;
  Dst.References.Assign(Src.References);
  Dst.ReplyTo := Src.ReplyTo;    
  Dst.ExtraFields.Assign(Src.ExtraFields);
  Dst.Subject := Src.Subject;  
end;
{------------------------------------------------------------------------------}
procedure SendHTMLEmail(RV: TCustomRichView; Smtp: TclSmtp; Msg: TclMailMessage;
  HTMLKind: THTMLKind; Attachments: TStrings);
var Helper: TSendHelper;
    Msg2: TclMailMessage;
    OldOnSaveImage2: TRVSaveImageEvent2;
    OldOnLoadAttachment: TclGetBodyStreamEvent;
begin
  Helper := TSendHelper.Create;
  Msg2 := TclMailMessage.Create(nil);
  OldOnSaveImage2 := rv.OnSaveImage2;
  OldOnLoadAttachment := Msg.OnLoadAttachment;
  try
    rv.OnSaveImage2 := Helper.DoSaveImage2;
    Msg.OnLoadAttachment := Helper.DoMailMessageLoadAttachment;
    Msg.CharSet := 'utf-8';
    AssignMsg(Msg, Msg2);
    Msg.BuildMessage(GetText(RV), GetHTML(RV, HTMLKind), Helper.Images, Attachments);
    AssignMsg(Msg2, Msg);
    //Msg.MessageSource;

    SMTP.Open;
    try
      SMTP.Send(Msg);
    finally
      SMTP.Close();
    end;

  finally
    rv.OnSaveImage2 := OldOnSaveImage2;
    Msg.OnLoadAttachment := OldOnLoadAttachment;
    Helper.Free;
    Msg2.Free;
  end;
end;

procedure CopyHTMLEmailFromRVToMsg(RV: TCustomRichView; Msg: TclMailMessage;
  HTMLKind: THTMLKind = htmlAdvanced; Attachments: TStrings = nil);
var Helper: TSendHelper;
    Msg2: TclMailMessage;
    OldOnSaveImage2: TRVSaveImageEvent2;
    OldOnLoadAttachment: TclGetBodyStreamEvent;
begin
  Helper := TSendHelper.Create;
  Msg2 := TclMailMessage.Create(nil);
  OldOnSaveImage2 := rv.OnSaveImage2;
  OldOnLoadAttachment := Msg.OnLoadAttachment;
  try
    rv.OnSaveImage2 := Helper.DoSaveImage2;
    Msg.OnLoadAttachment := Helper.DoMailMessageLoadAttachment;
    Msg.CharSet := 'utf-8';
    AssignMsg(Msg, Msg2);
    Msg.BuildMessage(GetText(RV), GetHTML(RV, HTMLKind), Helper.Images, Attachments);
    AssignMsg(Msg2, Msg);
    Msg.MessageSource;
  finally
    rv.OnSaveImage2 := OldOnSaveImage2;
    Msg.OnLoadAttachment := OldOnLoadAttachment;
    Helper.Free;
    Msg2.Free;
  end;
end;

end.
