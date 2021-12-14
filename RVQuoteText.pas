{*******************************************************}
{                                                       }
{       TRichView                                       }
{       Procedure to quote text using the specified     }
{       prefix.                                         }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVQuoteText;

interface

{$I RV_Defs.inc}

uses Windows, Messages,
     RVStyle, RVFuncs, RVScroll, RVItem, CRVData, RichView, DLines, RVTypes,
     RVUni, RVNormalize, RVRVData;


{
  This function quotes all text in rv.
  Parameters:
  Prefix - text to insert before each line (for example, '>', or 'John>')
  MaxLength - length of lines, in pixels. Lines exceeding this with will be
    separated into smaller lines. Width of Prefix is not taken into account.

  This procedure formats document. It is not necessary to format document
  before calling this procedure.

  This is not an editing operation. If it is called for TRichViewEdit, call
  RichViewEdit.ClearUndo after.

}
procedure QuoteText(const Prefix: TRVUnicodeString; MaxLength: Integer;
  rv: TCustomRichView);

implementation

{------------------------------------------------------------------------------}
procedure QuoteText(const Prefix: TRVUnicodeString; MaxLength: Integer;
  rv: TCustomRichView);
    {..........................................................}
    procedure SplitItem(ItemNo, Len: Integer);
    var
      s1, s2: TRVUnicodeString;
      Item: TCustomRVItemInfo;
    begin
      s1 := rv.GetItemTextW(ItemNo);
      s2 := Copy(s1, Len, Length(s1));
      s1 := Copy(s1, 1, Len - 1);
      rv.RVData.SetItemTextW(ItemNo, s1);
      Item := RichViewTextItemClass.Create(rv.RVData);
      Item.Assign(rv.GetItem(ItemNo));
      Item.Tag := RV_CopyTag(rv.GetItemTag(ItemNo), rvoTagsArePChars in rv.Options);
      Item.SameAsPrev := False;
      Item.Inserting(rv.RVData, s2, False);
      rv.RVData.Items.InsertObject(ItemNo+1, s2, Item);
      Item.Inserted(rv.RVData, ItemNo);
    end;
    {..........................................................}
    function GetTextStyleNoForParagraph(ItemNo: Integer): Integer;
    var i: Integer;
    begin
      Result := 0;
      for i := ItemNo to rv.ItemCount-1 do
      begin
        if rv.GetItemStyle(i)>=0 then
        begin
          Result := rv.GetItemStyle(i);
          exit;
        end;
        if rv.IsFromNewLine(i) then
          exit;
      end;
    end;
    {..........................................................}
    procedure InsertPrefix(ItemNo: Integer);
    var
      Item: TCustomRVItemInfo;
      LPrefix: TRVUnicodeString;
    begin
      if rv.GetItem(ItemNo).GetBoolValue(rvbpFullWidth) then
        exit;
      if rv.GetItemStyle(ItemNo)=rvsListMarker then
        inc(ItemNo);
      LPrefix := Prefix;
      Item := RichViewTextItemClass.Create(rv.RVData);
      Item.Assign(rv.GetItem(ItemNo));
      Item.StyleNo := GetTextStyleNoForParagraph(ItemNo);
      Item.Tag := RV_CopyTag(rv.GetItemTag(ItemNo), rvoTagsArePChars in rv.Options);
      Item.Inserting(rv.RVData, LPrefix, False);
      rv.RVData.Items.InsertObject(ItemNo, LPrefix, Item);
      Item.Inserted(rv.RVData, ItemNo);
      rv.GetItem(ItemNo+1).SameAsPrev := True;
    end;
    {..........................................................}
var
  OldMaxTextWidth, i: Integer;
  OldOptions: TRVOptions;
  DItem: TRVDrawLineInfo;
begin
  SendMessage(rv.Handle, WM_SETREDRAW, 0, 0);
  try
    OldMaxTextWidth := rv.MaxTextWidth;
    OldOptions := rv.Options;
    rv.MaxTextWidth := MaxLength;
    rv.Options := rv.Options - [rvoClientTextWidth];
    try
      rv.Format;
    finally
      rv.Options := OldOptions;
      rv.MaxTextWidth := OldMaxTextWidth;
    end;
    for i := rv.RVData.DrawItems.Count-1 downto 0 do
    begin
      DItem := rv.RVData.DrawItems[i];
      if DItem.FromNewLine then
      begin
        if (DItem.Offs<=rv.GetOffsBeforeItem(DItem.ItemNo)) and
          not rv.IsFromNewLine(DItem.ItemNo) then
          rv.GetItem(DItem.ItemNo).SameAsPrev := False
        else if DItem.Offs>rv.GetOffsBeforeItem(DItem.ItemNo) then
          SplitItem(DItem.ItemNo, DItem.Offs);
      end;
    end;
    for i := rv.ItemCount-1 downto 0 do
      if rv.IsFromNewLine(i) then
        InsertPrefix(i);
  finally
    SendMessage(rv.Handle, WM_SETREDRAW, 1, 0);
  end;
  NormalizeRichView(rv.RVData);
  rv.Format;
end;

end.
