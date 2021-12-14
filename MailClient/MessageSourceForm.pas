unit MessageSourceForm;

interface

uses
  Forms, ComCtrls, Classes, Controls, StdCtrls, Graphics, RVStyle,
  RVScroll, RichView;

type
  TfrmMessageSource = class(TForm)
    RichView1: TRichView;
    RVStyle1: TRVStyle;
  public
    class procedure ShowMessageSource(AMessage: TStrings);
  end;

implementation

{$R *.dfm}

{ TfrmMessageSource }

class procedure TfrmMessageSource.ShowMessageSource(AMessage: TStrings);
var
  Dlg: TfrmMessageSource;
begin
  Dlg := TfrmMessageSource.Create(nil);
  try
    Dlg.RichView1.AddTextNL(AMessage.Text, 0, 0, 0);
    Dlg.RichView1.Format;
    Dlg.ShowModal();
  finally
    Dlg.Free();
  end;
end;

end.
