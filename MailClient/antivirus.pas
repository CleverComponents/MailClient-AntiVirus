unit antivirus;

interface

uses
  Classes, SysUtils, clMailMessage, clUtils, ClamAV;

type
  TclAVScanner = class
  private
    FMailMessage: TclMailMessage;
    FAttachments: TStrings;
    FTempDir: string;
    
    procedure DoMailMessageSaveAttachment(Sender: TObject;
      ABody: TclAttachmentBody; var AFileName: string; var AData: TStream;
      var Handled: Boolean);
    procedure DeleteTempFiles;
    function ScanAttachments: Boolean;
    procedure SetTempDir(const Value: string);
  protected
    function ScanMessageSource(Msg: TStrings): Boolean; virtual; abstract;
    function ScanAttachment(const AFileName: string): Boolean; virtual; abstract;
    procedure BeforeScan; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function ScanMessage(Msg: TStrings): Boolean;

    property TempDir: string read FTempDir write SetTempDir;
  end;

  TclClamAVScanner = class(TclAVScanner)
  private
    FAV: TClamAV;

    function GetDBDir: string;
    procedure SetDBDir(const Value: string);
  protected
    function ScanMessageSource(Msg: TStrings): Boolean; override;
    function ScanAttachment(const AFileName: string): Boolean; override;
    procedure BeforeScan; override;
  public
    constructor Create(const ADBDir: string);
    destructor Destroy; override;

    property DBDir: string read GetDBDir write SetDBDir;
  end;

implementation

{ TclAVScanner }

constructor TclAVScanner.Create;
begin
  inherited Create();

  FAttachments := TStringList.Create();
  
  FMailMessage := TclMailMessage.Create(nil);
  FMailMessage.OnSaveAttachment := DoMailMessageSaveAttachment; 
end;

procedure TclAVScanner.DeleteTempFiles;
var
  i: Integer;
begin
  for i := 0 to FAttachments.Count - 1 do
  begin
    DeleteFile(FAttachments[i]);
  end;
end;

destructor TclAVScanner.Destroy;
begin
  DeleteTempFiles();
  FMailMessage.Free();
  FAttachments.Free();
  
  inherited Destroy();
end;

procedure TclAVScanner.DoMailMessageSaveAttachment(Sender: TObject;
  ABody: TclAttachmentBody; var AFileName: string; var AData: TStream; var Handled: Boolean);
var
  fileName: string;
begin
  fileName := ExtractFileName(AFileName);
  if (fileName = '') then
  begin
    fileName := 'tempattachment.dat';
  end;
  fileName := AddTrailingBackSlash(TempDir) + fileName;
  fileName := GetUniqueFileName(fileName);

  AData := TFileStream.Create(fileName, fmCreate);
  FAttachments.Add(fileName);
  Handled := True;
end;

function TclAVScanner.ScanAttachments: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FAttachments.Count - 1 do
  begin
    Result := ScanAttachment(FAttachments[i]);
    if not Result then Exit;
  end;
end;

function TclAVScanner.ScanMessage(Msg: TStrings): Boolean;
begin
  BeforeScan();
  
  Result := ScanMessageSource(Msg);
  if not Result then Exit;
  
  try
    FAttachments.Clear();
    FMailMessage.MessageSource := Msg;
    Result := ScanAttachments();
  finally
    DeleteTempFiles();
    FAttachments.Clear();
  end;
end;

procedure TclAVScanner.SetTempDir(const Value: string);
begin
  if (FTempDir = Value) then Exit;
  
  FTempDir := Value;
  if (FTempDir <> '') then
  begin
    ForceFileDirectories(FTempDir);
  end;
end;

{ TclAVScanner }

procedure TclClamAVScanner.BeforeScan;
begin
  FAV.Active := True;
end;

constructor TclClamAVScanner.Create(const ADBDir: string);
begin
  inherited Create();

  FAV := TClamAV.Create(nil);
  DBDir := ADBDir;
end;

destructor TclClamAVScanner.Destroy;
begin
  FAV.Free();

  inherited Destroy();
end;

function TclClamAVScanner.GetDBDir: string;
begin
  Result := FAV.DBDir;
end;

procedure TclClamAVScanner.SetDBDir(const Value: string);
begin
  if (FAV.DBDir = Value) then Exit;
  
  FAV.Active := False;
  FAV.DBDir := Value;
end;

function TclClamAVScanner.ScanAttachment(const AFileName: string): Boolean;
begin
  Result := (FAV.ScanFile(AFileName) = '');
end;

function TclClamAVScanner.ScanMessageSource(Msg: TStrings): Boolean;
begin
  Result := (FAV.ScanMem(Msg.Text) = '');
end;

end.
