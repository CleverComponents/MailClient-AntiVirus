unit ClamAV;

interface

//By Rene Tegel 2004
//Component wrapper for libclamav
//License: MAL

//Known issues:
//Cannot handle large zip files

uses
  Windows, SysUtils, Classes, Forms,
  libpclamav;
  
type
  TClamAV = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FLimits: TCl_Limits;
    FOptions: Word;
    FSigNo: Integer;
    FDB: Pointer;
    FActive: Boolean;
    FDBLoaded: Boolean;
    FDllLoaded: Boolean;
    FDBDir: TFileName;
    FDBFile: TFileName;
    procedure SetActive(const Value: Boolean);
    procedure SetDBDir(const Value: TFileName);
    procedure SetDBFile(const Value: TFileName);
    function GetLastError: String;
  public
    { Public declarations }
    FClamErr: Integer;
    FScanResult: Integer;
    FScanned: Boolean;
    FVirusName: String;
    function IsValidCVD (Value: TFileName): Boolean;
    function ScanFile (Value: TFileName): String;
    function ScanMem (Buffer: String): String;
    constructor Create (AOwner: TComponent); override;
    property Options: Word read FOptions write FOptions;
    property LastError: String read GetLastError;
    property DllLoaded: Boolean read FDllLoaded;
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive;
    property DBFile: TFileName read FDBFile write SetDBFile;
    property DBDir: TFileName read FDBDir write SetDBDir;
    property maxreclevel: Integer read FLimits.maxreclevel write FLimits.maxreclevel;
    property maxfiles: Integer read FLimits.maxfiles write FLimits.maxfiles;
    property maxratio: Integer read FLimits.maxratio write FLimits.maxratio;
    property archivememlim: SmallInt read FLimits.archivememlim write FLimits.archivememlim;
    property maxfilesize: LongInt read FLimits.maxfilesize write FLimits.maxfilesize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VisualSynapse', [TClamAV]);
end;

{ TClamAV }

constructor TClamAV.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  //set initial values
  Flimits.maxfiles := 1000;
  Flimits.maxratio := 1000;
  Flimits.archivememlim := 0;
  Flimits.maxfilesize := 10 * 1048576;
  Flimits.maxreclevel := 6;
  Foptions := CL_SCAN_RAW or CL_SCAN_ARCHIVE or CL_SCAN_MAIL or
              CL_SCAN_OLE2 or CL_SCAN_BLOCKENCRYPTED or CL_SCAN_HTML or
              CL_SCAN_PE or CL_SCAN_BLOCKBROKEN or CL_SCAN_MAILURL or CL_SCAN_BLOCKMAX;
  FDllLoaded := LoadClamavDLL;
end;

function TClamAV.GetLastError: String;
begin
  if FActive then
    Result := Trim (clam_strerror(FClamErr))
  else
    Result := 'Not active';
end;

function TClamAV.IsValidCVD(Value: TFileName): Boolean;
begin
  Result := LoadClamavDLL and
            (0 = clam_cvdverify (PChar(Value)));
end;

function TClamAV.ScanFile(Value: TFileName): String;
var VirName: String;
    scanned: Integer;
begin
  FVirusName := '';
  SetLength (VirName, 512);
  if FActive then
    begin
      FScanResult := clam_scanfile (PChar(Value), PChar (VirName), scanned, FDB, @FLimits, FOptions);
      FScanned := scanned = 0;
      if FScanResult = CL_VIRUS then
        begin
          FVirusName := PChar (VirName);
        end;
    end
  else
    FScanned := False;
  Result := FVirusName;
end;

function TClamAV.ScanMem(Buffer: String): String;
var VirName: STring;
begin
  FVirusName := '';
  SetLength (VirName, 512);
  if FActive then
    begin
      FScanResult := clam_scanbuff (PChar(Buffer), length (Buffer), PChar(VirName), FDB);
      FScanned := True;
      if FScanResult = CL_VIRUS then
        begin
          FVirusName := PChar (VirName);
        end;
    end
  else
    FScanned := False;
  Result := FVirusName;
end;

procedure TClamAV.SetActive(const Value: Boolean);
begin
  if Value = FActive then
    exit;
  if Value then
    begin
      if LoadClamavDLL then
        begin
          FDLLLoaded := True;

          if Assigned (FDB) then
            clam_free (FDB);
          FDB := nil;

          if FDBFile<>'' then
            begin
              FDB := clam_loaddb (PChar(FDBFile), FSigNo, FClamErr);
            end
          else
          if FDBDir<>'' then
            begin
              FDB := clam_loaddbdir (PChar(FDBDir), FSigNo, FClamErr);
            end;
          if Assigned (FDB) then
            begin
              FDBLoaded := True;
              FActive := 0 = clam_build(FDB);
            end;
        end;
    end
  else //just set to false, not dll unloading here
    FActive := False;

end;

procedure TClamAV.SetDBDir(const Value: TFileName);
begin
  FDBDir := Value;
end;

procedure TClamAV.SetDBFile(const Value: TFileName);
begin
  FDBFile := Value;
end;

end.
 