unit libpclamav;

(*
  As taken from clamav for windows package

  Pascal DLL renamed to libpclamav.dll to avoid loading wrong dll version.
  Pascal DLL compiled by Rene using minGW ** issues on bzip support **

  Added support for dynamic loading


*)
{**************************************************************************}
{    Partial Delphi header for clamav.h                                                                      }
{    Generated Date: 2004-10-09                                            }
{    Generated Time: 12:48:04                                              }
{                                                                          }
{**************************************************************************}

interface
uses
 Windows;


const
  clamdll='libpclamav.dll';
  
  CL_COUNT_PRECISION = 4096;
{+// return codes*/ }
  CL_CLEAN = 0; {/* virus not found*/}
  CL_VIRUS = 1; {/* virus found*/}
  CL_EMAXREC = 10; {/* recursion level limit exceeded*/}
  CL_EMAXSIZE = 11; {/* size limit exceeded*/}
  CL_EMAXFILES = 12; {/* files limit exceeded*/}
  CL_ERAR = 100; {/* rar handler error*/}
  CL_EZIP = 101; {/* zip handler error*/}
  CL_EMALFZIP = 102; {/* malformed zip*/}
  CL_EGZIP = 103; {/* gzip handler error*/}
  CL_EBZIP = 104; {/* bzip2 handler error*/}
  CL_EOLE2 = 105; {/* OLE2 handler error*/}
  CL_EMSCOMP = 106; {/* compress.exe handler error*/}
  CL_EMSCAB = 107; {/* MS CAB module error*/}
  CL_EACCES = 200; {/* access denied*/}
  CL_ENULLARG = 300; {/* null argument error*/}
  CL_ETMPFILE = -1; {/* tmpfile() failed*/}
  CL_EFSYNC = -2; {/* fsync() failed*/}
  CL_EMEM = -3; {/* memory allocation error*/}
  CL_EOPEN = -4; {/* file open error*/}
  CL_EMALFDB = -5; {/* malformed database*/}
  CL_EPATSHORT = -6; {/* pattern too short*/}
  CL_ETMPDIR = -7; {/* mkdir() failed*/}
  CL_ECVD = -8; {/* not a CVD file (or broken)*/}
  CL_ECVDEXTR = -9; {/* CVD extraction failure*/}
  CL_EMD5 = -10; {/* MD5 verification error*/}
  CL_EDSIG = -11; {/* digital signature verification error*/}
  CL_EIO = -12; {/* general I/O error*/}
  CL_EFORMAT = -13; {/* bad format or broken file*/}
{+// scan options*/ }
  CL_SCAN_RAW = 0;
  CL_SCAN_ARCHIVE = 1;
  CL_SCAN_MAIL = 2;
  CL_SCAN_DISABLERAR = 4;
  CL_SCAN_OLE2 = 8;
  CL_SCAN_BLOCKENCRYPTED = 16;
  CL_SCAN_HTML = 32;
  CL_SCAN_PE = 64;
  CL_SCAN_BLOCKBROKEN = 128;
  CL_SCAN_MAILURL = 256;
  CL_SCAN_BLOCKMAX = 512;
{+// recommended options*/ }
  CL_SCAN_STDOPT = CL_SCAN_ARCHIVE or CL_SCAN_MAIL or CL_SCAN_OLE2 or CL_SCAN_HTML or CL_SCAN_PE;
{+// aliases for backward compatibility*/ }
  CL_RAW = CL_SCAN_RAW;
  CL_ARCHIVE = CL_SCAN_ARCHIVE;
  CL_MAIL = CL_SCAN_MAIL;
  CL_DISABLERAR = CL_SCAN_DISABLERAR;
  CL_OLE2 = CL_SCAN_OLE2;
  CL_ENCRYPTED = CL_SCAN_BLOCKENCRYPTED;


type
  Pcli_bm_patt = ^Tcli_bm_patt;
  Tcli_bm_patt = record
      pattern : PChar;
      virname : PChar;
      offset : PChar;
      viralias : PChar;
      length : Integer;
      target : Word;
      next : pcli_bm_patt;
  end;
  {
      char *pattern, *virname, *offset;
      const char *viralias;
      unsigned int length;
      unsigned short target;
      struct cli_bm_patt *next;
  }


  Pcli_ac_patt = ^Tcli_ac_patt;
  Tcli_ac_patt = record
      pattern : PSmallInt;
      length : Cardinal;
      mindist : Cardinal;
      maxdist : Cardinal;
      virname : PChar;
      offset : PChar;
      viralias : PChar;
      sigid : Word;
      parts : Word;
      partno : Word;
      alt : Word;
      altn : Word;
      typed : Word;
      target : Word;
      altc : PChar;
      next : pcli_ac_patt;
  end;

  {
      short int *pattern;
      unsigned int length, mindist, maxdist;
      char *virname, *offset;
      const char *viralias;
      unsigned short int sigid, parts, partno, alt, *altn;
      unsigned short type, target;
      char **altc;
      struct cli_ac_patt *next;
  }


  Pcli_ac_node = ^Tcli_ac_node;
  Tcli_ac_node = record
      islast : Char;
      list : pcli_ac_patt;
      trans : array[0..255] of pcli_ac_node;
      fail : pcli_ac_node;
  end;


  {
      char islast;
      struct cli_ac_patt *list;
      struct cli_ac_node *trans[256], *fail;
  }


  Pcli_md5_node = ^Tcli_md5_node;
  TCli_md5_node = record
       virname : PChar;
       viralias : PChar;
       md5 : PChar;
       size : Cardinal;
       next : pcli_md5_node;
  end;

  {
      char *virname, *viralias;
      unsigned char *md5;
      unsigned int size;
      struct cli_md5_node *next;
  }



  Ppcl_node = ^pcl_node;
  Pcl_node = ^Tcl_node;
  Tcl_node = record
   maxpatlen : Cardinal;
   bm_shift : PInteger;
   ac_root : Tcli_ac_node;
   ac_nodetable : ^pcli_ac_node;
   ac_partsigs : Cardinal;
   ac_nodes : Cardinal;
   md5_hlist : ^pcli_md5_node;
  end;


  {
      unsigned int maxpatlen; /* maximal length of pattern in db */

      /* Extended Boyer-Moore */
      int *bm_shift;
      struct cli_bm_patt **bm_suffix;

      /* Extended Aho-Corasick */
      struct cli_ac_node *ac_root, **ac_nodetable;
      unsigned int ac_partsigs, ac_nodes;

      /* MD5 */
      struct cli_md5_node **md5_hlist;
  }



    Pcl_limits = ^Tcl_limits;
    Tcl_limits = record
      maxreclevel: Integer;
      maxfiles: Integer;
      maxratio: Integer;
      archivememlim: SmallInt;
      maxfilesize: LongInt;
    end;


    Pcl_cvd = ^Tcl_cvd;
    Tcl_cvd = record
      time: PChar;
      version: Integer;
      sigs: Integer;
      fl: SmallInt;
      md5: PChar;
      dsig: PChar;
      builder: PChar;
      stime: Integer;
    end;

  //scan memory buffer
  TClam_scanbuff=function (const buffer: PChar;
                       length: integer;
                       const virname: PChar;
                       root : Pointer): Integer ; cdecl;



  //scan file
  TClam_scanfile=function (const filename: PChar;
                       const virname: PChar;
                       var scanned: LongInt;
                       root : Pointer;
                       limits: pcl_limits;
                       options: Word): Integer ; cdecl;

  //scan file descriptor (open handle to file)
  TClam_scandesc=function (desc : integer;
                       const virname: PChar;
                       var scanned: LongInt;
                       root : Pointer;
                       limits: pcl_limits;
                       options: Word): Integer ; cdecl;


  TClam_retflevel=function : Integer ; cdecl;

  TClam_retver=function : PChar ; cdecl;

  TClam_debug = procedure; cdecl;
  TClam_build=function (root : Pointer) : integer ; cdecl;
  TClam_free=procedure (root : Pointer) ; cdecl;
  TClam_loaddbdir=function (const dirname: PChar;
                        var signo: Integer;var errcode : Integer): Pointer ; cdecl;

  TClam_loaddb=function (const filename: PChar;
                     var signo: integer;var errcode: Integer): Pointer ; cdecl;

  TClam_cvdverify=function (filename : PChar) : integer ; cdecl;
  TClam_strerror=function (clerror : integer) : PChar ; cdecl;

var
  Clam_scanbuff : TClam_scanbuff;
  Clam_scanfile : TClam_scanfile;
  Clam_scandesc : TClam_scandesc;
  Clam_retflevel : TClam_retflevel;
  Clam_retver : TClam_retver;
  Clam_strerror : TClam_strerror;
  Clam_debug : TClam_debug;
  Clam_build : TClam_build;
  Clam_free : TClam_free;
  Clam_loaddbdir : TClam_loaddbdir;
  Clam_loaddb : TClam_loaddb;
  Clam_cvdverify : TClam_cvdverify;

  function LoadClamavDLL: Boolean;

implementation

var
  LibLoaded: Boolean = False;
  HDLL: THandle=0;

function LoadClamavDLL: Boolean;
var Loaded: Boolean;

  function ProcAddress (Proc: String): Pointer;
  begin
    {$IFNDEF FPC}
    Result := GetProcAddress (HDLL, PChar(Proc));
    {$ELSE}
    Result := GetProcedureAddress (HDLL, PChar(Proc)); //verify this call (it's not correct now).
    {$ENDIF}
    if not Assigned(Result) then
      Loaded := False;
  end;

begin
  Result := LibLoaded;
  if Result then
    exit;
  if HDLL = 0 then
    begin
      HDLL := LoadLibrary (ClamDLL);
      if HDLL <> 0 then
        begin
          Loaded := True;
          Clam_scanbuff := ProcAddress ('clam_scanbuff');
          Clam_scanfile := ProcAddress ('clam_scanfile');
          Clam_scandesc := ProcAddress ('clam_scandesc');
          Clam_retflevel := ProcAddress ('clam_retflevel');
          Clam_retver := ProcAddress ('clam_retver');
          Clam_strerror := ProcAddress ('clam_strerror');
          Clam_debug := ProcAddress ('clam_debug');
          Clam_build := ProcAddress ('clam_build');
          Clam_free := ProcAddress ('clam_free');
          Clam_loaddbdir := ProcAddress ('clam_loaddbdir');
          Clam_loaddb := ProcAddress ('clam_loaddb');
          Clam_cvdverify := ProcAddress ('clam_cvdverify');
          LibLoaded := Loaded;
          Result := LibLoaded;
          if not Result then
            begin
              FreeLibrary (HDLL);
              HDLL := 0;
            end;
        end;
    end;
end;

end.
