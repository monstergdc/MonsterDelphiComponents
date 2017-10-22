unit loglib;

{----------------------------------------------------}
{ TLogger component for Delphi 3-7 or higher         }
{ Version: 1.04 now Lazarus/Linux friendly           }
{ Created: 2003.08.23                                }
{ E-mail:  monster@Noniewicz.com                     }
{ WWW:     http://www.Noniewicz.com                  }
{ Legal:   (c)2003-2017 MoNsTeR/GDC, Jakub Noniewicz }
{ Licence: BSD 2-Clause License                      }
{----------------------------------------------------}
{ Description:                                       }
{ The TLogger component is intended for simple       }
{ events (or whatever) logging to plain text file.   }
{----------------------------------------------------}
{ History:                                           }
{ Version 1.00, last update: 2003.08.23              }
{ Version 1.01, last update: 2004.02.04              }
{ Version 1.02, last update: 2004.02.06              }
{ Version 1.02, last update: 2005.06.15              }
{ Version 1.03, last update: ??????????              }
{ Version 1.04, last update: 2017.08.05              }
{ Version 1.04, last update: 2017.10.22 GitHub       }
{----------------------------------------------------}

{CHANGELOG:
# v1.00 - v1.02
- base stuff
# v1.03
- property CommaSep added
# v1.04
- Linux friendly
- loglevel added
}

interface

uses
     {$ifdef Win32}
     Windows,
     {$ENDIF}
     classes, SysUtils;

const
  //log levels
  LOGLEVEL_NONE      = -1;
  LOGLEVEL_ERROR     =  0;
  LOGLEVEL_WARNING   =  1;
  LOGLEVEL_INFO      =  2;
  LOGLEVEL_DEBUG     =  3;

type
  TLogger = class(TComponent)
  private
    LogFile: TextFile;
    FFileName: string;
    FUseLevel: boolean;
    FUseDate: boolean;
    FUseTime: boolean;
    FLogPid: boolean;
    FCommaSep: boolean;
    FCreateIfNoFile: boolean;
    FDateFormat: String;
    FTimeFormat: String;
    FOnOpenFail: TNotifyEvent;
    FLogLevel: integer;
    FWasOpen: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function OpenLog: boolean;
    function WriteLog(msg: string; level: integer = LOGLEVEL_DEBUG): boolean;
    procedure CloseLog;
  published
    property FileName: String read FFileName write FFileName;
    property UseLevel: boolean read FUseLevel write FUseLevel;
    property UseDate: boolean read FUseDate write FUseDate;
    property UseTime: boolean read FUseTime write FUseTime;
    property LogPid: boolean read FLogPid write FLogPid;
    property CreateIfNoFile: boolean read FCreateIfNoFile write FCreateIfNoFile;
    property DateFormat: String read FDateFormat write FDateFormat;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
    property CommaSep: boolean read FCommaSep write FCommaSep;
    property OnOpenFail: TNotifyEvent read FOnOpenFail write FOnOpenFail;
    property LogLevel: integer read FLogLevel write FLogLevel;
  end;



procedure Register;

implementation




procedure Register;
begin
  RegisterComponents('Monster', [TLogger]);
end;


function TLogger.OpenLog: boolean;
var OpenLog1: boolean;
begin
  OpenLog1 := true;
  try
    if FileExists(FileName) then
    begin
      AssignFile(LogFile, FileName);
      Append(LogFile);
    end
    else
      if FCreateIfNoFile then
      begin
        AssignFile(LogFile, FileName);
        Rewrite(LogFile);
      end
  except
    OpenLog1 := false;
    if assigned(FOnOpenFail) then FOnOpenFail(self);
  end;
  Result := OpenLog1;
  FWasOpen := OpenLog1;
end; { OpenLog }

function TLogger.WriteLog(msg: string; level: integer = LOGLEVEL_DEBUG): boolean;
var fmt: string;
    sep: string;
begin
  Result := true;

  if not FWasOpen then exit;
  if level > FLogLevel then exit;

  if FCommaSep then sep := ',' else sep := ' ';

  try
    fmt := '';
    if FUseDate then
      fmt := FDateFormat;
    if FUseTime then
      fmt := fmt + ' ' + FTimeFormat;
    if FUseDate or FUseTime then
    begin
      if FCommaSep then
        Write(LogFile, FormatDateTime(fmt, Now)+sep)
      else
        Write(LogFile, '['+FormatDateTime(fmt, Now)+']'+sep);
    end;
    if FLogPid then
    begin
      {$ifdef Win32}
      Write(LogFile, inttostr(GetCurrentProcessId)+sep);
      {$ENDIF}
    end;
    if FUseLevel then
    begin
      case level of
        LOGLEVEL_NONE: ;
        LOGLEVEL_ERROR:   Write(LogFile, 'E'+sep);
        LOGLEVEL_WARNING: Write(LogFile, 'W'+sep);
        LOGLEVEL_INFO:    Write(LogFile, 'I'+sep);
        LOGLEVEL_DEBUG:   Write(LogFile, 'D'+sep);
      end;
    end;
    WriteLn(LogFile, msg);
    Flush(LogFile);
  except
    Result := false;
  end;
end; { WriteLog }

procedure TLogger.CloseLog;
begin
  if FWasOpen then
  try
    CloseFile(LogFile);
  except
  end;
  FWasOpen := false;
end; { CloseLog }

constructor TLogger.Create(AOwner: TComponent);
begin
  inherited;
  FOnOpenFail := nil;
  FFileName := '';
  FUseLevel := true;
  FUseDate := true;
  FUseTime := true;
  FLogPid := true;
  FCreateIfNoFile := true;
  FDateFormat := 'yyyy-mm-dd';
  FTimeFormat := 'hh:nn:ss';
  FCommaSep := false;
  FLogLevel := LOGLEVEL_DEBUG;
  FWasOpen := false;
end; { Create }

destructor TLogger.Destroy;
begin
  inherited;
end; { Destroy }

end.
