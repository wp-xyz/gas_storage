unit gUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

function CreateIni: TCustomIniFile;

function Download(const URL, API_Key: String; const AStream: TStream; var AErrMsg: String): Boolean;

function NumberToStr(AValue: Double; AFormat: String = ''): String;
function StrToNumber(Str: String): Double;

var
  App_FormatSettings: TFormatSettings;
  App_DataDirectory: String = '';

implementation

uses
 {$IFDEF MSWINDOWS}
  windows, wininet, URIParser,
 {$ELSE}
  {$IF FPC_FullVersion >= 30200}opensslsockets,{$IFEND}
  fphttpclient,
 {$ENDIF}
  math;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(GetAppConfigFile(false), '.ini');;
  Result := TIniFile.Create(fn);
end;

{$IFDEF MSWINDOWS}

{ This function translates a WinInet Error Code to a description of the error.
  From: https://theroadtodelphi.com/category/wininet/ }
function GetWinInetError(ErrorCode: Cardinal): string;
const
  winetdll = 'wininet.dll';
var
  len: Integer;
  buffer: PChar;
begin
  len := FormatMessage(
    FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    Pointer(GetModuleHandle(winetdll)), ErrorCode, 0, @buffer, SizeOf(Buffer), nil
  );
  try
    while (len > 0) and (Buffer[len - 1] in [#0..#32, '.']) do
      dec(len);
    SetString(Result, buffer, len);
  finally
    LocalFree(HLOCAL(buffer));
  end;
end;

function Download(const URL, API_Key: string; const AStream: TStream; var AErrMsg: String): Boolean;
const
  KB = 1024;
var
  netHandle: HInternet;
  urlHandle: HInternet;
  buffer: array[0..4*KB-1] of Char;
  bytesRead: dWord = 0;
  errCode: Integer = 0;
  header: String;
begin
  Result := false;
  AErrMsg := '';
  NetHandle := InternetOpen('Mozilla/5.0(compatible; WinInet)', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  // NetHandle valid?
  if netHandle = nil then
  begin
    errCode := GetLastError;
    AErrMsg := GetWinInetError(errCode);
    exit;
  end;

  try
    header := 'x-key: ' + API_Key;
    urlHandle := InternetOpenUrl(netHandle, PChar(URL), PChar(header), Length(header), INTERNET_FLAG_RELOAD, 0);

    // UrlHandle valid?
    if urlHandle = nil then
    begin
      errCode := GetLastError;
      AErrMsg := GetWinInetError(errCode);
      exit;
    end;

    try
      repeat
        InternetReadFile(urlHandle, @buffer, SizeOf(buffer), bytesRead);
        if bytesRead > 0 then
          AStream.Write(buffer, bytesRead);
      until bytesRead = 0;
      AStream.Position := 0;
      Result := true;
    finally
      InternetCloseHandle(urlHandle);
    end
  finally
    InternetCloseHandle(netHandle);
  end;
end;

{$ELSE}

function Download(const URL, API_Key: String; const AStream: TStream; var AErrMsg: String): Boolean;
begin
  AErrMsg := '';

  // Get file from the internet
  with TFPHttpClient.Create(nil) do
    try
      try
        AllowRedirect := true;
        AddHeader('x-key', API_Key);
        Get(URL, AStream);
        AStream.Position := 0;
        Result := true;
      except
        on E:EHTTPClient do begin
          AErrMsg := E.Message;
          Result := false;
        end;
      end;
    finally
      Free;
    end;
end;
{$ENDIF}

function NumberToStr(AValue: Double; AFormat: String = ''): String;
begin
  if not IsNaN(AValue) then
  begin
    if AFormat = '' then
      Result := SysUtils.FloatToStr(AValue, App_FormatSettings)
    else
      Result := SysUtils.FormatFloat(AFormat, AValue, App_FormatSettings)
  end
  else
    Result := '';
end;

function StrToNumber(Str: String): Double;
begin
  if not TryStrToFloat(Str, Result, App_FormatSettings) then
    Result := NaN;
end;


initialization
  App_FormatSettings := DefaultFormatSettings;
  App_FormatSettings.DecimalSeparator := '.';
  App_FormatSettings.DateSeparator := '-';
  App_FormatSettings.ShortDateFormat := 'yyyy/mm/dd';

end.

