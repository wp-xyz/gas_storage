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

implementation

uses
  {$IF FPC_FullVersion >= 30200}
  opensslsockets,
  {$IFEND}
  fphttpclient,
  math;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(GetAppConfigFile(false), '.ini');;
  Result := TIniFile.Create(fn);
end;

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

