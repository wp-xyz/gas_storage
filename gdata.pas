unit gData;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl;

type
  TGasDataItem = record
    Injection: Double;
    Withdrawal: Double;
    PercentFull: Double;
    procedure Init;
  end;

  TGasDataList = specialize TFPGMap<TDate, TGasDataItem>;

  TGasData = class
  private
    FList: TGasDataList;
    FCountry: String;
    FCountryCode: String;
    FDataDir: String;
    FFileName: String;
    FModified: Boolean;
    function GetCount: Integer;
    function GetDate(AIndex: Integer): TDate;
    function GetInjection(AIndex: Integer): Double;
    function GetPercentFull(AIndex: Integer): Double;
    function GetWithdrawal(AIndex: Integer): Double;
  protected
  public
    constructor Create(ADataDir, ACountryCode: String);
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure LoadFromJSON(const AStream: TStream; out ErrMsg: String);
    procedure Save;

    property Count: Integer read GetCount;
    property Country: String read FCountry;
    property Date[AIndex: Integer]: TDate read GetDate;
    property FileName: String read FFileName;
    property Injection[AIndex: Integer]: Double read GetInjection;
    property PercentFull[AIndex: Integer]: Double read GetPercentFull;
    property Withdrawal[AIndex: Integer]: Double read GetWithdrawal;
  end;


implementation

uses
  math, fpjson, LazFileUtils,
  gUtils;

const
  BASE_FILENAME = 'gas-%s.csv';
  FIELD_SEPARATOR = #9;

{ TGasDataItem }

procedure TGasDataItem.Init;
begin
  PercentFull := NaN;
  Injection := NaN;
  Withdrawal := NaN;
end;

{ TGasData }

constructor TGasData.Create(ADataDir, ACountryCode: String);
begin
  inherited Create;
  FList := TGasDataList.Create;
  FList.Sorted := true;
  FCountryCode := ACountryCode;
  FDataDir := ADataDir;
  if ACountryCode[Length(ACountryCode)] = '*' then
    ACountryCode[Length(ACountryCode)] := '!';
  FFileName := AppendPathDelim(FDataDir) + Format(BASE_FILENAME, [ACountryCode]);
  Load;
end;

destructor TGasData.Destroy;
begin
  if FModified then Save;
  FList.Free;
  inherited;
end;

procedure TGasData.Clear;
begin
  FList.Clear;
end;

function TGasData.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGasData.GetDate(AIndex: Integer): TDate;
begin
  Result := FList.Keys[AIndex];
end;

function TGasData.GetInjection(AIndex: Integer): Double;
begin
  Result := FList.Data[AIndex].Injection;
end;

function TGasData.GetPercentFull(AIndex: Integer): Double;
begin
  Result := FList.Data[AIndex].PercentFull;
end;

function TGasData.GetWithdrawal(AIndex: Integer): Double;
begin
  Result := FList.Data[AIndex].Withdrawal;
end;

procedure TGasData.Load;
var
  F: TextFile;
  s: String;
  sa: TStringArray;
  lDate: TDate;
  item: TGasDataItem;
begin
  Clear;
  if not FileExists(FFileName) then
    exit;

  AssignFile(F, FFileName);
  Reset(F);

  ReadLn(F, s);           // Signature
  if s <> '# gas storage file' then
    exit;
  ReadLn(F, FCountry);    // Country name from json file
  Delete(FCountry, 1, 2);
  ReadLn(F, s);           // Column headers

  while not EoF(F) do
  begin
    ReadLn(F, s);
    if s = '' then
      Continue;
    sa := s.Split(FIELD_SEPARATOR);
    if not TryStrToDate(sa[0], lDate, App_FormatSettings) then
      Continue;
    item.Init;
    if (Length(sa) > 1) then item.PercentFull := StrToNumber(sa[1]);
    if (Length(sa) > 2) then item.Injection := StrToNumber(sa[2]);
    if (Length(sa) > 3) then item.Withdrawal := StrToNumber(sa[3]);
    FList.Add(lDate, item);
  end;

  CloseFile(F);
  FModified := false;
end;

procedure TGasData.LoadFromJSON(const AStream: TStream; out ErrMsg: String);
var
  json: TJSONData;
  jData: TJSONData;
  jDataArray: TJSONArray;
  jDataObj: TJSONObject;
  jDate: TJSONData;
  jPercentFull: TJSONData;
  jInjection: TJSONData;
  jWithdrawal: TJSONData;
  i, idx: Integer;
  lDate: TDate;
  item: TGasDataItem;
  lCountry: String = '';
  stream: TFileStream;
begin
  ErrMsg := '';

  json := GetJSON(AStream);
  try
    jData := json.FindPath('error');
    if jData <> nil then begin
      ErrMsg := jData.AsString;
      jData := json.FindPath('message');
      ErrMsg := ErrMsg + LineEnding + jData.AsString;
      stream := TFileStream.Create('error.json', fmCreate);
      try
        AStream.Position := 0;
        stream.CopyFrom(AStream, AStream.Size);
      finally
        stream.Free;
      end;
      exit;
    end;

    jData := json.FindPath('data');
    if (jData is TJSONArray) then
    begin
      jDataArray := TJSONArray(jData);
      for i := 0 to jDataArray.Count-1 do
        if jDataArray.Types[i] = jtObject then
        begin
          jDataObj := TJSONObject(jDataArray[i]);
          if lCountry = '' then
          begin
            lCountry := jDataObj.Find('name').AsString;
            FCountry := lCountry;
          end;
          jDate := jDataObj.Find('gasDayStart');
          jPercentFull := jDataObj.Find('full');
          jInjection := jDataObj.Find('injection');
          jWithdrawal := jDataObj.Find('withdrawal');
          if (jDate = nil) or not TryStrToDate(jDate.AsString, lDate, App_FormatSettings) then
            Continue;
          item.Init;
          if (jPercentFull <> nil) then
            item.PercentFull := StrToNumber(jPercentFull.AsString);
          if (jInjection <> nil) then
            item.Injection := StrToNumber(jInjection.AsString);
          if (jWithdrawal <> nil) then
            item.Withdrawal := StrToNumber(jWithdrawal.AsString);
          if FList.Find(lDate, idx) then
            FList.Data[idx] := item
          else
            FList.Add(lDate, item);
        end;
      FModified := true;
    end;
  finally
    json.Free;
  end;
end;

procedure TGasData.Save;
var
  i: Integer;
  F: TextFile;
  sDate, sPercentFull, sInjection, sWithdrawal: String;
  item: TGasDataItem;
begin
  AssignFile(F, FFileName);
  Rewrite(F);
  WriteLn(F, '# gas storage file');
  WriteLn(F, '# ' + FCountry);
  WriteLn(F, '# date', FIELD_SEPARATOR, 'percent full', FIELD_SEPARATOR, 'injection', FIELD_SEPARATOR, 'withdrawal');
  for i := 0 to Count-1 do
  begin
    sDate := DateToStr(Date[i], App_FormatSettings);
    item := FList.Data[i];
    sPercentFull := NumberToStr(item.PercentFull, '0.00');
    sInjection := NumberToStr(item.Injection, '0.00');
    sWithdrawal := NumberToStr(item.Withdrawal, '0.00');
    if (sPercentFull <> '') or (sInjection <> '') or (sWithdrawal <> '') then
      WriteLn(F, sDate, FIELD_SEPARATOR, sPercentFull, FIELD_SEPARATOR, sInjection, FIELD_SEPARATOR, sWithdrawal);
  end;
  CloseFile(F);

  FModified := false;
end;


end.

