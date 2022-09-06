unit gMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, TAGraph, TAIntervalSources, TASources, TASeries, gData,
  TACustomSource, TAChartExtentLink;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnDownload: TButton;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart2: TChart;
    InjectionSeries: TLineSeries;
    WithdrawalSeries: TLineSeries;
    ChartExtentLink: TChartExtentLink;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    lbCountries: TListBox;
    pnlData: TPanel;
    pnlCountries: TPanel;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    FillPercentSource: TUserDefinedChartSource;
    InjectionSource: TUserDefinedChartSource;
    WithdrawalSource: TUserDefinedChartSource;
    procedure btnDownloadClick(Sender: TObject);
    procedure DateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FillPercentSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure InjectionSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure lbCountriesClick(Sender: TObject);
    procedure WithdrawalSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    FData: TGasData;
    FApiKey: String;
    FCountryCode: String;
    FDataDir: String;
    function GetCountryCode(AIndex: Integer): String;
    function GetCountryName(AIndex: Integer): String;
    procedure SetCountryCode(ACountryCode: String);
    procedure ReadIni;
    procedure WriteIni;
    procedure StoreAPIKey;
    procedure UpdateSeries;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, StrUtils, IniFiles, LazFileUtils,
  gUtils;

const
  BASE_URL = 'https://agsi.gie.eu/api';
  SIZE = 300;

{ TMainForm }

{ Download from the GIE AGSI site requires an API key which can be obtained
  for free after registration at https://agsi.gie.eu/account.
  Enter the API key here when requested, it is stored in the ini file (gas.ini)
  in section "Settings" under key "ApiKey". }
procedure TMainForm.btnDownloadClick(Sender: TObject);
var
  stream: TStream;
  s, url: String;
  err: String = '';
  startDate, endDate: TDate;
  fromDateStr: String;
  toDateStr: String;
  n: Integer;
  store_API_Key: Boolean = false;
begin
  if FApiKey = '' then begin
    FApiKey := InputBox('Please enter your API key', 'API-Key', FApiKey);
    if FApiKey = '' then
    begin
      MessageDlg('Api-Key required for download of gas storage data.' + LineEnding +
        'Please register at https://agsi.gie.eu/account.', mtError, [mbOK], 0);
      exit;
    end;
    store_API_Key := true;
  end;

  FCountryCode := GetCountryCode(lbCountries.ItemIndex);
  FreeAndNil(FData);
  FData := TGasData.Create(FDataDir, FCountryCode);
  if FData.Count > 0 then
    startDate := FData.Date[FData.Count-1] - 1
  else
  if FCountryCode = 'gb*' then  // UK, post-brexit
    startDate := EncodeDate(2021, 1, 1)
  else
    startDate := EncodeDate(2011, 1, 1);

  while (startDate <= Now()) do
  begin
    endDate := startDate + SIZE;
    if endDate > Trunc(Now) then
      endDate := Trunc(Now);
    fromDateStr := DateToStr(startDate, App_FormatSettings);
    toDateStr := DateToStr(endDate, App_FormatSettings);
    n := trunc(endDate) - trunc(startDate) + 1;
    url := Format('%s?country=%s&from=%s&to=%s&size=%d', [
      BASE_URL, FCountryCode, fromDateStr, toDateStr, n
    ]);

    s := GetCountryName(lbCountries.ItemIndex);
    StatusBar.Panels[1].Text := Format('%s: Downloading %s - %s...', [s, fromDateStr, toDateStr]);
    StatusBar.Repaint;

    stream := TMemoryStream.Create;
    try
      if Download(url, FApiKey, stream, err) then
      begin
        stream.Position := 0;
        TMemoryStream(stream).SaveToFile('test.json');
        FData.LoadFromJSON(stream, err);
        if err <> '' then
        begin
          FData.Clear;
          UpdateSeries;
          StatusBar.Panels[1].Text := '';
          MessageDlg(err, mtError, [mbOK], 0);
          exit;
        end;
      end else begin
        StatusBar.Panels[1].Text := '';
        MessageDlg('Download error:' + LineEnding + err, mtError, [mbOK], 0);
        exit;
      end;
    finally
      stream.Free;
    end;

    startDate := startDate + SIZE;
  end;

  if err = '' then
  begin
    FData.Save;
    UpdateSeries;
    if store_API_Key then StoreAPIKey;
    StatusBar.Panels[1].Text := 'Stored as "' + FData.FileName + '"';
  end else
    StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.DateTimeIntervalChartSourceDateTimeStepChange(
  Sender: TObject; ASteps: TDateTimeStep);
begin
  exit;

  if ASteps = dtsQuarter then
    DateTimeIntervalChartSource.Params.MaxLength := 100
  else
  if ASteps >= dtsMonth then
    DateTimeIntervalChartSource.Params.MaxLength := 150
  else
    DateTimeIntervalChartSource.Params.MaxLength := 80;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cInputQueryEditSizePercents := 0;
  StatusBar.Panels[0].Text := 'Data source: GIE AGSI (https://agsi.gie.eu/)';
  //FDataDir := GetAppConfigDir(true);
  FDataDir := Application.Location + AppendPathDelim('data');
  ForceDirectories(FDataDir);

  ReadIni;
  lbCountriesClick(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FData);
end;

function TMainForm.GetCountryCode(AIndex: Integer): String;
var
  s: String;
  p: Integer;
begin
  s := lbCountries.Items[AIndex];
  p := rpos('-', s);
  Result := Copy(s, p+2);
end;

function TMainForm.GetCountryName(AIndex: Integer): String;
var
  s: String;
  p: Integer;
begin
  s := lbCountries.Items[AIndex];
  p := rpos('-', s);
  Result := Copy(s, 1, p-2);
end;

procedure TMainForm.lbCountriesClick(Sender: TObject);
begin
  Statusbar.Panels[1].Text := '';
  if lbCountries.ItemIndex > -1 then
  begin
    FreeAndNil(FData);
    FCountryCode := GetCountryCode(lbCountries.ItemIndex);
    FData := TGasData.Create(FDataDir, FCountryCode);
    UpdateSeries;
    Statusbar.Panels[1].Text := Format('File "%s" loaded, %d data points', [FData.FileName, FData.Count]);
  end;
end;

procedure TMainForm.SetCountryCode(ACountryCode: String);
var
  i: Integer;
  code: String;
begin
  for i := 0 to lbCountries.Items.Count-1 do
  begin
    code := GetCountryCode(i);
    if code = ACountryCode then
    begin
      FCountryCode := ACountryCode;
      lbCountries.ItemIndex := i;
      exit;
    end;
  end;
end;

procedure TMainForm.FillPercentSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  if (FData <> nil) and (AIndex < FData.Count) then
  begin
    AItem.X := FData.Date[AIndex];
    AItem.Y := FData.PercentFull[AIndex];
  end;
end;

procedure TMainForm.InjectionSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  if (FData <> nil) and (AIndex < FData.Count) then
  begin
    AItem.X := FData.Date[AIndex];
    AItem.Y := FData.Injection[AIndex];
  end;
end;

procedure TMainForm.WithdrawalSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  if (FData <> nil) and (AIndex < FData.Count) then
  begin
    AItem.X := FData.Date[AIndex];
    AItem.Y := FData.Withdrawal[AIndex];
  end;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
begin
  ini := CreateIni;
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(ini.ReadInteger('MainForm', 'WindowState', 0));
    pnlCountries.Width := ini.ReadInteger('MainForm', 'Countries_Width', pnlCountries.Width);
    FApiKey := ini.ReadString('Settings', 'API_Key', FApiKey);
    SetCountryCode(ini.ReadString('Settings', 'Country', FCountryCode));
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
begin
  ini := CreateIni;
  try
    if WindowState = wsMaximized then
    begin
      L := RestoredLeft;
      T := RestoredTop;
      W := RestoredWidth;
      H := RestoredHeight;
    end else
    begin
      L := Left;
      T := Top;
      W := Width;
      H := Height;
    end;
    ini.WriteInteger('MainForm', 'Top', T);
    ini.WriteInteger('MainForm', 'Left', L);
    ini.WriteInteger('MainForm', 'Width', W);
    ini.WriteInteger('MainForm', 'Height', H);
    ini.WriteInteger('MainForm', 'WindowState', Integer(WindowState));
    ini.WriteInteger('MainForm', 'Countries_Width', pnlCountries.Width);
    ini.WriteString('Settings', 'Country', FCountryCode);
    ini.WriteString('Settings', 'API_Key', FApiKey);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.StoreAPIKey;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    ini.WriteString('Settings', 'API_Key', FApiKey);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.UpdateSeries;
begin
  FillPercentSource.Reset;
  FillPercentSource.PointsNumber := FData.Count;
  InjectionSource.Reset;
  InjectionSource.PointsNumber := FData.Count;
  WithdrawalSource.Reset;
  WithdrawalSource.PointsNumber := FData.Count;

  Chart1.Title.Text.Text := FData.Country;
  Chart1.Title.Visible := true;

  Chart1.Invalidate;
  Chart2.Invalidate;
end;

end.
