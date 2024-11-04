unit gMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Types, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons,
  TAGraph, TAIntervalSources, TASources, TASeries, TACustomSource,
  TAChartExtentLink, TATools, gData;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnDownload: TBitBtn;
    Chart1: TChart;
    FillLevelSeries: TAreaSeries;
    Chart2: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1DataPointCrosshairTool2: TDataPointCrosshairTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1PanDragTool2: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomDragTool2: TZoomDragTool;
    ChartToolset2: TChartToolset;
    ImageList: TImageList;
    InjectionSeries: TLineSeries;
    btnAbout: TSpeedButton;
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
    procedure btnAboutClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure ChartToolset1DataPointCrosshairTool1Draw(
      ASender: TDataPointDrawTool);
    procedure DateTimeIntervalChartSourceDateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure lbCountriesClick(Sender: TObject);
  private
    FData: TGasData;
    FApiKey: String;
    FCountryCode: String;
    FActivated: boolean;
    procedure DownloadCountry(ACountryIndex: Integer; var ErrorMsg: String);
    function GetCountryCode(AIndex: Integer): String;
    function GetCountryName(AIndex: Integer): String;
    procedure SetCountryCode(ACountryCode: String);
    procedure StoreAPIKey;
    procedure ReadIni;
    procedure WriteIni;
    procedure UpdateSeries;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, StrUtils, IniFiles, LazFileUtils,
  TACustomSeries,
  gUtils, gAbout;

const
  BASE_URL = 'https://agsi.gie.eu/api';
  SIZE = 300;

{ TMainForm }

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  ShowAbout;
end;

{ Download from the GIE AGSI site requires an API key which can be obtained
  for free after registration at https://agsi.gie.eu/account.
  Enter the API key here when requested, it is stored in the ini file (gas.ini)
  in section "Settings" under key "ApiKey". }
procedure TMainForm.btnDownloadClick(Sender: TObject);
var
  err: String;
  i: Integer;
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

  err := '';
  for i := 0 to lbCountries.Items.Count-1 do
    DownloadCountry(i, err);

  if err = '' then
  begin
    FreeAndNil(FData);
    FCountryCode := GetCountryCode(lbCountries.ItemIndex);
    FData := TGasData.Create(App_DataDirectory, FCountryCode);
    if store_API_Key then StoreAPIKey;
    StatusBar.Panels[1].Text := 'Download complete.';
  end else
  begin
    StatusBar.Panels[1].Text := '';
    MessageDlg(err, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.DownloadCountry(ACountryIndex: Integer; var ErrorMsg: String);
var
  stream: TStream;
  s, url: String;
  err: String = '';
  startDate, endDate: TDate;
  fromDateStr: String;
  toDateStr: String;
  n: Integer;

  countryCode: String;
  countryName: String;
  data: TGasData;
begin
  countryCode := GetCountryCode(ACountryIndex);
  countryName := GetCountryName(ACountryIndex);
  data := TGasData.Create(App_DataDirectory, countryCode);
  try
    if data.Count > 0 then
      startDate := data.Date[data.Count-1] - 1
    else
    if countryCode = 'gb*' then  // UK, post-brexit
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
        BASE_URL, countryCode, fromDateStr, toDateStr, n
      ]);

      StatusBar.Panels[1].Text := Format('Downloading %s: %s - %s...', [countryName, fromDateStr, toDateStr]);
      StatusBar.Repaint;

      stream := TMemoryStream.Create;
      try
        if Download(url, FApiKey, stream, err) then
          data.LoadFromJSON(stream, err);
        if err <> '' then
        begin
          StatusBar.Panels[1].Text := '';
          if ErrorMsg = '' then
            ErrorMsg := err
          else
            ErrorMsg := ErrorMsg + LineEnding + err;
          exit;
        end;
      finally
        stream.Free;
      end;
      startDate := startDate + SIZE;
    end;

    data.Save;
  finally
    data.Free;
  end;
end;

procedure TMainForm.ChartToolset1DataPointCrosshairTool1Draw(
  ASender: TDataPointDrawTool);
var
  sDate: String;
  sFillLevelValue: String;
  sInjectionValue: String;
  sWithdrawalValue: String;
  idx: Integer;
begin
  if (ASender = nil) or (ASender.Series = nil) then
    exit;
  idx := ASender.PointIndex;
  if idx = -1 then
    exit;
  sDate := DateToStr(FillLevelSeries.XValue[ASender.PointIndex]);
  sFillLevelValue := FormatFloat('0.00', FillLevelSeries.YValue[idx]) + ' %';
  sInjectionValue := FormatFloat('0.00', InjectionSeries.YValue[idx]) + ' GWh/day';
  sWithdrawalValue := FormatFloat('0.00', WithdrawalSeries.YValue[idx]) + ' GWh/day';
  Statusbar.Panels[1].Text := Format('Date: %s - %s = %s, %s = %s, %s = %s', [
    sDate,
    'Fill level', sFillLevelValue,
    'Injection', sInjectionValue,
    'Withdrawal', sWithdrawalValue
  ]);
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

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    ReadIni;
    lbCountriesClick(nil);
    btnAbout.Width := btnAbout.Height;
    pnlCountries.Constraints.MinWidth := btnAbout.Left + btnAbout.Width;
  end;
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

  App_DataDirectory := Application.Location + AppendPathDelim('data');
  ForceDirectories(App_DataDirectory);
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
    FData := TGasData.Create(App_DataDirectory, FCountryCode);
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

procedure TMainForm.ChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  if (FData <> nil) and (AIndex < FData.Count) then
  begin
    AItem.X := FData.Date[AIndex];
    if ASource = FillPercentSource then
      AItem.Y := FData.PercentFull[AIndex]
    else if ASource = InjectionSource then
      AItem.Y := FData.Injection[AIndex]
    else if ASource = WithdrawalSource then
      AItem.Y := FData.Withdrawal[AIndex]
    else
      raise Exception.Create('Unknown chart source.');
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
    App_DataDirectory := ini.ReadString('Settings', 'DataDirectory', App_DataDirectory);

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
    ini.WriteString('Settings', 'DataDirectory', App_DataDirectory);
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

