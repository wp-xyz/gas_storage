unit gAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, EditBtn;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    btnClose: TBitBtn;
    AppImage: TImage;
    edDataDirectory: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    infoDataSource: TLabel;
    lblDataDirectory: TLabel;
    lblImageSource: TLabel;
    lblDataSource: TLabel;
    infoImageSource: TLabel;
    procedure FormShow(Sender: TObject);
    procedure infoDataSourceClick(Sender: TObject);
    procedure infoDataSourceMouseEnter(Sender: TObject);
    procedure infoDataSourceMouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

procedure ShowAbout;

implementation

{$R *.lfm}

uses
  Types, LCLIntf,
  gUtils;

procedure ShowAbout;
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.edDataDirectory.Directory := App_DataDirectory;
    if F.ShowModal = mrOK then
    begin
      App_DataDirectory := F.edDataDirectory.Directory;
      ForceDirectories(App_DataDirectory);
    end;
  finally
    F.Free;
  end;
end;


{ TAboutForm }

procedure TAboutForm.FormShow(Sender: TObject);
begin
  AppImage.Picture.Icon := Application.Icon;
  AppImage.Stretch := true;
  AppImage.Picture.Icon.Current := AppImage.Picture.Icon.GetBestIndexForSize(Size(256, 256));
end;

procedure TAboutForm.infoDataSourceClick(Sender: TObject);
begin
  OpenURL('https://agsi.gie.eu/');
end;

procedure TAboutForm.infoDataSourceMouseEnter(Sender: TObject);
begin
  infoDataSource.Font.Style := [fsUnderline];
end;

procedure TAboutForm.infoDataSourceMouseLeave(Sender: TObject);
begin
  infoDataSource.Font.Style := [];
end;

end.

