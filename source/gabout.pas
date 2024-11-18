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
    procedure LinkClick(Sender: TObject);
    procedure LinkMouseEnter(Sender: TObject);
    procedure LinkMouseLeave(Sender: TObject);
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

const
  DATASOURCE_LINK = 'https://agsi.gie.eu';
  ROLAND_HAHN_LINK = 'https://www.rhsoft.de';

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
  infoDataSource.Hint := DATASOURCE_LINK;
  infoImageSource.Hint := ROLAND_HAHN_LINK;

  AppImage.Picture.Icon := Application.Icon;
  AppImage.Stretch := true;
  AppImage.Picture.Icon.Current := AppImage.Picture.Icon.GetBestIndexForSize(Size(256, 256));
end;

procedure TAboutForm.LinkClick(Sender: TObject);
var
  url: String;
begin
  url := TLabel(Sender).Hint;
  OpenURL(url);
end;

procedure TAboutForm.LinkMouseEnter(Sender: TObject);
begin
  TControl(Sender).Font.Style := [fsUnderline];
end;

procedure TAboutForm.LinkMouseLeave(Sender: TObject);
begin
  TControl(Sender).Font.Style := [];
end;

end.

