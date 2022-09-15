unit gAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btnClose: TBitBtn;
    AppImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    infoDataSource: TLabel;
    lblImageSource: TLabel;
    lblDataSource: TLabel;
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
  Types, LCLIntf;

procedure ShowAbout;
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.ShowModal;
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

