unit mainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtDlgs, ExtCtrls;

type
  TForm1 = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses GraphicEx;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
//var graphic: TGraphic;
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    SaveGraphicToFile(Image1.Picture.Graphic,SavePictureDialog1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SavePictureDialog1.Filter:=FileFormatList.GetGraphicFilter([ftEnableSaving],fstNone,[foIncludeExtension],nil);
end;

end.
