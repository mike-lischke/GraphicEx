unit Properties;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GraphicEx, ComCtrls;

type
  TPropertyDialog = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    TiledCheckBox: TCheckBox;
    EndianCheckBox: TCheckBox;
    MinCheckBox: TCheckBox;
    ReversedCheckBox: TCheckBox;
    GroupBox2: TGroupBox;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    WidthUpDown: TUpDown;
    HeightUpDown: TUpDown;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    XResUpDown: TUpDown;
    YResUpDown: TUpDown;
    GroupBox4: TGroupBox;
    CompressionComboBox: TComboBox;
    GroupBox5: TGroupBox;
    Label3: TLabel;
    ColorComboBox: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    BPSLabel: TLabel;
    SPPLabel: TLabel;
    BPPLabel: TLabel;
    DescriptionLabel: TLabel;
    InterlacedCheckBox: TCheckBox;
    GroupBox6: TGroupBox;
    FileGammaEdit: TEdit;
    FileGammaLabel: TLabel;
    Label8: TLabel;
    DisplayGammaLabel: TLabel;
    GammaCheckBox: TCheckBox;
    procedure FormActivate(Sender: TObject);
  private
  public
    Graphic: TGraphic;
  end;

var
  PropertyDialog: TPropertyDialog;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  GraphicColor;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyDialog.FormActivate(Sender: TObject);

var
  ImageProperties: TImageProperties;
  
begin
  CancelButton.SetFocus;
  ZeroMemory(@ImageProperties, SizeOf(ImageProperties));
  with ImageProperties do
  begin
    if Graphic is TGraphicExGraphic then
    begin
      Caption := FileFormatList.GetDescription(TGraphicClass(Graphic.ClassType));
      ImageProperties := TGraphicExGraphic(Graphic).ImageProperties;
    end
    else
    begin
      Caption := 'Image properties';
      // Delphi built-in classes, try to get out at least some information
      Width := Graphic.Width;
      Height := Graphic.Height;
      if Graphic is TBitmap then
      begin
        case TBitmap(Graphic).PixelFormat of
          pf1Bit:
           begin
             BitsPerSample := 1;
             SamplesPerPixel := 1;
             ColorScheme := csIndexed;
           end;
          pf4Bit:
           begin
             BitsPerSample := 4;
             SamplesPerPixel := 1;
             ColorScheme := csIndexed;
           end;
          pf8Bit:
           begin
             BitsPerSample := 8;
             SamplesPerPixel := 1;
             ColorScheme := csIndexed;
           end;
          pf15Bit, pf16Bit:
           begin
             BitsPerSample := 5;
             SamplesPerPixel := 3;
             ColorScheme := csBGR;
           end;
          pf24Bit:
           begin
             BitsPerSample := 8;
             SamplesPerPixel := 3;
             ColorScheme := csBGR;
           end;
          pf32Bit:
           begin
             BitsPerSample := 8;
             SamplesPerPixel := 4;
             ColorScheme := csBGRA;
           end;
          pfDevice:
           begin
             BitsPerPixel := GetDeviceCaps(Canvas.Handle, BITSPIXEL);
             SamplesPerPixel := 8;
             BitsPerSample := BitsPerPixel div SamplesPerPixel;
             ColorScheme := csBGRA;
           end;
        end;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
      end;
    end;

    // options
    TiledCheckBox.Checked := ioTiled in Options;
    EndianCheckBox.Checked := ioBigEndian in Options;
    MinCheckBox.Checked := ioMinIsWhite in Options;
    ReversedCheckBox.Checked := ioReversed in Options;
    InterlacedCheckBox.Checked := Interlaced;
    GammaCheckBox.Checked := ioUseGamma in Options;

    // colors
    if ColorScheme = csUnknown then ColorComboBox.ItemIndex := -1
                               else ColorComboBox.ItemIndex := Ord(ColorScheme) - 1;
    BPSLabel.Caption := IntToStr(BitsPerSample);
    SPPLabel.Caption := IntToStr(SamplesPerPixel);
    BPPLabel.Caption := IntToStr(BitsPerPixel);
    case ColorScheme of
      csG, csGA:
        DescriptionLabel.Caption := Format('%d gray levels', [1 shl BitsPerPixel]);
      csRGB, csRGBA, csBGR, csBGRA, csCMY, csCMYK, csCIELab, csYCbCr, csPhotoYCC:
        DescriptionLabel.Caption := Format('%d bit true color', [BitsPerPixel]);
    else
      // indexed colors
      if BitsPerPixel = 1 then DescriptionLabel.Caption := 'black and white'
                          else DescriptionLabel.Caption := Format('%d palette colors', [1 shl BitsPerPixel]);
    end;

    // compression
    if Compression = ctUnknown then CompressionComboBox.ItemIndex := -1
                               else CompressionComboBox.ItemIndex := Ord(Compression) - 1;

    // size
    WidthUpDown.Position := Width;
    HeightUpDown.Position := Height;

    // resolution
    XResUpDown.Position := Round(XResolution);
    YResUpDown.Position := Round(YResolution);

    // gamma
    DisplayGammaLabel.Caption := FloatToStr(DefaultDisplayGamma);
    FileGammaEdit.Text := FloatToStrF(FileGamma, ffFixed, 5, 2);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

