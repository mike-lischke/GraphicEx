program OneImage;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  proj_common in '..\proj_common.pas',
  GraphicColor in '..\..\GraphicColor.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  GraphicEx in '..\..\GraphicEx.pas',
  GraphicStrings in '..\..\GraphicStrings.pas',
  JpegCompression in '..\..\JpegCompression.pas',
  MZLib in '..\..\MZLib.pas',
  TIFF in '..\..\TIFF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
