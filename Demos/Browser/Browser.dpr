program Browser;

uses
  Forms,
  GraphicColor in '..\..\GraphicColor.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  GraphicEx in '..\..\GraphicEx.pas',
  GraphicStrings in '..\..\GraphicStrings.pas',
  JpegCompression in '..\..\JpegCompression.pas',
  Main in 'Main.pas' {MainForm},
  MZLib in '..\..\MZLib.pas',
  TIFF in '..\..\TIFF.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

