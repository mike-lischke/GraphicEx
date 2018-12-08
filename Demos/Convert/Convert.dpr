program Convert;

uses
  Forms,
  GraphicEx in '..\..\GraphicEx.pas',
  GraphicColor in '..\..\GraphicColor.pas',
  GraphicStrings in '..\..\GraphicStrings.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  JpegCompression in '..\..\JpegCompression.pas',
  Main in 'Main.pas' {MainForm},
  MZLib in '..\..\MZLib.pas',
  Properties in 'Properties.pas' {PropertyDialog},
  TIFF in '..\..\TIFF.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertyDialog, PropertyDialog);
  Application.Run;
end.

