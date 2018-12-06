program SaveAsDemo;

{%File '..\..\3rd party\DelphiZlib\readme.txt'}

uses
  Forms,
  mainform in 'mainform.pas' {Form1},
  GraphicEx in '..\..\GraphicEx.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  ZLibEx in '..\..\3rd party\DelphiZlib\ZLibEx.pas',
  ZLibExApi in '..\..\3rd party\DelphiZlib\ZLibExApi.pas',
  GraphicStrings in '..\..\GraphicStrings.pas',
  GraphicColor in '..\..\GraphicColor.pas',
  JpegCompression in '..\..\JpegCompression.pas',
  MZLib in '..\..\MZLib.pas',
  TIFF in '..\..\TIFF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
