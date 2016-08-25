program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GraphicColor in '..\GraphicColor.pas',
  GraphicCompression in '..\GraphicCompression.pas',
  GraphicEx in '..\GraphicEx.pas',
  GraphicStrings in '..\GraphicStrings.pas',
  MZLib in '..\MZLib.pas',
  ZLibExApi in '..\3rd party\DelphiZlib\ZLibExApi.pas',
  ZLibEx in '..\3rd party\DelphiZlib\ZLibEx.pas',
  TIFF in '..\TIFF.pas',
  LibStub in '..\Common\LibStub.pas',
  JPG in '..\JPG.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
