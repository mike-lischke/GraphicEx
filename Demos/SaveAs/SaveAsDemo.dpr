program SaveAsDemo;

{%File '..\..\3rd party\DelphiZlib\readme.txt'}

uses
  Forms,
  mainform in 'mainform.pas' {Form1},
  GraphicEx in '..\..\GraphicEx.pas',
  JPG in '..\..\JPG.pas',
  LibStub in '..\..\Common\LibStub.pas',
  Scanf in '..\..\Common\Scanf.pas',
  Scanf_c in '..\..\Common\Scanf_c.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  ZLibEx in '..\..\3rd party\DelphiZlib\ZLibEx.pas',
  ZLibExApi in '..\..\3rd party\DelphiZlib\ZLibExApi.pas',
  GraphicStrings in '..\..\GraphicStrings.pas',
  GraphicColor in '..\..\GraphicColor.pas',
  MZLib in '..\..\MZLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
