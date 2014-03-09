program OneImage;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  proj_common in '..\proj_common.pas',
  GraphicColor in '..\..\GraphicColor.pas',
  GraphicCompression in '..\..\GraphicCompression.pas',
  GraphicEx in '..\..\GraphicEx.pas',
  JPG in '..\..\JPG.pas',
  MZLib in '..\..\MZLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
