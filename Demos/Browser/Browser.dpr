program Browser;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  proj_common in '..\proj_common.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

