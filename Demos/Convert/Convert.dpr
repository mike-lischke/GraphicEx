program Convert;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Properties in 'Properties.pas' {PropertyDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertyDialog, PropertyDialog);
  Application.Run;
end.

