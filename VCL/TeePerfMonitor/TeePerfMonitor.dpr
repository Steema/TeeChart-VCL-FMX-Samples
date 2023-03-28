program TeePerfMonitor;
{$I TeeDefs.inc}

uses
  Forms,
  FrmMain in 'FrmMain.pas' {MainForm},
  TeePerfCounter in 'TeePerfCounter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TeeChart Performance Monitor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
