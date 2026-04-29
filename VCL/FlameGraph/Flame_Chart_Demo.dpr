program Flame_Chart_Demo;

uses
  Vcl.Forms,
  Unit_Main in 'Unit_Main.pas' {FormFlame},
  TeeFlameSeries in 'TeeFlameSeries.pas',
  Unit_Pro in 'Unit_Pro.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFlame, FormFlame);
  Application.Run;
end.
