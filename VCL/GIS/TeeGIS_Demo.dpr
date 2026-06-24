program TeeGIS_Demo;

uses
  Vcl.Forms,
  Unit_GIS in 'Unit_GIS.pas' {MainForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
