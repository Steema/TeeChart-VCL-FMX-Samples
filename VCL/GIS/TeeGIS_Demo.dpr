program TeeGIS_Demo;

uses
  Vcl.Forms,
  Unit_GIS in 'Unit_GIS.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
