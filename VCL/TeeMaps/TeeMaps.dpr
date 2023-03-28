program TeeMaps;
{$I TeeDefs.inc}

uses
  Forms,
  UnitTestSHP in 'UnitTestSHP.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TeeChart GIS / Mapping Example';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
