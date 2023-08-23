program Test_Chart3D;

uses
  FastMM4,
  Forms,
  Unit_Test_Chart3D in 'Unit_Test_Chart3D.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
