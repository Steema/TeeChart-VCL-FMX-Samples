program Test_Pie_3D;

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  Forms,
  Unit_test_Pie in 'Unit_test_Pie.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
