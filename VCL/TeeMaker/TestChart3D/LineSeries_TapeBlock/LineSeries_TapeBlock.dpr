program LineSeries_TapeBlock;

uses
  FastMM4,
  Forms,
  Unit_LineSeries_Tape in 'Unit_LineSeries_Tape.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
