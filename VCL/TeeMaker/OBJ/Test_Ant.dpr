program Test_Ant;

uses
  Forms,
  Unit_Test_Ant in 'Unit_Test_Ant.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
