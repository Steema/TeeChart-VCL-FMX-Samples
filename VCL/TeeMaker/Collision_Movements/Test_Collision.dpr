program Test_Collision;

uses
  Forms,
  Unit_Test_Collision in 'Unit_Test_Collision.pas' {Form1},
  TeePhysics in 'TeePhysics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
