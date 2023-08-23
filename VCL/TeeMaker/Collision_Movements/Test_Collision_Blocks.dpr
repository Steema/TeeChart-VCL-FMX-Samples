program Test_Collision_Blocks;

uses
  Forms,
  Unit_Test_Collision_Blocks in 'Unit_Test_Collision_Blocks.pas' {Form1};
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
