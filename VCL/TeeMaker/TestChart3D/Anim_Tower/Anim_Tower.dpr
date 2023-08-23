program Anim_Tower;

uses
  Forms,
  Unit_Anim_Tower in 'Unit_Anim_Tower.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
