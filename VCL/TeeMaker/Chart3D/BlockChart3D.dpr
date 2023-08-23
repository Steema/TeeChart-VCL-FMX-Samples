program BlockChart3D;

uses
  Forms,
  UChart3D in 'UChart3D.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
