program Boundary_Tool_Demo;

uses
  Vcl.Forms,
  Boundary_Tool_Main in 'Boundary_Tool_Main.pas' {FormBoundary};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBoundary, FormBoundary);
  Application.Run;
end.
