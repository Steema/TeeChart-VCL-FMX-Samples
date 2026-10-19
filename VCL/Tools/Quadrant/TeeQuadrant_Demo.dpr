program TeeQuadrant_Demo;

uses
  Vcl.Forms,
  Unit_Quadrant in 'Unit_Quadrant.pas' {FormQuadrant};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormQuadrant, FormQuadrant);
  Application.Run;
end.
