program Test_Bubble_3D;

uses
  FastMM4,
  Forms,
  Unit_Bubble_3D in 'Unit_Bubble_3D.pas' {Form1},
  TeeVisualsEditor in '..\..\..\..\Sources9\TeeVisualsEditor.pas' {VisualsEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
