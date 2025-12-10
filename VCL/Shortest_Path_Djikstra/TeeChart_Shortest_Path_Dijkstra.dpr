program TeeChart_Shortest_Path_Dijkstra;

uses
  Vcl.Forms,
  Main_Unit in 'Main_Unit.pas' {MainForm},
  TeeShortestPath in 'TeeShortestPath.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
