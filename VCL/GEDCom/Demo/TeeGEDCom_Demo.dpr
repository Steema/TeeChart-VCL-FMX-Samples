program TeeGEDCom_Demo;

uses
  Vcl.Forms,
  Unit_Main_GED in 'Unit_Main_GED.pas' {MainForm},
  TeeGEDCOM in '..\TeeGEDCOM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
