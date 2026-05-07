program MultiPanel_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMultiPanel in 'UMultiPanel.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
