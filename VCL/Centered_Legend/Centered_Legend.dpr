program Centered_Legend;

uses
  Vcl.Forms,
  Unit_Centered_Legend in 'Unit_Centered_Legend.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
