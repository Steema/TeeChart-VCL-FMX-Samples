program Test_StripBlock;

uses
  FastMM4,
  Forms,
  Unit_Test in 'Unit_Test.pas' {Form1},
  TeeStripBlock in 'TeeStripBlock.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
