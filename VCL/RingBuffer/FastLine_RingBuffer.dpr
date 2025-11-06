program FastLine_RingBuffer;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FormRingBuffer},
  TeeRingBuffer in 'TeeRingBuffer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormRingBuffer, FormRingBuffer);
  Application.Run;
end.
