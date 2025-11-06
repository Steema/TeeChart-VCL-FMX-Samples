program FastLine_RingBuffer_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {FormRingBuffer},
  TeeRingBuffer in '..\..\VCL\RingBuffer\TeeRingBuffer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRingBuffer, FormRingBuffer);
  Application.Run;
end.
