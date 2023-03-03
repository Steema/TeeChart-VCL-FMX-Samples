program PPolarContour;

uses
  Forms,
  UPolarContour in 'UPolarContour.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
