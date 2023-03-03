program PJScrpExp;

uses
  Forms,
  UJScrExp in 'UJScrExp.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
