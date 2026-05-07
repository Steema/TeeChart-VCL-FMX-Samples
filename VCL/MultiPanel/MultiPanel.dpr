{********************************************}
{ MultiPanel TeeChart realtime demo          }
{ Copyright (c) 2026 by Steema Software      }
{ All Rights Reserved                        }
{********************************************}
program MultiPanel;

uses
  Vcl.Forms,
  UMultiPanel in 'UMultiPanel.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
