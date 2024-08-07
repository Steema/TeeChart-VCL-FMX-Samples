unit Commander_Gradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeComma, TeeEdiGrad;

type
  TCommanderGradient = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    ButtonGradient1: TButtonGradient;
    Series1: TPieSeries;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCommanderGradient.FormCreate(Sender: TObject);
begin
  inherited;

  with TeeCommander1.Gradient do
  begin
    StartColor := clWhite;

    // Example: Always use ColorToRGB for system colors like "clGrayText":
    EndColor := ColorToRGB(clGrayText);

    MidColor := RGB(255,255,128);

    Visible:= true;
  end;

  ButtonGradient1.LinkGradient(TeeCommander1.Gradient);
end;

initialization
  RegisterClass(TCommanderGradient);
end.
