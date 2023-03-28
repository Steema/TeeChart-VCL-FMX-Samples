unit Series_LineGradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeEdiGrad;

type
  TSeriesLineGradient = class(TBaseForm)
    ButtonGradient1: TButtonGradient;
    ButtonGradient2: TButtonGradient;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesLineGradient.FormCreate(Sender: TObject);
begin
  inherited;

  with Series1.Gradient do
  begin
    EndColor:=clYellow;
    StartColor:=Series1.Color;
    Visible:=True;
  end;

  ButtonGradient1.LinkGradient(Series1.Gradient);

  with Series2.Gradient do
  begin
    EndColor:=clYellow;
    StartColor:=Series2.Color;
    Visible:=True;
  end;

  ButtonGradient2.LinkGradient(Series2.Gradient);
end;

initialization
  RegisterClass(TSeriesLineGradient);
end.
