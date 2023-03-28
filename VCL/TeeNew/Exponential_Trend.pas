unit Exponential_Trend;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, CurvFitt, Series, TeEngine, TeeProcs, Chart;

type
  TExponentialTrendForm = class(TBaseForm)
    Series1: TFastLineSeries;
    TeeFunction1: TExpTrendFunction;
    Series2: TFastLineSeries;
    Series3: TLineSeries;
    TeeFunction2: TTrendFunction;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TExponentialTrendForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(1000);
end;

initialization
  RegisterClass(TExponentialTrendForm);
end.
