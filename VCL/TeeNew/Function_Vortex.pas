unit Function_Vortex;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeGDIPlus, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls,
  ComCtrls, Series, OHLChart, CandleCh;

type
  TVortexFunctionForm = class(TBaseForm)
    Series1: TCandleSeries;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }

    VortexSeries : TFastLineSeries;
    TeeFunction1 : TVortexFunction;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TVortexFunctionForm.FormCreate(Sender: TObject);
begin
  inherited;

  // Example creating a function by code.
  // This can also be done using the chart editor (Gallery->Functions->Vortex),
  // with no code.

  TeeFunction1:=TVortexFunction.Create(Self);

  VortexSeries:=TFastLineSeries.Create(Self);
  VortexSeries.Color:=clGreen;
  VortexSeries.SetFunction(TeeFunction1);

  VortexSeries.ParentChart:=Chart1;

  VortexSeries.DataSource:=Series1;
  VortexSeries.Title:='Vortex';

  // Some random data
  Series1.FillSampleValues(300);
  Series1.Title:='Acme Inc.';

  // Cosmetic, set Vortex to right axis
  Chart1.Axes.Left.EndPosition:=50;
  Chart1.Axes.Right.StartPosition:=50;

  VortexSeries.VertAxis:=aRightAxis;
  TeeFunction1.Negative.VertAxis:=aRightAxis;
end;

procedure TVortexFunctionForm.Edit1Change(Sender: TObject);
begin
  inherited;
  TeeFunction1.Period:=UpDown1.Position;
end;

initialization
  RegisterClass(TVortexFunctionForm);
end.
