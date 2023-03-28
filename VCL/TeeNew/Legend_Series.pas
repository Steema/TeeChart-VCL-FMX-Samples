unit Legend_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, Series, TeEngine, TeeProcs, Chart;

type
  TLegendSeriesForm = class(TBaseForm)
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Series1: TBarSeries;
    Series2: TLineSeries;
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLegendSeriesForm.RadioButton1Click(Sender: TObject);
begin
  Chart1.Legend.Series:=Series1;
end;

procedure TLegendSeriesForm.RadioButton2Click(Sender: TObject);
begin
  Chart1.Legend.Series:=Series2;
end;

procedure TLegendSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(5);
  Series2.FillSampleValues(8);
end;

initialization
  RegisterClass(TLegendSeriesForm);
end.
