unit Chart_ScrollBar;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeScroB, TeeProcs, Chart;

type
  TChartScrollBarForm = class(TBaseForm)
    ChartScrollBar1: TChartScrollBar;
    Series1: TFastLineSeries;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChartScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartScrollBarForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(1000);

  { show the first 100 points... }
  Chart1.BottomAxis.SetMinMax(0,99);

  { adjust the ScrollBar }
  ChartScrollBar1.RecalcPosition;
end;

procedure TChartScrollBarForm.Button1Click(Sender: TObject);
begin
  ChartScrollBar1.Position:=ChartScrollBar1.Max;  { go to end }
end;

procedure TChartScrollBarForm.Button2Click(Sender: TObject);
begin
  ChartScrollBar1.Position:=ChartScrollBar1.Min;  { go to begin }
end;

procedure TChartScrollBarForm.ChartScrollBar1Change(Sender: TObject);
begin
  { enable / disable the "go to begin" and "go to end" buttons... }
  Button1.Enabled:=ChartScrollBar1.Position < ChartScrollBar1.Max;
  Button2.Enabled:=ChartScrollBar1.Position > ChartScrollBar1.Min;
end;

initialization
  RegisterClass(TChartScrollBarForm);
end.
