unit Spark_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Base, TeeProcs, TeEngine, Chart, TeeSubChart;

type
  TSparkChartForm = class(TBaseForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Grid : TMultiChartTool;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Series, TeeTools;

procedure TSparkChartForm.FormShow(Sender: TObject);
var Row : Integer;
begin
  inherited;

  Grid:=TMultiChartTool.Create(Self);

  Chart1.Tools.Add(Grid);

  Grid.SetSize(10,4);

  for Row:=0 to Grid.Rows.Count-1 do
  begin
    Grid[Row,0].Tools.Add(TAnnotationTool);
    Grid[Row,1].AddSeries(TFastLineSeries).FillSampleValues;
    Grid[Row,2].AddSeries(TPieSeries).FillSampleValues;
    Grid[Row,3].AddSeries(TBarSeries).FillSampleValues;
  end;
end;

initialization
  RegisterClass(TSparkChartForm);
end.
