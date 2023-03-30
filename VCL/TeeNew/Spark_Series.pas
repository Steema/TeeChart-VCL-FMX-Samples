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
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }

    Grid : TChartLayoutTool;

    function AddSeries(const ARow,AColumn:Integer;
                       const AClass:TChartSeriesClass;
                       const ARandom:Integer):TChartSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Series, TeeTools;

function TSparkChartForm.AddSeries(const ARow,AColumn:Integer;
                                   const AClass:TChartSeriesClass;
                                   const ARandom:Integer):TChartSeries;
begin
  result:=Grid[ARow,AColumn].AddSeries(AClass);

  result.Marks.Hide;
  result.FillSampleValues(ARandom);
end;

procedure TSparkChartForm.FormShow(Sender: TObject);
var Row : Integer;
    Annotation : TAnnotationTool;
    Bar : TChartSeries;
begin
  inherited;

  // Hide axes and walls
  Chart1.Axes.Hide;
  Chart1.Walls.Hide;

  // Add Layout tool
  Grid:=TChartLayoutTool.Create(Self);
  Chart1.Tools.Add(Grid);

  // We want 10x4 = 40 charts
  Grid.SetSize(10,4);

  // Set custom columns sizes. Automatic=0

  Grid.Columns[0].Size:=60;
  Grid.Columns[0].SizeUnits:=muPixels;

  Grid.Columns[1].Size:=60;
  Grid.Columns[1].SizeUnits:=muPercent;

  Grid.Columns[2].Size:=110;
  Grid.Columns[2].SizeUnits:=muPixels;

  // First row, custom size
  Grid.Rows[0].Size:=90;
  Grid.Rows[0].SizeUnits:=muPixels;

  // Add Series to all charts

  for Row:=0 to Grid.Rows.Count-1 do
  begin
    Annotation:=Grid[Row,0].Tools.Add(TAnnotationTool) as TAnnotationTool;

    Annotation.Text:='Row '+IntToStr(Row);

    AddSeries(Row,1,TFastLineSeries,100).ParentChart.Color:=RGB(250,240,230);

    AddSeries(Row,2,TPieSeries,6).ParentChart.MarginTop:=10;

    Bar:=AddSeries(Row,3,TBarSeries,6);

    Bar.ColorEachPoint:=True;
    Bar.Marks.Show;
    Bar.Marks.Transparent:=True;
  end;
end;

// Some animations, just for fun

procedure TSparkChartForm.Timer1Timer(Sender: TObject);
var Chart : TChart;
    Index,
    Row,
    Column : Integer;

    Pie : TPieSeries;
begin
  // Choose a random chart

  Row:=Random(Grid.Rows.Count);
  Column:=1+Random(Grid.Columns.Count-1);

  Chart:=Grid[Row,Column];

  case Column of
    1: begin
         Chart[0].Add(Random(Round(Chart[0].YValues.MaxValue)));
       end;

    2: begin
         Pie:=(Chart[0] as TPieSeries);
         Pie.Rotate(5);
       end;

    3: begin
         // Choose a random point in the series
         Index:=Random(Chart[0].Count);

         // Change that point a random +- amount
         Chart[0].YValue[Index]:=Chart[0].YValue[Index]+Random(100)-50;
       end;
  end;
end;

procedure TSparkChartForm.CheckBox1Click(Sender: TObject);
begin
  Timer1.Enabled:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TSparkChartForm);
end.
