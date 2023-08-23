unit UBlockChart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TeeProcs, Chart, TeeMakerControl, TeeChartBlock, StdCtrls, Series,
  TeeComma, TeeOpenGL, TeeDonut, TeeThemes, TeeThemeEditor, TeeBlocks, Buttons, TeeExtruded,
  Math, TeCanvas;

type
  TForm1 = class(TForm)
    TeeCommander1: TTeeCommander;
    Panel1: TPanel;
    Panel2: TPanel;
    Maker1: TMaker;
    Label1: TLabel;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    CBAfterDraw: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Maker1AfterDraw(Sender: TObject);
    procedure CBAfterDrawClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Create_AddSeries;
    { Public declarations }
  end;

var
  Form1: TForm1;
  blockChart : TChartBlock;
  pieSeries : TPieSeries;
  donutSeries : TDonutSeries;
  barSeries : TBarSeries;
  areaSeries : TAreaSeries;
  lineSeries : TLineSeries;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
const
  TwoPiFrac=TwoPi*0.1;

var eBlock:TExtrudedBlock;
    points:TPointCollection;
    t      : Integer;
    tmpSin : Extended;
    tmpCos : Extended;
begin
  eBlock:=TExtrudedBlock.Create(Maker1);

  Maker1.Blocks.Add(eBlock);

  eBlock.Format.Color:= RGB(Random(255),Random(255),Random(255));

  for t:=9 downto 0 do
  begin
    SinCos(t*TwoPiFrac,tmpSin,tmpCos);
    eBlock.Points.Add(tmpSin*Random(1000)*0.001,tmpCos*Random(1000)*0.001);
  end;

  eBlock.Visible:=True;
  eBlock.Rotation.X:=270;
  eBlock.Rotation.Y:=270;
  eBlock.Rotation.Z:=90;
  eBlock.Move(0,-40,0);

  Maker1.Blocks.Draw;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  blockChart.Chart.Series[0].FillSampleValues(blockChart.Chart.Series[0].Count);
end;

procedure TForm1.CBAfterDrawClick(Sender: TObject);
begin
  blockChart.Chart.Refresh;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Create_AddSeries;
end;

procedure TForm1.Create_AddSeries;
begin
  blockChart.Chart.RemoveAllSeries;

  case Combobox1.ItemIndex of
   0 :
   Begin
     areaSeries := TAreaSeries.Create(blockChart);
     areaSeries.ColorEachPoint:=True;
     areaSeries.FillSampleValues(5);
     blockChart.Chart.AddSeries(areaSeries);
   End;
   1 :
   Begin
     barSeries := TBarSeries.Create(blockChart);
     barSeries.ColorEachPoint:=True;
     barSeries.FillSampleValues();
     blockChart.Chart.AddSeries(barSeries);
   End;
   2 :
   Begin
     donutSeries := TDonutSeries.Create(blockChart);
     donutSeries.FillSampleValues();
     blockChart.Chart.AddSeries(donutSeries);
   End;
   3 :
   Begin
     lineSeries := TLineSeries.Create(blockChart);
     lineSeries.FillSampleValues();
     blockChart.Chart.AddSeries(lineSeries);
   End;
   4 :
   Begin
     pieSeries := TPieSeries.Create(blockChart);
     pieSeries.FillSampleValues();
     blockChart.Chart.AddSeries(pieSeries);
   End;
  end;

  CBAfterDraw.Enabled:= Combobox1.ItemIndex = 0;
  if Combobox1.ItemIndex <> 0 Then CBAfterDraw.Checked := False;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  blockChart:=TChartBlock.Create(Maker1);

  blockChart.Chart.Title.Font.Color := clWhite;
  blockChart.Chart.Title.Text.Strings[0]:='TeeChart VCL V2010 Preview';
  blockChart.Chart.Title.Text.Add('Presentation Chart');
  blockChart.Chart.Chart3DPercent:=50;

  with TOperaTheme.Create(blockChart.Chart) do
  try
    Apply;
  finally
    Free;
  end;

  blockChart.Chart.Gradient.Visible:=False; //undo Opera back gradient
  blockChart.Chart.Walls.Back.Visible:=False;

  Combobox1.ItemIndex:=0;
  Create_AddSeries;

  Maker1.Render.Antialias:=True;
  Maker1.Gradient.StartColor:=clGray;
  Maker1.Gradient.EndColor:=clBlack;
  Maker1.Gradient.Visible:=True;
  Maker1.Options.Floor.Visible:=False;

  Maker1.Blocks.Add(blockChart);
  Maker1.Aspect.VertOffsetFloat:=140; //align Chart in centre of frame

  TeeCommander1.Panel:=blockChart.Chart;

  Maker1.Refresh;
end;

procedure TForm1.Maker1AfterDraw(Sender: TObject);
const
  TwoPiFrac=TwoPi*0.1;

Var t: Integer;
    points:TFourPoints;
    tmpSin : Extended;
    tmpCos : Extended;
    left,top: Integer;

    Function MidY: Double;
    Begin
      if areaSeries.YValues[2] > areaSeries.YValues[1] then
        result:=points[2].Y + ((points[3].Y-points[2].Y) / 2)
      else
        result:=points[1].Y + ((points[3].Y-points[1].Y) / 2);
    End;

begin

  if CBAfterDraw.Checked then
    if (blockChart.Chart.SeriesList[0] is TAreaSeries) then
    Begin

      left:=Round(blockChart.Location.Point.X);
      top:=-250;

      points[0]:=Point(areaSeries.CalcXPos(1)+left,blockChart.Chart.Axes.Left.CalcYPosValue(blockChart.Chart.Axes.Left.Minimum)+top);
      points[1]:=Point(areaSeries.CalcXPos(1)+left,areaSeries.CalcYPos(1)+top);
      points[2]:=Point(areaSeries.CalcXPos(2)+left,areaSeries.CalcYPos(2)+top);
      points[3]:=Point(areaSeries.CalcXPos(2)+left,blockChart.Chart.Axes.Left.CalcYPosValue(blockChart.Chart.Axes.Left.Minimum)+top);

      Maker1.Canvas.Brush.Color:=RGB(115,255,115);
      Maker1.Canvas.PlaneWithZ(points,-1);

      Maker1.Canvas.TextOut3D(Round(points[0].X + ((points[2].X-points[0].X) / 2)
                              -(Maker1.Canvas.TextWidth('Value:')/2)),
                              Round(MidY),-2,
                              'Value:');
      Maker1.Canvas.TextOut3D(Round(points[0].X + ((points[2].X-points[0].X) / 2)
                              -(Maker1.Canvas.TextWidth('Value:')/2)),
                              Round(MidY)+Maker1.Canvas.FontHeight,-2,
                              FloatToStr(areaSeries.YValues[2]));
    End;
end;

end.
