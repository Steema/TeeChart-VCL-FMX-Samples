unit UChart3D;

interface

uses
  Windows, Messages, SysUtils,
  Types,
  Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, TeCanvas, TeeBlocks, TeeChartBlock, ExtCtrls,
  TeeProcs, TeeMakerControl, TeeChart3D, Chart, StdCtrls, Buttons, TeeComma, TeeThemes,
  TeeDonut, TeeGDIPlus;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    CBAfterDraw: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Chart3D1: TChart3D;
    ChartBlock1: TChartBlock;
    Series1: TAreaSeries;
    TeeCommander1: TTeeCommander;
    procedure FormCreate(Sender: TObject);
    procedure Chart3D1AfterDraw(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure Create_AddSeries;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pieSeries : TPieSeries;
  donutSeries : TDonutSeries;
  barSeries : TBarSeries;
  areaSeries : TAreaSeries;
  lineSeries : TLineSeries;

implementation

{$R *.dfm}

procedure TForm1.Create_AddSeries;
begin
  Chart3D1.Chart.RemoveAllSeries;

  case Combobox1.ItemIndex of
   0 :
   Begin
     areaSeries := TAreaSeries.Create(Chart3D1);
     areaSeries.ColorEachPoint:=True;
     areaSeries.FillSampleValues(5);
     Chart3D1.Chart.AddSeries(areaSeries);
   End;
   1 :
   Begin
     barSeries := TBarSeries.Create(Chart3D1);
     barSeries.ColorEachPoint:=True;
     barSeries.FillSampleValues();
     Chart3D1.Chart.AddSeries(barSeries);
   End;
   2 :
   Begin
     donutSeries := TDonutSeries.Create(Chart3D1);
     donutSeries.FillSampleValues();
     Chart3D1.Chart.AddSeries(donutSeries);
   End;
   3 :
   Begin
     lineSeries := TLineSeries.Create(Chart3D1);
     lineSeries.FillSampleValues();
     Chart3D1.Chart.AddSeries(lineSeries);
   End;
   4 :
   Begin
     pieSeries := TPieSeries.Create(Chart3D1);
     pieSeries.FillSampleValues();
     Chart3D1.Chart.AddSeries(pieSeries);
   End;
  end;

  CBAfterDraw.Enabled:= Combobox1.ItemIndex = 0;
  if Combobox1.ItemIndex <> 0 Then CBAfterDraw.Checked := False;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart3D1.Options.Floor.Visible:=False;
  Chart3D1.Gradient.Visible:=False;
  Chart3D1.Chart[0].FillSampleValues();

  Chart3D1.Chart.Title.Font.Color := clWhite;
  Chart3D1.Chart.Title.Text.Strings[0]:='TeeChart VCL V2010 Preview';
  Chart3D1.Chart.Title.Text.Add('Presentation Chart');
  Chart3D1.Chart.Chart3DPercent:=50;

  with TOperaTheme.Create(Chart3D1.Chart) do
  try
    Apply;
  finally
    Free;
  end;

  Chart3D1.Chart.Gradient.Visible:=False; //undo Opera back gradient
  Chart3D1.Chart.Walls.Back.Visible:=False;

  Combobox1.ItemIndex:=0;
  Create_AddSeries;

  Chart3D1.Render.Antialias:=True;
  Chart3D1.Gradient.StartColor:=clGray;
  Chart3D1.Gradient.EndColor:=clBlack;
  Chart3D1.Gradient.Visible:=True;
  Chart3D1.Options.Floor.Visible:=False;

  TeeCommander1.Panel:=Chart3D1.Chart;

  Chart3D1.Refresh;
end;

procedure TForm1.Chart3D1AfterDraw(Sender: TObject);
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
    if (Chart3D1.Chart.SeriesList[0] is TAreaSeries) then
    Begin

      left:=Round(Chart3D1.ChartBlock.Location.Point.X);
      top:=-250;

      points[0]:=Point(areaSeries.CalcXPos(1)+left,Chart3D1.Chart.Axes.Left.CalcYPosValue(Chart3D1.Chart.Axes.Left.Minimum)+top);
      points[1]:=Point(areaSeries.CalcXPos(1)+left,areaSeries.CalcYPos(1)+top);
      points[2]:=Point(areaSeries.CalcXPos(2)+left,areaSeries.CalcYPos(2)+top);
      points[3]:=Point(areaSeries.CalcXPos(2)+left,Chart3D1.Chart.Axes.Left.CalcYPosValue(Chart3D1.Chart.Axes.Left.Minimum)+top);

      Chart3D1.Canvas.Brush.Color:=RGB(115,255,115);
      Chart3D1.Canvas.PlaneWithZ(points,-1);

      Chart3D1.Canvas.TextOut3D(Round(points[0].X + ((points[2].X-points[0].X) / 2)
                              -(Chart3D1.Canvas.TextWidth('Value:')/2)),
                              Round(MidY),-2,
                              'Value:');
      Chart3D1.Canvas.TextOut3D(Round(points[0].X + ((points[2].X-points[0].X) / 2)
                              -(Chart3D1.Canvas.TextWidth('Value:')/2)),
                              Round(MidY)+Chart3D1.Canvas.FontHeight,-2,
                              FloatToStr(areaSeries.YValues[2]));
    End;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Create_AddSeries;
end;

end.
