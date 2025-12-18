{********************************************}
{ Dijkstra Shortest Path Algorithm Demo      }
{ Copyright (c) 2025 by Steema Software      }
{ All Rights Reserved                        }
{********************************************}
unit Main_Unit;

{
  Example using the TShortestPath class, that implements the Dijkstra algorithm.

  A path is calculated between point "Start" and point "Finish" of Series1.

  The result is then added to Series2.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,

  TeEngine, Series, TeeProcs, Chart,

  TeeShortestPath, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    SeriesPath: TLineSeries;
    Button1: TButton;
    StartFinish: TPointSeries;
    LPathInfo: TLabel;
    CBEdges: TCheckBox;
    CBWeights: TCheckBox;
    Series1: TPointSeries;
    CBDirection: TCheckBox;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    LDistance: TLabel;
    LTotalLength: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBEdgesClick(Sender: TObject);
    procedure Series1BeforeDrawValues(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure CBWeightsClick(Sender: TObject);
    function Series1GetPointerStyle(Sender: TChartSeries;
      ValueIndex: Integer): TSeriesPointerStyle;
    procedure CBDirectionClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }

    Start,
    Finish : Integer;

    ShortestPath : TShortestPath;

    procedure AddEdges;
    procedure AddPath(const APath:Array of Integer);
    procedure CalculatePath;
    procedure InitRandomPoints;
    procedure SetupSeries;
    procedure ShowGrids;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  TeCanvas, Types;

// Adds point AIndex from ASource to ADest series
procedure AddPoint(const ADest,ASource:TChartSeries; const AIndex:Integer);
begin
  ADest.AddXY(ASource.XValues[AIndex], ASource.YValues[AIndex]);
end;

// Add output points using the APath, array of point indexes
procedure TMainForm.AddPath(const APath:Array of Integer);
var t : Integer;
begin
  SeriesPath.Clear;
  SeriesPath.XValues.Order:=loNone;

  for t in APath do
      AddPoint(SeriesPath,Series1,t);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  // Select 2 points randomly, one to Start and one to Finish
  Start:=Random(Series1.Count);

  repeat
    Finish:=Random(Series1.Count);
  until Finish<>Start; // prevent same point

  CalculatePath;
end;

// Calculate the shortest path from Start to Finish, and add it to Chart
procedure TMainForm.CalculatePath;
begin
  if (Start=-1) or (Finish=-1) then
     Exit;

  AddPath(ShortestPath.Calculate(Start,Finish));

  // Highlight in red the Start and Finish points
  StartFinish.Clear;
  AddPoint(StartFinish,Series1,Start);
  AddPoint(StartFinish,Series1,Finish);

  // Show at label
  LPathInfo.Caption:='Start: '+IntToStr(Start)+' Finish: '+IntToStr(Finish)+ ' Hops: '+IntToStr(SeriesPath.Count);
  LTotalLength.Caption:='Total length: '+FormatFloat('0.#',ShortestPath.TotalDistance);
end;

// Add some random edges between nearby points, so the path will be calculated with
// this restriction, it should go through edges only.
procedure TMainForm.AddEdges;
var t,tt : Integer;
    Distance : TDistance;
    Direction : TEdgeDirection;
begin
  ShortestPath.ClearEdges;

  for t:=0 to Series1.Count-1 do
      for tt:=t+1 to Series1.Count-1 do
      begin
        Distance:=ShortestPath.Distance(t,tt);

        if Distance<ShortestPath.MaxDistance then
        begin
          Direction:=TEdgeDirection(Random(1+Ord(High(TEdgeDirection))));

          ShortestPath.AddEdge(t,tt,Direction);

          if Length(ShortestPath.Edges[t])>2 then
             break;
        end;
      end;
end;

// Show or hide edges (roads), use them to calculate the path
procedure TMainForm.CBDirectionClick(Sender: TObject);
begin
  ShortestPath.EdgesBothWays:=CBDirection.Checked;

  CalculatePath;
end;

procedure TMainForm.CBEdgesClick(Sender: TObject);
begin
  CBDirection.Enabled:=CBEdges.Checked;

  ShortestPath.UseEdges:=CBEdges.Checked;

  ShowGrids;

  CalculatePath;

  Chart1.Invalidate;
end;

// When clicking a point, calculate a path to it
procedure TMainForm.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Index : Integer;
begin
  Index:=Series1.Clicked(X,Y);

  if Index=-1 then
     Exit;

  if Start=-1 then
     Start:=Index
  else
  begin
    Finish:=Index;
    CalculatePath;
  end;
end;

procedure TMainForm.CBWeightsClick(Sender: TObject);
begin
  ShortestPath.UseWeights:=CBWeights.Checked;

  if Start<>-1 then
     CalculatePath;

  Chart1.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RandSeed:=12345678;  // just to obtain repeated random values at each run

  LPathInfo.Caption:='';
  LTotalLength.Caption:='';

  Chart1.Color:=clWhite;
  Chart1.Align:=alClient;

  ShowGrids;

  StartFinish.Pointer.Size:=18;

  SetupSeries;

  // Create the algorithm class
  ShortestPath:=TShortestPath.Create;
  ShortestPath.MaxDistance:=200;
  ShortestPath.UseEdges:=True;
  ShortestPath.UseWeights:=False;

  InitRandomPoints;

  // Initialize path points
  Start:=11;
  Finish:=21;

  CalculatePath;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ShortestPath.Free;
end;

// Add random points to Series1
procedure TMainForm.InitRandomPoints;
var t : Integer;
begin
  for t:=1 to 50 do
      Series1.AddXY(Random(1000),Random(1000));

  SetLength(ShortestPath.Points,Series1.Count);
  SetLength(ShortestPath.Weights,Series1.Count);

  for t:=1 to Series1.Count-1 do
  begin
    ShortestPath.Points[t].X:= Series1.XValues[t];
    ShortestPath.Points[t].Y:= Series1.YValues[t];

    ShortestPath.Weights[t]:= Random(50);
  end;

  AddEdges;
end;

// Cosmetics, draw edges
procedure TMainForm.Series1BeforeDrawValues(Sender: TObject);

  procedure DrawEdges;
  var t : Integer;
      Mid, FromPoint, ToPoint : TPoint;
      Edge : TEdge;
  begin
    Chart1.Canvas.Pen.Color:=RGB(140,140,140); // Light silver
    Chart1.Canvas.Pen.Width:=1;
    Chart1.Canvas.Pen.Style:=psSolid;

    for t:=0 to High(ShortestPath.Edges) do
    begin
      FromPoint.X:=Series1.CalcXPos(t);
      FromPoint.Y:=Series1.CalcYPos(t);

      for Edge in ShortestPath.Edges[t] do
      begin
        ToPoint.X:=Series1.CalcXPos(Edge.ToIndex);
        ToPoint.Y:=Series1.CalcYPos(Edge.ToIndex);

        Chart1.Canvas.Line(FromPoint,ToPoint);

        if Edge.Direction<>TEdgeDirection.BothWays then
        begin
          // Draw arrows at line midpoint
          Mid.X:=(FromPoint.X+ToPoint.X) div 2;
          Mid.Y:=(FromPoint.Y+ToPoint.Y) div 2;

          if Edge.Direction=TEdgeDirection.FromWay then
             Chart1.Canvas.Arrow(False,FromPoint,Mid,14,14,0)
          else
             Chart1.Canvas.Arrow(False,Mid,ToPoint,14,14,0);
        end;
      end;
    end;
  end;

begin
  if ShortestPath.UseEdges then
     DrawEdges;
end;

function TMainForm.Series1GetPointerStyle(Sender: TChartSeries;
  ValueIndex: Integer): TSeriesPointerStyle;
begin
  result:=psCircle;

  if CBWeights.Checked then
     Series1.Pointer.SizeFloat:=8+(ShortestPath.Weights[ValueIndex]/2)
  else
     Series1.Pointer.SizeFloat:=14;
end;

// Cosmetics
procedure TMainForm.SetupSeries;
begin
  Series1.Pointer.Size:=14;
  Series1.ColorEachPoint:=True;
  Series1.VertAxis:=aBothVertAxis;
  Series1.HorizAxis:=aBothHorizAxis;

  Chart1.ColorPaletteIndex:=6;

  Series1.Marks.Show;
  Series1.Marks.Font.Size:=6;
  Series1.Marks.Style:=smsPointIndex;
  Series1.Marks.Arrow.Hide;
  Series1.Marks.ArrowLength:=-8;
end;

// Show chart grid lines only when Edges are not used (cosmetic)
procedure TMainForm.ShowGrids;
begin
  Chart1.Axes.Left.Grid.Visible:=not CBEdges.Checked;
  Chart1.Axes.Bottom.Grid.Visible:=not CBEdges.Checked;

  Chart1.Axes.Right.Grid.Hide;
  Chart1.Axes.Top.Grid.Hide;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  ShortestPath.MaxDistance:=TrackBar1.Position;
  LDistance.Caption:=FloatToStr(ShortestPath.MaxDistance);

  CalculatePath;
end;

end.
