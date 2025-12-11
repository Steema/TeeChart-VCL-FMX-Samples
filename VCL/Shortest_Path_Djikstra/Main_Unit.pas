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

  TeeShortestPath;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    Series1: TPointSeries;
    Series2: TLineSeries;
    Button1: TButton;
    StartFinish: TPointSeries;
    Label1: TLabel;
    CBEdges: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBEdgesClick(Sender: TObject);
    procedure Series1BeforeDrawValues(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
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
  Series2.Clear;

  for t in APath do
      AddPoint(Series2,Series1,t);
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
  AddPath(ShortestPath.Calculate(Start,Finish));

  // Highlight in red the Start and Finish points
  StartFinish.Clear;
  AddPoint(StartFinish,Series1,Start);
  AddPoint(StartFinish,Series1,Finish);

  // Show at label
  Label1.Caption:='Start: '+IntToStr(Start)+' Finish: '+IntToStr(Finish);
end;

// Add some random edges between nearby points, so the path will be calculated with
// this restriction, it should go through edges only.
procedure TMainForm.AddEdges;
var t,tt : Integer;
    Distance : TDistance;
begin
  for t:=0 to Series1.Count-1 do
      for tt:=t+1 to Series1.Count-1 do
      begin
        Distance:=ShortestPath.Distance(t,tt);

        if Distance<ShortestPath.MaxDistance then
        begin
          ShortestPath.AddEdge(t,tt);

          if Length(ShortestPath.Edges[t])>2 then
             break;
        end;
      end;
end;

procedure TMainForm.CBEdgesClick(Sender: TObject);
begin
  ShowGrids;

  ShortestPath.ClearEdges;

  if CBEdges.Checked then
     AddEdges;

  if (Start<>-1) and (Finish<>-1) then
     CalculatePath;
end;

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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Label1.Caption:='';

  Chart1.Color:=clWhite;
  Chart1.Align:=alClient;

  ShowGrids;

  Start:=-1;
  Finish:=-1;

  StartFinish.Pointer.Size:=18;

  SetupSeries;

  // Create the algorithm class
  ShortestPath:=TShortestPath.Create(Series1);
  ShortestPath.MaxDistance:=200;

  InitRandomPoints;
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

  if CBEdges.Checked then
     AddEdges;
end;

// Cosmetics
procedure TMainForm.Series1BeforeDrawValues(Sender: TObject);
var t,Index : Integer;
    FromPoint, ToPoint : TPoint;
begin
  Chart1.Canvas.Pen.Color:=RGB(160,160,160); // Light silver
  Chart1.Canvas.Pen.Width:=1;
  Chart1.Canvas.Pen.Style:=psSolid;

  for t:=0 to High(ShortestPath.Edges) do
  begin
    FromPoint.X:=Series1.CalcXPos(t);
    FromPoint.Y:=Series1.CalcYPos(t);

    for Index in ShortestPath.Edges[t] do
    begin
      ToPoint.X:=Series1.CalcXPos(Index);
      ToPoint.Y:=Series1.CalcYPos(Index);

      Chart1.Canvas.Line(FromPoint,ToPoint);
    end;
  end;
end;

procedure TMainForm.SetupSeries;
begin
  Series1.Pointer.Size:=14;
  Series1.ColorEachPoint:=True;

  Chart1.ColorPaletteIndex:=6;

  Series1.Marks.Show;
  Series1.Marks.Style:=smsPointIndex;
  Series1.Marks.Arrow.Hide;
  Series1.Marks.ArrowLength:=-5;
end;

// Show chart grid lines only when Edges are not used (cosmetic)
procedure TMainForm.ShowGrids;
begin
  Chart1.Axes.Left.Grid.Visible:=not CBEdges.Checked;
  Chart1.Axes.Bottom.Grid.Visible:=not CBEdges.Checked;
end;

end.
