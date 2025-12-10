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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, VCLTee.TeEngine,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.StdCtrls,

  TeeShortestPath;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    Series1: TPointSeries;
    Splitter1: TSplitter;
    Chart2: TChart;
    Series2: TLineSeries;
    Button1: TButton;
    StartFinish: TPointSeries;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Start,
    Finish : Integer;

    ShortestPath : TShortestPath;

    procedure AddPath(const APath:Array of Integer);
    procedure InitRandomPoints;
    procedure SetupSeries;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

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

  // Now calculate the shortest path from Start to Finish
  AddPath(ShortestPath.Calculate(Start,Finish));

  StartFinish.Clear;
  AddPoint(StartFinish,Series1,Start);
  AddPoint(StartFinish,Series1,Finish);

  Label1.Caption:='Start: '+IntToStr(Start)+' Finish: '+IntToStr(Finish);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Label1.Caption:='';

  Chart1.Color:=clWhite;
  Chart2.Hide;
  Chart1.Align:=alClient;

  StartFinish.Pointer.Size:=18;

  SetupSeries;

  // Create the algorithm class
  ShortestPath:=TShortestPath.Create(Series1);
  ShortestPath.MaxDistance:=400;

  InitRandomPoints;
end;

// Add random points to Series1
procedure TMainForm.InitRandomPoints;
var t : Integer;
begin
  for t:=1 to 50 do
      Series1.AddXY(Random(1000),Random(1000));
 end;

// Cosmetics
procedure TMainForm.SetupSeries;
begin
  Series1.ColorEachPoint:=True;
  Series1.Pointer.Size:=14;

  Series1.Marks.Show;
  Series1.Marks.Style:=smsPointIndex;
  Series1.Marks.Arrow.Hide;
  Series1.Marks.ArrowLength:=-5;
end;

end.
