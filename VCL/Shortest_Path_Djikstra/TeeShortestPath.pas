{********************************************}
{ Dijkstra Shortest Path Algorithm           }
{ Copyright (c) 2025 by Steema Software      }
{ All Rights Reserved                        }
{********************************************}
unit TeeShortestPath;
{$I TeeDefs.inc}

interface

uses
  TeEngine;

type
  TIndex=Integer; // a point index inside Series

  TPointsPath=Array of TIndex; // a list of points (a path)

  TDistance=Single; // type of the float used to calculate distances (Single, Double or Extended)

  TShortestPath=class
  private
    FSeries : TChartSeries;

    function Distance(const A,B:TIndex):TDistance; overload;
  public
    MaxDistance : TDistance;

    Constructor Create(const ASeries:TChartSeries);
    Destructor Destroy; override;

    // Returns the shortest path from AStart to AFinish
    function Calculate(const AStart,AFinish:TIndex):TPointsPath;

    class function Distance(const X0,Y0,X1,Y1:TChartValue): TDistance; overload; static;
  end;

implementation

uses
  Math;

{ TShortestPath }

Constructor TShortestPath.Create(const ASeries: TChartSeries);
begin
  inherited Create;

  MaxDistance:=MaxSingle;
  FSeries:=ASeries;
end;

Destructor TShortestPath.Destroy;
begin
  inherited;
end;

// Euclidean between X0,Y0 and X1,Y1
class function TShortestPath.Distance(const X0,Y0,X1,Y1:TChartValue): TDistance;
begin
  result:=Sqrt(Sqr(X0-X1) + Sqr(Y0-Y1));
end;

// Euclidean between point A and point B
function TShortestPath.Distance(const A, B: TIndex): TDistance;
begin
  result:=Distance(FSeries.XValues.Value[A],FSeries.YValues.Value[A],
                   FSeries.XValues.Value[B],FSeries.YValues.Value[B]);
end;

// Algorithm based on Dijkstra "A"

const
  MissingFlag=-1;

type
  TDistanceTo=record
    Distance : TDistance;
    Index : TIndex;
  end;

  TQueue=record
  public
    Items : Array of TDistanceTo;
    Count : Integer;

    function GetMinimum(var node: TIndex): TDistance;
    procedure Insert(const AIndex:TIndex; const ADistance:TDistance);
  end;

function TQueue.GetMinimum(var node: TIndex):TDistance;
var
  pqhead,
  child, tempNode: TIndex;

  rootDist : TDistance;
begin
  Result := Items[0].Distance;
  node   := Items[0].Index;

  Dec(Count);
  if Count = 0 then
     Exit;

  rootDist := Items[Count].Distance;
  tempNode := Items[Count].Index;

  pqHead := 0;
  while True do
  begin
    child := pqHead*2 + 1;

    if child >= Count then
       Break;

    if (child+1 < Count) and (Items[child+1].Distance < Items[child].Distance) then
       Inc(child);

    if rootDist <= Items[child].Distance then
       Break;

    Items[pqHead] := Items[child];
    pqHead := child;
  end;

  Items[pqHead].Distance := rootDist;
  Items[pqHead].Index := tempNode;
end;

// Insert at the sorted position
procedure TQueue.Insert(const AIndex:TIndex; const ADistance:TDistance);
var t : Integer;
begin
  t:=Count;

  Inc(Count);

  SetLength(Items,Count);

  while (t>0) and (Items[(t-1) div 2].Distance > ADistance) do
  begin
    Items[t]:=Items[(t-1) div 2];
    t:=(t-1) div 2;
  end;

  Items[t].Distance:=ADistance;
  Items[t].Index:=AIndex;
end;

// Returns the shortest path from AStart to AFinish
function TShortestPath.Calculate(const AStart,AFinish:Integer): TPointsPath;
var
  Neighbors : Array of Array of TIndex;
  Distances : Array of TDistance;
  Previous : Array of Integer;

  procedure Init;
  var t : Integer;
  begin
    SetLength(Neighbors,FSeries.Count);
    SetLength(Distances,FSeries.Count);
    SetLength(Previous,FSeries.Count);

    for t:=0 to High(Distances) do
    begin
      Distances[t]:=MaxSingle;
      Previous[t]:=MissingFlag; // -1 = flag empty point index
    end;
  end;

var Queue : TQueue;

    u,v : TIndex;
    d,
    alt : TDistance;
begin
  result:=nil;

  Init;

  // Start
  Distances[AStart]:=0;

  Queue.Count:=0;
  Queue.Insert(AStart,0);

  while Queue.Count>0 do
  begin
    Distances[u]:=Queue.GetMinimum(u);

    if u = AFinish then
       Break;

    for v := 0 to FSeries.Count-1 do
    begin
      if v = u then Continue;

      d := Distance(u, v);
      if d > MaxDistance then
         Continue;

      alt := Distances[u] + d;

      if alt < Distances[v] then
      begin
        Distances[v] := alt;
        Previous[v] := u;
        Queue.Insert(v,alt);
      end;
    end;
  end;

  if Distances[AFinish]=MaxSingle then
     Exit;

  // Create the resulting path
  u := AFinish;

  while u <> MissingFlag do
  begin
    Insert(u,result,0);
    u := Previous[u];
  end;
end;


end.

