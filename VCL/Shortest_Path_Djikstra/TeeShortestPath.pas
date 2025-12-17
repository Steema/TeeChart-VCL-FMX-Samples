{********************************************}
{ Dijkstra Shortest Path Algorithm           }
{ Copyright (c) 2025 by Steema Software      }
{ All Rights Reserved                        }
{********************************************}
unit TeeShortestPath;
{$I TeeDefs.inc}

interface

type
  TIndex=Integer; // a point index

  TPointsPath=Array of TIndex; // a list of points (a path)

  TEdgeDirection=(BothWays,FromWay,ToWay);   // Egress (outbound), Ingress (inbound)

  // A connection (road) between one point to another
  TEdge=record
    ToIndex : TIndex;
    Direction : TEdgeDirection;
  end;

  TEdges=Array of TEdge; // list of edges from one point to othere points

  TDistance=Single; // type of the float used to calculate distances (Single, Double or Extended)

  TCoordinate=Single; // type of the float used for XY coordinates and Weights

  // Simple XY point
  TPoint=record
     X,Y : TCoordinate;
  end;

  // Returns the shortest (or "cheapest") route path from one point to another.

  TShortestPath=class
  private
    function Count:Integer; inline;
  public
    Edges : Array of TEdges;  // For each point, an optional array of edges to other points
    MaxDistance : TDistance;  // When there are no Edges, use a Max distance limit to follow the path
    Points : Array of TPoint; // The source XY points
    Weights : Array of TDistance;  // Optional, one value associated to each point

    UseWeights : Boolean; // When True, the shortest path will be the "cheapest" path (with less Weights)

    Constructor Create;
    Destructor Destroy; override;

    // Optional edges (roads) between points
    procedure AddEdge(const AFrom,ATo:TIndex; const ADirection:TEdgeDirection=TEdgeDirection.BothWays);
    procedure ClearEdges;

    // Returns the shortest path from AStart to AFinish point index
    function Calculate(const AStart,AFinish:TIndex):TPointsPath;

    // Euclidean between point X0,Y0 and point X1,Y1
    class function Distance(const X0,Y0,X1,Y1:TCoordinate): TDistance; overload; static;

    function Distance(const A,B:TIndex):TDistance; overload; // between points
  end;

implementation

uses
  Math;

{ TShortestPath }

Constructor TShortestPath.Create;
begin
  inherited Create;

  MaxDistance:=MaxSingle;
end;

Destructor TShortestPath.Destroy;
begin
  inherited;
end;

// Euclidean between X0,Y0 and X1,Y1
class function TShortestPath.Distance(const X0,Y0,X1,Y1:TCoordinate): TDistance;
begin
  result:=Sqrt(Sqr(X0-X1) + Sqr(Y0-Y1));
end;

// Euclidean between point A and point B
function TShortestPath.Distance(const A, B: TIndex): TDistance;
begin
  result:=Distance(Points[A].X,Points[A].Y,
                   Points[B].X,Points[B].Y);
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

  if Count=0 then
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

function TShortestPath.Count:Integer;
begin
  result:=Length(Points);
end;

procedure TShortestPath.AddEdge(const AFrom, ATo: TIndex; const ADirection:TEdgeDirection=TEdgeDirection.BothWays);
var tmp : TEdge;
begin
  if Edges=nil then
     SetLength(Edges,Count);

  tmp.ToIndex:=ATo;
  tmp.Direction:=ADirection;

  Insert(tmp,Edges[AFrom],0);
end;

procedure TShortestPath.ClearEdges;
begin
  Edges:=nil;
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
    SetLength(Neighbors,Count);
    SetLength(Distances,Count);
    SetLength(Previous,Count);

    for t:=0 to High(Distances) do
    begin
      Distances[t]:=MaxSingle;
      Previous[t]:=MissingFlag; // -1 = flag empty point index
    end;
  end;

  // True if the AElement index is inside the AItems array
  function Contains(const AItems:TEdges; const AElement:TIndex):Boolean;
  var t : Integer;
  begin
    for t:=Low(AItems) to High(AItems) do
        if (AItems[t].ToIndex=AElement) and
           ( (AItems[t].Direction=TEdgeDirection.BothWays) or
             (AItems[t].Direction=TEdgeDirection.ToWay)
           ) then
        begin
          result:=True;
          Exit;
        end;

    result:=False;
  end;

  // Bidirectional
  function ExistsEdge(const A,B:TIndex):Boolean;
  begin
    result:=Contains(Edges[A],B) or Contains(Edges[B],A);
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

    for v := 0 to Count-1 do
    begin
      if v = u then Continue;

      if (Edges=nil) or ExistsEdge(u,v) then
         d := Distance(u, v)
      else
         Continue;

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

