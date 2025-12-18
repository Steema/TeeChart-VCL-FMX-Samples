## Shortest path algorithm (Dijkstra) with TeeChart

A basic implementation of the [Dijkstra](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) algorithm using TeeChart to display its output.

### Features:

- No dependencies, just the RTL.
- Optional edges (roads) between points, bidirectional or one-way
- Optional weights associated to points, to calculate the "cost" of passing through them.
  
Usage:

```delphi
uses
  TeeShortestPath;

var
  ShortestPath : TShortestPath;

  ShortestPath:=TShortestPath.Create;
  ShortestPath.MaxDistance:=200;

  ...
  ShortestPath.UseEdges:=True;
  ShortestPath.AddEdge(4,5); // add optional "roads" between points
  ...

  // Index of two points: Start --> Finish
  AddPath(ShortestPath.Calculate(Start,Finish));

procedure AddPath(const APath:Array of Integer);
var t : Integer;
begin
  Series2.Clear;

  for t in APath do
      AddPoint(Series2,Series1,t);
end;
  
```

### Possible Improvements:

- Using other "distance" functions than the default Euclidean
- Adding other path-finding algorithms than Djikstra
- Support for XYZ 3D points
- Calculate more than one path, not just the shortest or cheapest
  
##

<img width="1830" height="1300" alt="image" src="https://github.com/user-attachments/assets/dc67551f-f275-468a-b445-74339a3fd677" />



