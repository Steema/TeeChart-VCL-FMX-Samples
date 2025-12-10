## Shortest path algorithm (Dijkstra) with TeeChart

A basic implementation of the [Dijkstra](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) algorithm using TeeChart.

Usage:

```delphi
uses
  TeeShortestPath;

var
  ShortestPath : TShortestPath;

  ShortestPath:=TShortestPath.Create(Series1);
  ShortestPath.MaxDistance:=200;

  AddPath(ShortestPath.Calculate(Start,Finish)); // index of two points

procedure AddPath(const APath:Array of Integer);
var t : Integer;
begin
  Series2.Clear;

  for t in APath do
      AddPoint(Series2,Series1,t);
end;
  
```

### Possible Improvements:

- Supporting "edges" (or "roads") between source points to restrict the path.
- Using other "distance" functions than the default Euclidean
- Adding other path-finding algorithms than Djikstra
- Support for XYZ 3D points
  
##

<img width="1098" height="818" alt="image" src="https://github.com/user-attachments/assets/12a6f708-de62-4d2c-a457-6d5cce15e804" />
