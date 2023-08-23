// DB Dec-2010

unit TeeSubdivideMesh;
{$I TeeDefs.inc}

// Catmull-Clark subdivision
// http://www.rorydriscoll.com/2008/08/01/catmull-clark-subdivision-the-basics

// Pending: Half-Edge data,  Corner, Boundary, Crease, Dart / Spike, vertex...

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, TeeFacesBlock;

type
  TSubDivideMesh=class
  private
    Face : TFacesBlock;
  public
    Constructor Create(const AFace:TFacesBlock);
    Destructor Destroy; override;

    procedure SubDivide;
  end;

  TSubDivideEditor = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeCanvas, TeeBlocks;

{ TSubDivideMesh }

Constructor TSubDivideMesh.Create(const AFace: TFacesBlock);
begin
  inherited Create;
  Face:=AFace;
end;

Destructor TSubDivideMesh.Destroy;
begin
  inherited;
end;

type
  TBlockAccess=class(TCustomBlock);
  
// Pending: weighted, crease and boundary edges.

procedure TSubDivideMesh.SubDivide;

  function GetVertexIndex(AFace,APoint:Integer):Integer;
  begin
    result:=Face.Faces[AFace].Data[APoint].Vertex;
  end;

  function GetVertex(AFace,APoint:Integer):TPoint3DFloat;
  begin
    with Face do
         result:=Geometry.Vertex[GetVertexIndex(AFace,APoint)-1].Vertex;
  end;

var
  Counts : Array of Integer;

  function CalcCentroid(Index:Integer):TPoint3DFloat;
  var t : Integer;
      P : TPoint3DFloat;
      tmp : Double;
  begin
    with result do
    begin
      X:=0;
      Y:=0;
      Z:=0;

      with Face.Geometry,Face.Faces[Index] do
      begin
        tmp:=1/Counts[Index];

        for t:=0 to Counts[Index]-1 do
        begin
          P:=GetVertex(Index,t);

          X:=X+P.X;
          Y:=Y+P.Y;
          Z:=Z+P.Z;
        end;
      end;

      X:=X*tmp;
      Y:=Y*tmp;
      Z:=Z*tmp;
    end;
  end;

  function PreviousPoint(ALength,APoint:Integer):Integer;
  begin
    if APoint=0 then
       result:=ALength-1
    else
       result:=Pred(APoint);
  end;

  function NextPoint(ALength,APoint:Integer):Integer;
  begin
    if APoint=ALength-1 then
       result:=0
    else
       result:=Succ(APoint);
  end;

type
  TVertexFace=packed record
    Face  : Integer;
    Point : Integer;
  end;

var
  VertexFaces : Array of Array of TVertexFace;

  function FindFaceEdge(CurrentFace,P0,P1:Integer; var OtherFaceEdgeP1:Integer):Integer;
  var V0,
      V1  : Integer;
      t   : Integer;
      tmp : Integer;
  begin
    with Face.Faces[CurrentFace] do
    begin
      V0:=Data[P0].Vertex;
      V1:=Data[P1].Vertex;
    end;

    for t:=0 to Length(VertexFaces[V0-1])-1 do
    with VertexFaces[V0-1,t] do
    if (Face<>CurrentFace) then
    begin
      tmp:=NextPoint(Counts[Face],Point);

      if GetVertexIndex(Face,tmp)=V1 then
      begin
        OtherFaceEdgeP1:=tmp;
        result:=Face;

        exit;
      end
      else
      begin
        tmp:=PreviousPoint(Counts[Face],Point);

        if GetVertexIndex(Face,tmp)=V1 then
        begin
          OtherFaceEdgeP1:=tmp;
          result:=Face;

          exit;
        end;
      end;
    end;

    result:=-1;
  end;

type
  TSharedEdge=packed record
    Face  : Integer;
    Point : Integer;
    Vertex : TPoint3DFloat;
    VertexIndex : Integer;
  end;

var
  Centroid : TPoint3DArray;
  SharedEdge: packed Array of Array of TSharedEdge;

  procedure CalcEdgePoint(AFace,APoint:Integer);
  const
    OneThird = 1.0/3.0;

  var OtherPoint : Integer;
      OtherFace  : Integer;
      tmpL       : Integer;
      t          : Integer;
      P          : TPoint3DFloat;
      result     : TPoint3DFloat;
  begin
    tmpL:=Counts[AFace];

    OtherPoint:=NextPoint(tmpL,APoint);

    // Red 2
    OtherFace:=FindFaceEdge(AFace,APoint,OtherPoint,SharedEdge[AFace,APoint].Point);
    SharedEdge[AFace,APoint].Face:=OtherFace;

    result:=GetVertex(AFace,APoint);  // Pink 0

    P:=GetVertex(AFace,OtherPoint);

    // Pink 1 + Red 1 (Centroid)
    with result do
    begin
      X:=X+P.X+Centroid[AFace].X;
      Y:=Y+P.Y+Centroid[AFace].Y;
      Z:=Z+P.Z+Centroid[AFace].Z;
    end;

    // Red 2

    if OtherFace=-1 then
    with result do
    begin
      X:=X*OneThird;
      Y:=Y*OneThird;
      Z:=Z*OneThird;
    end
    else
    begin
      with Centroid[OtherFace] do
      begin
        result.X:=(result.X+X)*0.25;
        result.Y:=(result.Y+Y)*0.25;
        result.Z:=(result.Z+Z)*0.25;
      end;

      if Length(SharedEdge[OtherFace])=0 then
      begin
         SetLength(SharedEdge[OtherFace],Counts[OtherFace]);

         for t:=0 to Counts[OtherFace]-1 do
         with SharedEdge[OtherFace,t] do
         begin
           Face:=-1;
           VertexIndex:=-1;
         end;
      end;

      with SharedEdge[OtherFace,SharedEdge[AFace,APoint].Point] do
      begin
        Face:=AFace;
        Point:=APoint;
        Vertex:=result;
      end;
    end;

    SharedEdge[AFace,APoint].Vertex:=result;
  end;

  function CalcValenceAndAverageFacePoints(AFace,APoint:Integer; var Q:TPoint3DFloat):Integer;
  var t : Integer;
      V : Integer;
      InvResult : Single;
  begin
    result:=0;

    with Q do
    begin
      X:=0;
      Y:=0;
      Z:=0;
    end;

    with Face.Faces[AFace] do
         V:=Data[APoint].Vertex-1;

    for t:=0 to Length(VertexFaces[V])-1 do
    with VertexFaces[V,t] do
    begin
      with Q do
      begin
        X:=X+Centroid[Face].X;
        Y:=Y+Centroid[Face].Y;
        Z:=Z+Centroid[Face].Z;
      end;

      Inc(result);
    end;

    if result<>0 then
    begin
      InvResult:=1/result;

      with Q do
      begin
        X:=X*InvResult;
        Y:=Y*InvResult;
        Z:=Z*InvResult;
      end;
    end;
  end;

  function NextVertexIndex(AFace,ALength,APoint:Integer):Integer;
  begin
    result:=GetVertexIndex(AFace,NextPoint(ALength,APoint));
  end;

  function PreviousVertexIndex(AFace,ALength,APoint:Integer):Integer;
  begin
    result:=GetVertexIndex(AFace,PreviousPoint(ALength,APoint));
  end;

  function AverageMidPoints(AFace,APoint:Integer):TPoint3DFloat;
  var
    Added : Integer;

    procedure AddResult(TheFace,ThePoint:Integer);
    begin
      if Added=0 then
         result:=GetVertex(AFace,APoint)
      else
      with GetVertex(AFace,APoint) do
      begin
        result.X:=result.X+X;
        result.Y:=result.Y+Y;
        result.Z:=result.Z+Z;
      end;

      with GetVertex(TheFace,ThePoint) do
      begin
        result.X:=result.X+X;
        result.Y:=result.Y+Y;
        result.Z:=result.Z+Z;
      end;

      Inc(Added,2);
    end;

  var OtherPoint : Integer;
      OtherPoint2: Integer;
      V          : Integer;
      t          : Integer;
      OtherVertex: Integer;
      tmpInv     : Double;
      tmpL       : Integer;
  begin
    Added:=0;

    tmpL:=Counts[AFace];

    // First edge, previous point
    OtherPoint:=PreviousPoint(tmpL,APoint);
    AddResult(AFace,OtherPoint);

    // Second edge, next point
    OtherPoint2:=NextPoint(tmpL,APoint);
    AddResult(AFace,OtherPoint2);

    // Other faces:

    with Face.Faces[AFace] do
         V:=Data[APoint].Vertex-1;

    OtherVertex:=GetVertexIndex(AFace,OtherPoint);

    for t:=0 to Length(VertexFaces[V])-1 do
    with VertexFaces[V,t] do
    if Face<>AFace then
    begin
      tmpL:=Counts[Face];

      if NextVertexIndex(Face,tmpL,Point)=OtherVertex then
         AddResult(Face,PreviousPoint(tmpL,Point))
      else
      if PreviousVertexIndex(Face,tmpL,Point)=OtherVertex then
         AddResult(Face,NextPoint(tmpL,Point))
    end;

    tmpInv:=1/Added;

    with result do
    begin
      X:=X*tmpInv;
      Y:=Y*tmpInv;
      Z:=Z*tmpInv;
    end;
  end;

  procedure AddVertex(var Data:TFaceVertex; AVertex:Integer);
  begin
    with Data do
    begin
      Vertex:=AVertex;
      Normal:=-1;
    end;
  end;

var t  : Integer;
    tt : Integer;
    tmpL : Integer;
    tmpV : Integer;
    n    : Integer;
    InvN : Double;
    NewPoint : TPoint3DArray;
    Q    : TPoint3DFloat;
    R    : TPoint3DFloat;
    S    : TPoint3DFloat;

    QFactor,
    RFactor,
    SFactor : Single;
    
    tmpNewVertex   : Integer;
    tmpAddingVertex: Integer;

    NewFaces    : TFaces;
    NewFacesCount : Integer;
    NewGeometry : TGeometry;
begin
  NewGeometry:=TGeometry.Create;

  // Copy Geometry (Vertex only)
  with Face.Geometry do
  begin
    NewGeometry.VertexCount:=VertexCount;
    SetLength(NewGeometry.Vertex,VertexCount);

    for t:=0 to VertexCount-1 do
        NewGeometry.Vertex[t].Vertex:=Vertex[t].Vertex;
  end;

  NewFacesCount:=0;

  // Loop all Faces

  with Face do
  begin
    // Calculate Centroids

    SetLength(Centroid,FacesCount);
    SetLength(Counts,FacesCount);

    SetLength(VertexFaces,Geometry.VertexCount);

    for t:=0 to FacesCount-1 do
    begin
      Counts[t]:=Length(Faces[t].Data);
      Centroid[t]:=CalcCentroid(t);

      with Faces[t] do
      for tt:=0 to Length(Data)-1 do
      begin
        tmpV:=Data[tt].Vertex-1;
        tmpL:=Length(VertexFaces[tmpV]);
        SetLength(VertexFaces[tmpV],tmpL+1);

        with VertexFaces[tmpV,tmpL] do
        begin
          Face:=t;
          Point:=tt;
        end;
      end;
    end;

    SetLength(SharedEdge,FacesCount);

    for t:=0 to FacesCount-1 do
    with Faces[t] do
    begin
      tmpL:=Counts[t];

      if Length(SharedEdge[t])=0 then
      begin
        SetLength(SharedEdge[t],tmpL);

        for tt:=0 to tmpL-1 do
        with SharedEdge[t,tt] do
        begin
          Face:=-1;
          VertexIndex:=-1;
        end;
      end;

      SetLength(NewPoint,tmpL);

      for tt:=0 to tmpL-1 do
      begin
        if SharedEdge[t,tt].Face=-1 then
           CalcEdgePoint(t,tt);  // Blue

        n:=CalcValenceAndAverageFacePoints(t,tt,Q);  // Red --> blue

        if n=1 then
           NewPoint[tt]:=GetVertex(t,tt)
        else
        begin
          InvN:=1/n;

          R:=AverageMidPoints(t,tt);  // Yellow

          S:=GetVertex(t,tt);  // Pink

          QFactor:=1*InvN;
          RFactor:=2*InvN;
          SFactor:=1*(n-3)*InvN;

          with NewPoint[tt] do
          begin
            // --> Faces[t].Data[tt].Vertex
            X:=(Q.X*QFactor) + (R.X*RFactor) + (S.X*SFactor);
            Y:=(Q.Y*QFactor) + (R.Y*RFactor) + (S.Y*SFactor);
            Z:=(Q.Z*QFactor) + (R.Z*RFactor) + (S.Z*SFactor);
          end;
        end;
      end;

      // Add new Vertexes:

      // Find new, not shared:

      tmpNewVertex:=0;

      for tt:=0 to tmpL-1 do
      with SharedEdge[t,tt] do
      if (Face<>-1) and (Face<t) then
         VertexIndex:=SharedEdge[Face,Point].VertexIndex
      else
      begin
        Inc(tmpNewVertex);
        VertexIndex:=-1;
      end;

      with NewGeometry do
      begin
        tmpV:=VertexCount;

        SetLength(Vertex,tmpV+1+tmpNewVertex);

        Vertex[tmpV].Vertex:=Centroid[t];

        tmpAddingVertex:=0;

        for tt:=0 to tmpL-1 do
        if SharedEdge[t,tt].VertexIndex=-1 then
        begin
          SharedEdge[t,tt].VertexIndex:=tmpV+1+tmpAddingVertex+1;
          Vertex[SharedEdge[t,tt].VertexIndex-1].Vertex:=SharedEdge[t,tt].Vertex;
          Inc(tmpAddingVertex);
        end;

        for tt:=0 to tmpL-1 do
            Vertex[GetVertexIndex(t,tt)-1].Vertex:=NewPoint[tt];

        Inc(VertexCount,1+tmpNewVertex);
      end;

      // Add new Faces:

      SetLength(NewFaces,NewFacesCount+tmpL);

      for tt:=0 to tmpL-1 do
      with NewFaces[NewFacesCount+tt] do
      begin
        SetLength(Data,4);

        AddVertex(Data[0],GetVertexIndex(t,tt));
        AddVertex(Data[1],SharedEdge[t,tt].VertexIndex);
        AddVertex(Data[2],tmpV+1);

        if tt=0 then
           AddVertex(Data[3],SharedEdge[t,tmpL-1].VertexIndex)
        else
           AddVertex(Data[3],SharedEdge[t,tt-1].VertexIndex);
      end;

      Inc(NewFacesCount,tmpL);

      NewPoint:=nil;
    end;

    for t:=0 to Geometry.VertexCount-1 do
        VertexFaces[t]:=nil;

    VertexFaces:=nil;
    
    for t:=0 to FacesCount-1 do
        SharedEdge[t]:=nil;

    SharedEdge:=nil;
    Counts:=nil;
    Centroid:=nil;
  end;

  // Replace Faces with New ones:

  with Face do
  begin
    // Remove current faces:
    for t:=0 to FacesCount-1 do
        Faces[t].Data:=nil;

    Faces:=nil;

    Faces:=NewFaces;
    FacesCount:=NewFacesCount;
  end;

  // Clear Geometry Normals and Texture coords
  with Face.Geometry do
  begin
    Texture:=nil;
    TextureCount:=0;

    Normals:=nil;
    NormalCount:=0;
  end;

  // Copy NewGeometry

  Face.Geometry.VertexCount:=NewGeometry.VertexCount;
  SetLength(Face.Geometry.Vertex,NewGeometry.VertexCount);

  for t:=0 to NewGeometry.VertexCount-1 do
  with Face.Geometry.Vertex[t] do
  begin
    Vertex:=NewGeometry.Vertex[t].Vertex;

    NormalCount:=0;
    NormalGroups:=nil;
  end;

  NewGeometry.Free;

  TBlockAccess(Face).DeleteLists;
end;

end.
