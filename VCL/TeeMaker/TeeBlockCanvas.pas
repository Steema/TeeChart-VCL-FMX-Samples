{********************************************}
{    TeeChart Pro Block Canvas               }
{ Copyright (c) 2007-2023 by Steema Software }
{       All Rights Reserved                  }
{********************************************}
unit TeeBlockCanvas;
{$I TeeDefs.inc}

interface

uses {$IFNDEF LINUX}
     Windows,
     {$ENDIF}
     Classes,

     {$IFDEF D17}
     System.UITypes,
     {$ENDIF}
 
     {$IFDEF CLX}
     QGraphics,
     {$ELSE}
     Graphics,
     {$ENDIF}
     {$IFDEF D9}
     Types,
     {$ENDIF}
     {$IFDEF BLOCKS}
     Interfaces,
     {$ENDIF}

     TeCanvas, TeeBlocks, TeePipe;

type
  TBlockCanvas = class(TTeeNeutralCanvas)
  private
    { Private declarations }
    FDepth         : Integer;

    FWidth         : Integer;
    FHeight        : Integer;

    { internal }
    FDC            : TTeeCanvasHandle;

    FX             : Double;
    FY             : Double;
    FZ             : Double;
    FIs3D          : Boolean;

    FUseBuffer     : Boolean;
    IDestCanvas    : TCanvas;
    ITransp        : TTeeTransparency;

    IBlocks        : TBlocks;
    IRestoreBlocks : Array of TBlocks;
    IOwner         : TComponent;

    IXOffset       : Double;

    procedure AddBlock(ABlock:TCustomBlock);
    function RectangleBlock(AOwner:TComponent):TRectangleBlock;
    procedure SetFormat(ABlock:TCustomBlock);
    Procedure SetLocationSize(ABlock:TCustomBlock; const Left,Top,Right,Bottom,Z0,Z1:Double);
    procedure SetPipe(P:TPipeBlock);
    procedure SetTitle(ABlock:TCustomBlock; const ATitle:String); {$IFDEF D9}inline;{$ENDIF}
  protected
    { Protected declarations }
    procedure BeginEntity(const Entity:String; const Transform:TTeeTransform=nil); override;
    procedure EndEntity; override;

    procedure InternalCylinder(Vertical:Boolean; Left,Top,Right,Bottom,
                        Z0,Z1:Integer; Dark3D:Boolean; ConePercent:Integer;
                        const Side1:TTeeBrush=nil; const Side2:TTeeBrush=nil);

    { 2d }
    Function GetHandle:TTeeCanvasHandle; override;
    Function GetIsNoBMPGrid:Boolean; override;
    Function GetPixel(x,y:Integer):TColor; override;
    function GetPixel3D(X,Y,Z:Integer): TColor; override;
    Function GetSupports3DText:Boolean; override;
    Function GetSupportsFullRotation:Boolean; override;
    Function GetSupportsXORMode:Boolean; override;
    Function GetUseBuffer:Boolean; override;

    procedure SetPixel(X, Y: Integer; Value: TColor); override;
    procedure SetPixel3D(X,Y,Z:Integer; Value: TColor); override;
    Procedure SetUseBuffer(Value:Boolean); override;
    procedure SmoothShadow(const Shadow:TTeeShadow; const Rect:TRect;
                             Ellipse:Boolean; RoundSize:Integer;
                             const P:Array of TPoint;
                             DonutPercent:Integer=0;
                             const StartAngle:Double=0;
                             const EndAngle:Double=360); override;
  public
    BeveledCubes : Boolean;
    LinesAsPipes : Boolean;
    PieTorus     : Boolean;
    TextAsPictures : Boolean;
    TextDepth      : Double;

    { Public declarations }
    Constructor CreateBlocks(const ABlocks:TBlocks);

    class function Description:String; override;
    Procedure Repaint;

    { 3d }
    Procedure DisableRotation; override;
    Procedure EnableRotation; override;

    Function BeginBlending(const R:TRect; Transparency:TTeeTransparency):TTeeBlend; override;
    procedure EndBlending(const Blend:TTeeBlend); override;

    procedure Arc(const Left, Top, Right, Bottom, StartX, StartY, EndX, EndY: Integer); override;
    procedure Donut( XCenter,YCenter,XRadius,YRadius:Integer;
                     Const StartAngle,EndAngle,HolePercent:Double); override;
    procedure Draw(X, Y: Integer; const Graphic: TGraphic); override;
    procedure EraseBackground(const Rect: TRect); override;
    procedure Ellipse(const X1, Y1, X2, Y2: Integer); override;
    procedure FillRect(const Rect: TRect); override;
    procedure LineTo(const X,Y:Integer); override;
    procedure MoveTo(const X,Y:Integer); override;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
    procedure Rectangle(const X0,Y0,X1,Y1:Integer); override;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); override;
    procedure StretchDraw(const Rect: TRect; const Graphic: TGraphic); override;
    Procedure TextOut(X,Y:Integer; const Text:String); override;

    procedure ClipRectangle(Const Rect:TRect); override;
    procedure ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer); override;

    Procedure GradientFill( Const Rect:TRect;
                            StartColor,EndColor:TColor;
                            Direction:TGradientDirection;
                            Balance:Integer=50;
                            RadialX:Integer=0;
                            RadialY:Integer=0); override;
    Procedure Invalidate; override;
    procedure Polyline({$IFDEF D6}const{$ENDIF} Points:Array of TPoint); override;
    Procedure Polygon({$IFDEF D6}const{$ENDIF} Points:Array of TPoint); override;
    procedure RotateLabel(x,y:Integer; Const St:String; RotDegree:Double; AllowHTML:Boolean=False); override;
    procedure UnClipRectangle; override;

    { 3d }
    function Calculate2DPosition(const x,y,z:Double):TPoint; overload;
    Procedure Calculate2DPosition(Var x,y:Integer; z:Integer); overload; override;
    Function Calculate3DPosition(x,y,z:Integer):TPoint; override;
    Function InitWindow( const DestCanvas:TCanvas;
                         const A3DOptions:TView3DOptions;
                         ABackColor:TColor;
                         Is3D:Boolean;
                         Const UserRect:TRect):TRect; override;
    Procedure Projection(MaxDepth:Integer; const Bounds,Rect:TRect); override;
    Procedure ShowImage(const DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect); override;
    Function ReDrawBitmap:Boolean; override;

    Procedure Arrow( Filled:Boolean;
                     Const FromPoint,ToPoint:TPoint;
                     ArrowWidth,ArrowHeight,Z0,Z1:Integer;
                     const ArrowPercent:Double); override;

    procedure Cone( Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer;
                    Dark3D:Boolean; ConePercent:Integer); override;
    Procedure Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean=True;
                   RoundSize:Integer=0); overload; override;
    Procedure Cube(Left,Right,Top,Bottom,Z0,Z1,BevelSize:Integer; DarkSides:Boolean=True;
                   RoundSize:Integer=0); overload; override;
    Procedure Cube(Left,Right,Top,Bottom,Z0,Z1:Double; DarkSides:Boolean); overload;

    procedure Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; DarkCover:Boolean); override;
    procedure Diamond(const Left,Top,Right,Bottom:TCoordinate; Z0,Z1:Integer); override;
    procedure EllipseWithZ(const X1, Y1, X2, Y2, Z:Integer); override;
    procedure Line(const A,B:TPoint3DFloat);
    procedure LineTo3D(const X,Y,Z:Integer); override;
    Procedure LineWithZ(X0,Y0,X1,Y1,Z:Integer); override;
    procedure MoveTo3D(const X,Y,Z:Integer); override;

    procedure Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                      Const StartAngle,EndAngle:Double;
                      DarkSides,DrawSides:Boolean;
                      DonutPercent:Integer=0;
                      Gradient:TCustomTeeGradient=nil;
                      BevelPercent:Integer=0;
                      EdgeStyle:TEdgeStyle=edNone;
                      Transparency:TTeeTransparency=0;
                      BevelBright:Integer=0;
                      BevelBorder:Boolean=True;
                      HideSides:Boolean=False);  override;

    procedure Plane3D(Const A,B:TPoint; Z0,Z1:Integer); override;
    procedure PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer); override;
    procedure PlaneFour3D(Var Points:TFourPoints; Z0,Z1:Integer); override;
    procedure Polygon3DFloat(const Points: array of TPoint3DFloat); overload;
    procedure Polygon3D(const Points: array of TPoint3D); overload; override;
    procedure PolygonWithZ(const Points: array of TPoint; Z:Integer); override;
    procedure Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean); override;
    Procedure PyramidTrunc(Const R:TRectF; const StartZ,EndZ,TruncX,TruncZ:Double;
                           TopCover:Boolean=True; BottomCover:Boolean=True); override;
    Procedure RectangleWithZ(Const Rect:TRect; Z:Integer); override;
    Procedure RectangleY(const Left,Top,Right,Z0,Z1:Integer); override;
    Procedure RectangleZ(const Left,Top,Bottom,Z0,Z1:Integer); override;
    procedure RotateLabel3D(x,y,z:Integer;
                            const St:String; RotDegree:Double;
                            AllowHTML:Boolean=False); override;
    procedure RoundRect(Const R:TRect; X,Y,Z0,Z1:Integer); override;
    procedure Sphere(x,y,z:Integer; Const Radius:Double); overload; override;
    procedure Sphere(const x,y,z,Radius:Double); overload;
    procedure StretchDraw(const Rect:TRect; const Graphic: TGraphic;
                          const Pos:Double; Plane:TCanvas3DPlane=cpZ); override;
    procedure Surface3D( Style:TTeeCanvasSurfaceStyle;
                         SameBrush,CreateCached,AddNormals:Boolean;
                         NumXValues,NumZValues:Integer;
                         const CalcPoints:TTeeCanvasCalcPoints;
                         var Cached:TTeeCachedObject); override;
    procedure Tetrahedron(Left,Top,Right,Bottom,z0,z1:Double);
    Procedure TextOut3D(X,Y,Z:Integer; const Text:String; AllowHTML:Boolean=False); override;
    procedure Triangle3D(Const Points:TTrianglePoints3D; Const Colors:TTriangleColors3D); override;
    procedure TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer); override;
    procedure TriSurface(NumTriangles:Integer; CreateCached,AddNormals:Boolean;
                        const Blend:TTeeBlend; Transparency:TTeeTransparency;
                        const CalcPoints:TTeeCanvasTriPoints;
                        var Cached:TTeeCachedObject); override;
  end;

implementation

uses
  Math, TeeRoundRect, TeeExtruded, TeeTerrain, TeeMesh;

{ TBlockCanvas }

Constructor TBlockCanvas.CreateBlocks(const ABlocks:TBlocks);
begin
  Create;

  SupportsID:=True;
  FUseBuffer:=True;
  TextAlign:=TA_LEFT;

  IBlocks:=ABlocks;
  SetLength(IRestoreBlocks,1);
  IRestoreBlocks[0]:=IBlocks;
end;

class function TBlockCanvas.Description:String;
begin
  result:='3D Blocks';  // TeeMsg_3DBlocks
end;

function TBlockCanvas.Calculate2DPosition(const x,y,z:Double):TPoint;
begin
  result.X:=Round(X);
  result.Y:=Round(Y);
end;

Procedure TBlockCanvas.Calculate2DPosition(Var x,y:Integer; z:Integer);
var P : TPoint;
begin
  P:=Calculate2DPosition(1.0*x,y,z);
  x:=P.X;
  y:=P.Y;
end;

Function TBlockCanvas.Calculate3DPosition(x,y,z:Integer):TPoint;
begin
  result.x:=x;
  result.y:=y;
end;

Procedure TBlockCanvas.Projection(MaxDepth:Integer; const Bounds,Rect:TRect);
begin
  RectSize(Bounds,FWidth,FHeight);
  RectCenter(Rect,FXCenter,FYCenter);
  FDepth:=MaxDepth;
end;

Procedure TBlockCanvas.Cube(Left,Right,Top,Bottom,Z0,Z1:Integer;
                            DarkSides:Boolean=True; RoundSize:Integer=0);
var tmp : Double;
begin
  tmp:=Left;
  Cube(tmp,Right,Top,Bottom,Z0,Z1,DarkSides {,RoundSize});
end;

Procedure TBlockCanvas.Cube(Left,Right,Top,Bottom,Z0,Z1,BevelSize:Integer;
                            DarkSides:Boolean=True; RoundSize:Integer=0);
var // tmp : Double;
    bevelCube : TBeveledCubeBlock;
begin
  //tmp:=Left;

  bevelCube:=TBeveledCubeBlock.Create(IOwner);
  bevelCube.Style:=bsBevel;
  bevelCube.BevelSize.Value:=BevelSize;

  SetFormat(bevelCube);
  SetLocationSize(bevelCube,Left,Top,Right,Bottom,Z0,Z1);
  bevelCube.Format.Border.Visible:=False;
  AddBlock(bevelCube);
end;

Procedure TBlockCanvas.SetLocationSize(ABlock:TCustomBlock; const Left,Top,Right,Bottom,Z0,Z1:Double);
begin
  with ABlock do
  begin
    with Location.Point do
    begin
      X:=((Left+Right)*0.5)-IXOffset;
      Y:=(Z0+Z1)*0.5;
      Z:=FHeight-(Top+Bottom)*0.5;
    end;

    with Size.Point do
    begin
      X:=Abs(Right-Left);
      Y:=Abs(Z1-Z0);
      Z:=Abs(Bottom-Top);
    end;
  end;
end;

procedure TBlockCanvas.SetTitle(ABlock:TCustomBlock; const ATitle:String);
begin
  ABlock.Title:=ATitle;
end;

Procedure TBlockCanvas.Cube(Left,Right,Top,Bottom,Z0,Z1:Double; DarkSides:Boolean);
var tmp : TCustomBlock;
begin
  if BeveledCubes then
     tmp:=TBeveledCubeBlock.Create(IOwner)
  else
     tmp:=TCubeBlock.Create(IOwner);

  SetTitle(tmp,'Cube');

  SetFormat(tmp);
  SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);
  AddBlock(tmp);
end;

Function TBlockCanvas.InitWindow( const DestCanvas:TCanvas;
                               const A3DOptions:TView3DOptions;
                               ABackColor:TColor;
                               Is3D:Boolean;
                               Const UserRect:TRect):TRect;

begin
  FBounds:=UserRect;
  RectSize(Bounds,FWidth,FHeight);

  IXOffset:=0;

  FX:=0;
  FY:=0;
  FIs3D:=Is3D;                               

  if Assigned(A3DOptions) then
     FontZoom:=A3DOptions.FontZoom;

  if (IDestCanvas<>DestCanvas) or (View3DOptions<>A3DOptions) then
  begin
    IDestCanvas:=DestCanvas;
    View3DOptions:=A3DOptions;

    FDC:=DestCanvas.Handle;
  end;

  SetCanvas(DestCanvas);

  BackColor:=ABackColor;
  result:=UserRect;

  IBlocks.Clear;
end;

Procedure TBlockCanvas.ShowImage(const DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect);
begin
  SetCanvas(DefaultCanvas);
end;

Function TBlockCanvas.ReDrawBitmap:Boolean;
begin
  result:=False;
end;

procedure TBlockCanvas.Rectangle(const X0,Y0,X1,Y1:Integer);
var tmp : TCustomBlock;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'Rectangle');
  SetFormat(tmp);

  SetLocationSize(tmp,X0,Y0,X1,Y1,0,0);
  AddBlock(tmp);
end;

procedure TBlockCanvas.MoveTo(const X, Y: Integer);
begin
  FX:=X;
  FY:=Y;
end;

procedure TBlockCanvas.Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean);
var tmp : TCustomBlock;
begin
  tmp:=TPyramidBlock.Create(IOwner);

  SetTitle(tmp,'Pyramid');
  SetFormat(tmp);

  with TPyramidBlock(tmp) do
  begin
    Side1.Point.X:=100;
    Side1.Point.Y:=100;
    Side2.Point.X:=100;
    Side2.Point.Y:=100;
  end;

  if Vertical and (Top>Bottom) then
     tmp.Rotation.Y:=180;  // Inverted Pyramid

  SetLocationSize(tmp,Left,Top,Right,Bottom,z0,z1);
  AddBlock(tmp);
end;

procedure TBlockCanvas.Tetrahedron(Left,Top,Right,Bottom,z0,z1:Double);
var tmp : TCustomBlock;
begin
  tmp:=TTetrahedronBlock.Create(IOwner);
  SetTitle(tmp,'Tetrahedron');

  SetLocationSize(tmp,Left,Top,Right,Bottom,z0,z1);
  AddBlock(tmp);
end;

procedure TBlockCanvas.InternalCylinder(Vertical:Boolean;
                     Left,Top,Right,Bottom,Z0,Z1:Integer; Dark3D:Boolean;
                     ConePercent:Integer;
                     const Side1:TTeeBrush=nil;
                     const Side2:TTeeBrush=nil);

var tmp : TCustomBlock;
begin
  if ConePercent=100 then
     tmp:=TCylinderBlock.Create(IOwner)
  else
  begin
    tmp:=TConeBlock.Create(IOwner);
    TConeBlock(tmp).ConeSize.Point:=PointFloat(100-ConePercent,100-ConePercent);
  end;

  if ConePercent=100 then
     SetTitle(tmp,'Cylinder')
  else
     SetTitle(tmp,'Cone');

  if not Vertical then
     tmp.Rotation.Y:=90
  else
  if (ConePercent<>100) and (Top>Bottom) then
     tmp.Rotation.Y:=180; // Inverted Cone

  SetFormat(tmp);
  
  SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);
  AddBlock(tmp);
end;

procedure TBlockCanvas.Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; DarkCover:Boolean);
begin
  InternalCylinder(Vertical,Left,Top,Right,Bottom,Z0,Z1,DarkCover,100);
end;

procedure TBlockCanvas.Cone(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; Dark3D:Boolean; ConePercent:Integer);
begin
  InternalCylinder(Vertical,Left,Top,Right,Bottom,Z0,Z1,Dark3D,ConePercent);
end;

procedure TBlockCanvas.Sphere(const x,y,z,Radius:Double);
var tmp : TCustomBlock;
begin
  tmp:=TSphereBlock.Create(IOwner);

  SetFormat(tmp);
  SetTitle(tmp,'Sphere');

  SetLocationSize(tmp,x-Radius,y-Radius,x+Radius,y+Radius,z-Radius,z+Radius);
  AddBlock(tmp);
end;

procedure TBlockCanvas.Sphere(x,y,z:Integer; Const Radius:Double);
begin
  Sphere(1.0*x,y,z,Radius);
end;

procedure TBlockCanvas.SmoothShadow(const Shadow:TTeeShadow; const Rect:TRect;
                             Ellipse:Boolean; RoundSize:Integer;
                             const P:Array of TPoint;
                             DonutPercent:Integer=0;
                             const StartAngle:Double=0;
                             const EndAngle:Double=360);
var tmp : TCustomBlock;
begin
  if Ellipse then
     if (DonutPercent=0) and (EndAngle-StartAngle=360) then
        tmp:=TEllipseBlock.Create(IOwner)
     else
     begin
       tmp:=TPieSliceBlock.Create(IOwner);
       TPieSliceBlock(tmp).StartAngle:=StartAngle;
       TPieSliceBlock(tmp).Angle:=EndAngle-StartAngle;
       TPieSliceBlock(tmp).DonutPercent:=DonutPercent;
     end
  else
     tmp:=TRectangleBlock.Create(IOwner);

  SetTitle(tmp,'Shadow');
  SetFormat(tmp);

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,Shadow.ZPosition,Shadow.ZPosition);

  tmp.Format.Texture.PictureAlpha:=True;
  tmp.Format.Texture.PictureLink:=TeeMakerLibraryTag+'Transparent\Shadow.bmp';

  tmp.Format.Texture.PictureTransparent:=True;
  tmp.Format.Transparency:=1+Shadow.Transparency;
  tmp.Format.Color:=clBlack;
  tmp.Format.Solid:=True;
  tmp.Format.Border.Visible:=False;

  AddBlock(tmp);
end;

procedure TBlockCanvas.StretchDraw(const Rect:TRect; const Graphic: TGraphic;
                          const Pos:Double; Plane:TCanvas3DPlane=cpZ);
var tmp : TCustomBlock;
begin
  tmp:=TImageBlock.Create(IOwner);
  SetTitle(tmp,'Image');
  SetFormat(tmp);
  tmp.Format.Texture.Picture.Graphic:=Graphic;

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,Pos,Pos);

  if Plane=cpY then
     tmp.Rotation.Y:=90
  else
  if Plane=cpX then
     tmp.Rotation.x:=90;

  AddBlock(tmp);
end;

procedure TBlockCanvas.SetFormat(ABlock:TCustomBlock);
begin
  with ABlock.Format do
  begin
    Solid:=Self.Brush.Style<>bsClear;
    Color:=Self.Brush.Color;

    if Assigned(Self.Brush.Bitmap) then
       Texture.Picture.Graphic:=Self.Brush.Bitmap;

    Border.Visible:=Self.Pen.Style<>psClear;
    Border.Color:=Self.Pen.Color;
    Border.Style:=Self.Pen.Style;
    Border.Width:=Self.Pen.Width;

    Transparency:=ITransp;
  end;
end;

procedure CalcMinMax(const Points: TPointCollection;
                     var AMin,AMax : TPoint3DFloat);
var t : Integer;
begin
  AMin:=Points.Point[0].Point.Point;
  AMax:=AMin;

  for t:=1 to Points.Count-1 do
  with Points[t].Point.Point do
  begin
    if X<AMin.X then AMin.X:=X else
    if X>AMax.X then AMax.X:=X;

    if Y<AMin.Y then AMin.Y:=Y else
    if Y>AMax.Y then AMax.Y:=Y;

    if Z<AMin.Z then AMin.Z:=Z else
    if Z>AMax.Z then AMax.Z:=Z;
  end;
end;

procedure TBlockCanvas.SetPipe(P:TPipeBlock);
var AMin,
    AMax : TPoint3DFloat;
    tmpDist : Double;
begin
  if LinesAsPipes then
  begin
    P.Format.Border.Visible:=False;
    P.Format.Texture.PictureLink:=TeeMakerLibraryTag+'Basic\rocks.bmp';
    P.Format.Solid:=True;

    P.Radius.Point.X:=1+Pen.Width;
    P.Radius.Point.Y:=1+Pen.Width;
  end
  else
  begin
    P.Radius.Point.X:=0;
    P.Radius.Point.Y:=0;

    P.Format.Solid:=False;
  end;

  CalcMinMax(P.Points,AMin,AMax);

  with P.Location.Point do
  begin
    X:=(AMin.X+AMax.X)*0.5;
    Y:=(AMin.Y+AMax.Y)*0.5;
    Z:=(AMin.Z+AMax.Z)*0.5;
  end;

  with P.Size.Point do
  begin
    X:=Max(1,Abs(AMax.X-AMin.X));
    Y:=Max(1,Abs(AMax.Y-AMin.Y));
    Z:=Max(1,Abs(AMax.Z-AMin.Z));
  end;

  if LinesAsPipes then
  begin
    P.Size.Point.Y:=1+Pen.Width;

    with P.Size.Point do
         tmpDist:=Sqrt(Sqr(X)+Sqr(Y)+Sqr(Z));

    P.Format.Texture.Scale.Point.Y:=tmpDist*0.1;
  end;
end;

procedure TBlockCanvas.LineTo(const X, Y: Integer);
var tmp : TPipeBlock;
begin
  tmp:=TPipeBlock.Create(IOwner);
  SetTitle(tmp,'Line');
  SetFormat(tmp);

  tmp.Points.Add(FX,0,FHeight-FY);
  tmp.Points.Add(X-IXOffset,0,FHeight-Y);

  SetPipe(tmp);

  AddBlock(tmp);

  FX:=X-IXOffset;
  FY:=Y;
end;

procedure TBlockCanvas.ClipRectangle(Const Rect:TRect);
begin
end;

procedure TBlockCanvas.ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer);
begin
end;

procedure TBlockCanvas.UnClipRectangle;
begin
end;

Function TBlockCanvas.GetIsNoBMPGrid:Boolean;
begin
  result:=True;
end;

Function TBlockCanvas.GetPixel(x,y:Integer):TColor;
begin
  result:=clWhite; //?
end;

procedure TBlockCanvas.StretchDraw(const Rect: TRect; const Graphic: TGraphic);
begin
  StretchDraw(Rect,Graphic,0);
end;

procedure TBlockCanvas.Draw(X, Y: Integer; const Graphic: TGraphic);
begin
  if Assigned(Graphic) then
     StretchDraw(TeeRect(X,Y,X+Graphic.Width,Y+Graphic.Height),Graphic);
end;

Procedure TBlockCanvas.GradientFill( Const Rect:TRect;
                                  StartColor,EndColor:TColor;
                                  Direction:TGradientDirection;
                                  Balance:Integer=50;
                                  RadialX:Integer=0;
                                  RadialY:Integer=0);
var tmp  : TGradientBlock;
    tmpZ : Integer;
begin
  tmp:=TGradientBlock.Create(IOwner);
  SetTitle(tmp,'Gradient');

  tmp.Format.Border.Visible:=False;

  tmp.Gradient.Direction:=Direction;
  tmp.Gradient.StartColor:=StartColor;
  tmp.Gradient.EndColor:=EndColor;

  tmpZ:=GradientZ;
  if tmpZ=0 then
     tmpZ:=100;

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,tmpZ,tmpZ);

  AddBlock(tmp);
end;

Procedure TBlockCanvas.RectangleY(const Left,Top,Right,Z0,Z1:Integer);
var tmp  : TCustomBlock;
    midY : Double;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'Rectangle Y');

  tmp.Rotation.Y:=90;

  SetFormat(tmp);

  midY:=Abs(Z1-Z0)*0.5;
  SetLocationSize(tmp,Left,Top-midY,Right,Top+midY,Z0,Z1);

  tmp.Size.Y:=0;

  AddBlock(tmp);
end;

function TBlockCanvas.RectangleBlock(AOwner:TComponent):TRectangleBlock;
begin
  if Gradient.Visible then
  begin
    result:=TGradientBlock.Create(AOwner);
    TGradientBlock(result).Gradient:=Gradient;
  end
  else
    result:=TRectangleBlock.Create(AOwner);
end;

Procedure TBlockCanvas.RectangleWithZ(Const Rect:TRect; Z:Integer);
var tmp : TCustomBlock;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'Rectangle');
  SetFormat(tmp);

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,Z,Z);

  tmp.Size.Y:=0;

  AddBlock(tmp);
end;

Procedure TBlockCanvas.RectangleZ(const Left,Top,Bottom,Z0,Z1:Integer);
var tmp  : TCustomBlock;
    midX : Double;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'Rectangle Z');

  tmp.Rotation.X:=90;

  SetFormat(tmp);

  midX:=Abs(Z1-Z0)*0.5;

  SetLocationSize(tmp,Left-midX,Top,Left+midX,Bottom,Z0,Z1);

  AddBlock(tmp);
end;

procedure TBlockCanvas.FillRect(const Rect: TRect);
var tmp : TCustomBlock;
begin
  if Brush.Style<>bsClear then
  begin
    tmp:=RectangleBlock(IOwner);
    SetTitle(tmp,'FillRect');
    SetFormat(tmp);
    tmp.Format.Border.Visible:=False;

    with Rect do
         SetLocationSize(tmp,Left,Top,Right,Bottom,0,0);

    AddBlock(tmp);
  end;
end;

procedure TBlockCanvas.Ellipse(const X1, Y1, X2, Y2: Integer);
begin
  EllipseWithZ(X1,Y1,X2,Y2,0);
end;

procedure TBlockCanvas.EllipseWithZ(const X1, Y1, X2, Y2, Z: Integer);
var tmp : TCustomBlock;
begin
  tmp:=TEllipseBlock.Create(IOwner);
  SetTitle(tmp,'Ellipse');
  SetFormat(tmp);
  SetLocationSize(tmp,X1,Y1,X2,Y2,Z,Z);
  AddBlock(tmp);
end;

Procedure TBlockCanvas.EnableRotation;
begin
end;

Procedure TBlockCanvas.DisableRotation;
begin
end;

function TBlockCanvas.GetPixel3D(X,Y,Z:Integer): TColor;
begin
  result:=clWhite; //?
end;

procedure TBlockCanvas.SetPixel3D(X,Y,Z:Integer; Value: TColor);
begin
{
  if Pen.Style<>psClear then
  begin
    glBegin(GL_POINTS);
    SetColor(Value);
    TeeVertex3D(X,Y,Z);
    glEnd;
    Assert(CheckGLError,'Pixel3D');
  end;
  }
end;

procedure TBlockCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
{
  if Pen.Style<>psClear then
  begin
    glBegin(GL_POINTS);
    SetColor(Value);
    TeeVertex2D(X,Y);
    glEnd;
    Assert(CheckGLError,'Pixel');
  end;
  }
end;

procedure TBlockCanvas.AddBlock(ABlock: TCustomBlock);
begin
  Self.IBlocks.Add(ABlock);
end;

procedure TBlockCanvas.Arc(const Left, Top, Right, Bottom, StartX, StartY, EndX, EndY: Integer);
var tmp : TPieSliceBlock;
begin
  tmp:=TPieSliceBlock.Create(IOwner);
  SetTitle(tmp,'Arc 2D');
  SetFormat(tmp);
  SetLocationSize(tmp,Left,Top,Right,Bottom,0,0);
  AddBlock(tmp);
end;

Function TBlockCanvas.BeginBlending(const R:TRect; Transparency:TTeeTransparency):TTeeBlend;
begin
  ITransp:=Transparency;
  result:=nil;
end;

procedure TBlockCanvas.EndBlending(const Blend:TTeeBlend);
begin
  ITransp:=0;
end;

procedure TBlockCanvas.Donut( XCenter,YCenter,XRadius,YRadius:Integer;
                              const StartAngle,EndAngle,HolePercent:Double);
begin
  Pie3D(XCenter,YCenter,XRadius,YRadius,0,0,StartAngle,EndAngle,True,True,Round(HolePercent));
end;

procedure TBlockCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var Step, XC,YC : Single;
    Theta,Theta2  : Single;
    tmpSin,tmpCos :  {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    t : Integer;
    tmpX, tmpY    : Single;
    P : Array of TPoint;
begin
  CalcPieAngles(X1,Y1,X2,Y2,X3,Y3,X4,Y4,Theta,Theta2);

  Step:=(Theta2-Theta)/(NumCirclePoints-1);

  XC:=(X2+X1)*0.5;
  YC:=(Y2+Y1)*0.5;

  SetLength(P,NumCirclePoints);

  P[0].X:=Round(XC);
  P[0].Y:=Round(YC);

  tmpX:=XC-X1;
  tmpY:=YC-Y1;

  for t:=1 to NumCirclePoints-1 do
  begin
    SinCos(((Pi*0.5)+Theta+(t*Step))*TeePiStep,tmpSin,tmpCos);
    P[t].X:=Round(XC+(tmpX*tmpCos+tmpY*tmpSin));
    P[t].Y:=Round(YC+(-tmpX*tmpSin+tmpY*tmpCos));
  end;

  Polygon(P);

  P:=nil;
end;

procedure TBlockCanvas.Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                        const StartAngle,EndAngle:Double;
                        DarkSides,DrawSides:Boolean;
                        DonutPercent:Integer;
                        Gradient:TCustomTeeGradient;
                        BevelPercent:Integer;
                        EdgeStyle:TEdgeStyle;
                        Transparency:TTeeTransparency;
                        BevelBright:Integer;
                        BevelBorder:Boolean;
                        HideSides:Boolean);

var tmpP : TPieSliceBlock;
//    tmpZ : Double;
    tmpA : Double;
    tmpT : TTorusBlock;
    tmp  : TCustomBlock;
begin
  tmpT:=nil;
  tmpP:=nil;

  if PieTorus then
  begin
    tmpT:=TTorusBlock.Create(IOwner);
    tmp:=tmpT;
  end
  else
  begin
    tmpP:=TPieSliceBlock.Create(IOwner);
    tmp:=tmpP;
  end;

  SetTitle(tmp,'Pie 3D');
  SetFormat(tmp);
  SetLocationSize(tmp,XCenter-XRadius,YCenter-YRadius,XCenter+XRadius,YCenter+YRadius,Z0,Z1);

  //tmpZ:=Abs(Z1-Z0)*0.5;

  if PieTorus then
  begin
    tmp.Format.Border.Visible:=False;

    tmpT.Radius.X:=50+0.5*DonutPercent;
    tmpT.Radius.Y:=tmpT.Radius.X;
  end
  else
  begin
    tmp.Rotation.Y:=270;
    tmp.Scale.Y:=tmp.Size.Y/tmp.Size.Z;
    tmp.Scale.Z:=1/tmp.Scale.Y;
  end;

  if PieTorus then
     tmpT.TotalAngle:=((EndAngle-StartAngle)*180/Pi)
  else
     tmpP.Angle:=((EndAngle-StartAngle)*180/Pi);

  tmpA:=90+(StartAngle*180/Pi);

  if PieTorus then
     tmpT.StartAngle:=90+360-tmpA-tmpT.TotalAngle
  else
  begin
    tmpP.StartAngle:=90+360-tmpA-tmpP.Angle;
    tmpP.DonutPercent:=DonutPercent;

    tmpP.Edges.OuterTop.X:=20;
    tmpP.Edges.OuterTop.Y:=20;

    tmpP.Edges.InnerTop.X:=20;
    tmpP.Edges.InnerTop.Y:=20;

    tmpP.Edges.OuterBottom.X:=20;
    tmpP.Edges.OuterBottom.Y:=20;

    tmpP.Edges.InnerBottom.X:=20;
    tmpP.Edges.InnerBottom.Y:=20;
  end;

  AddBlock(tmp);
end;

procedure TBlockCanvas.Polyline({$IFDEF D6}const{$ENDIF} Points:Array of TPoint);
var Count : Integer;
    t     : Integer;
    tmp   : TPipeBlock;
begin
  Count:=Length(Points);

  if Count>0 then
  begin
    tmp:=TPipeBlock.Create(IOwner);
    SetTitle(tmp,'Polyline');
    SetFormat(tmp);

    for t:=0 to Count-1 do
        tmp.Points.Add(Points[t].X-IXOffset,0,FHeight-Points[t].Y);

    with Points[Count-1] do
    begin
      FX:=X-IXOffset;
      FY:=0;
      FZ:=Y;
    end;

    SetPipe(tmp);

    AddBlock(tmp);
  end;
end;

procedure TBlockCanvas.Polygon({$IFDEF D6}const{$ENDIF} Points: Array of TPoint);
begin
  PolygonWithZ(Points,0);
end;

procedure TBlockCanvas.PlaneFour3D(Var Points:TFourPoints; Z0,Z1:Integer);

{    With Points[0] do TeeVertex3D(x,y,z0);
    With Points[1] do TeeVertex3D(x,y,z0);
    With Points[2] do TeeVertex3D(x,y,z1);
    With Points[3] do TeeVertex3D(x,y,z1);}

var tmp : TCustomBlock;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'PlaneFour3D');
  SetLocationSize(tmp,0,0,0,0,Z0,Z1);
  AddBlock(tmp);
end;

procedure TBlockCanvas.Polygon3D(const Points: Array of TPoint3D);

{  Procedure AddPoints;
  var t : Integer;
  begin
    for t:=Low(Points) to High(Points) do
    With Points[t] do
    begin
      TeeNormal(x,y,z);
      glTexCoord3i(x,y,z);
      TeeVertex3D(x,y,z);
    end;
  end;
}
begin
{  if Brush.Style<>bsClear then
  begin
    SetColor(Brush.Color);
    SetBrushBitmap;
    glBegin(GL_POLYGON);
    AddPoints;
    glEnd;
    EndBrushBitmap;
  end;

  if Pen.Style<>psClear then
  begin
    SetPen;
    glBegin(GL_LINE_LOOP);
    AddPoints;
    glEnd;
  end;

  Assert(CheckGLError,'Polygon3D');}
end;

procedure TBlockCanvas.Polygon3DFloat(const Points: Array of TPoint3DFloat);
var t : Integer;
    tmp : TExtrudedBlock;
begin
  if Length(Points)>0 then
  begin
    tmp:=TExtrudedBlock.Create(IOwner);
    SetTitle(tmp,'Polygon3D');

    for t:=Low(Points) to High(Points)-1 do
    with Points[t] do
         tmp.Points.Add(x,y,z);

    AddBlock(tmp);
  end;
end;

procedure TBlockCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  RoundRect(TeeRect(X1,Y1,X2,Y2),X3,Y3,0,0);
end;

procedure TBlockCanvas.RoundRect(const R:TRect; X,Y,Z0,Z1:Integer);
var tmp : TRoundRectBlock;
begin
  tmp:=TRoundRectBlock.Create(IOwner);
  SetTitle(tmp,'Round Rect.');

  with R do
  SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);

  tmp.Corners.RoundSize:=PointFloat(X,Y);

  AddBlock(tmp);
end;

Procedure TBlockCanvas.Repaint;
begin
  if Assigned(View3DOptions) then
     View3DOptions.Repaint;
end;

Procedure TBlockCanvas.Invalidate;
begin
end;

Procedure TBlockCanvas.TextOut3D(X,Y,Z:Integer; const Text:String; AllowHTML:Boolean=False);
var tmpS : TSize;

  function AddText(const ATitle:String; AX,AY,AZ:Integer; AColor:TColor;
                   UsePicture:Boolean):TeeBlocks.TTeeTextBlock;
  var tmpLength : Integer;
      tmpAlign  : Integer;
      P         : TPointFloat;
      tmpExtrusion : Double;
      tmpH      : Double;
  begin
    result:=TeeBlocks.TTeeTextBlock.Create(IOwner);
    SetTitle(result,ATitle);
    result.Text:=Text;

    tmpLength:=Length(Text);

    P.X:=AX;
    P.Y:=AY-(tmpS.cy*0.2);

    tmpAlign:=TextAlign;
    if tmpAlign>=TA_BOTTOM then
       Dec(tmpAlign,TA_BOTTOM)
    else
       P.y:=P.y+(0.7*tmpS.Cy);

    if tmpAlign=TA_CENTER then
       P.x:=P.x-(0.55*tmpS.Cx)
    else
    if tmpAlign=TA_RIGHT then
       P.x:=P.x-tmpS.Cx+(tmpLength*0.5);

    if Font.Depth<>0 then
       tmpExtrusion:=Font.Depth
    else
       tmpExtrusion:=TextDepth*0.5;

    tmpH:=tmpS.cy*0.5;
    SetLocationSize(result,P.x,P.y-tmpH,P.x+tmpS.cx,P.y+tmpH,AZ-tmpExtrusion-0.5,AZ-0.5);

    //if not FIs3D then
    //   result.FontStyle:=fsBitmap;

    result.Format.Color:=AColor;

    result.Font.Assign(Font);

    if UsePicture then
       if Font.Picture.Graphic<>nil then
          result.Format.Texture.Picture.Graphic:=Font.Picture.Filtered;

    AddBlock(result);
  end;

var tmpW : Double;
    p1   : TImageBlock;
    b    : TBitmap;
    tmpCanvas : TTeeCanvas3D;
begin
  if TextAsPictures then
  begin
    p1:=TImageBlock.Create(IOwner);
    SetTitle(p1,'Text ('+Text+')');

    b:=TBitmap.Create;
    try
      tmpS:=b.Canvas.TextExtent(Text);

      TeeSetBitmapSize(b,tmpS.cx,tmpS.cy);

      tmpCanvas:=TTeeCanvas3D.Create;
      tmpCanvas.ReferenceCanvas:=b.Canvas;

      tmpCanvas.AssignFont(Font);

      tmpCanvas.TextOut(0,0,Text);

      tmpCanvas.Free;

      b.TransparentColor:=clWhite;
      b.TransparentMode:=tmFixed;
      b.Transparent:=True;

      p1.Format.Border.Visible:=False;

      p1.Format.Texture.Picture.Assign(b);
      p1.Format.Texture.PictureTransparent:=True;
    finally
      b.Free;
    end;

    //ReferenceCanvas.Font.Assign(Font);
    //tmpS:=ReferenceCanvas.TextExtent(Text);

    tmpW:=tmpS.cx*0.5;
    //tmpH:=tmpS.cy*0.5;

    SetLocationSize(p1,X-tmpW,Y,X+tmpW,Y+tmpS.cy,z+0.1,z+0.1);
    AddBlock(p1);
  end
  else
  begin
    ReferenceCanvas.Font.Assign(Font);
    tmpS:=ReferenceCanvas.TextExtent(Text);

    with Font.Shadow do
    if Visible and (Size<>0) then
       AddText('Text Shadow',X+HorizSize,Y+VertSize,Z+1,Color,False).Format.Transparency:=Transparency;

    AddText('Text ('+Text+')',X,Y,Z,Font.Color,True);
  end;
end;

Procedure TBlockCanvas.TextOut(X,Y:Integer; const Text:String);
begin
  TextOut3D(x,y,0,Text);
end;

procedure TBlockCanvas.MoveTo3D(const X,Y,Z:Integer);
begin
  FX:=X-IXOffset;
  FY:=Y;
  FZ:=Z;
end;

procedure TBlockCanvas.Line(const A,B:TPoint3DFloat);
var tmp : TPipeBlock;
begin
  tmp:=TPipeBlock.Create(IOwner);
  SetTitle(tmp,'Line');
  SetFormat(tmp);

  tmp.Points.Add(A.X-IXOffset,A.Z,FHeight-A.Y);
  tmp.Points.Add(B.X-IXOffset,B.Z,FHeight-B.Y);

  SetPipe(tmp);

  AddBlock(tmp);

  FX:=B.X-IXOffset;
  FY:=B.Y;
  FZ:=B.Z;
end;

procedure TBlockCanvas.LineTo3D(const X,Y,Z:Integer);
var tmp : TPipeBlock;
begin
  tmp:=TPipeBlock.Create(IOwner);

  SetTitle(tmp,'Line3D');
  SetFormat(tmp);

  tmp.Points.Add(FX,FZ,FHeight-FY);
  tmp.Points.Add(X-IXOffset,Z,FHeight-Y);

  SetPipe(tmp);

  AddBlock(tmp);

  FX:=X-IXOffset;
  FY:=Y;
  FZ:=Z;
end;

procedure TBlockCanvas.PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer);
var tmp : TExtrudedBlock;
    min : TPoint;
    max : TPoint;

  procedure AddPoint(const P:TPoint);
  begin
    tmp.Points.Add( ((P.X-min.X)/(tmp.Size.X*0.5))-1, 0,
                    1-((P.Y-min.Y)/(tmp.Size.Z*0.5)));
  end;

begin
  tmp:=TExtrudedBlock.Create(IOwner);
  SetTitle(tmp,'PlaneZ');

  min.X:=P1.X;
  if P2.X<min.X then min.X:=P2.X;
  if P3.X<min.X then min.X:=P3.X;
  if P4.X<min.X then min.X:=P4.X;

  min.Y:=P1.Y;
  if P2.Y<min.Y then min.Y:=P2.Y;
  if P3.Y<min.Y then min.Y:=P3.Y;
  if P4.Y<min.Y then min.Y:=P4.Y;

  max.X:=P1.X;
  if P2.X>max.X then max.X:=P2.X;
  if P3.X>max.X then max.X:=P3.X;
  if P4.X>max.X then max.X:=P4.X;

  max.Y:=P1.Y;
  if P2.Y>max.Y then max.Y:=P2.Y;
  if P3.Y>max.Y then max.Y:=P3.Y;
  if P4.Y>max.Y then max.Y:=P4.Y;

  tmp.Size.SetPoint(max.X-min.X,0,max.Y-min.Y);

  tmp.Location.SetPoint((max.X+min.X)*0.5,Z,FHeight-((max.Y+min.Y)*0.5));

  SetFormat(tmp);

  AddPoint(P1);
  AddPoint(P2);
  AddPoint(P3);
  AddPoint(P4);

  AddBlock(tmp);
end;

procedure TBlockCanvas.Plane3D(Const A,B:TPoint; Z0,Z1:Integer);
var tmp : TRectangleBlock;
    tmpA : Double;
begin
  tmp:=RectangleBlock(IOwner);
  SetTitle(tmp,'Plane 3D');

  with tmp.Size.Point do
  begin
    X:=Sqrt(Sqr(B.X-A.X)+Sqr(B.Y-A.Y));
    Y:=0;
    Z:=Abs(Z1-Z0);
  end;

  if A.Y=B.Y then
     tmpA:=0
  else
     tmpA:=ArcSin((A.Y-B.Y)/tmp.Size.Point.X);

  if B.X<A.X then
     tmp.Rotation.X:=(180*tmpA/Pi)
  else
     tmp.Rotation.X:=180-(180*tmpA/Pi);

  tmp.Rotation.Y:=90;

  SetFormat(tmp);

  tmp.Location.SetPoint((A.X+B.X)*0.5,(Z0+Z1)*0.5,FHeight-((A.Y+B.Y)*0.5));

  AddBlock(tmp);
end;

Function TBlockCanvas.GetSupports3DText:Boolean;
begin
  result:=True;
end;

Function TBlockCanvas.GetSupportsFullRotation:Boolean;
begin
  result:=True;
end;

Function TBlockCanvas.GetSupportsXORMode:Boolean;
begin
  result:=False;
end;

Function TBlockCanvas.GetUseBuffer:Boolean;
begin
  result:=FUseBuffer;
end;

Procedure TBlockCanvas.SetUseBuffer(Value:Boolean);
begin
  FUseBuffer:=Value;
  IDestCanvas:=nil;
end;

Function TBlockCanvas.GetHandle:TTeeCanvasHandle;
begin
  result:=FDC;
end;

procedure TBlockCanvas.RotateLabel3D(x,y,z:Integer; Const St:String; RotDegree:Double;
                                     AllowHTML:Boolean=False);
var tmp : TeeBlocks.TTeeTextBlock;
    tmpH : Double;
    tmpExtrusion : Double;
begin
  tmp:=TeeBlocks.TTeeTextBlock.Create(IOwner);
  SetTitle(tmp,'Text 3D');
  tmp.Text:=St;

  //tmp.Extrusion:=1.5;
  tmpExtrusion:=1; // tmp.Extrusion;

  tmpH:=TextHeight(St);
  SetLocationSize(tmp,x,y-tmpH,x,y+tmpH,z-tmpExtrusion-1,z-tmpExtrusion);

  tmp.Format.Color:=Font.Color;
  tmp.Font.Assign(Font);
  tmp.Rotation.Y:=RotDegree;
  AddBlock(tmp);
end;

procedure TBlockCanvas.RotateLabel(x,y:Integer; Const St:String; RotDegree:Double;
                                   AllowHTML:Boolean=False);
begin
  RotateLabel3D(x,y,0,St,RotDegree,AllowHTML);
end;

procedure TBlockCanvas.EraseBackground(const Rect: TRect);
begin
end;

Procedure TBlockCanvas.Arrow( Filled:Boolean;
                           Const FromPoint,ToPoint:TPoint;
                           ArrowWidth,ArrowHeight,Z0,Z1:Integer;
                           const ArrowPercent:Double);
var tmp : TArrowBlock;
begin
  tmp:=TArrowBlock.Create(IOwner);

  SetTitle(tmp,'Arrow');
  SetFormat(tmp);

  if ToPoint.Y>FromPoint.Y then
     tmp.Rotation.Y:=180;

  SetLocationSize(tmp,FromPoint.X-ArrowWidth*0.5,FromPoint.Y,
                      ToPoint.X+ArrowWidth*0.5,ToPoint.Y,Z0,Z1);

  tmp.Head.X:=ArrowPercent;
  tmp.Head.Y:=ArrowPercent;

  AddBlock(tmp);
end;

Procedure TBlockCanvas.LineWithZ(X0,Y0,X1,Y1,Z:Integer);
begin
  MoveTo3D(X0,Y0,Z);
  LineTo3D(X1,Y1,Z);
end;

type
  TPathBlockAccess=class(TPathBlock);

procedure TBlockCanvas.PolygonWithZ(const Points: Array of TPoint; Z:Integer);
var tmp : TExtrudedBlock;
    tmpPath : TPathBlock;
    t   : Integer;
    IMin,
    IMax : TPoint3DFloat;
begin
  tmp:=TExtrudedBlock.Create(IOwner);
  SetTitle(tmp,'Polygon Z');

  SetFormat(tmp);

  tmp.Location.Y:=Z;
  tmp.Size.Y:=0;

  for t:=Low(Points) to High(Points)-1 do
  with Points[t] do
       tmp.Points.Add(X,Y);

  tmpPath:=tmp;
  TPathBlockAccess(tmpPath).CalcMinMax(IMin,IMax);

  tmp.Location.X:=(IMin.X+IMax.X)*0.5;
  tmp.Location.Z:=(IMin.Z+IMax.Z)*0.5;

  tmp.Size.X:=(IMax.X-IMin.X);
  tmp.Size.Z:=(IMax.Z-IMin.Z);

  for t:=Low(Points) to High(Points)-1 do
  with tmp.Points[t].Point.Point do
  begin
    X:=(X-IMin.X)/tmp.Size.Point.X;
    Z:=(Z-IMin.Y)/tmp.Size.Point.Z;
  end;

  AddBlock(tmp);
end;

procedure TBlockCanvas.Triangle3D( Const Points:TTrianglePoints3D;
                                   Const Colors:TTriangleColors3D);
var tmp : TTriangleBlock;
    AMin, AMax : TPoint3DFloat;
begin
  tmp:=TTriangleBlock.Create(IOwner);
  SetTitle(tmp,'Triangle 3D');

  SetFormat(tmp);

  // Minimum
  AMin.X:=Points[0].X;
  if Points[1].X<AMin.X then
     if Points[2].X<AMin.X then
        AMin.X:=Points[2].X
     else
        AMin.X:=Points[1].X;

  AMin.Y:=Points[0].Y;
  if Points[1].Y<AMin.Y then
     if Points[2].Y<AMin.Y then
        AMin.Y:=Points[2].Y
     else
        AMin.Y:=Points[1].Y;

  AMin.Z:=Points[0].Z;
  if Points[1].Z<AMin.Z then
     if Points[2].Z<AMin.Z then
        AMin.Z:=Points[2].Z
     else
        AMin.Z:=Points[1].Z;

  // Maximum
  AMax.X:=Points[0].X;
  if Points[1].X>AMax.X then
     if Points[2].X>AMax.X then
        AMax.X:=Points[2].X
     else
        AMax.X:=Points[1].X;

  AMax.Y:=Points[0].Y;
  if Points[1].Y>AMax.Y then
     if Points[2].Y>AMax.Y then
        AMax.Y:=Points[2].Y
     else
        AMax.Y:=Points[1].Y;

  AMax.Z:=Points[0].Z;
  if Points[1].Z>AMax.Z then
     if Points[2].Z>AMax.Z then
        AMax.Z:=Points[2].Z
     else
        AMax.Z:=Points[1].Z;

  SetLocationSize(tmp,AMin.X,AMin.Y,AMax.X,AMax.Y,AMin.Z,AMax.Z);

  AddBlock(tmp);
end;

procedure TBlockCanvas.TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer);
begin
  PolygonWithZ([P1,P2,P3],Z);
end;

function ColorToGL(const AColor:TColor):TRGBAlpha; overload;
begin
  result.Red:=Byte(AColor);
  result.Green:=Byte(AColor shr 8);
  result.Blue:=Byte(AColor shr 16);
  result.Alpha:=1;
end;

function ColorToGL(const AColor:TColor; const Transparency:TTeeTransparency):TRGBAlpha; overload;
begin
  result.Red:=Byte(AColor);
  result.Green:=Byte(AColor shr 8);
  result.Blue:=Byte(AColor shr 16);
  result.Alpha:=Round((100-Transparency)*2.55); //1;
end;

procedure TBlockCanvas.Surface3D( Style:TTeeCanvasSurfaceStyle;
                         SameBrush,CreateCached,AddNormals:Boolean;
                         NumXValues,NumZValues:Integer;
                         const CalcPoints:TTeeCanvasCalcPoints;
                         var Cached:TTeeCachedObject);
var tmpX,
    tmpZ : Integer;

    V : TSurfaceVertex;

    {
    tmpColor0,
    tmpColor1 : TColor;
    P0        : TPoint3DFloat;
    P1        : TPoint3DFloat;
    }

    P0Y,
    P1Y       : Integer;
    
    Rect      : TRect;
    Z0,Z1     : Double;
    tmpZPos   : Integer;

    tmp : TTerrainBlock;
begin
  tmp:=TTerrainBlock.Create(IOwner);
  SetTitle(tmp,'Terrain');

  SetFormat(tmp);

  tmp.Format.Solid:=Style=tcsSolid;

  SetLength(tmp.Grid,NumXValues,NumZValues);

  if not SameBrush then
     SetLength(tmp.Colors,NumXValues,NumZValues);

  Z0:=0;
  Z1:=0;
  Rect.Top:=MaxLongint;
  Rect.Bottom:=-MaxLongint;

  for tmpX:=2 to NumXValues do
  begin
    for tmpZ:=NumZValues downto 1 do
    begin
      CalcPoints(tmpX,tmpZ,not SameBrush,V);

      if V.Flag then
      begin
        tmpZPos:=NumZValues-tmpZ;

        tmp.Grid[tmpX-1-1,tmpZPos]:=FHeight-V.P0.y;
        tmp.Grid[tmpX-1,tmpZPos]:=FHeight-V.P1.y;

        if not SameBrush then
        begin
          if V.C0<>clNone then
             tmp.Colors[tmpX-1-1,tmpZPos]:=ColorToGL(V.C0,ITransp);

          if V.C1<>clNone then
             tmp.Colors[tmpX-1,tmpZPos]:=ColorToGL(V.C1,ITransp);
        end;

        P0Y:=Round(V.P0.y);

        if P0y<Rect.Top then
           Rect.Top:=P0y;

        if P0y>Rect.Bottom then
           Rect.Bottom:=P0y;

        P1Y:=Round(V.P1.Y);

        if P1y<Rect.Top then
           Rect.Top:=P1y;

        if P1y>Rect.Bottom then
           Rect.Bottom:=P1y;

        with Rect do
        begin
          if tmpZ=NumZValues then
             Z1:=V.P0.Z
          else
          if tmpZ=1 then
             Z0:=V.P0.Z;
        end;
      end;
    end;

    with Rect do
    begin
      if tmpX=2 then
         Left:=Round(V.P0.X)
      else
      if tmpX=NumXValues then
         Right:=Round(V.P1.X);
    end;
  end;

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);

  tmp.RecalcBounds;

  AddBlock(tmp);
end;

procedure TBlockCanvas.TriSurface(NumTriangles:Integer; CreateCached,AddNormals:Boolean;
                        const Blend:TTeeBlend; Transparency:TTeeTransparency;
                        const CalcPoints:TTeeCanvasTriPoints;
                        var Cached:TTeeCachedObject);
var
  Rect : TRect;
  Z0,
  Z1   : Integer;
  FirstPoint : Boolean;
  tmp  : TMeshBlock;

  procedure CheckPoint(Index:Integer; const PInt:TPoint3D);
  begin
    with tmp.Points[Index].Point do
    begin
      X:=PInt.X;
      Y:=PInt.Y;
      Z:=PInt.Z;
    end;

    with Rect do
    begin
      if PInt.X<Left then Left:=PInt.X else
      if PInt.X>Right then Right:=PInt.X;

      if PInt.Y<Top then Top:=PInt.Y else
      if PInt.Y>Bottom then Bottom:=PInt.Y;

      if PInt.Z<Z0 then Z0:=PInt.Z else
      if PInt.Z>Z1 then Z1:=PInt.Z;
    end;
  end;

var t    : Integer;
    tmpL : Integer;
    tmpP : TTrianglePoints3D;
    tmpC : TTriangleColors3D;
    tmpI : TTriangleIndexes;
begin
  tmp:=TMeshBlock.Create(IOwner);
  SetTitle(tmp,'Mesh');

  SetFormat(tmp);

  tmp.Format.Solid:=Brush.Style<>bsClear;

  SetLength(tmp.Triangles,NumTriangles);

  tmpL:=-1;

  FirstPoint:=True;

  for t:=0 to NumTriangles-1 do
  begin
    CalcPoints(t,tmpP,tmpC,tmpI);

    if tmpL<tmpI.Point0 then
    begin
      tmpL:=tmpI.Point0;
      SetLength(tmp.Points,tmpL+1);
    end;

    if tmpL<tmpI.Point1 then
    begin
      tmpL:=tmpI.Point1;
      SetLength(tmp.Points,tmpL+1);
    end;

    if tmpL<tmpI.Point2 then
    begin
      tmpL:=tmpI.Point2;
      SetLength(tmp.Points,tmpL+1);
    end;

    if FirstPoint then
    with tmpP[0] do
    begin
      Rect.Left:=X;
      Rect.Right:=X;

      Rect.Top:=Y;
      Rect.Bottom:=Y;

      Z0:=Z;
      Z1:=Z0;

      FirstPoint:=False;
    end;

    with tmp.Triangles[t] do
    begin
      Point0:=tmpI.Point0;
      Point1:=tmpI.Point1;
      Point2:=tmpI.Point2;

      CheckPoint(Point0,tmpP[0]);
      CheckPoint(Point1,tmpP[1]);
      CheckPoint(Point2,tmpP[2]);

      tmp.Points[Point0].Color:=ColorToGL(tmpC[0]);
      tmp.Points[Point1].Color:=ColorToGL(tmpC[1]);
      tmp.Points[Point2].Color:=ColorToGL(tmpC[2]);
    end;
  end;

  with Rect do
       SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);

  AddBlock(tmp);
end;

procedure TBlockCanvas.Diamond(const Left,Top,Right,Bottom:TCoordinate; Z0,Z1:Integer);
var tmp : TRombusBlock;
begin
  tmp:=TRombusBlock.Create(IOwner);
  SetTitle(tmp,'Diamond');
  SetFormat(tmp);

  SetLocationSize(tmp,Left,Top,Right,Bottom,Z0,Z1);

  AddBlock(tmp);
end;

procedure TBlockCanvas.PyramidTrunc(Const R: TRectF; const StartZ, EndZ, TruncX, TruncZ:Double;
                                    TopCover:Boolean=True; BottomCover:Boolean=True);
var tmp : TPyramidBlock;
begin
  tmp:=TPyramidBlock.Create(IOwner);
  SetTitle(tmp,'Pyramid');
  SetFormat(tmp);

  with R do
       SetLocationSize(tmp,Left,Top,Right,Bottom,StartZ,EndZ);

  tmp.Side1.Point.X:=TruncX;
  tmp.Side1.Point.Y:=TruncZ;
  tmp.Side2.Point.X:=TruncX;
  tmp.Side2.Point.Y:=TruncZ;

  // tmp.Cover.Top.Visible := TopCover;
  // tmp.Cover.Bottom.Visible := BottomCover;

  AddBlock(tmp);
end;

procedure TBlockCanvas.BeginEntity(const Entity:String; const Transform:TTeeTransform=nil);
var tmp  : TCustomBlock;
    tmpL : Integer;
begin
{  if Assigned(Visual) then
     tmp:=Visual as TCustomBlock
  else}
     tmp:=TObjectBlock.Create(IOwner);

  //result:=tmp;

  SetTitle(tmp,Entity);

  IBlocks.Add(tmp);

  if tmp is TCustomObjectBlock then
     IBlocks:=TCustomObjectBlock(tmp).Items;

  tmpL:=Length(IRestoreBlocks);
  SetLength(IRestoreBlocks,tmpL+1);
  IRestoreBlocks[tmpL]:=IBlocks;
end;

type
  TBlocksAccess=class(TBlocks);

procedure TBlockCanvas.EndEntity;
var tmpL : Integer;
begin
  tmpL:=Length(IRestoreBlocks);

  if tmpL>1 then
  begin
    IBlocks:=IRestoreBlocks[tmpL-2];
    SetLength(IRestoreBlocks,tmpL-1);
  end;
end;

end.

