unit TeeExtruded;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls,
  {$ELSE}
  Graphics, Controls,
  {$ENDIF}
  TeCanvas, TeeBlocks,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeAnimate, TeeRoundRect;

type
  TPointItem=class(TCollectionItem)
  private
    FLink  : TPropertyLink;
    FPoint : TPointXYZColor;

    IRealYPropertyName : String;
    IRealZPropertyName : String;

    procedure DataChanged(Sender:TObject);
    function GetLink:TPropertyLink;
    function GetPoint:TPointXYZColor;
    procedure SetLink(const Value:TPropertyLink);
    procedure SetPoint(const Value: TPointXYZColor);
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Link:TPropertyLink read GetLink write SetLink;
    property Point:TPointXYZColor read GetPoint write SetPoint;
  end;

  TPointCollection=class(TOwnedCollection)
  private
    IChanged : Boolean;
    IOwner   : TVisualBlock;

    function Get(Index: Integer): TPointItem; {$IFDEF D10}inline;{$ENDIF}
    procedure Put(Index: Integer; const Value: TPointItem);
  protected
    IConvex : Boolean;

    procedure DoChanged;
    function GetPoints:TPointFloatArray;
    procedure Update(Item: TCollectionItem); override;
  public
    procedure Repaint;

    function Add(const AX,AZ:Single):TPointItem; overload;
    function Add(const AX,AY,AZ:Single):TPointItem; overload;
    function Add(const XYZ:TPoint3DFloat):TPointItem; overload;
    function Add(const XYZ:TeCanvas.TPoint3D):TPointItem; overload;

    function Add(const Point:TPoint):TPointItem; overload;
    procedure Add(const Points:TPointArray); overload;
    procedure Add(const Points:TFourPoints); overload;

    function IsConvexPolygon:Boolean;

    property Point[Index:Integer]:TPointItem read Get write Put; default;
  end;

  TPointerBlock=class(TCustomBlock)
  private
    FColorEach : Boolean;
    FPointer   : TCustomBlock;
    FSelected  : Integer;

    IFac       : TPoint3DFloat;
    IOff       : TPoint3DFloat;

    procedure SetColorEach(const Value: Boolean);
    procedure SetPointer(const Value: TCustomBlock);
    procedure SetSelected(const Value: Integer);
  protected
    IMax : TPoint3DFloat;
    IMin : TPoint3DFloat;

    function CalcPoint(const P:TPoint3DFloat):TPoint3DFloat;
    procedure DrawSelected; override;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure PrepareCalcPoint;
    function SelectedPoint:TPoint3DFloat; virtual; abstract;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Draw; override;

    procedure Assign(Source:TPersistent); override;
    property Selected:Integer read FSelected write SetSelected;
  published
    property ColorEach:Boolean read FColorEach write SetColorEach default True;
    property Pointer:TCustomBlock read FPointer write SetPointer;
  end;

  TPathBlock=class(TPointerBlock)
  private
    FPoints      : TPointCollection;

    IStorePoints : Boolean;

    function IsPointsStored:Boolean;
    procedure SetPoints(const Value:TPointCollection);
  protected
    IPointCount : Integer;

    procedure AddPathPoint(const X,Y:Single; const AColor:TColor=clDefault); override;
    function CalcMinMax(var AMin, AMax: TPoint3DFloat):Boolean;
    procedure ClearPath; override;
    procedure PrepareForGallery; override;
    function SelectedPoint:TPoint3DFloat; override;
  public
    HasYValues : Boolean;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure Draw; override;
  published
    property Points:TPointCollection read FPoints write SetPoints stored IsPointsStored;
  end;

  TTapeCorners=class(TRoundCorners)
  private
    FEnabled : Boolean;

    procedure SetEnabled(const Value:Boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default False;
  end;

  TBoundVertex=packed record
    Normal  : TPoint3DFloat;
    Color   : TColor;
    Point   : TFloatPoint;
    TexPos  : Single;
  end;

  TBoundVertexs=packed Array of TBoundVertex;

  TTapeBlock=class(TPathBlock)
  private
    FAdjustTexture: Boolean;
    FClosed    : Boolean;
    FCorners   : TTapeCorners;
    FRadius    : Single;
    FRoundness : Integer;
    FSlices    : Integer;
    FSlices3D  : Integer;
    FTape3D    : Single;
    FTapeColorEach: Boolean;

    IBounds    : TBoundVertexs;
    IExtruded  : Boolean;
    IList      : Integer;
    IListArea  : Integer;
    IListLine  : Integer;
    ITotalLength : Single;

    procedure ColorChanged(Sender:TObject);
    procedure CornersChanged(Sender:TObject);
    procedure SetAdjustTexture(const Value: Boolean);
    procedure SetClosed(const Value:Boolean);
    procedure SetCorners(const Value: TTapeCorners);
    procedure SetRadius(const Value:Single);
    procedure SetRoundness(const Value: Integer);
    procedure SetSlices(const Value: Integer);
    procedure SetSlices3D(const Value: Integer);
    procedure SetTape3D(const Value: Single);
    procedure SetTapeColorEach(const Value: Boolean);
  protected
    ForceClosed : Boolean;

    procedure DeleteLists; override;
    procedure SizeChanged(Sender:TObject); override;
  public
    AreaRound   : Boolean;
    ConcaveArea : Boolean;
    IsArea      : Boolean;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;
  published
    property AdjustTexture:Boolean read FAdjustTexture write SetAdjustTexture default True;
    property Closed:Boolean read FClosed write SetClosed default False;
    property Corners:TTapeCorners read FCorners write SetCorners;
    property Radius:Single read FRadius write SetRadius;
    property Roundness:Integer read FRoundness write SetRoundness default 0;
    property Slices:Integer read FSlices write SetSlices default 32;
    property Slices3D:Integer read FSlices3D write SetSlices3D default 32;
    property Tape3D:Single read FTape3D write SetTape3D;
    property TapeColorEach:Boolean read FTapeColorEach write SetTapeColorEach default True;
  end;

  TExtrudedBlock=class(TTapeBlock)
  private
    FBack  : TBlockFormat;
    FFront : TBlockFormat;

    IDrawSides : Boolean;

    IList1  : Integer;
    IList2  : Integer;
    INeedsRecalc : Boolean;

    function GetBack: TBlockFormat;
    function GetFront: TBlockFormat;
    function IsBackStored:Boolean;
    function IsFrontStored:Boolean;
    procedure SetBack(const Value: TBlockFormat);
    procedure SetFront(const Value: TBlockFormat);
  protected
    procedure DeleteLists; override;
    procedure DoChanged(Sender:TObject);
    procedure PrepareForGallery; override;
    procedure ReadState(Reader: TReader); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;

    function HasBack:Boolean;
    function HasFront:Boolean;
  published
    property Back:TBlockFormat read GetBack write SetBack stored IsBackStored;
    property Closed default True;
    property Front:TBlockFormat read GetFront write SetFront stored IsFrontStored;
  end;

  TBridgeBlock=class(TExtrudedBlock)
  private
    FColumnSize   : TPointXYFloat;
    FRounded      : Boolean;
    FRoundSlices  : Integer;

    IPoints       : TPointFloatArray;
    ITexture      : TPointFloatArray;

    procedure SetColumnSize(const Value:TPointXYFloat);
    procedure SetRounded(const Value: Boolean);
    procedure SetRoundSlices(const Value: Integer);
  protected
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
  published
    property ColumnSize:TPointXYFloat read FColumnSize write SetColumnSize;
    property Rounded:Boolean read FRounded write SetRounded default True;
    property RoundSlices:Integer read FRoundSlices write SetRoundSlices default 32;
  end;

  TTeePolygonBlock=class(TExtrudedBlock)
  private
    procedure CreatePoints(Sides:Integer);
  protected
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TPentagonBlock=class(TTeePolygonBlock)
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  THexagonBlock=class(TTeePolygonBlock)
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TOctagonBlock=class(TTeePolygonBlock)
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TArrowBlock=class(TExtrudedBlock)
  private
    FHead   : TPointXYFloat;
    FIndent : Single;

    procedure SetHead(const Value:TPointXYFloat);
    procedure SetIndent(const Value: Single);
  protected
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Head:TPointXYFloat read FHead write SetHead;
    property Indent:Single read FIndent write SetIndent;
  end;

  TRombusBlock=class(TExtrudedBlock)
  public
    Constructor Create(AOwner: TComponent); override;
  end;

  TCrossBlock=class(TExtrudedBlock)
  private
    FCrossCenter : TPointXYFloat;
    FCrossSize   : TPointXYFloat;

    procedure SetCrossCenter(const Value:TPointXYFloat);
    procedure SetCrossSize(const Value:TPointXYFloat);
  protected
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property CrossCenter:TPointXYFloat read FCrossCenter write SetCrossCenter;
    property CrossSize:TPointXYFloat read FCrossSize write SetCrossSize;
  end;

  TStarBlock=class(TExtrudedBlock)
  private
    FSlant: Single;
    FInner: Single;

    ISides : Integer;

    procedure CreatePoints;
    function IsInnerStored: Boolean;
    function IsSlantStored: Boolean;
    procedure SetInner(const Value: Single);
    procedure SetSlant(const Value: Single);
  public
    Constructor Create(AOwner: TComponent); override;
  published
    property InnerSize:Single read FInner write SetInner stored IsInnerStored;
    property SlantAngle:Single read FSlant write SetSlant stored IsSlantStored;
  end;

  TPentagramBlock=class(TStarBlock)
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  THexagramBlock=class(TStarBlock)
  public
    Constructor Create(AOwner:TComponent); override;
  end;

  TRectPyramidBlock=class(TExtrudedBlock)
  private
    FLeft  : Single;
    FRight : Single;

    procedure CreatePoints;
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
  protected
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;

    procedure Assign(Source:TPersistent); override;
  published
    property LeftPercent:Single read FLeft write SetLeft;
    property RightPercent:Single read FRight write SetRight;
  end;

procedure CalcBezier(CurvePoints:Integer; const P1,P2,P3,P4:TPointFloat;
                     out Output:TPointFloatArray; NumPoints:Integer=3);

implementation

uses
  Math, TeeGLCanvas, OpenGL2, TeeGeometry,
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  {$IFDEF D6}
  Types,
  {$ENDIF}
  TypInfo;

procedure CalcBezier(CurvePoints:Integer; const P1,P2,P3,P4:TPointFloat;
                     out Output:TPointFloatArray; NumPoints:Integer=3);
var InvCurve : Single;
    t        : Integer;
    mu       : Single;
    mu2      : Single;
    mu3      : Single;
    mum1     : Single;
    mum12    : Single;
    mum13    : Single;
begin
  InvCurve:=1/CurvePoints;

  SetLength(Output,CurvePoints+1);

  for t:=0 to CurvePoints do
  begin
    mu:=t*InvCurve;
    mu2:=Sqr(mu);
    mum1:=1-mu;

    if NumPoints=3 then
    begin
      mum12:=Sqr(mum1);
      Output[t].x:=(P1.x * mum12 + 2*P2.x*mum1*mu + P3.x*mu2);
      Output[t].y:=(P1.y * mum12 + 2*P2.y*mum1*mu + P3.y*mu2);
    end
    else
    begin
      mum13:=mum1*mum1*mum1;
      mu3:=mu*mu*mu;
      Output[t].x:=(P1.x * mum13 + 3*P2.x*mum1*mum1*mu + 3*mu2*mum1*P3.x + mu3*P4.x);
      Output[t].y:=(P1.y * mum13 + 3*P2.y*mum1*mum1*mu + 3*mu2*mum1*P3.y + mu3*P4.y);
    end;
  end;
end;

{ TPointItem }

Constructor TPointItem.Create(Collection: TCollection);
begin
  inherited;
  FPoint:=TPointXYZColor.Create(TPointCollection(Collection).IOwner,0,DataChanged);
end;

Destructor TPointItem.Destroy;
begin
  FLink.Free;
  FPoint.Free;
  inherited;
end;

procedure TPointItem.DataChanged(Sender:TObject);
begin
  TPointCollection(Collection).IChanged:=True;
end;

procedure TPointItem.Assign(Source:TPersistent);
begin
  if Source is TPointItem then
  begin
    FPoint.Assign(TPointItem(Source).FPoint);
    Link:=TPointItem(Source).FLink;
  end
  else
     inherited;
end;

procedure TPointItem.SetPoint(const Value: TPointXYZColor);
begin
  FPoint.Assign(Value);
  TPointCollection(Collection).Repaint;
end;

function TPointItem.GetLink: TPropertyLink;
begin
  if not Assigned(FLink) then
     FLink:=TPropertyLink.Create(TPointCollection(Collection).GetOwner as TComponent);

  result:=FLink;
end;

procedure TPointItem.SetLink(const Value: TPropertyLink);
begin
  if Assigned(Value) then Link.Assign(Value)
                     else FreeAndNil(FLink);
end;

function TPointItem.GetPoint: TPointXYZColor;
begin
  if Assigned(FLink) then
  with FLink do
  if Assigned(Instance) then
  begin
    FPoint.Point.X:=PropertyValue;

    if IRealYPropertyName<>'' then
       FPoint.Point.Y:=PropertyValue(IRealYPropertyName);

    if IRealZPropertyName<>'' then
       FPoint.Point.Z:=PropertyValue(IRealZPropertyName);
  end;

  result:=FPoint;
end;

{ TPointCollection }

function TPointCollection.IsConvexPolygon:Boolean;
var tmp : TPointFloatArray;
begin
  tmp:=GetPoints;
  result:=TTeeCanvas.IsConvexPolygon(tmp);
  tmp:=nil;
end;

function TPointCollection.Get(Index: Integer): TPointItem;
begin
  result:=TPointItem(GetItem(Index));
end;

function TPointCollection.Add(const XYZ:TeCanvas.TPoint3D):TPointItem;
begin
  result:=TPointItem(inherited Add);

  with result.FPoint.Point do
  begin
    X:=XYZ.x;
    Y:=XYZ.y;
    Z:=XYZ.z;
  end;
end;

function TPointCollection.Add(const AX,AZ:Single):TPointItem;
begin
  result:=TPointItem(inherited Add);

  with result.FPoint.Point do
  begin
    X:=AX;
    Z:=AZ;
  end;
end;

function TPointCollection.Add(const AX,AY,AZ:Single):TPointItem;
begin
  result:=TPointItem(inherited Add);

  with result.FPoint.Point do
  begin
    X:=AX;
    Y:=AY;
    Z:=AZ;
  end;
end;

function TPointCollection.Add(const XYZ:TPoint3DFloat):TPointItem;
begin
  result:=TPointItem(inherited Add);
  result.FPoint.Point:=XYZ;
end;

procedure TPointCollection.Add(const Points:TPointArray);
var t : Integer;
begin
  for t:=Low(Points) to High(Points) do
      Add(Points[t].X,Points[t].Y);
end;

function TPointCollection.Add(const Point:TPoint):TPointItem;
begin
  result:=Add(Point.X,Point.Y);
end;

procedure TPointCollection.Add(const Points:TFourPoints);
begin
  Add(Points[0]);
  Add(Points[1]);
  Add(Points[2]);
  Add(Points[3]);
end;

procedure TPointCollection.Put(Index: Integer; const Value: TPointItem);
begin
  TPointItem(Items[Index]).Assign(Value);
end;

procedure TPointCollection.Repaint;
begin
  IOwner.Repaint;
end;

function TPointCollection.GetPoints: TPointFloatArray;
var t : Integer;
begin
  SetLength(result,Count);

  for t:=0 to Count-1 do
  with Point[t].Point do
  begin
    result[t].X:=X;
    result[t].Y:=Z;
  end;
end;

procedure TPointCollection.Update(Item: TCollectionItem);
begin
  inherited;
  IChanged:=True;
end;

procedure TPointCollection.DoChanged;
begin
  if IOwner is TExtrudedBlock then
     TExtrudedBlock(IOwner).DoChanged(Self)
  else
     IOwner.Repaint;
end;

{ TPointerBlock }

Constructor TPointerBlock.Create(AOwner: TComponent);
begin
  inherited;

  FSelected:=-1;
  FColorEach:=True;
end;

Destructor TPointerBlock.Destroy;
begin
  Pointer:=nil;
  inherited;
end;

procedure TPointerBlock.Assign(Source: TPersistent);
begin
  if Source is TPointerBlock then
  with TPointerBlock(Source) do
  begin
    Self.FColorEach:=FColorEach;
    Self.Pointer:=FPointer
  end;

  inherited;
end;

procedure TPointerBlock.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FPointer) and (AComponent=FPointer) then
     Pointer:=nil;
end;

procedure TPointerBlock.SetColorEach(const Value: Boolean);
begin
  FColorEach:=Value;
  Repaint;
end;

procedure TPointerBlock.SetPointer(const Value: TCustomBlock);
begin
  if FPointer<>Value then
  begin
    if Assigned(FPointer) then
       FPointer.RemoveFreeNotification(Self);

    FPointer:=Value;

    if Assigned(FPointer) then
       FPointer.FreeNotification(Self);

    Repaint;
  end;
end;

procedure TPointerBlock.PrepareCalcPoint;
begin
  IFac.Y:=IMax.Y-IMin.Y;

  if IFac.Y=0 then
     IOff.Y:=0
  else
  begin
    IOff.Y:=(Size.Point.Y*IMin.Y/IFac.Y)+Size.Point.Y*0.5;
    IFac.Y:=Size.Point.Y/IFac.Y;
  end;

  IFac.X:=IMax.X-IMin.X;

  if IFac.X=0 then
     IOff.X:=Size.Point.X*0.5
  else
  begin
    IOff.X:=(Size.Point.X*IMin.X/IFac.X)+Size.Point.X*0.5;
    IFac.X:=Size.Point.X/IFac.X;
  end;

  IFac.Z:=IMax.Z-IMin.Z;

  if IFac.Z=0 then
     IOff.Z:=Size.Point.Z*0.5
  else
  begin
    IOff.Z:=(Size.Point.Z*IMin.Z/IFac.Z)+Size.Point.Z*0.5;
    IFac.Z:=Size.Point.Z/IFac.Z;
  end;
end;

function TPointerBlock.CalcPoint(const P:TPoint3DFloat):TPoint3DFloat;
begin
  with P do
  begin
    result.X:=IFac.X*X -IOff.X;
    result.Y:=IFac.Y*Y -IOff.Y;
    result.Z:=IFac.Z*Z -IOff.Z;
  end;
end;

procedure TPointerBlock.DrawSelected;
var tmp : TPoint3DFloat;
begin
  inherited;

  if Selected<>-1 then
  begin
    {$IFDEF BLOCKS}
    ICanvas.setPenColor(clGreen);
    {$ELSE}
    ICanvas.Pen.Color:=clGreen;
    {$ENDIF}

    if Assigned(FPointer) then
    with FPointer.Size.Point do
    begin
      tmp.X:=X*0.5;
      tmp.Y:=Y*0.5;
      tmp.Z:=Z*0.5;
    end
    else
    begin
      PrepareCalcPoint;

      tmp:=PointFloat(1,1,1);
    end;

    glPushMatrix;

    with Size.Point do
         glScalef(2/X, 2/Z, 2/Y);

    with CalcPoint(SelectedPoint) do
         ICanvas.{$IFDEF BLOCKS}iCube{$ELSE}Cube{$ENDIF}(X-tmp.X,X+tmp.X,-Z-tmp.Z,-Z+tmp.Z,Y-tmp.Y,Y+tmp.Y,True);

    glPopMatrix;
  end;
end;

procedure TPointerBlock.SetSelected(const Value: Integer);
begin
  if FSelected<>Value then
  begin
    FSelected:=Value;
    Repaint;
  end;
end;

procedure TPointerBlock.Draw;
begin
end;

{ TPathBlock }

Constructor TPathBlock.Create(AOwner: TComponent);
begin
  inherited;

  HasYValues:=True;
  IStorePoints:=True;

  FPoints:=TPointCollection.Create(Self,TPointItem);
  FPoints.IOwner:=Self;
end;

Destructor TPathBlock.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TPathBlock.Assign(Source: TPersistent);
begin
  if Source is TPathBlock then
  with TPathBlock(Source) do
       Self.Points:=Points;

  inherited;
end;

function TPathBlock.SelectedPoint:TPoint3DFloat;
begin
  result:=Points[Selected].Point.Point;
end;

type
  TPointXYZColorAccess=class(TPointXYZColor);

procedure TPathBlock.AddPathPoint(const X,Y:Single; const AColor:TColor=clDefault);
begin
  TPointXYZColorAccess(Points.Add(X,Y).Point).FColor:=AColor;
end;

procedure TPathBlock.ClearPath;
begin
  Points.Clear;
end;

function TPathBlock.CalcMinMax(var AMin, AMax: TPoint3DFloat):Boolean;
var t : Integer;
begin
  IPointCount:=FPoints.Count;

  result:=IPointCount>0;

  if result then
  begin
    AMin:=Points[0].Point.Point;
    AMax:=AMin;

    for t:=1 to IPointCount-1 do
    with Points[t].Point.Point do
    begin
      if X<AMin.X then
         AMin.X:=X
      else
      if X>AMax.X then
         AMax.X:=X;

      if Y<AMin.Y then
         AMin.Y:=Y
      else
      if Y>AMax.Y then
         AMax.Y:=Y;

      if Z<AMin.Z then
         AMin.Z:=Z
      else
      if Z>AMax.Z then
         AMax.Z:=Z;
    end;
  end
  else
  begin
    AMin.X:=0;
    AMin.Y:=0;
    AMin.Z:=0;

    AMax.X:=0;
    AMax.Y:=0;
    AMax.Z:=0;
  end;
end;

type
  TBlockAccess=class(TCustomBlock);
  TBlockFormatAccess=class(TBlockFormat);

procedure TPathBlock.Draw;
var t   : Integer;
    Old : TPoint3DFloat;
    OldParent : TBlocks;
    OldColor  : TColor;
begin
  inherited;

  IPointCount:=FPoints.Count;

  if Points.IChanged then
  begin
    CalcMinMax(IMin,IMax);
    Points.IChanged:=False;
  end;

  if Assigned(FPointer) then
  begin
    glPushMatrix;

    with Size.Point do
         glScalef(2/X, 2/Z, 2/Y);

    PrepareCalcPoint;

    TBlockFormatAccess(Format).Finish;

    Old:=FPointer.Location.Point;
    OldParent:=FPointer.Parent;

    TBlockAccess(FPointer).IBlocks:=IBlocks;
    TBlockAccess(FPointer).ICanvas:=ICanvas;

    OldColor:=FPointer.Format.Color;

    for t:=0 to IPointCount-1 do
    with FPoints[t] do
    begin
      FPointer.Location.Point:=CalcPoint(Point.Point);

      if Self.ColorEach then
      begin
        if Point.Color=clDefault then
           FPointer.Format.Color:=OldColor
        else
           FPointer.Format.Color:=Point.Color;
      end;

      FPointer.DrawBlock;
    end;

    FPointer.Format.Color:=OldColor;

    FPointer.Location.Point:=Old;
    TBlockAccess(FPointer).IBlocks:=OldParent;

    TBlockFormatAccess(Format).Start;

    glPopMatrix;
  end;
end;

function TPathBlock.IsPointsStored:Boolean;
begin
  result:=IStorePoints and (Points.Count>0);
end;

procedure TPathBlock.SetPoints(const Value: TPointCollection);
begin
  FPoints.Assign(Value);
  Repaint;
end;

procedure TPathBlock.PrepareForGallery;
begin
  inherited;

  if IStorePoints then
  begin
    Points.Clear;
    Points.Add(-100,0,0);
    Points.Add(0,0,-100);
    Points.Add(100,0,0);
    Points.Add(100,0,100);
  end;
end;

{ TExtrudedBlock }

Constructor TExtrudedBlock.Create(AOwner: TComponent);
begin
  inherited;

  FClosed:=True;
  ForceClosed:=True;

  IDrawSides:=True;
  IExtruded:=True;
  INeedsRecalc:=True;
end;

Destructor TExtrudedBlock.Destroy;
begin
  FFront.Free;
  FBack.Free;
  inherited;
end;

procedure TExtrudedBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList1);
  DeleteList(IList2);
end;

procedure TExtrudedBlock.ReadState(Reader: TReader);
begin
  inherited;
  Points.IConvex:=Points.IsConvexPolygon;
end;

procedure TExtrudedBlock.PrepareForGallery;
const
  TwoPiFrac=TwoPi*0.1;

var t      : Integer;
    tmpSin,
    tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  inherited;

  if IStorePoints then
  begin
    Points.Clear;

    for t:=9 downto 0 do
    begin
      SinCos(t*TwoPiFrac,tmpSin,tmpCos);
      Points.Add(tmpSin*Random(1000)*0.001,tmpCos*Random(1000)*0.001);
    end;

    Points.IConvex:=Points.IsConvexPolygon;
  end;
end;

function TExtrudedBlock.IsBackStored:Boolean;
begin
  result:=Assigned(FBack);
end;

function TExtrudedBlock.GetBack:TBlockFormat;
begin
  if not Assigned(FBack) then
  begin
    FBack:=TBlockFormat.Create(Self);
    FBack.Assign(Format);
    FBack.Color:=clDefault;
  end;

  result:=FBack;
end;

function TExtrudedBlock.IsFrontStored:Boolean;
begin
  result:=Assigned(FBack);
end;

function TExtrudedBlock.GetFront:TBlockFormat;
begin
  if not Assigned(FFront) then
  begin
    FFront:=TBlockFormat.Create(Self);
    FFront.Assign(Format);
    FFront.Color:=clDefault;
  end;

  result:=FFront;
end;

function TExtrudedBlock.HasBack: Boolean;
begin
  result:=Assigned(FBack);
end;

function TExtrudedBlock.HasFront: Boolean;
begin
  result:=Assigned(FFront);
end;

procedure TExtrudedBlock.SetBack(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Back.Assign(Value)
  else
     FreeAndNil(FBack);

  Repaint;
end;

procedure TExtrudedBlock.SetFront(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Front.Assign(Value)
  else
     FreeAndNil(FFront);

  Repaint;
end;

procedure TExtrudedBlock.Draw;
var
  tmp : TPoint3DArray;
  t   : Integer;
  tmpFormat : TBlockFormat;

  CanCull : Boolean;
  Draw1   : Boolean;
  Draw2   : Boolean;
begin
  if Points.IChanged then
     DeleteLists;

  inherited;

  if IDrawSides then
  begin
    if (IList1=0) or (IList2=0) then
    begin
      SetLength(tmp,Length(IBounds));

      for t:=0 to Length(tmp)-1 do
      with tmp[t] do
      begin
        X:=IBounds[t].Point.x;

        if Points.IConvex then
           Y:=-IBounds[t].Point.y
        else
           Y:=IBounds[t].Point.y;

        Z:=1;
      end;
    end;

    if Assigned(FFront) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(FFront).Start;

      tmpFormat:=FFront;
    end
    else
      tmpFormat:=Format;

    Draw1:=(not Assigned(FFront)) or FFront.Solid;
    Draw2:=(not Assigned(FBack)) or FBack.Solid;

    CanCull:=(not ShouldDrawInterior) and Closed and Draw1 and Draw2;

    if Size.Point.Y<>0 then
    begin
      if CanCull then
         glEnable(GL_CULL_FACE);

      if tmpFormat.Solid then
         if IList1=0 then
         begin
           if Points.IConvex then
              TBlockFormatAccess(tmpFormat).ConvexPolygon(IList1,tmp,False)
           else
              TBlockFormatAccess(tmpFormat).ConcavePolygon(IList1,tmp,False);
         end
         else
           glCallList(IList1);

      if Assigned(FFront) then
         TBlockFormatAccess(FFront).Finish;

      // Set Back format or reset default Format if Front was used
      if Assigned(FBack) then
      begin
        if not Assigned(FFront) then
           TBlockFormatAccess(Format).Finish;

        TBlockFormatAccess(FBack).Start;
        tmpFormat:=FBack;
      end
      else
      if Assigned(FFront) then
      begin
        TBlockFormatAccess(Format).Start;
        tmpFormat:=Format;
      end;
    end;

    if tmpFormat.Solid then
       if IList2=0 then
       begin
         for t:=0 to Length(tmp)-1 do
             tmp[t].Z:=-1;

         if Points.IConvex then
            TBlockFormatAccess(tmpFormat).ConvexPolygon(IList2,tmp,True)
         else
            TBlockFormatAccess(tmpFormat).ConcavePolygon(IList2,tmp,True);
       end
       else
         glCallList(IList2);

    tmp:=nil;

    if Size.Point.Y<>0 then
    begin
      if CanCull then
         glDisable(GL_CULL_FACE);

      if Assigned(FBack) then
      begin
        TBlockFormatAccess(FBack).Finish;
        TBlockFormatAccess(Format).Start;
      end;
    end;
  end;
end;

function TExtrudedBlock.GetEditor: String;
begin
  if IStorePoints then
     result:='TExtrudedEditor'
  else
     result:=inherited GetEditor;
end;

procedure TExtrudedBlock.DoChanged(Sender:TObject);
begin
  INeedsRecalc:=True;
  DeleteLists;
end;

procedure TExtrudedBlock.Assign(Source: TPersistent);
begin
  if Source is TExtrudedBlock then
  with TExtrudedBlock(Source) do
  begin
    Self.Back:=FBack;
    Self.Front:=FFront;
  end;

  inherited;
end;

{ TBridgeBlock }

Constructor TBridgeBlock.Create(AOwner: TComponent);
begin
  inherited;

  IStorePoints:=False;
  IDrawSides:=False;
  Points.IConvex:=False;

  FRoundSlices:=32;
  FRounded:=True;

  FColumnSize:=TPointXYFloat.Create(Self,15,DoChanged);
end;

Destructor TBridgeBlock.Destroy;
begin
  ITexture:=nil;
  IPoints:=nil;

  FColumnSize.Free;
  inherited;
end;

procedure TBridgeBlock.SetColumnSize(const Value:TPointXYFloat);
begin
  FColumnSize.Assign(Value);
end;

procedure TBridgeBlock.Assign(Source: TPersistent);
begin
  if Source is TBridgeBlock then
  with TBridgeBlock(Source) do
  begin
    Self.ColumnSize:=FColumnSize;
    Self.FRounded:=FRounded;
    Self.FRoundSlices:=FRoundSlices;

    Self.DoChanged(Self);
  end;

  inherited;
end;

function TBridgeBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(0.5,1,0),'ColumnSize.X,MinMax:0;100,Invert').Format.Color:=clYellow;
    AddHandle(Point3D(1,1,0.5),'ColumnSize.Y,MinMax:0;100,Invert').Format.Color:=clAqua;
    AddHandle(Point3D(-1,1,0.5),'','Rounded=not Rounded').Format.Color:=clGreen;
  end;
end;

procedure TBridgeBlock.Draw;
var
  tmpColSize : Single;
  tmpColHeight : Single;
  CirclePoints : Integer;

  procedure CalcTextures;
  var tmp : Integer;
      t   : Integer;
  begin
    if Rounded then tmp:=8+4*CirclePoints
               else tmp:=12;

    SetLength(ITexture,tmp);

    ITexture[0]:=PointFloat(0,0);
    ITexture[1]:=PointFloat(0,1);
    ITexture[2]:=PointFloat(0.5*tmpColSize,1);
    ITexture[3]:=PointFloat(0.5*tmpColSize,0);
    ITexture[4]:=PointFloat(1-0.5*tmpColSize,0);
    ITexture[5]:=PointFloat(1-0.5*tmpColSize,1);
    ITexture[6]:=PointFloat(1,1);
    ITexture[7]:=PointFloat(1,0);

    if Rounded then
    begin
      for t:=1 to CirclePoints do
      begin
        tmp:=8+4*(t-1);

        with Points[5+1+t].Point do
             ITexture[tmp]:=PointFloat(0.5*(1+X),1-0.5*(1+Z));

        Inc(tmp);

        with Points[5+1+t-1].Point do
             ITexture[tmp]:=PointFloat(0.5*(1+X),1-0.5*(1+Z));

        Inc(tmp);

        ITexture[tmp]:=PointFloat(0.5*(1+Points[5+1+t-1].Point.X),0);

        Inc(tmp);
        ITexture[tmp]:=PointFloat(0.5*(1+Points[5+1+t].Point.X),0);
      end;
    end
    else
    begin
      ITexture[8]:=PointFloat(1-tmpColSize,1-0.5*tmpColHeight);
      ITexture[9]:=PointFloat(1-tmpColSize,0);
      ITexture[10]:=PointFloat(0.5*tmpColSize,0);
      ITexture[11]:=PointFloat(0.5*tmpColSize,1-0.5*tmpColHeight);
    end;
  end;

  procedure CalcPoints;
  var tmp : Integer;
      t   : Integer;
  begin
    if Rounded then tmp:=8+4*CirclePoints
               else tmp:=12;

    SetLength(IPoints,tmp);

    IPoints[0]:=PointFloat(-1,1);
    IPoints[1]:=PointFloat(-1,-1);
    IPoints[2]:=PointFloat(-1+tmpColSize,-1);
    IPoints[3]:=PointFloat(-1+tmpColSize,1);
    IPoints[4]:=PointFloat(1-tmpColSize,1);
    IPoints[5]:=PointFloat(1-tmpColSize,-1);
    IPoints[6]:=PointFloat(1,-1);
    IPoints[7]:=PointFloat(1,1);

    if Rounded then
    begin
      for t:=1 to CirclePoints do
      begin
        tmp:=8+4*(t-1);

        with Points[5+1+t].Point do
             IPoints[tmp]:=PointFloat(X,Z);

        with Points[5+1+t-1].Point do
             IPoints[tmp+1]:=PointFloat(X,Z);

        IPoints[tmp+2]:=PointFloat(Points[5+1+t-1].Point.X,1);
        IPoints[tmp+3]:=PointFloat(Points[5+1+t].Point.X,1);
      end;
    end
    else
    begin
      IPoints[8]:=PointFloat(1-tmpColSize,-1+tmpColHeight);
      IPoints[9]:=PointFloat(1-tmpColSize,1);
      IPoints[10]:=PointFloat(-1+tmpColSize,1);
      IPoints[11]:=PointFloat(-1+tmpColSize,-1+tmpColHeight);
    end;
  end;

  procedure DrawCover(const AZ:Single; ANormal:Integer; Invert:Boolean=False);

    procedure AddPoint(Index:Integer);
    begin
      with ITexture[Index] do
           glTexCoord2f(X,Y);

      with IPoints[Index] do
           glVertex3f(X,Y,-AZ);
    end;

  var t : Integer;
  begin
    glBegin(GL_QUADS);
    glNormal3i(0,ANormal,0);

    if Invert then
    for t:=Length(IPoints)-1 downto 0 do
        AddPoint(t)
    else
    for t:=0 to Length(IPoints)-1 do
        AddPoint(t);

    glEnd;
  end;

  procedure DoCalcPoints;
  var t     : Integer;
      tmpS,
      tmpC  : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
      tmpRX : Single;
      tmpRY : Single;
  begin
    if Rounded then
       CirclePoints:=RoundSlices
    else
       CirclePoints:=0;

    Points.Clear;
    Points.Add(-1,-1);
    Points.Add(-1,1);
    Points.Add(1,1);
    Points.Add(1,-1);

    tmpColSize:=ColumnSize.X*0.01;

    tmpRX:=1-tmpColSize;

    Points.Add(tmpRX,-1);

    tmpColHeight:=ColumnSize.Y*0.02;

    Points.Add(tmpRX,-1+tmpColHeight);

    if Rounded then
    begin
      tmpRY:=1-(tmpColHeight*0.5);

      for t:=0 to CirclePoints-1 do
      begin
        SinCos(HalfPi+(t*Pi/CirclePoints),tmpS,tmpC);
        Points.Add(tmpS*tmpRX,-1+tmpColHeight-tmpC*tmpRY);
      end;
    end;

    Points.Add(-1+tmpColSize,-1+tmpColHeight);
    Points.Add(-1+tmpColSize,-1);

    CalcPoints;
    CalcTextures;
  end;

var CanCull : Boolean;
begin
  if INeedsRecalc then
  begin
    DoCalcPoints;
    INeedsRecalc:=False;
  end;

  inherited;

  if Format.Solid then
  begin
    CanCull:=(not ShouldDrawInterior) and (Size.Point.Y<>0);

    if CanCull then
       glEnable(GL_CULL_FACE);

    if Assigned(FFront) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(Front).Start;
    end;

    DrawCover(1,1,True);

    if Assigned(FFront) then
       TBlockFormatAccess(Front).Finish;

    if Size.Point.Y<>0 then
    begin
      if Assigned(FBack) then
      begin
        if not Assigned(FFront) then
           TBlockFormatAccess(Format).Finish;

        TBlockFormatAccess(Back).Start;
      end
      else
      if Assigned(FFront) then
         TBlockFormatAccess(Format).Start;

      DrawCover(-1,-1);

      if Assigned(FBack) then
      begin
        TBlockFormatAccess(Back).Finish;
        TBlockFormatAccess(Format).Start;
      end;
    end;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;
end;

procedure TBridgeBlock.SetRounded(const Value: Boolean);
begin
  FRounded := Value;
  DoChanged(Self);
  Repaint;
end;

procedure TBridgeBlock.SetRoundSlices(const Value: Integer);
begin
  FRoundSlices:=Value;
  DoChanged(Self);
  Repaint;
end;

{ TTeePolygonBlock }

Constructor TTeePolygonBlock.Create(AOwner:TComponent);
begin
  inherited;
  IStorePoints:=False;
  Points.IConvex:=True;
end;

procedure TTeePolygonBlock.CreatePoints(Sides:Integer);
var t       : Integer;
    PiStep  : Single;
    tmpSin,
    tmpCos  : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  Points.Clear;

  PiStep:=TwoPi/Sides;

  for t:=0 to Sides-1 do
  begin
    SinCos(TeePiStep+((t+0.5)*PiStep),tmpSin,tmpCos);
    Points.Add(tmpCos,tmpSin);
  end;
end;

procedure TTeePolygonBlock.PrepareForGallery;
begin
end;

{ TPentagonBlock }
Constructor TPentagonBlock.Create(AOwner:TComponent);
begin
  inherited;
  CreatePoints(5);
end;

{ THexagonBlock }
Constructor THexagonBlock.Create(AOwner:TComponent);
begin
  inherited;
  CreatePoints(6);
end;

{ TOctagonBlock }
Constructor TOctagonBlock.Create(AOwner:TComponent);
begin
  inherited;
  CreatePoints(8);
end;

{ TArrowBlock }

Constructor TArrowBlock.Create(AOwner: TComponent);
begin
  inherited;
  IStorePoints:=False;
  FHead:=TPointXYFloat.Create(Self,50,DoChanged);
end;

Destructor TArrowBlock.Destroy;
begin
  FHead.Free;
  inherited;
end;

procedure TArrowBlock.SetHead(const Value:TPointXYFloat);
begin
  FHead.Assign(Value);
end;

procedure TArrowBlock.Draw;
var tmpX : Single;
    tmpZ : Single;
    tmpIndent : Single;
begin
  if INeedsRecalc then
  begin
    tmpX:=Head.X*0.01;
    tmpZ:=Head.Y*0.02;

    tmpIndent:=tmpZ*FIndent*0.005;

    Points.Clear;
    Points.Add(-tmpX,-1);
    Points.Add(tmpX,-1);
    Points.Add(tmpX,1-tmpZ+tmpIndent);
    Points.Add(1,1-tmpZ);
    Points.Add(0,1);
    Points.Add(-1,1-tmpZ);
    Points.Add(-tmpX,1-tmpZ+tmpIndent);

    Points.IConvex:=False;

    INeedsRecalc:=False;
  end;

  inherited;
end;

procedure TArrowBlock.SetIndent(const Value: Single);
begin
  if FIndent<>Value then
  begin
    FIndent:=Value;
    DoChanged(Self);
    Repaint;
  end;
end;

procedure TArrowBlock.Assign(Source: TPersistent);
begin
  if Source is TArrowBlock then
  with TArrowBlock(Source) do
  begin
    Self.Head:=FHead;
    Self.FIndent:=FIndent;
  end;

  inherited;
end;

function TArrowBlock.DesignHandles(AOwner:TComponent): TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    Format.Color:=clYellow;
    AddHandle(Point3D(0.75,1,0.75),'Indent,MinMax:0;100,Invert');

    HandleClass:=TArrowBlock;
    with AddHandle(Point3D(0.25,1,0.75),'Head.X,MinMax:0;100,Invert') do
    begin
      Format.Color:=clAqua;
      Rotation.Z:=90;
    end;

    with AddHandle(Point3D(0.75,1,0.25),'Head.Y,MinMax:0;100,Invert') do
    begin
      Format.Color:=clFuchsia;
      Rotation.Z:=0;
    end;
  end;
end;

{ TRombusBlock }

Constructor TRombusBlock.Create(AOwner: TComponent);
begin
  inherited;

  IStorePoints:=False;

  Points.Clear;
  Points.Add( 0, 1);
  Points.Add(-1, 0);
  Points.Add( 0,-1);
  Points.Add( 1, 0);

  Points.IConvex:=True;
end;

{ TCrossBlock }

Constructor TCrossBlock.Create(AOwner: TComponent);
begin
  inherited;
  IStorePoints:=False;
  FCrossSize:=TPointXYFloat.Create(Self,25,DoChanged);
  FCrossCenter:=TPointXYFloat.Create(Self,50,DoChanged);
end;

Destructor TCrossBlock.Destroy;
begin
  FCrossCenter.Free;
  FCrossSize.Free;
  inherited;
end;

function TCrossBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(-1.0,1,0),'CrossCenter.X,MinMax:0;100').Format.Color:=clFuchsia;
    AddHandle(Point3D(1.0,1,0),'CrossCenter.Y,MinMax:0;100').Format.Color:=clGreen;
    AddHandle(Point3D(-1.0,0,1),'CrossSize.X,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(Point3D(1.0,0,-1),'CrossSize.Y,MinMax:0;100').Format.Color:=clAqua;
  end;
end;

procedure TCrossBlock.Draw;
var tmpX : Single;
    tmpZ : Single;
    XC   : Single;
    YC   : Single;
begin
  if INeedsRecalc then
  begin
    XC:=(CrossCenter.X-50)*0.02;
    YC:=(CrossCenter.Y-50)*0.02;

    tmpX:=CrossSize.X*0.01;
    tmpZ:=CrossSize.Y*0.01;

    Points.Clear;
    Points.Add(XC-tmpX,-1);
    Points.Add(XC+tmpX,-1);
    Points.Add(XC+tmpX,YC-tmpZ);
    Points.Add(1,YC-tmpZ);
    Points.Add(1,YC+tmpZ);
    Points.Add(XC+tmpX,YC+tmpZ);
    Points.Add(XC+tmpX,1);
    Points.Add(XC-tmpX,1);
    Points.Add(XC-tmpX,YC+tmpZ);
    Points.Add(-1,YC+tmpZ);
    Points.Add(-1,YC-tmpZ);
    Points.Add(XC-tmpX,YC-tmpZ);

    Points.IConvex:=False;

    INeedsRecalc:=False;
  end;

  inherited;
end;

procedure TCrossBlock.SetCrossSize(const Value: TPointXYFloat);
begin
  FCrossSize.Assign(Value);
end;

procedure TCrossBlock.SetCrossCenter(const Value: TPointXYFloat);
begin
  FCrossCenter.Assign(Value);
end;

procedure TCrossBlock.Assign(Source: TPersistent);
begin
  if Source is TCrossBlock then
  with TCrossBlock(Source) do
  begin
    Self.CrossCenter:=FCrossCenter;
    Self.CrossSize:=FCrossSize;
  end;

  inherited;
end;

type
  TBlockBorderAccess=class(TBlockBorder);

{ TTapeBlock }

Constructor TTapeBlock.Create(AOwner: TComponent);
begin
  inherited;
  HasYValues:=False;
  FSlices:=32;
  FSlices3D:=32;
  FTapeColorEach:=True;
  FAdjustTexture:=True;

  FCorners:=TTapeCorners.Create(Self,60);
  FCorners.OnChanged:=CornersChanged;

  TBlockFormatAccess(Format).OnColorChanged:=ColorChanged;
end;

Destructor TTapeBlock.Destroy;
begin
  FCorners.Free;
  IBounds:=nil;
  inherited;
end;

procedure TTapeBlock.Assign(Source:TPersistent);
begin
  if Source is TTapeBlock then
  with TTapeBlock(Source) do
  begin
    Self.FAdjustTexture:=FAdjustTexture;
    Self.FClosed:=FClosed;
    Self.Corners:=FCorners;
    Self.FRadius:=FRadius;
    Self.FRoundness:=FRoundness;
    Self.FSlices:=FSlices;
    Self.FSlices3D:=FSlices3D;
    Self.FTape3D:=FTape3D;
    Self.FTapeColorEach:=FTapeColorEach;
  end;

  inherited;
end;

procedure TTapeBlock.ColorChanged(Sender:TObject);
begin
  DeleteLists;
end;

function RightAngle(const A,B:Single; const Dist:Single):Single;
begin
  if Dist=0 then
     result:=0
  else
     result:=ArcSin((B-A)/Dist);
end;

procedure TTapeBlock.SizeChanged(Sender:TObject);
begin
  inherited;
  DeleteLists;
end;

type
  TTextureAccess=class(TBlockTexture);

  TAreaPoint=packed record
    Point  : TPoint3DFloat;
    ZFront : Single;
  end;

function TTapeBlock.GetEditor:String;
begin
  result:='TTapeBlockEditor';
end;

procedure TTapeBlock.Draw;
var
  tmpColor : TColor;

  procedure AddPlane(const Bx,By:Single);
  var tmpL    : Integer;
      P0,
      P1,
      P2 : TPoint3DFloat;
  begin
    tmpL:=Length(IBounds);
    SetLength(IBounds,tmpL+1);

    with IBounds[tmpL] do
    begin
      if tmpL=0 then
      begin
        Normal.X:=0;
        Normal.Y:=0;
        Normal.Z:=0;
      end
      else
      begin
        with IBounds[tmpL-1].Point do
        begin
          P0.X:=X;
          P0.Y:=Y;
          P0.Z:=-1;
        end;

        P1.X:=Bx;
        P1.Y:=By;
        P1.Z:=-1;

        P2.X:=Bx;
        P2.Y:=By;
        P2.Z:=1;

        Normal:=CalculateNormal(P0,P1,P2);
      end;

      Point.X:=Bx;
      Point.Y:=By;

      Color:=tmpColor;
    end;
  end;

var
  tmpSlice : TPointFloatArray;

  procedure AddSlices(const a,b,c,d:TPointFloat; const Start,Angle:Single);
  var t : Integer;
      tmpL : Integer;
      P0,P1,P2 : TPoint3DFloat;
  begin
    CalcBezier(FSlices,a,b,c,d,tmpSlice);

    tmpL:=Length(IBounds);

    SetLength(IBounds,tmpL+Slices);

    for t:=1 to FSlices do
    begin
      with IBounds[tmpL+t-1] do
      begin
        Point.X:=tmpSlice[t].X;
        Point.Y:=tmpSlice[t].Y;

        if (t=1) and (tmpL>0) then
        with IBounds[tmpL+t-2].Point do  // previous point
        begin
          P0.X:=X;
          P0.Y:=Y;
          P0.Z:=-1;
        end
        else
        with tmpSlice[t-1] do
        begin
          P0.X:=X;
          P0.Y:=Y;
          P0.Z:=-1;
        end;

        P1.X:=Point.X;
        P1.Y:=Point.Y;
        P1.Z:=-1;

        P2.X:=P1.X;
        P2.Y:=P1.Y;
        P2.Z:=1;

        Normal:=CalculateNormal(P0,P1,P2);
        Color:=tmpColor;
      end;
    end;
  end;

var
  P0 : TPoint3DFloat;

  procedure AddPoint(Index:Integer; OnlySlice:Boolean=False);
  var P1    : TPoint3DFloat;
      P11   : TPoint3DFloat;
      tmpP1 : TPoint3DFloat;
      tmpX  : Single;
      tmpY  : Single;
      tmpD  : Single;
      tmpDist2 : Single;
      tmpDist  : Single;
      tmpRadius : Single;
      tmpA  : Single;
      tmpSin,
      tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};

      a,a1,
      c1 : TPointFloat;
  begin
    tmpColor:=FPoints[Index].Point.Color;

    P1:=FPoints[Index].Point.Point;

    tmpP1:=P1;

    if (Radius<>0) and (not IBlocks.DrawBlocks.Shadows.Visible) then
    begin
      tmpX:=P1.X-P0.X;
      tmpY:=P1.Z-P0.Z;

      tmpD:=TeeDistance(tmpX,tmpY);

      if IExtruded then tmpRadius:=Radius*0.01
                   else tmpRadius:=Radius;

      if tmpRadius>(tmpD*0.5) then
         tmpRadius:=(tmpD*0.5);

      tmpDist:=tmpD-tmpRadius;
      tmpDist2:=tmpD-tmpRadius*Roundness*0.01;

      if tmpX=0 then
         tmpA:=0
      else
         tmpA:=HalfPi-ArcTan(tmpY/tmpX);

      if tmpX<=0 then
         tmpA:=tmpA+Pi;

      // Reduce end Segment
      if Closed or (Index<FPoints.Count-1) then
      begin
        SinCos(tmpA,tmpSin,tmpCos);

        P1.X:=P0.X+(tmpDist*tmpSin);

        if (tmpX=0) and (tmpY>0) then
            P1.Z:=P0.Z-(tmpDist*tmpCos)
        else
            P1.Z:=P0.Z+(tmpDist*tmpCos);

        P11.X:=P0.X+(tmpDist2*tmpSin);

        if (tmpX=0) and (tmpY>0) then
            P11.Z:=P0.Z-(tmpDist2*tmpCos)
        else
            P11.Z:=P0.Z+(tmpDist2*tmpCos);
      end;

      // Reduce start Segment
      if Closed or (Index>1) then
      begin
        tmpA:=HalfPi-tmpA;
        SinCos(tmpA,tmpSin,tmpCos);

        P0.x:=tmpP1.X-(tmpDist*tmpCos);

        if (tmpX=0) and (tmpY>0) then
           P0.Z:=tmpP1.Z+(tmpDist*tmpSin)
        else
           P0.Z:=tmpP1.Z-(tmpDist*tmpSin);

        if (Index>1) or (Closed and ((Index=0) or OnlySlice)) then
        begin
          c1.x:=tmpP1.X-(tmpDist2*tmpCos);

          if (tmpX=0) and (tmpY>0) then
             c1.y:=tmpP1.Z+(tmpDist2*tmpSin)
          else
             c1.y:=tmpP1.Z-(tmpDist2*tmpSin);

          AddSlices(a,a1,PointFloat(P0.X,P0.Z),c1,
                     RightAngle(a.y,a1.y,TeeDistance(a.x-a1.x,a.y-a1.y)),
                     HalfPi-tmpA);
        end;
      end;

      a.x:=P1.x;
      a.y:=P1.z;

      a1.x:=P11.x;
      a1.y:=P11.z;
    end;

    if not OnlySlice then
       AddPlane(P1.x,P1.z);

    P0:=tmpP1;
  end;

  procedure CalcPolygonBounds;
  var t : Integer;
  begin
    IBounds:=nil;

    if FPoints.Count>0 then
    begin
      tmpColor:=FPoints[0].Point.Color;

      P0:=FPoints[0].Point.Point;

      if (Radius=0) or (not Closed) then
         AddPlane(P0.X,P0.Z);

      if Radius<>0 then
         SetLength(tmpSlice,Slices+1);

      for t:=1 to FPoints.Count-1 do
          AddPoint(t);

      if Closed and (FPoints.Count>2) then
      begin
        AddPoint(0);

        if Radius<>0 then
        begin
          P0:=FPoints[0].Point.Point;
          AddPoint(1,True);
          P0:=FPoints[0].Point.Point;
          AddPoint(1);
        end;
      end;

      tmpSlice:=nil;
    end;
  end;

var
  tmpL : Integer;

  function CalcTotalLength:Single;
  var t : Integer;
      tmpInvCount : Single;
  begin
    result:=0;

    IBounds[0].TexPos:=0;

    if AdjustTexture then
    begin
      for t:=1 to tmpL do
      with IBounds[t] do
      begin
        result:=result+TeeDistance(Point.X-IBounds[t-1].Point.X,Point.Y-IBounds[t-1].Point.Y);
        TexPos:=result;
      end;

      if Closed and (tmpL>1) then
      with IBounds[tmpL].Point do
      begin
        IBounds[0].TexPos:=TeeDistance(X-IBounds[0].Point.X,Y-IBounds[0].Point.Y);
        result:=result+IBounds[0].TexPos;
      end;
    end
    else
    begin
      tmpInvCount:=1/tmpL;

      for t:=1 to tmpL do
          IBounds[t].TexPos:=t*tmpInvCount;
    end;
  end;

  procedure Polyline(const Z:Single);
  var t : Integer;
  begin
    glBegin(GL_LINE_STRIP);

    for t:=0 to tmpL do
    with IBounds[t].Point do
         glVertex3f(X,Y,Z);

    glEnd;
  end;

  procedure LineCapBounds(AIndex:Integer; const Offset:Single);
  begin
    with IBounds[AIndex] do
    begin
      glVertex3f(Point.X,Point.Y+Offset,-1);
      glVertex3f(Point.X,Point.Y+Offset,1);
    end;
  end;

  procedure DrawSegmentLines;
  var t : Integer;
  begin
    if Radius=0 then
    begin
      glBegin(GL_LINES);

      for t:=0 to tmpL do
          LineCapBounds(t,0);

      glEnd;
    end
    else
    if not FClosed then
    begin
      glBegin(GL_LINES);

      LineCapBounds(0,0);

      if tmpL>0 then
         LineCapBounds(tmpL,0);

      glEnd;
    end;
  end;

var
  tmpFactorY : Single;

  procedure DrawSegments;
  var
    tmpS,
    tmpC : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    tmpSinCos  : packed Array of TPointSinCos;

    function CalcRotatedPoint(const Index:Integer; const Center:TFloatPoint):TPoint3DFloat;
    var tmpY : Single;
    begin
      tmpY:=tmpSinCos[Index].Y*tmpFactorY;

      if IsArea then
      begin
        result.X:=Center.X;

        if ConcaveArea then
           result.Y:=Center.Y+tmpY
        else
           result.Y:=Center.Y-tmpY;
      end
      else
      begin
        result.X:=Center.X+(tmpY*tmpS);
        result.Y:=Center.Y+(tmpY*tmpC);
      end;

      result.Z:=tmpSinCos[Index].X;
    end;

  var
    IArea : Array of TAreaPoint;

    tmpTeX     : Single;
    tmpTeXA    : Single;
    tmpPointsA : TPoint3DArray;
    ILen3D     : Integer;
    tmpAngleFirst : Single;
    tmpColorFirst : TColor;
    tmpAngleLast  : Single;
    tmpColorLast  : TColor;
    OldColor : TColor;

    procedure DrawSlices(Segment:Integer);
    var tmpP,
        tmpB : TPoint3DFloat;
        tmp0 : TFloatPoint;
        tmp1 : TFloatPoint;
        t    : Integer;
        tmpEnd,
        tmpMid : Integer;
        tmp  : Single;
        tmpAngle,
        tmpDifX,
        tmpDifY,
        tmpBoundX,
        tmpBoundZ,
        tmpTeY  : Single;

        tmpNormal : TPoint3DFloat;
    begin
      tmp0:=IBounds[Segment-1].Point;
      tmp1:=IBounds[Segment].Point;

      tmpDifX:=tmp1.X-tmp0.X;
      tmpDifY:=tmp1.Y-tmp0.Y;

      tmpBoundX:=IMax.X-IMin.X;
      tmpBoundZ:=IMax.Z-IMin.Z;

      tmpDifX:=tmpDifX/tmpBoundX;
      tmpDifY:=tmpDifY/tmpBoundZ;

      tmpDifX:=tmpDifX*(Size.X/Size.Z);

      if Abs(tmpDifY)<0.0001 then
         if Abs(tmpDifX)<0.0001 then
         begin
           if IsArea then
              IArea[Segment]:=IArea[Segment-1];

           Exit;
         end
         else
            tmpAngle:=Pi
      else
      if Abs(tmpDifX)<0.0001 then
         if tmpDifY<0 then
            tmpAngle:=-HalfPi
         else
            tmpAngle:=HalfPi
      else
         tmpAngle:=Pi-ArcTan2(tmpDifY,tmpDifX);

      SinCos(tmpAngle,tmpS,tmpC);

      if Segment=1 then
      begin
        tmpAngleFirst:=tmpAngle;
        tmpColorFirst:=OldColor;

        for t:=0 to ILen3D-1 do
            tmpPointsA[t]:=CalcRotatedPoint(t,tmp0);
      end
      else
      if Segment=tmpL then
      begin
        tmpAngleLast:=tmpAngle;
        tmpColorLast:=OldColor;
      end;

      tmpP:=tmpPointsA[ILen3D-1];

      tmp:=1/(ILen3D-1);

      if IsArea then
      begin
        if Corners.Enabled then
           tmpMid:=2*Corners.Slices
        else
           tmpMid:=(ILen3D div 4);
      end
      else
         tmpMid:=0;

      if IsArea and Corners.Enabled then
         tmpEnd:=ILen3D-2
      else
         tmpEnd:=(ILen3D-tmpMid)-1;

      if IsArea and (Segment=1) then
      begin
        IArea[Segment-1].Point:=tmpPointsA[tmpEnd];
        IArea[Segment-1].ZFront:=tmpPointsA[tmpMid].Z;
      end;

      for t:=tmpMid to tmpEnd do
      begin
        tmpTeY:=tmp*t;

        tmpB:=CalcRotatedPoint(t,tmp1);

        tmpNormal:=CalculateNormal(tmpPointsA[t],tmpP,tmpB);

        with tmpNormal do
             glNormal3f(X,Y,Z);

        glTexCoord2f(tmpTeXA,tmpTeY);
        glVertex3fv(@tmpPointsA[t]);

        glTexCoord2f(tmpTeX,tmpTeY);
        glVertex3fv(@tmpB);

        tmpP:=tmpPointsA[t];

        tmpPointsA[t]:=tmpB;
      end;

      if IsArea then
      begin
        IArea[Segment].Point:=tmpPointsA[tmpEnd];
        IArea[Segment].ZFront:=tmpPointsA[tmpMid].Z;
      end;
    end;

  var
    tmpTapeColorEach : Boolean;

    procedure DrawCover(Segment:Integer; const Angle:Single; const AColor:TColor;
                        Invert:Boolean=False);
    var t : Integer;
        tmpP0 : TPoint3DFloat;
        tmpP1 : TPoint3DFloat;
        tmpP2 : TPoint3DFloat;
    begin
      if tmpTapeColorEach then
         TBlockFormatAccess(Format).SetDirectColor(AColor);

      glBegin(GL_TRIANGLE_FAN);

      SinCos(Angle,tmpS,tmpC);

      with IBounds[Segment] do
      begin
        glVertex3f(Point.X,Point.Y,0);

        tmpP1:=CalcRotatedPoint(0,Point);
        tmpP2:=CalcRotatedPoint(1,Point);

        tmpP0.X:=Point.X;
        tmpP0.Y:=Point.Y;
        tmpP0.Z:=0;

        with CalculateNormal(tmpP2,tmpP1,tmpP0) do
             glNormal3f(X,Y,Z);

        if Invert then
           for t:=ILen3D-1 downto 0 do
           with CalcRotatedPoint(t,Point) do
                glVertex3f(X,Y,Z)
        else
           for t:=0 to ILen3D-1 do
           with CalcRotatedPoint(t,Point) do
                glVertex3f(X,Y,Z);
      end;

      glEnd;
    end;

    procedure DrawArea(IsBack,Z0,IsSide:Boolean; AInit:Integer);

      procedure AddPoint(Index:Integer);
      var tmpZ : Single;
      begin
        with IArea[Index] do
        begin
          if Z0 then tmpZ:=ZFront
                else tmpZ:=Point.Z;

          glVertex3f(Point.X,Point.Y,tmpZ);
          glVertex3f(Point.X,IMin.Z,tmpZ);
        end;
      end;

      procedure AddPointZero;
      var tmpZ : Single;
      begin
        with IArea[0] do
        if Z0 then tmpZ:=ZFront
              else tmpZ:=Point.Z;

        with IBounds[0].Point do
        begin
          glVertex3f(X,Y+tmpFactorY,tmpZ);
          glVertex3f(X,IMin.Z,tmpZ);
        end;
      end;

    var t : Integer;
    begin
      if IsBack then
      begin
        for t:=tmpL downto AInit do
            AddPoint(t);

        AddPointZero;
      end
      else
      begin
        AddPointZero;

        for t:=AInit to tmpL do
            AddPoint(t);
      end;
    end;

    {
    procedure PolylineArea(const ZPos:Single);
    var t : Integer;
    begin
      glBegin(GL_LINE_STRIP);

      for t:=0 to Length(IArea)-1 do
      with IArea[t].Point do
           glVertex3f(X,Y,ZPos);

      glEnd;
    end;
    }

    procedure DoDrawArea(tmpInit:Integer);

      procedure DoAreaFlat;
      var tmpNormal : TPoint3DFloat;
          tmpX      : Single;
      begin
        glBegin(GL_QUAD_STRIP);

        glGetFloatv(GL_CURRENT_NORMAL,@tmpNormal);

        // Front

        //glNormal3i(0,0,1);
        DrawArea(False,True,False,tmpInit);

        tmpNormal.Z:=-tmpNormal.Z;
        glNormal3fv(@tmpNormal);

        // Back

        //glNormal3i(0,0,-1);
        DrawArea(True,False,False,tmpInit);

        // Left side
        glNormal3i(-1,0,0);
        DrawArea(False,True,True,tmpInit);

        glEnd;

        // Bottom
        glNormal3i(0,-1,0);
        glBegin(GL_QUADS);

        tmpX:=IBounds[0].Point.X;
        glVertex3f(tmpX,IMin.Z,IArea[0].ZFront);
        glVertex3f(tmpX,IMin.Z,IArea[0].Point.Z);

        tmpX:=IArea[Length(IArea)-1].Point.X;
        glVertex3f(tmpX,IMin.Z,IArea[0].Point.Z);
        glVertex3f(tmpX,IMin.Z,IArea[0].ZFront);

        glEnd;
      end;

      procedure DoAreaRound;

        procedure DoPoint(const P0,P1:TPoint3DFloat; const AZ:Single);
        var tt : Integer;
            tmpMid : Integer;
        begin
          glBegin(GL_QUAD_STRIP);

          with P1 do
               glVertex3f(X,Y,AZ);

          with P0 do
               glVertex3f(X,Y,AZ);

          tmpMid:=2*Corners.Slices-1;

          for tt:=tmpMid to ILen3D-1 do
          begin
            with tmpSinCos[tt] do
                 glNormal3f(0,0,X);

            glVertex3f(P1.X,tmpSinCos[tt].Y*tmpFactorY,tmpSinCos[tt].X);
            glVertex3f(P0.X,tmpSinCos[tt].Y*tmpFactorY,tmpSinCos[tt].X);
          end;

          glEnd;
        end;

      var tmp : TPoint3DFloat;
          t   : Integer;
      begin
        tmp.X:=IBounds[0].Point.X;
        tmp.Y:=IBounds[0].Point.Y;

        DoPoint(tmp,IArea[tmpInit].Point,IArea[tmpInit].ZFront);

        for t:=tmpInit to tmpL-1 do
            DoPoint(IArea[t].Point,IArea[t+1].Point,IArea[t].ZFront);
      end;

    begin
      if IListArea=0 then
      begin
        IListArea:=CreateNewList;

        if AreaRound then
           DoAreaRound
        else
           DoAreaFlat;

        {
        if TBlockFormatAccess(Format).PreparePen then
        begin
          PolylineArea(IArea[0].Point.Z);
          PolylineArea(IArea[0].ZFront);
        end;
        }

        glEndList;
      end
      else
         glCallList(IListArea);

      IArea:=nil;
    end;

  var tmpReal  : TColor;
      tmpInit  : Integer;
      t        : Integer;
      tt       : Integer;
      tmpAngle : Single;
      tmpCorners : TRoundPoints;
  begin
    if (FSlices3D<3) and (not FCorners.Enabled) then
       Exit;

    glBegin(GL_QUAD_STRIP);

    if IBlocks.DrawBlocks.Shadows.Visible then
       tmpTapeColorEach:=False
    else
       tmpTapeColorEach:=FTapeColorEach;

    tmpReal:=TBlockFormatAccess(Format).GetRealColor;
    OldColor:=tmpReal;

    if Tape3D=0 then
       tmpInit:=0
    else
    begin
      tmpInit:=1;

      tmpFactorY:=Tape3D*0.005*(IMax.Z-IMin.Z);

      if FCorners.Enabled then
      begin
        FCorners.CalcPoints(1,1,tmpCorners);
        ILen3D:=Length(tmpCorners.Points);

        SetLength(tmpSinCos,ILen3D+1);

        for tt:=0 to ILen3D-1 do
        with tmpCorners.Points[ILen3D-1-tt] do
        begin
          tmpSinCos[tt].X:=X;
          tmpSinCos[tt].Y:=Y;
        end;

        Inc(ILen3D);

        tmpSinCos[ILen3D-1]:=tmpSinCos[0];

        FCorners.FreePoints(tmpCorners);
      end
      else
      begin
        tmpAngle:=TwoPi/Pred(FSlices3D);

        SetLength(tmpSinCos,FSlices3D);

        for tt:=0 to FSlices3D-1 do
        with tmpSinCos[tt] do
             SinCos(tmpAngle*tt,X,Y);

        ILen3D:=FSlices3D;
      end;

      SetLength(tmpPointsA,ILen3D);

      with IBounds[0] do
      if AdjustTexture then
         tmpTeXA:=TexPos*ITotalLength
      else
         tmpTeXA:=TexPos;
    end;

    if IsArea then
       SetLength(IArea,tmpL+1);

    for t:=tmpInit to tmpL do
    with IBounds[t] do
    begin
      if tmpTapeColorEach then
         if Color<>OldColor then
         begin
           if Color=clDefault then
           begin
             if OldColor<>tmpReal then
             begin
               OldColor:=tmpReal;
               TBlockFormatAccess(Format).SetDirectColor(OldColor);
             end;
           end
           else
           begin
             OldColor:=Color;
             TBlockFormatAccess(Format).SetDirectColor(OldColor);
           end;
         end;

      if AdjustTexture then
         tmpTeX:=TexPos*ITotalLength
      else
         tmpTeX:=TexPos;

      if tmpInit=1 then
      begin
        // Thick
        DrawSlices(t);

        glEnd;

        glBegin(GL_QUAD_STRIP);

        tmpTeXA:=tmpTeX;
      end
      else
      begin
        // Flat tape

        with Normal do
             glNormal3f(X,Y,Z);

        glTexCoord2f(tmpTeX,1);
        glVertex3f(Point.X,Point.Y,-1);

        glTexCoord2f(tmpTeX,0);
        glVertex3f(Point.X,Point.Y,1);
      end;
    end;

    if tmpTapeColorEach then
       TBlockFormatAccess(Format).SetDirectColor(tmpReal);

    if tmpInit=0 then
       glEnd
    else
    begin
      glEnd;

      if not IsArea then
      begin
        DrawCover(0,tmpAngleFirst,tmpColorFirst);
        DrawCover(tmpL,tmpAngleLast,tmpColorLast,True);
      end;
    end;

    if IsArea then
       DoDrawArea(tmpInit);

    tmpPointsA:=nil;
    tmpSinCos:=nil;
  end;

  procedure DrawOutline;
  begin
    if IListLine=0 then
    begin
      IListLine:=CreateNewList;

      Polyline(-1);
      Polyline(1);

      DrawSegmentLines;

      glEndList;
    end
    else
      glCallList(IListLine);

    TBlockFormatAccess(Format).FinishPen;
  end;

var CanCull : Boolean;
begin
  if Points.IChanged then
  begin
    CalcPolygonBounds;

    DeleteLists;

    tmpL:=Length(IBounds)-1;

    if tmpL=-1 then
       ITotalLength:=0
    else
       ITotalLength:=1/CalcTotalLength;
  end
  else
    tmpL:=Length(IBounds)-1;

  inherited;

  if Points.Count>0 then
  begin
    if not IExtruded then
    begin
      glPushMatrix;
      glScalef(2/(IMax.X-IMin.X), 2/(IMax.Z-IMin.Z), 1);
      glTranslatef(-(IMin.X+IMax.X)*0.5,-(IMin.Z+IMax.Z)*0.5,0);
    end;

    CanCull:=not ShouldDrawInterior and (Tape3D<>0);
    if CanCull then
       glEnable(GL_CULL_FACE);

    if (IPicking or IBlocks.DrawBlocks.Shadows.Visible) and TapeColorEach then
       DrawSegments
    else
    if IList=0 then
    begin
      IList:=CreateNewList;
      DrawSegments;
      glEndList;
    end
    else
      glCallList(IList);

    if CanCull then
       glDisable(GL_CULL_FACE);

    if (Tape3D=0) and (tmpL>-1) and TBlockFormatAccess(Format).PreparePen then
       DrawOutline;

    if not IExtruded then
       glPopMatrix;
  end;
end;

procedure TTapeBlock.SetClosed(const Value:Boolean);
begin
  FClosed:=Value;
  FPoints.IChanged:=True;
  Repaint;
end;

procedure TTapeBlock.SetRadius(const Value: Single);
begin
  FRadius:=Value;
  FPoints.IChanged:=True;
  Repaint;
end;

procedure TTapeBlock.SetRoundness(const Value: Integer);
begin
  FRoundness:=Value;
  FPoints.IChanged:=True;
  Repaint;
end;

procedure TTapeBlock.SetSlices(const Value: Integer);
begin
  FSlices:=Value;
  FPoints.IChanged:=True;
  Repaint;
end;

procedure TTapeBlock.SetSlices3D(const Value: Integer);
begin
  FSlices3D:=Value;
  DeleteLists;
end;

procedure TTapeBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListArea);
  DeleteList(IListLine);
end;

procedure TTapeBlock.SetTape3D(const Value: Single);
begin
  FTape3D:=Value;
  DeleteLists;
end;

procedure TTapeBlock.SetTapeColorEach(const Value: Boolean);
begin
  FTapeColorEach:=Value;
  DeleteLists;
end;

procedure TTapeBlock.CornersChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TTapeBlock.SetAdjustTexture(const Value: Boolean);
begin
  FAdjustTexture := Value;
  FPoints.IChanged:=True;
  Repaint;
end;

procedure TTapeBlock.SetCorners(const Value: TTapeCorners);
begin
  FCorners.Assign(Value);
end;

{ TPentagramBlock }

const
  DefaultInner=0.4;

Constructor TStarBlock.Create(AOwner: TComponent);
begin
  inherited;
  IStorePoints:=False;
  FSlant:=0;
  FInner:=DefaultInner;
end;

procedure TStarBlock.CreatePoints;
var t       : Integer;
    tmp     : Single;
    tmpSlant: Single;
    PiStep  : Single;
    tmpSin,
    tmpCos  : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  Points.Clear;

  PiStep:=TwoPi/ISides;

  tmpSlant:=0.6+(FSlant/(360/ISides))*PiStep;

  for t:=0 to ISides-1 do
  begin
    tmp:=TeePiStep+(t*PiStep)+0.3;

    SinCos(tmp,tmpSin,tmpCos);
    Points.Add(tmpCos,tmpSin);

    SinCos(tmp+tmpSlant,tmpSin,tmpCos);
    Points.Add(tmpCos*FInner,tmpSin*FInner);
  end;

  Points.IConvex:=False;
end;

function TStarBlock.IsInnerStored: Boolean;
begin
  result:=FInner<>DefaultInner;
end;

function TStarBlock.IsSlantStored: Boolean;
begin
  result:=FSlant<>0;
end;

procedure TStarBlock.SetInner(const Value: Single);
begin
  FInner := Value;
  CreatePoints;
  Repaint;
end;

procedure TStarBlock.SetSlant(const Value: Single);
begin
  FSlant := Value;
  CreatePoints;
  Repaint;
end;

{ TPentagramBlock }

Constructor TPentagramBlock.Create(AOwner: TComponent);
begin
  inherited;
  ISides:=5;
  CreatePoints;
end;

{ THexagramBlock }

Constructor THexagramBlock.Create(AOwner: TComponent);
begin
  inherited;
  ISides:=6;
  CreatePoints;
end;

{ TRectPyramidBlock }

Constructor TRectPyramidBlock.Create(AOwner: TComponent);
begin
  inherited;
  IStorePoints:=False;
  CreatePoints;
end;

procedure TRectPyramidBlock.Assign(Source:TPersistent);
begin
  if Source is TRectPyramidBlock then
  with TRectPyramidBlock(Source) do
  begin
    Self.FLeft:=FLeft;
    Self.FRight:=FRight;
  end;

  inherited;
end;

procedure TRectPyramidBlock.CreatePoints;
begin
  Points.Clear;

  if FLeft<>100 then
     Points.Add(-1,((100-FLeft)*0.02)-1);

  Points.Add(-1,-1);
  Points.Add(1,-1);

  if FRight<>0 then
     Points.Add(1,(FRight*0.02)-1);

  Repaint;
end;

procedure TRectPyramidBlock.SetLeft(const Value: Single);
begin
  FLeft := Value;
  CreatePoints;
end;

procedure TRectPyramidBlock.SetRight(const Value: Single);
begin
  FRight := Value;
  CreatePoints;
end;

function TRectPyramidBlock.DesignHandles(AOwner: TComponent): TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(-1,1,0.5),'LeftPercent,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(Point3D( 1,1,0.5),'RightPercent,MinMax:0;100').Format.Color:=clAqua;
  end;
end;

{ TTapeCorners }

procedure TTapeCorners.Assign(Source: TPersistent);
begin
  if Source is TTapeCorners then
     FEnabled:=TTapeCorners(Source).FEnabled;

  inherited;
end;

procedure TTapeCorners.SetEnabled(const Value: Boolean);
begin
  FEnabled:=Value;
  TBlockAccess(IOwner).DeleteLists;
end;

initialization
  RegisterBlocks([ TArrowBlock,
                   TBridgeBlock,
                   TCrossBlock,
                   TExtrudedBlock,
                   THexagonBlock,
                   THexagramBlock,
                   TOctagonBlock,
                   TPentagonBlock,
                   TPentagramBlock,
                   TRectPyramidBlock,
                   TRombusBlock,
                   TTapeBlock ]);
end.
