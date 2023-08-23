unit TeePhysics;
{$I TeeDefs.inc}

interface

uses
  Classes, SysUtils,
  TeCanvas, TeeBlocks, TeeAnimate, TeePlayMP3;

type
  TMovement=packed record
    Block     : TCustomBlock;
    Rotation  : Double;
    Elevation : Double;
    Direction : TPoint3DFloat;
    Speed     : Double;
  end;

  TMovements=packed Array of TMovement;

  TCollision=packed record
    Index    : Integer;
    Angle    : Double;
    IsStatic : Boolean;
  end;

  TBlockPhysics=class(TComponent)
  private
    Movements : TMovements;
    Statics   : Array of TCustomBlock;

    function InternalCollision(ABlock:TCustomBlock; const NewLocation:TPoint3DFloat;
                               ABlocks:TBlocks=nil):TCollision;
    function MovementOf(Block:TCustomBlock):Integer;
    procedure SetDirection(Index:Integer; const ARotation,AElevation:Double);
    procedure Timer1Timer(Sender: TObject);
  public
    Deceleration : Double;
    Gravity      : Double;
    Sound        : Boolean;
    ShowDebug    : Boolean;
    SoundHit     : String;

    T0 : TeeBlocks.TTeeTextBlock ;
    B0,B1,B2 : TCustomBlock;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Add(AObject:TCustomObjectBlock); overload;
    procedure Add(ABlocks:TBlocks); overload;
    procedure AddStatic(ABlocks:TBlocks);
    procedure Clear;
    function Collision(ABlock:TCustomBlock; ABlocks:TBlocks=nil): TCustomBlock;
    procedure Push(ABlock:TCustomBlock; const ASpeed,ARotation,AElevation:Double);
  end;

implementation

uses
  Math;

type
  TTeeAnimateAccess=class(TTeeAnimate);

{ TBlockPhysics }

Constructor TBlockPhysics.Create(AOwner: TComponent);
var tmp : TMultiTimer;
begin
  inherited;

  Deceleration:=30; // %
  Gravity:=9.8; // m/s2

  Sound:=True;
  SoundHit:='$(TEEMaker)\Effects\Pool Ball Hit.mp3'; // 'kick.wav';

  TPlayMP3Sound.SetGlobalPath(TBlocks.ParseFileName('Sounds','$(TEEMaker)\'));

  tmp:={$IFNDEF CLR}TTeeAnimateAccess{$ENDIF}(TTeeAnimate).GlobalTimer;
  tmp.AddEvent(Timer1Timer);
end;

Destructor TBlockPhysics.Destroy;
begin
  Movements:=nil;
  inherited;
end;

  function AngleToStr(const Angle:Double):String;
  begin
    result:=FormatFloat('#.0',Angle*180/Pi);
  end;

procedure TBlockPhysics.SetDirection(Index:Integer; const ARotation,AElevation:Double);
var tmpSin : Extended;
    tmpCos : Extended;
begin
  with Movements[Index] do
  begin
    Rotation:=ARotation;
    Elevation:=AElevation;

    SinCos(ARotation,tmpSin,tmpCos);

    Direction.X:=tmpSin;
    Direction.Y:=tmpCos;
    Direction.Z:=Sin(AElevation);

    if ShowDebug then
       T0.Text:='New: '+#13+
                'ABlock: '+AngleToStr(Rotation);
  end;
end;

procedure TBlockPhysics.Push(ABlock:TCustomBlock; const ASpeed,ARotation,AElevation:Double);
var tmp : Integer;
begin
  tmp:=MovementOf(ABlock);

  if tmp<>-1 then
  with Movements[tmp] do
  begin
    Speed:=Speed+ASpeed;
    SetDirection(tmp,ARotation,AElevation);
  end;
end;

procedure TBlockPhysics.Add(AObject: TCustomObjectBlock);
begin
  Add(AObject.Items);
end;

procedure TBlockPhysics.AddStatic(ABlocks: TBlocks);
var t   : Integer;
    tmp : Integer;
begin
  tmp:=Length(Statics);

  SetLength(Statics,tmp+ABlocks.Count);

  for t:=0 to ABlocks.Count-1 do
      Statics[tmp+t]:=ABlocks[t];
end;

procedure TBlockPhysics.Add(ABlocks: TBlocks);
var t   : Integer;
    tmp : Integer;
begin
  tmp:=Length(Movements);

  SetLength(Movements,tmp+ABlocks.Count);

  for t:=0 to ABlocks.Count-1 do
  with Movements[tmp+t] do
  begin
    Direction.X:=0;
    Direction.Y:=0;
    Direction.Z:=0;

    Speed:=0;
    Block:=ABlocks[t];
  end;

  ABlocks.Repaint;
end;

procedure ProjectBounds(const P:TPoint3DFloat; var AMin,AMax:TPoint3DFloat);
begin
  with AMin do
  begin
    X:=X+P.X;
    Y:=Y+P.Y;
    Z:=Z+P.Z;
  end;

  with AMax do
  begin
    X:=X+P.X;
    Y:=Y+P.Y;
    Z:=Z+P.Z;
  end;
end;

// Returns index and angle in ABlocks (or index in Movements[] array)
// of the first element that collides with ABlock.
// Returns result.Index = -1 if no element collides.
function TBlockPhysics.InternalCollision(ABlock:TCustomBlock;
           const NewLocation:TPoint3DFloat; ABlocks:TBlocks=nil):TCollision;

  function CubesIntersect(const AMin1,AMax1,AMin2,AMax2:TPoint3DFloat):Boolean;

    function CornerInCube(const P:TPoint3DFloat):Boolean; overload;
    begin
      with P do
         result:=(X>=AMin1.X) and (X<=AMax1.X) and
                 (Y>=AMin1.Y) and (Y<=AMax1.Y) and
                 (Z>=AMin1.Z) and (Z<=AMax1.Z);
    end;

    function CornerInCube(const X,Y,Z:Double):Boolean; overload;
    begin
      result:=(X>=AMin1.X) and (X<=AMax1.X) and
              (Y>=AMin1.Y) and (Y<=AMax1.Y) and
              (Z>=AMin1.Z) and (Z<=AMax1.Z);
    end;

  begin
    result:=CornerInCube(AMin2) or
            CornerInCube(AMax2) or
            CornerInCube(AMax2.X,AMin2.Y,AMin2.Z) or
            CornerInCube(AMin2.X,AMax2.Y,AMin2.Z) or
            CornerInCube(AMin2.X,AMin2.Y,AMax2.Z) or
            CornerInCube(AMin2.X,AMax2.Y,AMax2.Z) or
            CornerInCube(AMax2.X,AMin2.Y,AMax2.Z) or
            CornerInCube(AMax2.X,AMax2.Y,AMin2.Z);
  end;

var
  AMin : TPoint3DFloat;
  AMax : TPoint3DFloat;
  tmpSphere1 : Boolean;

  {
  function AngleOfTriangle(const P0,P1,P2:TPointDouble):Double;
  var n,d,a,b,c : Double;
  begin
    a:=Sqrt(Sqr(P1.X-P0.X)+Sqr(P1.Y-P0.Y));
    b:=Sqrt(Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y));
    c:=Sqrt(Sqr(P2.X-P0.X)+Sqr(P2.Y-P0.Y));

    n:=-(a*a)+(b*b)+(c*c);
    d:=2*b*c;
    result:=ArcCos(n/d);
  end;
  }

  function BlockCollision(Block:TCustomBlock; var Angle:Double):Boolean;
  var tmp     : TPoint3DFloat;
      tmpMin  : TPoint3DFloat;
      tmpMax  : TPoint3DFloat;
      tmpDist : Double;
      P0,
      P1,
      P2      : TFloatPoint;
      tmpI    : Integer;
  begin
    if tmpSphere1 and (Block is TSphereBlock) then
    begin
      tmp:=Block.Location.Point;

      tmpDist:=Sqrt( Sqr(NewLocation.X-tmp.X)+
                     Sqr(NewLocation.Y-tmp.Y)+
                     Sqr(NewLocation.Z-tmp.Z) );

      result:=tmpDist<0.5*(TSphereBlock(ABlock).Radius+TSphereBlock(Block).Radius);

      // Angle
      if result then
      begin
        P0.X:=ABlock.Location.Point.X;
        P0.Y:=ABlock.Location.Point.Y;

        P1.X:=NewLocation.X;
        P1.Y:=NewLocation.Y;

        P2.X:=Block.Location.Point.X;
        P2.Y:=Block.Location.Point.Y;

        //Angle:=ArcTan2(P1.Y-P0.Y,P1.X-P0.X)+ArcTan2(P2.Y-P1.Y,P2.X-P1.X);

        tmpI:=MovementOf(ABlock);

        if tmpI<>-1 then
           Angle:=AngleOf(P1,P0,P2)+Movements[tmpI].Rotation;

        if ShowDebug then
        begin
          T0.Text:='New: '+AngleToStr(Angle)+#13+
                   'ABlock: '+AngleToStr(Movements[tmpI].Rotation);

          B0.Location.Point:=ABlock.Location.Point;
          B1.Location.Point:=NewLocation;
          B2.Location.Point:=Block.Location.Point;

          B0.Location.Point.Z:=31;
          B1.Location.Point.Z:=31;
          B2.Location.Point.Z:=31;
        end;
      end;
    end
    else
    begin
      Block.BoundingBox(tmpMin,tmpMax);
      ProjectBounds(Block.Location.Point,tmpMin,tmpMax);

      result:=CubesIntersect(AMin,AMax,tmpMin,tmpMax) or
              CubesIntersect(tmpMin,tmpMax,AMin,AMax);

      if result then
         Angle:=Pi;
    end;
  end;

var t : Integer;
begin
  result.Index:=-1;
  result.IsStatic:=False;

  tmpSphere1:=ABlock is TSphereBlock;

  ABlock.BoundingBox(AMin,AMax);
  ProjectBounds(NewLocation,AMin,AMax);

  // Find collision
  if Assigned(ABlocks) then
  begin
    with ABlocks do
    for t:=0 to Count-1 do
        if Block[t]<>ABlock then
           if BlockCollision(Block[t],result.Angle) then
           begin
             result.Index:=t;
             break;
           end;
  end
  else
  begin
    for t:=0 to Length(Movements)-1 do
      with Movements[t] do
           if Block<>ABlock then
              if BlockCollision(Block,result.Angle) then
              begin
                result.Index:=t;
                Exit;
              end;

    for t:=0 to Length(Statics)-1 do
              if BlockCollision(Statics[t],result.Angle) then
              begin
                result.Index:=t;
                result.IsStatic:=True;
                Exit;
              end;
  end;
end;

// Returns the first block (in "Blocks" or in Movements array) that
// collide with Block parameter.
// Returns nil if Block does not collide with any block.
function TBlockPhysics.Collision(ABlock: TCustomBlock; ABlocks:TBlocks=nil): TCustomBlock;
var tmp : TCollision;
begin
  tmp:=InternalCollision(ABlock,ABlock.Location.Point,ABlocks);

  if tmp.Index=-1 then
     result:=nil
  else
  if Assigned(ABlocks) then
     result:=ABlocks[tmp.Index]
  else
     result:=Movements[tmp.Index].Block;
end;

function TBlockPhysics.MovementOf(Block: TCustomBlock): Integer;
var t : Integer;
begin
  result:=-1;

  for t:=0 to Length(Movements)-1 do
  if Movements[t].Block=Block then
  begin
    result:=t;
    break;
  end;
end;

procedure TBlockPhysics.Timer1Timer(Sender: TObject);

  function SetNewLocation(ABlock:TCustomBlock; const DeltaX,DeltaY,DeltaZ:Double):TCollision;
  var tmp : TPoint3DFloat;
  begin
    tmp:=ABlock.Location.Point;

    with tmp do
    begin
      X:=X+DeltaX;
      Y:=Y+DeltaY;
      Z:=Z+DeltaZ;
    end;

    result:=InternalCollision(ABlock,tmp);

    if result.Index<>-1 then
    begin
      if Sound then
         TPlayMP3Sound.PlayFile(SoundHit);
    end
    else
    begin
      // Set new location
      ABlock.Location.Point:=tmp;
      ABlock.Repaint;
    end;
  end;

var t : Integer;
    tmpCrash : TCollision;
begin
  for t:=0 to Length(Movements)-1 do
  with Movements[t] do
  begin
    (*
    if Gravity>0 then
    begin
      if Direction.Z>0 then
      begin
        Speed:=Speed*(1-Gravity*0.01);

        if Speed<=0 then
        begin
          Direction.Z:=-1;
          Speed:=Gravity*0.01;
        end;
      end
      else
      if Direction.Z<0 then
      begin
        {if Speed=0 then
           Speed:=Gravity*0.01
        else}
           Speed:=Speed*(1+Gravity*0.01);
      end;
    end;
    *)
    
    if Speed>0 then
    with Block.Location do
    begin
      tmpCrash:=SetNewLocation(Block, Speed*Direction.X,Speed*Direction.Y,
                                      Speed*Direction.Z);

      if tmpCrash.Index<>-1 then
      begin
        // After a crash, decelerate a little:
        Speed:=Speed*0.95;

        // Invert direction when crashing to a static object
        if tmpCrash.IsStatic then
           SetDirection(t,TwoPi-tmpCrash.Angle,0)
        else
        begin
          Push(Movements[tmpCrash.Index].Block,Speed,TwoPi-tmpCrash.Angle,0);

          SetDirection(t,Rotation-tmpCrash.Angle,Elevation);

          Speed:=Sin(Abs(tmpCrash.Angle-Pi))*Speed;

          {if Gravity<>0 then
             if Direction.Z<0 then
                Direction.Z:=0;}
        end;
      end
      else
      begin
        // Friction:

        if Speed<0.01 then
           Speed:=0
        else
           Speed:=Speed*(1-Deceleration*0.001);
      end;
    end;
  end;
end;

procedure TBlockPhysics.Clear;
begin
  Movements:=nil;
end;

end.
