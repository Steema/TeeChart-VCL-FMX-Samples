unit TeePipe;
{$I TeeDefs.inc}

interface

uses
  Windows,
  Classes, SysUtils,
  Graphics,
  TeCanvas, TeeBlocks,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeExtruded;

type
  TPipeBlock=class(TPathBlock)
  private
    FClosed    : Boolean;
    FConnector : TCustomBlock;
    FConnectorVisible : Boolean;
    FRadius    : TPointXYFloat;

    ICustomConnector : Boolean;
    IGlobalConnector : TCustomBlock;

    procedure CreateConnector;
    function IsConnectorStored:Boolean;
    procedure SetClosed(const Value:Boolean);
    procedure SetConnector(const Value:TCustomBlock);
    procedure SetConnectorVisible(const Value:Boolean);
    procedure SetRadius(const Value:TPointXYFloat);
  protected
    procedure DeleteLists; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
  published
    property Closed:Boolean read FClosed write SetClosed default False;
    property Connector:TCustomBlock read FConnector write SetConnector stored IsConnectorStored;
    property ConnectorVisible:Boolean read FConnectorVisible write SetConnectorVisible default True;
    property Radius:TPointXYFloat read FRadius write SetRadius;
  end;

implementation

uses
  TeeGLCanvas, OpenGL2, 
  {$IFDEF D6}
  Types,
  {$ENDIF}
  Math;

{ TPipeBlock }

Constructor TPipeBlock.Create(AOwner: TComponent);
begin
  inherited;
  FRadius:=TPointXYFloat.Create(Self,16);
  FConnectorVisible:=True;
  CreateConnector;
end;

Destructor TPipeBlock.Destroy;
begin
  if ICustomConnector then
     Connector:=nil;

  FRadius.Free;

  FreeAndNil(IGlobalConnector);

  inherited;
end;

procedure TPipeBlock.CreateConnector;
begin
  if not Assigned(IGlobalConnector) then
  begin
    IGlobalConnector:=TCylinderBlock.Create(nil);
    IGlobalConnector.Format.Border.Visible:=False;
  end;

  ICustomConnector:=False;
end;

procedure TPipeBlock.Assign(Source: TPersistent);
begin
  if Source is TPipeBlock then
  with TPipeBlock(Source) do
  begin
    Self.FClosed:=FClosed;
    Self.Connector:=Connector;
    Self.FConnectorVisible:=FConnectorVisible;
    Self.Radius:=FRadius;
  end;

  inherited;
end;

procedure TPipeBlock.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FConnector) and (AComponent=FConnector) then
     Connector:=nil;
end;

procedure TPipeBlock.SetConnector(const Value: TCustomBlock);
begin
  if FConnector<>Value then
  begin
    if Assigned(FConnector) then
       FConnector.RemoveFreeNotification(Self);

    FConnector:=Value;

    ICustomConnector:=Assigned(FConnector);

    if Assigned(FConnector) then
       FConnector.FreeNotification(Self);

    Repaint;
  end;
end;

type
  TBlockAccess=class(TCustomBlock);
  TBlockFormatAccess=class(TBlockFormat);
  TCollectionAccess=class(TPointCollection);

procedure TPipeBlock.Draw;
const
  PiStep=180/Pi;

  function AngleOf(const Dist,P0,P1,P0X,P1X:Double):Double;
  begin
    if P0=P1 then
       result:=HalfPi
    else
    begin
      result:=ArcSin((P0-P1)/Dist);

      if result<0 then
         result:=-HalfPi-result
      else
         result:=HalfPi-result;

      if P0X>P1X then
         result:=TwoPi-result;
    end;
  end;

  function XAngleOf(const Dist,P0,P1,P0X,P1X:Double):Double;
  begin
    if P0=P1 then
       result:=HalfPi
    else
    begin
      result:=ArcSin((P0-P1)/Dist);

      if result>0 then
         result:=-HalfPi+result
      else
         result:=HalfPi+result;

      if P0X>P1X then
         result:=TwoPi-result;

      result:=TwoPi-result;
    end;
  end;

var t  : Integer;

    P0 : TPoint3DFloat;
    P1 : TPoint3DFloat;

    tmpDistX,
    tmpDist : Double;

    tmpConn : TCustomBlock;
begin
  inherited;

  if FConnectorVisible then
  begin
    if IPointCount>0 then
    begin
      glPushMatrix;

      with Size.Point do
           glScalef(2/X, 2/Z, 2/Y);

      tmpConn:=FConnector;

      if not Assigned(tmpConn) then
      begin
        CreateConnector;
        tmpConn:=IGlobalConnector;
      end;

      PrepareCalcPoint;

      if (Radius.Point.x=0) and (Radius.Point.y=0) then
      begin
        if TBlockFormatAccess(Format).InternalPreparePen then
        begin
          if Closed then
             glBegin(GL_LINE_LOOP)
          else
             glBegin(GL_LINE_STRIP);

          for t:=0 to IPointCount-1 do
          with CalcPoint(Points[t].Point.Point) do
               glVertex3f(X,Z,-Y);

          glEnd;

          // Format.FinishPen ?
        end;
      end
      else
      begin
        with tmpConn.Size.Point do
        begin
          X:=Radius.Point.X;
          Y:=Radius.Point.Y;
        end;

        TBlockAccess(tmpConn).IBlocks:=IBlocks;
        TBlockAccess(tmpConn).ICanvas:=ICanvas;

        P0:=CalcPoint(Points[0].Point.Point);

        for t:=1 to IPointCount-1 do
        with Points[t] do
        begin
          P1:=CalcPoint(Point.Point);

          with tmpConn.Location.Point do
          begin
            X:=(P1.X+P0.X)*0.5;
            Y:=(P0.Y+P1.Y)*0.5;
            Z:=(P0.Z+P1.Z)*0.5;
          end;

          tmpDistX:=Sqrt(Sqr(P1.X-P0.X)+Sqr(P1.Z-P0.Z));

          tmpConn.Rotation.Point.Z:=PiStep*AngleOf(tmpDistX,P0.Z,P1.Z,P0.X,P1.X);

          tmpDist:=Sqrt(Sqr(tmpDistX)+Sqr(P1.Y-P0.Y));

          tmpConn.Rotation.Point.X:=PiStep*XAngleOf(tmpDist,P0.Y,P1.Y,P0.X,P1.X)-90;

          tmpConn.Size.Point.Z:=tmpDist;

          TBlockAccess(tmpConn).StartTransform;

          if ICustomConnector then
             TBlockFormatAccess(tmpConn.Format).Start;

          tmpConn.Draw;

          if ICustomConnector then
             TBlockFormatAccess(tmpConn.Format).Finish;

          TBlockAccess(tmpConn).EndTransform;

          P0:=P1;
        end;

        if not ICustomConnector then
           TBlockAccess(tmpConn).IBlocks:=nil;
      end;

      glPopMatrix;
    end;
  end;
end;

function TPipeBlock.IsConnectorStored:Boolean;
begin
  result:=Assigned(FConnector) and ICustomConnector;
end;

procedure TPipeBlock.SetClosed(const Value:Boolean);
begin
  FClosed:=Value;
  Repaint;
end;

procedure TPipeBlock.SetRadius(const Value:TPointXYFloat);
begin
  FRadius.Assign(Value);
end;

procedure TPipeBlock.DeleteLists;
begin
  inherited;

  if Assigned(FConnector) and (not ICustomConnector) then
     TBlockAccess(FConnector).DeleteLists;

  if Assigned(IGlobalConnector) then
     TBlockAccess(IGlobalConnector).DeleteLists;
end;

procedure TPipeBlock.SetConnectorVisible(const Value: Boolean);
begin
  FConnectorVisible:=Value;
  Repaint;
end;

initialization
  RegisterBlocks([TPipeBlock]);
finalization
end.

