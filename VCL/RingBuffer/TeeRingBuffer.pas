{********************************************}
{   Ring Buffer Series                       }
{ Copyright (c) 2025 by Steema Software      }
{   All Rights Reserved                      }
{********************************************}
unit TeeRingBuffer;
{$I TeeDefs.inc}

interface

{
  Special optimized TeeChart series style using a "Ring Buffer".

  A ring buffer is an array you can keep continously adding points, and it
  automatically recicles starting from the start when the buffer is full.

  The TRingBufferLine series:

  1) Do not uses its typical XValues and YValues arrays.
  2) Does not provide 3D display.
  3) Does not support "Null" (Missing) point values.
  4) Better call GetHorizAxis and GetVertAxis SetMinMax to avoid speed penalty.
  5) It does not support "Stairs" line mode.
  6) It does not support other features of the FastLine series, like:
     DrawStyle (segments or a single polyline)
     DrawAllPoints (there is no algorithm to reduce the quantity of points to draw)

}

uses
  {$IFNDEF LINUX}
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}
  Windows,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  Classes, Types, TeCanvas, TeeProcs, Series;

type
  // Generic circular buffer
  TRingBuffer<T>=record
  private
    function Full:Boolean;
  public
    Current,
    Count : Integer;

    Items : Array of T;

    procedure Append(const P:T);
    procedure Clear;
    function Empty:Boolean; inline;
    function Last:T;
    procedure Resize(const ACount:Integer); inline;
    function Size:Integer; inline;
  end;

  // Base series that includes the circular Ring Buffer
  TCustomRingBuffer<T>=class(TCustomSeries)
  private
    FAntialias : Boolean;

    procedure SetAntialias(const Value: Boolean);
  public
    Buffer : TRingBuffer<T>;

    Constructor Create(AOwner:TComponent); override;

    Procedure CalcFirstLastVisibleIndex; override;
  published
    property Antialias:Boolean read FAntialias write SetAntialias default True;
  end;

  TRingBufferXY=class(TCustomRingBuffer<TPointFloat>)
  protected
    procedure DrawAllValues; override;
  published
    property Active;
    property Cursor;
    property Depth;
    property HorizAxis;
    property HoverElement;
    property Marks;
    property ParentChart;
    property DataSource;  { after parentchart }
    property PercentFormat;
    property SeriesColor;
    property ShowInLegend;
    property Title;
    property ValueFormat;
    property VertAxis;
    property XLabelsSource;

    { events }
    property AfterDrawValues;
    property BeforeDrawValues;
    property OnAfterAdd;
    property OnBeforeAdd;
    property OnClearValues;
    property OnClick;
    property OnDblClick;
    property OnGetMarkText;
    property OnMouseEnter;
    property OnMouseLeave;

    // Published inherited properties
    property ClickableLine;
    property ClickTolerance;
    property Pointer;
    property Transparency;
    property XValues;
    property YValues;

    { events }
    property OnClickPointer;
    property OnGetPointerStyle;
  end;

implementation

uses
  TeeGDIPlus, // <-- dependency due to Antialias property. Rethink.
  TeEngine;

{ TRingBuffer<T> }

procedure TRingBuffer<T>.Append(const P: T);
begin
  Items[Current]:=P;

  if not Full then
     Inc(Count);

  if Current=High(Items) then
     Current:=0
  else
     Inc(Current);
end;

procedure TRingBuffer<T>.Clear;
begin
  Items:=nil;
  Current:=0;
  Count:=0;
end;

function TRingBuffer<T>.Empty: Boolean;
begin
  result:=Count=0;
end;

procedure TRingBuffer<T>.Resize(const ACount: Integer);
begin
  SetLength(Items,ACount);
end;

function TRingBuffer<T>.Size: Integer;
begin
  result:=Length(Items);
end;

function TRingBuffer<T>.Full: Boolean;
begin
  result:=Count=Size;
end;

function TRingBuffer<T>.Last: T;
begin
  Assert(Count>0);

  if Current=0 then
     result:=Items[Count-1]
  else
     result:=Items[Current-1];
end;

{ TCustomRingBuffer }

procedure TCustomRingBuffer<T>.CalcFirstLastVisibleIndex;
begin
//  inherited;

  if Buffer.Empty then
     FFirstVisibleIndex:=-1
  else
     FFirstVisibleIndex:=0;

  FLastVisibleIndex:=Buffer.Count-1;
end;

{ TRingBufferXY }

Constructor TCustomRingBuffer<T>.Create(AOwner: TComponent);
begin
  inherited;

  FAntialias:=True;

  Pointer.Hide;
  Pointer.Style:=psSmallDot;
  Pointer.Pen.Hide;
end;

procedure TRingBufferXY.DrawAllValues;

  Procedure CalcPosition(ValueIndex:Integer; out x,y:Integer);
  begin
    X:=GetHorizAxis.CalcXPosValue(Buffer.Items[ValueIndex].X);
    Y:=GetVertAxis.CalcYPosValue(Buffer.Items[ValueIndex].Y);
  end;

  procedure Draw(const AIndex:Integer);
  var X,Y : Integer;
  begin
    CalcPosition(AIndex,X,Y);

    if Pointer.Visible then
       Pointer.Draw(X,Y)
    else
       ParentChart.Canvas.LineTo(X,Y);
  end;

  procedure MoveTo(const AIndex:Integer);
  var X,Y : Integer;
  begin
    CalcPosition(AIndex,X,Y);
    ParentChart.Canvas.MoveTo(X,Y);
  end;

var t : Integer;
    OldAnti : Boolean;
begin
  if ParentChart.Canvas is TGDIPlusCanvas then
  begin
    OldAnti:=TGDIPlusCanvas(ParentChart.Canvas).AntiAlias;
    TGDIPlusCanvas(ParentChart.Canvas).AntiAlias:=FAntialias;
  end
  else
    OldAnti:=True;

  ParentChart.Canvas.AssignVisiblePenColor(Pen,Color);

  if Pointer.Visible then
     Pointer.PrepareCanvas(ParentChart.Canvas,SeriesColor);

  if Buffer.Full then
  begin
    if Buffer.Current=0 then
    begin
      MoveTo(0);

      for t:=1 to High(Buffer.Items) do
          Draw(t);
    end
    else
    begin
      MoveTo(Buffer.Current);

      for t:=Buffer.Current+1 to High(Buffer.Items) do
          Draw(t);

      //MoveTo(Buffer.Current-1);

      for t:=0 to Buffer.Current-1 do
          Draw(t);
    end;
  end
  else
  begin
    MoveTo(0);

    for t:=1 to Buffer.Current-1 do
        Draw(t);
  end;

  if ParentChart.Canvas is TGDIPlusCanvas then
     TGDIPlusCanvas(ParentChart.Canvas).AntiAlias:=OldAnti;
end;

procedure TCustomRingBuffer<T>.SetAntialias(const Value: Boolean);
begin
  SetBooleanProperty(FAntialias,Value);
end;

end.
