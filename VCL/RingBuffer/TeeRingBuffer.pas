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
  {$IFNDEF FPC}
  {$IFNDEF LINUX}
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}
  Windows,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF FPC}
  Types,
  {$ENDIF}

  Classes,

  {$IFDEF FMX}
  FMXTee.Canvas, FMXTee.Procs, FMXTee.Series
  {$ELSE}
  TeCanvas, TeeProcs, Series
  {$ENDIF}
  ;

{$IFOPT D-}
{$C-} // Assertions OFF in Release mode (for speed)
{$ENDIF}

type
  // Generic circular buffer
  TRingBuffer<T>=record
  private
    function Full:Boolean; inline;
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

    procedure Draw(const AIndex:Integer);
    procedure MoveTo(const AIndex:Integer);
    procedure SetAntialias(const Value: Boolean);
  protected
    procedure CalcPosition(ValueIndex:Integer; out x,y:Integer); virtual; abstract;
  public
    Buffer : TRingBuffer<T>;

    Constructor Create(AOwner:TComponent); override;

    Procedure CalcFirstLastVisibleIndex; override;
    procedure Clear; override;
    procedure DrawAllValues; override;
    Function MaxXValue:TChartValue; override;
    Function MinXValue:TChartValue; override;
  published
    property Antialias:Boolean read FAntialias write SetAntialias default True;

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

  TRingBufferXY=class(TCustomRingBuffer<TPointFloat>)
  protected
    procedure CalcPosition(ValueIndex:Integer; out x,y:Integer); override;
  end;

  TRingBuffer=class(TCustomRingBuffer<Single>)
  protected
    procedure CalcPosition(ValueIndex:Integer; out x,y:Integer); override;
  end;

implementation

uses
  {$IFDEF FMX}
  FMXTee.Engine
  {$ELSE}
  {$IFNDEF FPC}
  TeeGDIPlus, // <-- dependency due to Antialias property. Rethink.
  {$ENDIF}
  TeEngine
  {$ENDIF}
  ;

{ TRingBuffer<T> }

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
  Assert(Count>=0);

  SetLength(Items,ACount);

  if Count>ACount then
     Count:=ACount;

  if Current>=ACount then
     Current:=ACount-1;
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

procedure TRingBuffer<T>.Append(const P: T);
begin
  Assert(Size>0);

  Items[Current]:=P;

  if not Full then
     Inc(Count);

  if Current=High(Items) then
     Current:=0
  else
     Inc(Current);
end;

{ TCustomRingBuffer<T> }

Constructor TCustomRingBuffer<T>.Create(AOwner: TComponent);
begin
  inherited;

  FAntialias:=True;

  Pointer.Hide;
  Pointer.Style:={$IFDEF FMX}TSeriesPointerStyle.{$ENDIF}psSmallDot;
  Pointer.Pen.Hide;
end;

procedure TCustomRingBuffer<T>.CalcFirstLastVisibleIndex;
begin
  // inherited;

  if Buffer.Empty then
     FFirstVisibleIndex:=-1
  else
     FFirstVisibleIndex:=0;

  FLastVisibleIndex:=Buffer.Count-1;
end;

procedure TCustomRingBuffer<T>.Clear;
begin
  inherited;
  Buffer.Clear;
end;

procedure TCustomRingBuffer<T>.Draw(const AIndex:Integer);
var X,Y : Integer;
begin
  CalcPosition(AIndex,X,Y);

  if Pointer.Visible then
     Pointer.Draw(X,Y)
  else
     ParentChart.Canvas.LineTo(X,Y);
end;

function TCustomRingBuffer<T>.MaxXValue: TChartValue;
begin
  if Buffer.Empty then
     result:=0
  else
     result:=Buffer.Count-1;
end;

function TCustomRingBuffer<T>.MinXValue: TChartValue;
begin
  result:=0;
end;

procedure TCustomRingBuffer<T>.MoveTo(const AIndex:Integer);
var X,Y : Integer;
begin
  CalcPosition(AIndex,X,Y);
  ParentChart.Canvas.MoveTo(X,Y);
end;

procedure TCustomRingBuffer<T>.DrawAllValues;
var t : Integer;
    {$IFNDEF FPC}
    OldAnti : Boolean;
    {$ENDIF}
begin
  {$IFNDEF FPC}
  {$IFNDEF FMX}
  if ParentChart.Canvas is TGDIPlusCanvas then
  begin
    OldAnti:=TGDIPlusCanvas(ParentChart.Canvas).AntiAlias;
    TGDIPlusCanvas(ParentChart.Canvas).AntiAlias:=FAntialias;
  end
  else
  {$ENDIF}
    OldAnti:=True;
  {$ENDIF}

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

  {$IFNDEF FPC}
  {$IFNDEF FMX}
  if ParentChart.Canvas is TGDIPlusCanvas then
     TGDIPlusCanvas(ParentChart.Canvas).AntiAlias:=OldAnti;
  {$ENDIF}
  {$ENDIF}
end;

procedure TCustomRingBuffer<T>.SetAntialias(const Value: Boolean);
begin
  SetBooleanProperty(FAntialias,Value);
end;

{ TRingBufferXY }

Procedure TRingBufferXY.CalcPosition(ValueIndex:Integer; out x,y:Integer);
begin
  X:=GetHorizAxis.CalcXPosValue(Buffer.Items[ValueIndex].X);
  Y:=GetVertAxis.CalcYPosValue(Buffer.Items[ValueIndex].Y);
end;

{ TRingBuffer }

procedure TRingBuffer.CalcPosition(ValueIndex: Integer; out x, y: Integer);
begin
  X:=GetHorizAxis.CalcYPosValue(ValueIndex);
  Y:=GetVertAxis.CalcYPosValue(Buffer.Items[ValueIndex]);
end;

end.
