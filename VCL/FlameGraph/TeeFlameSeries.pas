{**********************************************}
{   TFlameSeries                               }
{   Copyright (c) 2026 by Steema Software      }
{**********************************************}
unit TeeFlameSeries;
{$I TeeDefs.inc}

interface

uses
  Classes, Graphics, Types,
  TeCanvas, TeEngine;

type
  TFlameSeries=class(TChartSeries)
  private
  protected
    procedure AddSampleValues(NumValues:Integer; OnlyMandatory:Boolean=False); override;
    procedure DrawValue(ValueIndex:Integer); override; { <-- main draw method }
  protected
    MaxDepth : Integer;

    function BoundsOf(const AValueIndex:Integer):TRect;
    function ColorOf(const AValueIndex: Integer):TColor;
    function DepthOf(const AValueIndex:Integer):Integer;
    procedure DrawMark(ValueIndex: Integer; const St: String;
                       APosition: TSeriesMarkPosition); override;
    function GetValueColor(ValueIndex:Integer):TColor; override;
    procedure SetParentChart(const Value: TCustomAxisPanel); override;
    function TotalOfParent(const AValueIndex:Integer):TChartValue;
    function ValueOffset(const AValueIndex:Integer):TChartValue;
  public
    Parents : Array of Integer; // -1 = no parent (root node)

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    function Add(const AParent:Integer; const AText:String; const AValue:TChartValue):Integer;
    Procedure Assign(Source:TPersistent); override;

    function CalcXPos(ValueIndex:Integer):Integer; override;
    function CalcYPos(ValueIndex:Integer):Integer; override;

    procedure Clear; override;
    function Clicked(x,y:Integer):Integer; override;

    Function MaxXValue:TChartValue; override;
    Function MaxYValue:TChartValue; override;
    Function MinXValue:TChartValue; override;
    Function MinYValue:TChartValue; override;
  published
  end;

implementation

uses
  UITypes, TeeProcs, Math;

{ TFlameSeries }

// Sample data
procedure TFlameSeries.AddSampleValues(NumValues: Integer;
  OnlyMandatory: Boolean);
var tmp1, tmp2, tmp3 : Integer;
begin
  tmp1:=Add(-1,'TApplication.Run',1290);

    tmp2:=Add(tmp1,'TApplication.HandleMessage',870);
           tmp3:=Add(tmp2,'TApplication.Idle',423);
            Add(tmp3,'TApplication.DoMouseIdle',423);
           Add(tmp2,'TApplication.ProcessMessage',447);

    tmp2:=Add(tmp1,'TCustomForm.SetVisible',320);
           tmp3:=Add(tmp2,'TControl.SetVisible',320);
            Add(tmp3,'TControl.Perform',300);
            Add(tmp3,'TCustomForm.RequestAlign',20);

    tmp2:=Add(tmp1,'TObject.Free',100);
           Add(tmp2,'TApplication.Destroy',90);
           Add(tmp2,'TCustomForm.Destroy',10);
end;

Constructor TFlameSeries.Create(AOwner: TComponent);
begin
  inherited;

  XValues.Order:=loNone;

  Marks.Show;
  Marks.Transparent:=True;
  Marks.TextAlignment:=taLeftJustify;
  Marks.Clip:=True;

  Brush.Gradient.StartColor:=clYellow;
  Brush.Gradient.EndColor:=clRed;
end;

Destructor TFlameSeries.Destroy;
begin
  inherited;
end;

procedure TFlameSeries.Clear;
begin
  inherited;

  MaxDepth:=0;
  Parents:=nil;
end;

// Returns the approximate color at percent % position from 0 (start) to 100 (end)
function GradientColor(const AGradient:TTeeGradient; const APercent:Single):TColor;
const
  Range=255;

var T0,T1,T2,
    D0,D1,D2 : Integer;
    tmp : Integer;
begin
  T0:=Byte(AGradient.StartColor);
  T1:=Byte(AGradient.StartColor shr 8);
  T2:=Byte(AGradient.StartColor shr 16);

  D0:=Byte(AGradient.EndColor)-T0;
  D1:=Byte(AGradient.EndColor shr 8)-T1;
  D2:=Byte(AGradient.EndColor shr 16)-T2;

  tmp:=Round(APercent*2.55);

  result:=(( T0 + ((tmp*D0) div Range) ) or
          (( T1 + ((tmp*D1) div Range) ) shl 8) or
          (( T2 + ((tmp*D2) div Range) ) shl 16))
end;

function TFlameSeries.TotalOfParent(const AValueIndex:Integer):TChartValue;
var t,
    tmpParent : Integer;
begin
  tmpParent:=Parents[AValueIndex];

  if tmpParent=-1 then // bottom element
     result:=YValues[AValueIndex]
  else
  begin
    result:=0;

    for t:=0 to Count-1 do
        if Parents[t]=tmpParent then
           result:=result+YValues[t];
  end;
end;

// The color that represents a given rectangle relative to its siblings
function TFlameSeries.ColorOf(const AValueIndex: Integer):TColor;
var tmpTotal : TChartValue;
    tmp : Single;
begin
  tmpTotal:=TotalOfParent(AValueIndex);

  if tmpTotal=0 then
     tmp:=0
  else
     tmp:=YValues[AValueIndex]*100/tmpTotal;

  result:=GradientColor(Brush.Gradient,tmp);
end;

// The value (not pixels) that a given rectangle starts (after its siblings)
function TFlameSeries.ValueOffset(const AValueIndex:Integer):TChartValue;
var t,
    tmpParent : Integer;
begin
  tmpParent:=Parents[AValueIndex];

  if tmpParent=-1 then // bottom element
     result:=0
  else
  begin
    result:=ValueOffset(tmpParent);

    for t:=0 to AValueIndex-1 do
        if Parents[t]=tmpParent then
           result:=result+YValues[t];
  end;
end;

// The horizontal X position in pixels for the BEGIN (left) side of a rectangle
function TFlameSeries.CalcXPos(ValueIndex:Integer):Integer;
begin
  result:=GetHorizAxis.CalcXPosValue(ValueOffset(ValueIndex));
end;

// The vertical Y position in pixels for the BEGIN (top) side of a rectangle
function TFlameSeries.CalcYPos(ValueIndex:Integer):Integer;
begin
  result:=GetVertAxis.CalcYPosValue(DepthOf(ValueIndex));
end;

function TFlameSeries.BoundsOf(const AValueIndex:Integer):TRect;
var tmpDepth : Integer;
begin
  result.Top:=CalcYPos(AValueIndex);

  tmpDepth:=DepthOf(AValueIndex);
  result.Bottom:=CalcYPosValue(tmpDepth-1);

  result.Left:=CalcXPos(AValueIndex);
  result.Right:=CalcXPosValue(ValueOffset(AValueIndex)+YValues[AValueIndex]);
end;

Function TFlameSeries.Clicked(x,y:Integer):Integer;
var t : Integer;
    tmp : TRect;
begin
  result:=TeeNoPointClicked;

  for t:=0 to Count-1 do
  begin
    tmp:=BoundsOf(t);

    if PointInRect(tmp,x,y) then
    begin
      result:=t;
      Exit;
    end;
  end;
end;

Function TFlameSeries.GetValueColor(ValueIndex:Integer):TColor;
Begin
  result:=InternalColor(ValueIndex);

  if result=clTeeColor then
     result:=ColorOf(ValueIndex);
end;

procedure TFlameSeries.DrawMark(ValueIndex: Integer; const St: String;
  APosition: TSeriesMarkPosition);
begin
  Inc(APosition.LeftTop.X,Round(ParentChart.Canvas.TextWidth(St)*0.5));
  Inc(APosition.LeftTop.Y,GetVertAxis.CalcSizeValue(0.5));

  if Marks.Clip then
     ParentChart.Canvas.ClipRectangle(BoundsOf(ValueIndex));

  inherited;

  if Marks.Clip then
     ParentChart.Canvas.UnClipRectangle;
end;

procedure TFlameSeries.DrawValue(ValueIndex: Integer);
var tmpR : TRect;
begin
  ParentChart.Canvas.AssignVisiblePen(Pen);
  ParentChart.Canvas.AssignBrush(Brush,ValueColor[ValueIndex]);

  tmpR:=BoundsOf(ValueIndex);

  ParentChart.Canvas.RoundRect(tmpR,5,5);
end;

// Sum of all depth zero items values
function TFlameSeries.MaxXValue: TChartValue;
begin
  result:=YValues.MaxValue;
end;

// Zero as minimum horizontal coordinate
function TFlameSeries.MinXValue: TChartValue;
begin
  result:=0;
end;

// Returns the maximum stack depth
function TFlameSeries.MaxYValue: TChartValue;
begin
  result:=MaxDepth+1;
end;

// Returns the minimum stack depth (zero)
function TFlameSeries.MinYValue: TChartValue;
begin
  result:=0;
end;

Procedure TFlameSeries.Assign(Source:TPersistent);
Begin
  if Source is TFlameSeries then
  With TFlameSeries(Source) do
  Begin
  end;
end;

// Recursive number of parents of a given AIndex node
function TFlameSeries.DepthOf(const AValueIndex:Integer):Integer;
var tmp : Integer;
begin
  tmp:=AValueIndex;

  result:=0;

  while tmp<>-1 do
  begin
    Inc(result);
    tmp:=Parents[tmp];
  end;
end;

function TFlameSeries.Add(const AParent:Integer; const AText: String;
  const AValue: TChartValue): Integer;
begin
  result:=inherited Add(AValue,AText);

  SetLength(Parents,result+1);
  Parents[result]:=AParent;

  if AParent<>-1 then
  begin
    MaxDepth:=Max(MaxDepth,DepthOf(AParent));

    // Children[AParent].Add(result);  <-- keep a cache of children to avoid lookups
  end;
end;

procedure TFlameSeries.SetParentChart(const Value: TCustomAxisPanel);
begin
  inherited;

  if ParentChart<>nil then
  begin
//    (ParentChart as TCustomChart).ColorPaletteIndex:=13;

    ParentChart.Axes.Left.Texts.Style:=talNone;
    ParentChart.Axes.Bottom.Texts.Style:=talValue;
  end;
end;

{$IFDEF TEEPRO}
initialization
  RegisterTeeSeries( TFlameSeries, @TeeMsg_GalleryFlameSeries,
                                 @TeeMsg_GalleryExtended, 1);
finalization
  UnRegisterTeeSeries([TFlameSeries]);
{$ENDIF}
end.
