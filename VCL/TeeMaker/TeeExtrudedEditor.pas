unit TeeExtrudedEditor;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QButtons, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls,
  {$ENDIF}
  TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeBlocks, TeeDragPoint,
  TeeExtruded, TeeGDIPlus;

type
  TExtrudedEditor = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    ChartTool1: TDragPointTool;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SBDelete: TSpeedButton;
    Label1: TLabel;
    CBShow: TComboFlat;
    CBShowBounds: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChartTool1DragPoint(Sender: TDragPointTool; Index: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SBDeleteClick(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBShowChange(Sender: TObject);
    procedure CBShowBoundsClick(Sender: TObject);
  private
    { Private declarations }

    ClickedPoint : Integer;
    IModified    : Boolean;
    Points       : TPointCollection;

    procedure DrawClickedPoint;
    procedure FillSeries;
    procedure UpdateBlock;
  public
    { Public declarations }
    procedure RefreshPoints(const APoints:TPointCollection);
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

procedure TExtrudedEditor.FormShow(Sender: TObject);
begin
  if Assigned(TObject(Tag)) then
  if TObject(Tag) is TPathBlock then
  begin
    Points:=TPathBlock(Tag).Points;

    FillSeries;

    if TObject(Tag) is TExtrudedBlock then
    begin
      with Chart1.Axes do
      begin
        Left.SetMinMax(-1,1);
        Right.SetMinMax(-1,1);
        Top.SetMinMax(-1,1);
        Bottom.SetMinMax(-1,1);
      end;
    end;
  end;
end;

procedure TExtrudedEditor.FillSeries;
var t : Integer;
begin
  Series1.Clear;

  for t:=0 to Points.Count-1 do
  with Points[t].Point do
       Series1.AddXY(X,Z);
end;

type
  TPointsAccess=class(TPointCollection);

procedure TExtrudedEditor.Chart1AfterDraw(Sender: TObject);

  procedure DrawAngle(Index:Integer);

    function CalcPos(AIndex:Integer):TFloatPoint;
    begin
      result.X:=Series1.XValues.Value[AIndex];
      result.Y:=Series1.YValues.Value[AIndex];
    end;

  var P0,P1,P2 : TFloatPoint;
      tmpAngle : Double;
      tmp      : String;
      tmpX,
      tmpY     : Integer;
  begin
    if Index=0 then
       P0:=CalcPos(Series1.Count-1)
    else
       P0:=CalcPos(Index-1);

    P1:=CalcPos(Index);

    if Index=Series1.Count-1 then
       P2:=CalcPos(0)
    else
       P2:=CalcPos(Index+1);

    tmpAngle:=AngleOf(P0,P1,P2);
    tmp:=FormatFloat('#.#',tmpAngle*180/Pi);

    tmpX:=Series1.CalcXPos(Index);
    tmpY:=Series1.CalcYPos(Index);
    Chart1.Canvas.TextOut(tmpX,tmpY,tmp);
  end;

  function PointsOf(const Series:TChartSeries):TPointFloatArray;
  var t : Integer;
  begin
    with Series1 do
    begin
      SetLength(result,Count);

      for t:=0 to Count-1 do
      begin
        result[t].X:=XValues.Value[t];
        result[t].Y:=YValues.Value[t];
      end;
    end;
  end;

  function PointsIntegerOf(const Series:TChartSeries):TPointArray;
  var t : Integer;
  begin
    with Series1 do
    begin
      SetLength(result,Count);

      for t:=0 to Count-1 do
      begin
        result[t].X:=CalcXPos(t);
        result[t].Y:=CalcYPos(t);
      end;
    end;
  end;

  procedure DrawBounds;
  var tmp    : TPointFloatArray;
      tmpInt : TPointArray;
  begin
    tmp:=PointsOf(Series1);
    TPointsAccess(Points).IConvex:=TTeeCanvas.IsConvexPolygon(tmp);
    tmp:=nil;

    if not TPointsAccess(Points).IConvex then
    begin
      tmpInt:=PointsIntegerOf(Series1);

      {
      if tmpBad then
      begin
        Chart1.Canvas.Pen.Color:=clYellow;
        Chart1.Canvas.Brush.Style:=bsClear;
        Chart1.Canvas.Polygon(tmpInt);
      end
      else
      }
      if Chart1.Canvas.ConvexHull(tmpInt) then
      begin
        Chart1.Canvas.Pen.Color:=clRed;
        Chart1.Canvas.Brush.Style:=bsClear;
        Chart1.Canvas.Polygon(tmpInt);
      end;

      tmpInt:=nil;
    end;
  end;

var t : Integer;
begin
  with Series1 do
  if Count>2 then
  begin
    Chart1.Canvas.AssignVisiblePenColor(LinePen,SeriesColor);
    Chart1.Canvas.MoveTo(CalcXPos(0),CalcYPos(0));
    Chart1.Canvas.LineTo(CalcXPos(Count-1),CalcYPos(Count-1));
  end;

  if CBShowBounds.Checked then
     DrawBounds;

  Chart1.Canvas.Brush.Style:=bsClear;
  Chart1.Canvas.BackMode:=cbmTransparent;

  if CBShow.ItemIndex=0 then
  begin
    with Series1 do
    if Count>2 then
    for t:=0 to Count-1 do
        DrawAngle(t);
  end
  else
  with Series1 do
  for t:=0 to Count-1 do
      Chart1.Canvas.TextOut(CalcXPos(t),CalcYPos(t),IntToStr(t));

  DrawClickedPoint;
end;

procedure TExtrudedEditor.DrawClickedPoint;
var tmpX : Integer;
    tmpY : Integer;
    tmpH : Integer;
    tmpV : Integer;
begin
  if ClickedPoint<>-1 then
  with Chart1.Canvas do
  begin
    Brush.Style:=bsClear;
    Pen.Style:=psSolid;
    Pen.Color:=clRed;
    Pen.Width:=1;

    tmpX:=Chart1[0].CalcXPos(ClickedPoint);
    tmpY:=Chart1[0].CalcYPos(ClickedPoint);

    tmpH:=2*(Chart1[0] as TCustomSeries).Pointer.HorizSize;
    tmpV:=2*(Chart1[0] as TCustomSeries).Pointer.VertSize;

    Rectangle(tmpX-tmpH,tmpY-tmpV,tmpX+tmpH+1,tmpY+tmpV+1);
  end;
end;

procedure TExtrudedEditor.FormCreate(Sender: TObject);
begin
  ClickedPoint:=-1;
  Series1.XValues.Order:=loNone;
end;

procedure TExtrudedEditor.UpdateBlock;
begin
  TPointsAccess(Points).DoChanged;
end;

procedure TExtrudedEditor.ChartTool1DragPoint(Sender: TDragPointTool;
  Index: Integer);
begin
  Assert(Index<Points.Count,'Extruded: Index >= Length Block.Points');
  Assert(Index<Series1.Count,'Extruded: Index >= Series1.Count');

  with Points[Index].Point do
  begin
    X:=Series1.XValues.Value[Index];
    Z:=Series1.YValues.Value[Index];
  end;

  UpdateBlock;

  IModified:=True;
end;

procedure TExtrudedEditor.SpeedButton1Click(Sender: TObject);
var tmp0 : Integer;
    tmp1 : Integer;
    tmpI : TPointItem;
begin
  if Series1.Count<2 then
     Points.Add(0,0,0)
  else
  begin
    tmp0:=ClickedPoint;

    if tmp0=Series1.Count-1 then
       tmp1:=0
    else
       tmp1:=Succ(tmp0);

    tmpI:=TPointItem(Points.Insert(tmp1));

    with tmpI.Point.Point, Series1 do
    begin
      X:=0.5*(XValues.Value[tmp1]+XValues.Value[tmp0]);
      Z:=0.5*(YValues.Value[tmp1]+YValues.Value[tmp0]);
    end;
  end;

  FillSeries;

  UpdateBlock;

  IModified:=True;
end;

procedure TExtrudedEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if IModified then
     ModalResult:=mrOk;
end;

procedure TExtrudedEditor.SBDeleteClick(Sender: TObject);
begin
  if Chart1[0].Count>1 then
     if ClickedPoint<>-1 then
     begin
       Chart1[0].Delete(ClickedPoint);

       Points.Delete(ClickedPoint);
       UpdateBlock;

       IModified:=True;
     end;
end;

procedure TExtrudedEditor.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  if Button=mbLeft then
  begin
    tmp:=Chart1[0].Clicked(x,y);

    if tmp<>ClickedPoint then
    begin
      ClickedPoint:=tmp;
      Chart1[0].Repaint;
      SBDelete.Enabled:=(Chart1[0].Count>1);
    end;
  end;
end;

procedure TExtrudedEditor.CBShowChange(Sender: TObject);
begin
  Chart1.Invalidate;
end;

procedure TExtrudedEditor.CBShowBoundsClick(Sender: TObject);
begin
  Chart1.Invalidate;
end;

procedure TExtrudedEditor.RefreshPoints(const APoints: TPointCollection);
begin
  Points:=APoints;

  if Assigned(Points) then
     FillSeries;
end;

initialization
  RegisterClass(TExtrudedEditor);
end.
