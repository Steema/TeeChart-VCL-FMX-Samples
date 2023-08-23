unit TeeRevolutionEditor;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QButtons, QStdCtrls, QMenus,
  QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, Menus,
  ComCtrls,
  {$ENDIF}
  TeeBlocks, TeEngine, TeeDragPoint, Series, TeeProcs, Chart, TeeTools,
  TeCanvas, TeeBezie, TeeRevolution, EditChar, TeeExtruded;

type
  TRevolutionPointsEditor = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    ChartTool1: TDragPointTool;
    ChartTool2: TColorLineTool;
    SBAddPoint: TSpeedButton;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    SBDelete: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Addnewpoint1: TMenuItem;
    Insertnewpoint1: TMenuItem;
    Deletepoint1: TMenuItem;
    Label1: TLabel;
    EBezier: TEdit;
    UDBezier: TUpDown;
    Label2: TLabel;
    ESlices: TEdit;
    UDSlices: TUpDown;
    Series1: TBezierSeries;
    Series2: TBezierSeries;
    LabelPoint: TLabel;
    N1: TMenuItem;
    EditChart1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBAddPointClick(Sender: TObject);
    procedure ChartTool1DragPoint(Sender: TDragPointTool; Index: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Addnewpoint1Click(Sender: TObject);
    procedure SBDeleteClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Insertnewpoint1Click(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure EBezierChange(Sender: TObject);
    function Series1GetPointerStyle(Sender: TChartSeries;
      ValueIndex: Integer): TSeriesPointerStyle;
    procedure ESlicesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditChart1Click(Sender: TObject);
  private
    { Private declarations }
    Block : TRevolutionBlock;

    ClickedPoint,
    SelectedIndex : Integer;

    procedure AddPointsToBlock;
    procedure DoRefresh;
  public
    { Public declarations }
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeBlockFormat;

{ TRevolutionEditor }

procedure TRevolutionPointsEditor.FormCreate(Sender: TObject);
begin
  ClickedPoint:=-1;
  SelectedIndex:=-1;

  Chart1[0].XValues.Order:=loNone;
  Chart1[1].XValues.Order:=loNone;

  Chart1.Axes.Left.SetMinMax(-1,1);
  Chart1.Axes.Bottom.SetMinMax(-1,1);

  ChartTool2.Value:=0;
end;

procedure TRevolutionPointsEditor.FormShow(Sender: TObject);
var t : Integer;
begin
  Block:=TRevolutionBlock(Tag);

  Chart1[0].Clear;

  if Assigned(Block) then
  begin
    for t:=0 to Block.OuterPoints.Count-1 do
    with Block.OuterPoints[t].Point do
        Chart1[0].AddXY(X,Z);

    UDBezier.Position:=Block.CurvePoints;
    UDSlices.Position:=Block.Slices;
  end;

  if Chart1[0].Count>0 then
     ClickedPoint:=0;
end;

procedure TRevolutionPointsEditor.SBAddPointClick(Sender: TObject);
var
  tmp : TChartSeries;

  procedure AddPoint;
  var tmpX : Double;
      tmpY : Double;
  begin
    if tmp.Count>0 then
    begin
      tmpX:=tmp.XValues.Last;
      if tmpX<0.9 then
         tmpX:=tmpX+0.1;

      tmpY:=tmp.YValues.Last;
    end
    else
    begin
      tmpX:=0;
      tmpY:=0;
    end;

    tmp.AddXY(tmpX,tmpY,'',clRed);
  end;

begin
  tmp:=Chart1[0];

  AddPoint;
  AddPoint;

  DoRefresh;
end;

procedure TRevolutionPointsEditor.AddPointsToBlock;
var t : Integer;
begin
  Block.OuterPoints.Clear;

  with Chart1[0] do
  for t:=0 to Count-1 do
      Block.OuterPoints.Add(XValues.Value[t],YValues.Value[t]);

  Block.DataChanged;
end;

procedure TRevolutionPointsEditor.DoRefresh;
var t : Integer;
begin
  Chart1[1].AssignValues(Chart1[0]);

  with Chart1[1].YValues do
  for t:=0 to Count-1 do
      Value[t]:=-Value[t];

  Chart1.Invalidate;

  AddPointsToBlock;
end;

procedure TRevolutionPointsEditor.ChartTool1DragPoint(Sender: TDragPointTool;
  Index: Integer);
begin
  if Chart1[0].ValueColor[Index]=clRed then
     Chart1[0].ValueColor[Index]:=clTeeColor;

  with Chart1[0].YValues do
  if Value[Index]<0 then
     Value[Index]:=0
  else
  if Value[Index]>1 then
     Value[Index]:=1;

  DoRefresh;
end;

procedure TRevolutionPointsEditor.Button1Click(Sender: TObject);
begin
  TBlockFormatEditor.ModalShow(Self,Block.Brush1);
end;

procedure TRevolutionPointsEditor.Button2Click(Sender: TObject);
begin
  TBlockFormatEditor.ModalShow(Self,Block.Brush2);
end;

procedure TRevolutionPointsEditor.Chart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  tmp:=Chart1[0].Clicked(x,y);

  if tmp<>SelectedIndex then
  begin
    if SelectedIndex<>-1 then
       Chart1[0].ValueColor[SelectedIndex]:=clTeeColor;

    SelectedIndex:=tmp;

    if SelectedIndex<>-1 then
       Chart1[0].ValueColor[SelectedIndex]:=clLime;
  end;
end;

procedure TRevolutionPointsEditor.Chart1AfterDraw(Sender: TObject);
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

procedure TRevolutionPointsEditor.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  if Button=mbLeft then
  begin
    tmp:=Chart1[0].Clicked(x,y);

    if tmp<>ClickedPoint then
    begin
      ClickedPoint:=tmp;
      LabelPoint.Caption:=TeeStr(tmp);

      Chart1[0].Repaint;

      SBDelete.Enabled:=(ClickedPoint<>-1) and (Chart1[0].Count>1);
    end;
  end;
end;

procedure TRevolutionPointsEditor.Addnewpoint1Click(Sender: TObject);
begin
  SBAddPointClick(Self);
end;

procedure TRevolutionPointsEditor.SBDeleteClick(Sender: TObject);
begin
  if Chart1[0].Count>1 then
     if ClickedPoint<>-1 then
     begin
       Chart1[0].Delete(ClickedPoint);
       DoRefresh;
     end;
end;

procedure TRevolutionPointsEditor.PopupMenu1Popup(Sender: TObject);
begin
  Deletepoint1.Enabled:=(ClickedPoint<>-1) and (Chart1[0].Count>1);
  Insertnewpoint1.Enabled:=(ClickedPoint<>-1);
end;

procedure TRevolutionPointsEditor.Insertnewpoint1Click(Sender: TObject);
var tmpX : Double;
    tmpY : Double;
    t    : Integer;
begin
  if ClickedPoint=Chart1[0].Count-1 then
     SBAddPointClick(Self)
  else
  begin
    with Chart1[0].XValues do
         tmpX:=(Value[ClickedPoint]+Value[ClickedPoint+1])*0.5;

    with Chart1[0].YValues do
         tmpY:=(Value[ClickedPoint]+Value[ClickedPoint+1])*0.5;

    Chart1[0].AddXY(tmpX,tmpY,'',clRed);

    for t:=Chart1[0].Count-1 downto ClickedPoint+1 do
    begin
      with Chart1[0].XValues do
           Value[t]:=Value[t-1];

      with Chart1[0].YValues do
           Value[t]:=Value[t-1];
    end;

    Chart1[0].XValues.Value[ClickedPoint+1]:=tmpX;
    Chart1[0].YValues.Value[ClickedPoint+1]:=tmpY;

    Inc(ClickedPoint);

    DoRefresh;
  end;
end;

procedure TRevolutionPointsEditor.Panel1DblClick(Sender: TObject);
begin
  EditChart(Self,Chart1);
  DoRefresh;
end;

procedure TRevolutionPointsEditor.EBezierChange(Sender: TObject);
begin
  if Showing then
  begin
    Block.CurvePoints:=UDBezier.Position;
    DoRefresh;
  end;
end;

function TRevolutionPointsEditor.Series1GetPointerStyle(Sender: TChartSeries;
  ValueIndex: Integer): TSeriesPointerStyle;
begin
  result:=psRectangle;

  if ValueIndex>=0 then
     if Chart1[0].XLabel[ValueIndex]='' then
        result:=psCircle;
end;

procedure TRevolutionPointsEditor.ESlicesChange(Sender: TObject);
begin
  if Showing then
  begin
    Block.Slices:=UDSlices.Position;
    DoRefresh;
  end;
end;

procedure TRevolutionPointsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ModalResult:=mrOk;
end;

procedure TRevolutionPointsEditor.EditChart1Click(Sender: TObject);
begin
  Panel1DblClick(Self);
end;

initialization
  RegisterClass(TRevolutionPointsEditor);
end.
