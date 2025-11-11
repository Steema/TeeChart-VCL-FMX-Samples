unit MainUnit;

interface

{
  Demo of TeeChart "Ring Buffer" series.

  This is a (fast) way to add and display lines or scatter dots.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Engine,
  FMXTee.Procs, FMXTee.Chart, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,

  TeeRingBuffer;

type
  TFormRingBuffer = class(TForm)
    Layout1: TLayout;
    Chart1: TChart;
    CBRun: TCheckBox;
    CBAntialias: TCheckBox;
    CBGrids: TCheckBox;
    CBPoints: TComboBox;
    CBSeries: TComboBox;
    CBStyle: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CBRunChange(Sender: TObject);
    procedure CBAntialiasChange(Sender: TObject);
    procedure CBGridsChange(Sender: TObject);
    procedure CBSeriesChange(Sender: TObject);
    procedure CBPointsChange(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure Label2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }

    procedure AddNewPoint;
    procedure Benchmark;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function MaxPoints:Integer;
    procedure RecreateSeries;
  public
    { Public declarations }

    LastX : Integer;

    Lines : Array of TRingBufferXY;
  end;

var
  FormRingBuffer: TFormRingBuffer;

implementation

{$R *.fmx}

uses
  System.Diagnostics;

const
  MaxRandom = 1000;

function TFormRingBuffer.MaxPoints:Integer;
begin
  result:=StrToInt(CBPoints.Text);
end;

procedure TFormRingBuffer.RecreateSeries;
var t : Integer;
begin
  LastX:=0;

  Chart1.FreeAllSeries;

  SetLength(Lines, StrToInt(CBSeries.Text));

  for t:=0 to High(Lines) do
  begin
    Lines[t]:=TRingBufferXY.Create(Self);

    Lines[t].Buffer.Resize(MaxPoints);

    Lines[t].Antialias:=CBAntialias.IsChecked;
    Lines[t].Pointer.Visible:=CBStyle.ItemIndex=1;

    Lines[t].ParentChart:=Chart1;
  end;
end;

procedure TFormRingBuffer.CBAntialiasChange(Sender: TObject);
var t : Integer;
begin
  // For each series
  for t:=0 to High(Lines) do
      Lines[t].Antialias:=CBAntialias.IsChecked;
end;

procedure TFormRingBuffer.CBGridsChange(Sender: TObject);
begin
  Chart1.Axes.Left.Grid.Visible:=CBGrids.IsChecked;
  Chart1.Axes.Bottom.Grid.Visible:=CBGrids.IsChecked;
end;

procedure TFormRingBuffer.CBPointsChange(Sender: TObject);
begin
  RecreateSeries;
end;

procedure TFormRingBuffer.CBRunChange(Sender: TObject);
begin
  if CBRun.IsChecked then
     Application.OnIdle:=Idle
  else
     Application.OnIdle:=nil;
end;

procedure TFormRingBuffer.CBSeriesChange(Sender: TObject);
begin
  RecreateSeries;
end;

procedure TFormRingBuffer.CBStyleChange(Sender: TObject);
var t : Integer;
begin
  // For each series
  for t:=0 to High(Lines) do
      Lines[t].Pointer.Visible:=CBStyle.ItemIndex=1;
end;

type
  TChartAccess=class(TCustomTeePanel);

procedure TFormRingBuffer.FormCreate(Sender: TObject);
begin
  RecreateSeries;

  // Set vertical scale
  Chart1.Axes.Left.SetMinMax(-MaxRandom,MaxRandom);

  // Speed optimizations
  Chart1.Legend.Hide;
  Chart1.ClipPoints:=False;
  Chart1.Walls.Back.Hide;
  Chart1.Axes.FastCalc:=True;
  TChartAccess(Chart1).FastTextSize:=True; // Speed trick: 20% faster

  {$IFDEF USE_OPENGL}
  Chart1.Canvas:=TGLCanvas.Create;
  {$ENDIF}

  {$IFDEF USE_SKIA}
  Chart1.Canvas:=TTeeSkiaCanvas.Create;
  {$ENDIF}

  // Start the animation
  Application.OnIdle:=Idle;
end;

procedure TFormRingBuffer.Idle(Sender: TObject; var Done: Boolean);
begin
  AddNewPoint;
end;

procedure TFormRingBuffer.Label2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Benchmark;
end;

procedure TFormRingBuffer.AddNewPoint;

  // Calculate new random Y value based on previous point
  function CalculateNewValue(const ALine:TRingBufferXY):Single;
  begin
    if ALine.Buffer.Empty then
       result:=Random(MaxRandom*2)-MaxRandom
    else
    repeat
       result:=ALine.Buffer.Last.Y+Random(51)-25;
    until (result<MaxRandom) and (result>-MaxRandom); // <-- fit vertical limits
  end;

var P : TPointF;
    t : Integer;
begin
  // Advance X
  P.X:=LastX;
  Inc(LastX);

  // For each series
  for t:=0 to High(Lines) do
  begin
    P.Y:=CalculateNewValue(Lines[t]);

    // Add the new point to the buffer
    Lines[t].Buffer.Append(P);
  end;

  // Scroll the bottom axis
  if LastX>MaxPoints then
     Chart1.Axes.Bottom.SetMinMax(LastX-MaxPoints,LastX-1)
  else
     Chart1.Axes.Bottom.SetMinMax(0,MaxPoints-1);

  // Tell chart to repaint (only if necessary)
  Chart1.Invalidate;
end;

// Just a speed test
procedure TFormRingBuffer.Benchmark;

  procedure RepaintChart;
  begin
    Chart1.DelphiCanvas.BeginScene;
    Chart1.Draw;
    Chart1.DelphiCanvas.EndScene;
  end;

var t1 : TStopWatch;
    t :  Integer;
begin
  CBRun.IsChecked:=False;
  CBRunChange(Self);

  RecreateSeries;

  RepaintChart;

  t1:=TStopwatch.StartNew;

  for t:=0 to 1000 do
  begin
    AddNewPoint;
    RepaintChart;
  end;

  Caption:=t1.ElapsedMilliseconds.ToString+' msec';

  Invalidate;
end;

end.
