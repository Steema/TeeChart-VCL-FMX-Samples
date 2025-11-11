unit MainUnit;

interface

{
  Demo of TeeChart "Ring Buffer" series.

  This is a (fast) way to add and display lines or scatter dots.
}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  TeeProcs, TeEngine, Chart,

  TeeRingBuffer;

type
  TFormRingBuffer = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    Label1: TLabel;
    CBSeries: TComboBox;
    Label2: TLabel;
    CBPoints: TComboBox;
    CBRun: TCheckBox;
    CBAntialias: TCheckBox;
    CBStyle: TComboBox;
    CBGrids: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBSeriesChange(Sender: TObject);
    procedure CBPointsChange(Sender: TObject);
    procedure CBAntialiasClick(Sender: TObject);
    procedure CBStyleCloseUp(Sender: TObject);
    procedure CBGridsClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure CBRunClick(Sender: TObject);
    procedure Label2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses

{.$DEFINE USE_OPENGL} // OpenGL fast graphics (needs TeeChart "Pro" version)
{$IFDEF USE_OPENGL}
  TeeGLCanvas,
{$ENDIF}

{.$DEFINE USE_SKIA} // Skia fast graphics (needs TeeChart "Pro" version)
{$IFDEF USE_SKIA}
  TeeSkia,
{$ENDIF}
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

    Lines[t].Antialias:=CBAntialias.Checked;
    Lines[t].Pointer.Visible:=CBStyle.ItemIndex=1;

    Lines[t].ParentChart:=Chart1;
  end;
end;

procedure TFormRingBuffer.CBSeriesChange(Sender: TObject);
begin
  RecreateSeries;
end;

procedure TFormRingBuffer.CBStyleCloseUp(Sender: TObject);
var t : Integer;
begin
  // For each series
  for t:=0 to High(Lines) do
      Lines[t].Pointer.Visible:=CBStyle.ItemIndex=1;
end;

procedure TFormRingBuffer.CBAntialiasClick(Sender: TObject);
var t : Integer;
begin
  // For each series
  for t:=0 to High(Lines) do
      Lines[t].Antialias:=CBAntialias.Checked;
end;

procedure TFormRingBuffer.CBGridsClick(Sender: TObject);
begin
  Chart1.Axes.Left.Grid.Visible:=CBGrids.Checked;
  Chart1.Axes.Bottom.Grid.Visible:=CBGrids.Checked;
end;

procedure TFormRingBuffer.CBPointsChange(Sender: TObject);
begin
  RecreateSeries;
end;

procedure TFormRingBuffer.CBRunClick(Sender: TObject);
begin
  if CBRun.Checked then
     Application.OnIdle:=Idle
  else
     Application.OnIdle:=nil;
end;

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

  {$IFDEF USE_OPENGL}
  Chart1.Canvas:=TGLCanvas.Create;
  {$ENDIF}

  {$IFDEF USE_SKIA}
  Chart1.Canvas:=TTeeSkiaCanvas.Create;
  {$ENDIF}

  // Start the animation
  Application.OnIdle:=Idle;
end;

procedure TFormRingBuffer.Label1Click(Sender: TObject);
begin
  AddNewPoint;
end;

procedure TFormRingBuffer.Label2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Benchmark;
end;

procedure TFormRingBuffer.Idle(Sender: TObject; var Done: Boolean);
begin
  AddNewPoint;
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

var P : TPointFloat;
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
var t1 : TStopWatch;
    t :  Integer;
begin
  CBRun.Checked:=False;
  CBRunClick(Self);

  RecreateSeries;

  Chart1.Draw;

  t1:=TStopwatch.StartNew;

  for t:=0 to 1000 do
  begin
    AddNewPoint;
    Chart1.Draw;
  end;

  Caption:=t1.ElapsedMilliseconds.ToString+' msec';

  Invalidate;
end;

end.
