unit MainUnit;

interface

{
  Demo of TeeChart "Ring Buffer" series.

  This is a (fast) way to add and display lines or scatter dots.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart,

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
  private
    { Private declarations }

    procedure AddNewPoint;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function MaxPoints:Integer;
    procedure Recreate;
  public
    { Public declarations }

    LastX : Integer;

    Lines : Array of TRingBufferXY;
  end;

var
  FormRingBuffer: TFormRingBuffer;

implementation

{$R *.dfm}

{.$DEFINE USE_OPENGL} // OpenGL fast graphics (needs TeeChart "Pro" version)
{$IFDEF USE_OPENGL}
uses
  TeeGLCanvas;
{$ENDIF}

{.$DEFINE USE_SKIA} // Skia fast graphics (needs TeeChart "Pro" version)
{$IFDEF USE_SKIA}
uses
  TeeSkia;
{$ENDIF}


const
  MaxRandom = 1000;

function TFormRingBuffer.MaxPoints:Integer;
begin
  result:=StrToInt(CBPoints.Text);
end;

procedure TFormRingBuffer.Recreate;
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
  Recreate;
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
  Recreate;
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
  Recreate;

  Chart1.Axes.Left.SetMinMax(-MaxRandom,MaxRandom);

  // Speed optimizations
  Chart1.Legend.Hide;
  Chart1.ClipPoints:=False;

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

end.
