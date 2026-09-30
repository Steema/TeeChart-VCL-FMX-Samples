unit Tool_Threshold;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Forms,
  Base, TeEngine, Series, TeeProcs, Chart, TeeThresholdTool;

type
  TThresholdToolForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TThresholdToolForm.FormCreate(Sender: TObject);
var
  LineSeries: TLineSeries;
  ThresholdTool: TThresholdTool;
begin
  inherited;

  Chart1.View3D:=False;
  Chart1.Legend.Visible:=False;

  LineSeries:=TLineSeries.Create(Self);
  Chart1.AddSeries(LineSeries);
  LineSeries.Pointer.Visible:=True;
  LineSeries.Pointer.Style:=psCircle;
  LineSeries.Pointer.HorizSize:=4;
  LineSeries.Pointer.VertSize:=4;
  LineSeries.FillSampleValues();

  Chart1.Axes.Left.Increment:=LineSeries.YValues.Range / 5;

  ThresholdTool:=TThresholdTool.Create(Self);
  Chart1.Tools.Add(ThresholdTool);

  with ThresholdTool do
  begin
    Axis:=Chart1.LeftAxis;
    Value:=LineSeries.YValues.MinValue+(LineSeries.YValues.Range/3);
    AllowDrag:=True;
    DragRepaint:=True;
    Pen.Visible:=True;
    Pen.Color:=clGray;
    Pen.Style:=psDash;
    Pen.Width:=1;
    Interpolate:=True;
    AddSeries(LineSeries);

    CrossingPointer.Visible:=True;
    CrossingPointer.Style:=psCircle;
    CrossingPointer.Color:=clRed;
    CrossingPointer.HorizSize:=8;
    CrossingPointer.VertSize:=8;

    UpperStyle.LinePen.Hide;
    LowerStyle.LinePen.Hide;
  end;

  Memo1.Clear;
  Memo1.Lines.Add('The Threshold Tool detects crossings on a line series.');
  Memo1.Lines.Add('The series pointers remain visible, while the larger red markers show the interpolated crosspoints.');
  Memo1.Lines.Add('Drag the dashed threshold line to move the crossing level.');
end;

initialization
  RegisterClass(TThresholdToolForm);
end.
