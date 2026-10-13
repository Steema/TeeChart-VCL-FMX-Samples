unit Tool_ThresholdMulti;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Forms,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeeTools, TeeThresholdTool;

type
  TThresholdToolMultiForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TThresholdToolMultiForm.FormCreate(Sender: TObject);
var
  Series1,Series2: TPointSeries;
  ThresholdTool: TThresholdTool;
begin
  inherited;

  Chart1.View3D:=False;
  Chart1.Legend.Visible:=True;

  Series1:=TPointSeries.Create(Self);
  Chart1.AddSeries(Series1);
  Series1.Title:='Series 1';
  Series1.Pointer.Visible:=True;
  Series1.Pointer.Style:=psCircle;
  Series1.Pointer.HorizSize:=5;
  Series1.Pointer.VertSize:=5;
  Series1.FillSampleValues();

  Series2:=TPointSeries.Create(Self);
  Chart1.AddSeries(Series2);
  Series2.Title:='Series 2';
  Series2.Pointer.Visible:=True;
  Series2.Pointer.Style:=psTriangle;
  Series2.Pointer.HorizSize:=5;
  Series2.Pointer.VertSize:=5;
  Series2.FillSampleValues();

  ThresholdTool:=TThresholdTool.Create(Self);
  Chart1.Tools.Add(ThresholdTool);

  with ThresholdTool do
  begin
    Axis:=Chart1.LeftAxis;
    Value:=Series2.YValues.MinValue+(Series2.YValues.Range/2);
    AllowDrag:=True;
    DragRepaint:=True;
    Pen.Visible:=True;
    Pen.Color:=clGray;
    Pen.Style:=psDash;
    Pen.Width:=1;
  end;

  with ThresholdTool.AddSeries(Series1) do
  begin
    UseLowerStyle:=True;
    LowerStyle.Pointer.Visible:=True;
    LowerStyle.Pointer.Style:=psDonut;
    LowerStyle.Pointer.Color:=ApplyDark(Series1.Color,64);
    LowerStyle.Pointer.HorizSize:=8;
    LowerStyle.Pointer.VertSize:=8;
  end;

  with ThresholdTool.AddSeries(Series2) do
  begin
    UseLowerStyle:=True;
    LowerStyle.Pointer.Visible:=True;
    LowerStyle.Pointer.Style:=psDownTriangle;
    LowerStyle.Pointer.Color:=ApplyDark(Series2.Color,64);
    LowerStyle.Pointer.HorizSize:=8;
    LowerStyle.Pointer.VertSize:=8;
  end;

  Memo1.Clear;
  Memo1.Lines.Add('Two point series share the same threshold.');
  Memo1.Lines.Add('Both series keep their visible pointers, while points below the threshold in use a different custom pointer style and color for each series.');
  Memo1.Lines.Add('Series 1 changes from a circle to a donut pointer and Series 2 changes from a triangle to a down triangle to show that each custom style applies to each series independently.');
end;

initialization
  RegisterClass(TThresholdToolMultiForm);
end.
