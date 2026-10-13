unit Tool_ThresholdArea;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Forms,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools, TeeThresholdTool;

type
  TThresholdToolAreaForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TThresholdToolAreaForm.FormCreate(Sender: TObject);
var
  AreaSeries: TAreaSeries;
  ThresholdTool: TThresholdTool;
begin
  inherited;

  Chart1.View3D:=False;
  Chart1.Legend.Visible:=False;

  AreaSeries:=TAreaSeries.Create(Self);
  Chart1.AddSeries(AreaSeries);
  AreaSeries.FillSampleValues();
  AreaSeries.AreaLinesPen.Hide;

  ThresholdTool:=TThresholdTool.Create(Self);
  Chart1.Tools.Add(ThresholdTool);

  with ThresholdTool do
  begin
    Axis:=Chart1.LeftAxis;
    Value:=AreaSeries.YValues.MinValue+(AreaSeries.YValues.Range/2);
    AllowDrag:=True;
    DragRepaint:=True;
    DrawBehind:=True;
    Pen.Visible:=True;
    Pen.Color:=clGray;
    Pen.Style:=psDash;
    Pen.Width:=1;
    Interpolate:=True;
    AddSeries(AreaSeries);

    UpperStyle.AreaBrush.Style:=bsSolid;
    UpperStyle.AreaBrush.Color:=clYellow;
    UpperStyle.Transparency:=25;
    UpperStyle.UseSourceTransparency:=False;
  end;

  Memo1.Clear;
  Memo1.Lines.Add('The Threshold Tool highlights the part of an area series above the threshold.');
  Memo1.Lines.Add('The upper area is filled yellow; the area below the threshold remains unfilled.');
  Memo1.Lines.Add('Drag the dashed threshold line to adjust the highlighted region.');
end;

initialization
  RegisterClass(TThresholdToolAreaForm);
end.
