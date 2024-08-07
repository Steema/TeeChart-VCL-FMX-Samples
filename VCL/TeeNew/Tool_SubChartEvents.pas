unit Tool_SubChartEvents;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeeComma, TeeSubChart, TeeTools,
  TeeMapSeries, TeeWorldSeries, TeeSurfa;

type
  TTool_SubChartEventsForm = class(TBaseForm)
    Series1: TBarSeries;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    tool1 : TSubChartTool;
    map : TWorldSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar;

procedure TTool_SubChartEventsForm.FormCreate(Sender: TObject);
begin
  inherited;

  tool1 := TSubChartTool.Create(Chart1);
  tool1.ParentChart := Chart1;

  Chart1.Tools.Add(tool1);

  with tool1 do
  begin
    Charts.AddChart('Chart1');

    with Charts[0] do
    begin
      Chart.View3D := false;
      Chart.Axes.Visible := false;
      Chart.Walls.Visible := false;
      Chart.Width := 360;
      Chart.Height := 260;
      Chart.Left := 335;
      Chart.Color:=clnone;

      Chart.Border.Show;

      AllowResize := True;
      AllowDrag   := True;
    end;

    map := TWorldSeries.Create(Self);
    map.ParentChart := Charts[0].Chart;
    Charts[0].Chart.AddSeries(map);

    map.Map := wmEurope;
    map.FillSampleValues;
    map.UseColorRange := False;
    map.UsePalette:= True;
    map.PaletteStyle := psRainbow;
  end;
end;

procedure TTool_SubChartEventsForm.Button1Click(Sender: TObject);
begin
  EditChartTool(Self,tool1);
end;

initialization
  RegisterClass(TTool_SubChartEventsForm);
end.
