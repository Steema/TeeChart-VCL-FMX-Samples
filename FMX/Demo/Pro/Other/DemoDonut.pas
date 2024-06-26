unit DemoDonut;

interface

uses
  {$IFDEF D17}
  FMX.StdCtrls, FMX.Controls.Presentation,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Base, FMX.Objects,
  FMXTee.Engine, FMXTee.Series, FMXTee.Series.Donut, FMXTee.Procs, FMXTee.Chart;

type
  TDemoDonutSeries = class(TBaseForm)
    Chart1: TChart;
    Series1: TDonutSeries;
    Text1: TText;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TDemoDonutSeries.TrackBar1Change(Sender: TObject);
begin
  Series1.DonutPercent:=Round(TrackBar1.Value);
end;

initialization
  RegisterClass(TDemoDonutSeries);
end.
