unit Donut_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList,
  Base, TeCanvas, TeEngine, Series, TeeDonut, TeeProcs, Chart,
  TeeChartActions;

type
  TDonutSeriesForm = class(TBaseForm)
    Series1: TDonutSeries;
    Label1: TLabel;
    EDonut: TEdit;
    UDDonut: TUpDown;
    CheckBox1: TCheckBox;
    ActionList1: TActionList;
    ChartAction3D1: TChartAction3D;
    procedure EDonutChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDonutSeriesForm.EDonutChange(Sender: TObject);
begin
  if Showing then Series1.DonutPercent:=UDDonut.Position;
end;

procedure TDonutSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

initialization
  RegisterClass(TDonutSeriesForm);
end.
