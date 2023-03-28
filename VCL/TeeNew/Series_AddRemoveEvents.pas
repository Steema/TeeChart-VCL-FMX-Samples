unit Series_AddRemoveEvents;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeComma;

type
  TSeriesAddRemoveEvents = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    procedure Chart1AddSeries(Sender: TCustomChartSeries);
    procedure Chart1RemoveSeries(Sender: TCustomChartSeries);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesAddRemoveEvents.Chart1AddSeries(Sender: TCustomChartSeries);
var Color : TColor;
begin
  Chart1.Title.Text.Text:='Added Series named : ' + Sender.Name;
  Color := RGB(Random(255),Random(255),Random(255));;

  Chart1.Gradient.StartColor:=Color;
  TeeCommander1.Gradient.EndColor:=Color;
end;

procedure TSeriesAddRemoveEvents.Chart1RemoveSeries(Sender: TCustomChartSeries);
var Color : TColor;
begin
  Chart1.Title.Text.Text:='Removed Series named : ' + Sender.Name;
  Color := RGB(Random(255),Random(255),Random(255));;

  Chart1.Gradient.StartColor:=Color;
  TeeCommander1.Gradient.EndColor:=Color;
end;

procedure TSeriesAddRemoveEvents.FormCreate(Sender: TObject);
begin
  inherited;
  TeeCommander1.Panel:=Chart1;
end;

initialization
  RegisterClass(TSeriesAddRemoveEvents);
end.

