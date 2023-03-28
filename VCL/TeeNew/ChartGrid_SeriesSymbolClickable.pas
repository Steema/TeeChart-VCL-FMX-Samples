unit ChartGrid_SeriesSymbolClickable;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Grids,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeChartGrid, TeeTools;

type
  TChartGridSeriesSymbolClickable = class(TBaseForm)
    cbSeriesSymbolClickable: TCheckBox;
    ChartGrid1: TChartGrid;
    Series1: TBarSeries;
    Series2: TPointSeries;
    GridBandTool1: TGridBandTool;
    procedure cbSeriesSymbolClickableClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartGridSeriesSymbolClickable.cbSeriesSymbolClickableClick(
  Sender: TObject);
begin
  ChartGrid1.SeriesSymbolClickable:=cbSeriesSymbolClickable.Checked;
end;

initialization
  RegisterClass(TChartGridSeriesSymbolClickable);
end.
