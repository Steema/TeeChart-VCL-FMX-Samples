unit ChartListBox_FontSeriesColor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeLisB;

type
  TChartListBoxFontSeriesColor = class(TBaseForm)
    ChartListBox1: TChartListBox;
    Series1: TLineSeries;
    Series2: TPointSeries;
    Series3: TLineSeries;
    Series4: TPointSeries;
    cbFont: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbFontClick(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartListBoxFontSeriesColor.Chart1AfterDraw(Sender: TObject);
var wasFalse: Boolean;
begin
  inherited;
  wasFalse:=not ChartListBox1.Visible;
  if wasFalse then
  begin
    ChartListBox1.Visible:=true;
    Chart1.Repaint;
  end;
end;

procedure TChartListBoxFontSeriesColor.FormCreate(Sender: TObject);
begin
  with ChartListBox1 do
  begin
    Chart:=Chart1;
    FontSeriesColor:=true;
    Visible:=false;
  end;

  inherited;
  Chart1.Draw;
end;

procedure TChartListBoxFontSeriesColor.cbFontClick(Sender: TObject);
begin
  ChartListBox1.FontSeriesColor:=cbFont.Checked;
end;

initialization
  RegisterClass(TChartListBoxFontSeriesColor);
end.
