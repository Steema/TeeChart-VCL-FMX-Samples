unit ChartListBox_Radio;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages, 
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Base, TeeLisB, TeeProcs, TeEngine, Chart, Series;

type
  TChartListBoxRadio = class(TBaseForm)
    RadioGroup1: TRadioGroup;
    ChartListBox1: TChartListBox;
    Series1: TBarSeries;
    Series2: TLineSeries;
    Series3: TAreaSeries;
    Series4: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChartListBoxRadio: TChartListBoxRadio;

implementation

{$R *.dfm}

procedure TChartListBoxRadio.Chart1AfterDraw(Sender: TObject);
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

procedure TChartListBoxRadio.FormCreate(Sender: TObject);
begin
  ChartListBox1.Visible:=false;
  inherited;

  Chart1.SeriesList.FillSampleValues;
  Chart1.Draw;
end;

procedure TChartListBoxRadio.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex=0 then
     ChartListBox1.CheckStyle:= cbsCheck
  else
     ChartListBox1.CheckStyle:= cbsRadio;
end;

initialization
  RegisterClass(TChartListBoxRadio);
end.
