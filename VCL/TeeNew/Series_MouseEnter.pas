unit Series_MouseEnter;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, Series, TeEngine, TeeProcs, Chart;

type
  TSeriesMouseEnter = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TLineSeries;
    Label1: TLabel;
    procedure Series1MouseLeave(Sender: TObject);
    procedure Series1MouseEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesMouseEnter.Series1MouseLeave(Sender: TObject);
begin
  Label1.Caption:='';
end;

procedure TSeriesMouseEnter.Series1MouseEnter(Sender: TObject);
var tmp     : TChartSeries;
    clicked : Integer;
begin
  tmp:=Sender as TChartSeries;  // Sender is the Series

  // Obtain point index under mouse cursor
  clicked:=tmp.GetCursorValueIndex;

  // Show Series name and point index and value
  Label1.Caption:='Series: '+tmp.Name+
                  ' point: '+IntToStr(clicked)+
                  ' value: '+tmp.YValues.ValueToString(clicked);
end;

initialization
  RegisterClass(TSeriesMouseEnter);
end.
