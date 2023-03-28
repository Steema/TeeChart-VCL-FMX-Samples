unit Series_WindRosePointerStyle;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePolar, TeeRose;

type
  TSeriesWindRosePointerStyle = class(TBaseForm)
    Series1: TWindRoseSeries;
    CheckBox1: TCheckBox;
    function Series1GetPointerStyle(Sender: TChartSeries;
      ValueIndex: Integer): TSeriesPointerStyle;
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

// This event is called for each point in Series1.
                                                 
function TSeriesWindRosePointerStyle.Series1GetPointerStyle(
  Sender: TChartSeries; ValueIndex: Integer): TSeriesPointerStyle;
begin
  if CheckBox1.Checked then
  begin

    if Series1.RadiusValues.Items[ValueIndex] > 600 then
    begin
      Series1.Pointer.HorizSize:=10;
      Series1.Pointer.VertSize:=10;
      result:=psCircle;
    end
    else
    begin
      Series1.Pointer.HorizSize:=4;
      Series1.Pointer.VertSize:=4;
      result:=TSeriesPointerStyle(Random(13));
    end;

  end
  else
    result:=Series1.Pointer.Style;
end;

procedure TSeriesWindRosePointerStyle.CheckBox1Click(Sender: TObject);
begin
  Series1.Repaint;
end;

initialization
  RegisterClass(TSeriesWindRosePointerStyle);
end.
