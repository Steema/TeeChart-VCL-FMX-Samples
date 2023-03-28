unit Candle_OpenClosePen;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, OHLChart, CandleCh, TeCanvas,
  TeePenDlg, TeeProcs, Chart;

type
  TCandleOpenClosePen = class(TBaseForm)
    Series1: TCandleSeries;
    ButtonColor1: TButtonColor;
    ButtonColor2: TButtonColor;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CandleOpenClosePen: TCandleOpenClosePen;
  
implementation

{$R *.dfm}

procedure TCandleOpenClosePen.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);
  Series1.CandleStyle:=csCandleBar;

  ButtonColor1.LinkProperty(Series1, 'OpenTickColor');
  ButtonColor2.LinkProperty(Series1, 'CloseTickColor');

  Series1.OpenTickColor:=clGreen;
  Series1.CloseTickColor:=clRed;

  Series1.CandleWidth:=15;
end;

initialization
  RegisterClass(TCandleOpenClosePen);
end.
