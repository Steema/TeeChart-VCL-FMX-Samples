unit Series_DoubleHorizBar;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeGDIPlus, TeEngine, Series, TeeDoubleHorizBar, ComCtrls,
  StdCtrls, TeeProcs, Chart, ExtCtrls, TeeSubChart;

type
  TDoubleHorizBarSeriesForm = class(TBaseForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Series1: TDoubleHorizBarSeries;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDoubleHorizBarSeriesForm.TrackBar1Change(Sender: TObject);
begin
  Series1.SplitSize:=TrackBar1.Position;
  Label2.Caption:=IntToStr(TrackBar1.Position)+'%';
end;

procedure TDoubleHorizBarSeriesForm.FormShow(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
  Series1.ColorEachPoint:=True;
end;

initialization
  RegisterClass(TDoubleHorizBarSeriesForm);
end.
