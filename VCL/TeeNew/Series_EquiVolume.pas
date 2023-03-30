unit Series_EquiVolume;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls, Series,
  OHLChart, CandleCh, TeeEquiVolume;

type
  TEquiVolumeSeriesForm = class(TBaseForm)
    Series1: TEquiVolumeSeries;
    Button1: TButton;
    CBMarks: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Series1Click(Sender: TChartSeries; ValueIndex: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar, TeeEquiVolEdi;

procedure TEquiVolumeSeriesForm.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TEquiVolumeSeriesForm.CBMarksClick(Sender: TObject);
begin
  Series1.Marks.Visible:=CBMarks.Checked;
end;

procedure TEquiVolumeSeriesForm.Chart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  inherited;

  tmp:=Series1.Clicked(x,y);

  if tmp=-1 then
     Label1.Caption:=''
  else
     Label1.Caption:=DateToStr(Series1.DateValues[tmp]);
end;

procedure TEquiVolumeSeriesForm.FormShow(Sender: TObject);
begin
  inherited;
  Chart1.Axes.Bottom.Grid.Show;
end;

procedure TEquiVolumeSeriesForm.Series1Click(Sender: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  Label2.Caption:='Clicked index: '+IntToStr(ValueIndex);
end;

initialization
  RegisterClass(TEquiVolumeSeriesForm);
end.
