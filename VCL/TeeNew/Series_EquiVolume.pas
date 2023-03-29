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
    procedure Button1Click(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar;

procedure TEquiVolumeSeriesForm.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TEquiVolumeSeriesForm.CBMarksClick(Sender: TObject);
begin
  Series1.Marks.Visible:=CBMarks.Checked;
end;

initialization
  RegisterClass(TEquiVolumeSeriesForm);
end.
