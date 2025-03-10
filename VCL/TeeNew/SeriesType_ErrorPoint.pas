unit SeriesType_ErrorPoint;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TErrorPointSeriesForm = class(TBaseForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses EditChar, TeeConst, TeeErrorPoint;

var Series1: TErrorPointSeries;

procedure TErrorPointSeriesForm.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TErrorPointSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;

  Series1:=Chart1.AddSeries(TErrorPointSeries) as TErrorPointSeries;
  Series1.ColorEachPoint:=true;
  Series1.FillSampleValues(10);
end;

initialization
  RegisterClass(TErrorPointSeriesForm);
end.
