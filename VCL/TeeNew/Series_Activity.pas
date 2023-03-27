unit Series_Activity;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeEngine, TeeTools, Series, TeeDonut,
  TeeActivityGauge, TeeProcs, Chart, ExtCtrls, StdCtrls;

type
  TActivitySeriesForm = class(TBaseForm)
    Series1: TActivityGauge;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TActivitySeriesForm.FormShow(Sender: TObject);
begin
  inherited;

  Series1.BackColor:=clWhite;
  Series1.FillSampleValues;

  Series1.CenterText.Text:='57%';
end;

initialization
  RegisterClass(TActivitySeriesForm);
end.
