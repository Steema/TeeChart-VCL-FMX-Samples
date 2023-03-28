unit SeriesType_OrgChart;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeePenDlg, TeEngine, Series, CandleCh, TeeProcs, Chart,
  TeeOrgSeries;

type
  TSeriesTypeOrgChart = class(TBaseForm)
    Button1: TButton;
    Series1: TOrgSeries;
    ComboFlat1: TComboFlat;
    procedure Button1Click(Sender: TObject);
    procedure ComboFlat1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses
  EditChar;

procedure TSeriesTypeOrgChart.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TSeriesTypeOrgChart.ComboFlat1Change(Sender: TObject);
begin
  Series1.LineStyle:=TOrgLineStyle(ComboFlat1.ItemIndex);
end;

procedure TSeriesTypeOrgChart.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
end;

initialization
  RegisterClass(TSeriesTypeOrgChart);
end.
