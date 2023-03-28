unit Chart_SeriesGroups;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TChartSeriesGroups = class(TBaseForm)
    Label1: TLabel;
    cbGroups: TComboFlat;
    Label2: TLabel;
    cbActive: TComboFlat;
    Series1: TBarSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TPointSeries;
    bEdit: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bEditClick(Sender: TObject);
    procedure cbGroupsChange(Sender: TObject);
    procedure cbActiveChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Editchar;

procedure TChartSeriesGroups.FormCreate(Sender: TObject);
var
  i : integer;
begin
  inherited;

  with Chart1 do
  begin
    SeriesList.AddGroup('Group A');
    SeriesGroups.Items[0].Add(Series1);
    SeriesGroups.Items[0].Add(Series2);

    SeriesList.AddGroup('Group B');
    SeriesGroups.Items[1].Add(Series3);
    SeriesGroups.Items[1].Add(Series4);

    Legend.LegendStyle:=lsSeriesGroups;

    for i:=0 to SeriesGroups.Count-1 do
        cbGroups.Add(SeriesGroups.Items[i].Name);

  end;

  cbGroups.ItemIndex:=0;
  cbActive.ItemIndex:=0;
end;

procedure TChartSeriesGroups.bEditClick(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

procedure TChartSeriesGroups.cbGroupsChange(Sender: TObject);
begin
  cbActive.ItemIndex:=Ord(Chart1.SeriesGroups.Items[cbGroups.ItemIndex].Active);
end;

procedure TChartSeriesGroups.cbActiveChange(Sender: TObject);
begin
  Chart1.SeriesGroups.Items[cbGroups.ItemIndex].Active:=TSeriesGroupActive(cbActive.ItemIndex);
end;

initialization
  RegisterClass(TChartSeriesGroups);
end.
