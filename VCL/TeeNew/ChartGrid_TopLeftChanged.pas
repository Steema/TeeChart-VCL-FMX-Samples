unit ChartGrid_TopLeftChanged;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Grids,
  Chart_Grid, TeeChartGrid, TeeNavigator, TeCanvas, TeEngine, Series,
  TeeProcs, Chart, TeeTools;

type
  TChartGridTopLeftChanged = class(TChartGridForm)
    CheckBox4: TCheckBox;
    lTopLeft: TLabel;
    CheckBox3: TCheckBox;
    GridBandTool1: TGridBandTool;
    Series2: TPointSeries;
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ChartGridTopLeftChanged(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartGridTopLeftChanged.CheckBox3Click(Sender: TObject);
begin
  ChartGrid1.ShowColors:=CheckBox3.Checked;
end;

procedure TChartGridTopLeftChanged.CheckBox4Click(Sender: TObject);
begin
  ChartGrid1.ShowFields:=CheckBox4.Checked;
end;

procedure TChartGridTopLeftChanged.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(25);
  Series2.FillSampleValues(25);

  ChartGrid1.OnTopLeftChanged := ChartGridTopLeftChanged;
  ChartGridTopLeftChanged(ChartGrid1);

  CheckBox1.Visible:=False;
  ChartGrid1.ShowLabels:=False;
end;

procedure TChartGridTopLeftChanged.ChartGridTopLeftChanged(Sender: TObject);
begin
  lTopLeft.Caption:='TopRow: '+IntToStr(ChartGrid1.TopRow)+', LeftCol: '+IntToStr(ChartGrid1.LeftCol);
end;

initialization
  RegisterClass(TChartGridTopLeftChanged);
end.
