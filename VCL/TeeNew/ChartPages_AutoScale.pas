unit ChartPages_AutoScale;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeNavigator, TeeEdiGene;

type
  TChartPagesAutoScale = class(TBaseForm)
    ChartPageNavigator1: TChartPageNavigator;
    CheckBox1: TCheckBox;
    Series1: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartPagesAutoScale.FormCreate(Sender: TObject);
begin
  inherited;

  Series1.FillSampleValues(100);

  Chart1.Pages.MaxPointsPerPage:=10;
  Chart1.Pages.AutoScale:=True;

  CheckBox1.Checked:=Chart1.Pages.AutoScale;
end;

procedure TChartPagesAutoScale.CheckBox1Click(Sender: TObject);
begin
  Chart1.Pages.AutoScale:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TChartPagesAutoScale);
end.
