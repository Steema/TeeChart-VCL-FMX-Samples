unit ChartGrid_Grid3D;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Grids, StdCtrls, ExtCtrls,
  Base, TeEngine, TeeSurfa, TeeChartGrid, TeeProcs, Chart;

type
  TChartGrid3D = class(TBaseForm)
    CheckBox1: TCheckBox;
    ChartGrid1: TChartGrid;
    Splitter1: TSplitter;
    Series1: TColorGridSeries;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartGrid3D.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(5);
  ChartGrid1.Grid3DMode:=True;

  // cosmetic
  ChartGrid1.DefaultColWidth:=38;
end;

procedure TChartGrid3D.CheckBox1Click(Sender: TObject);
begin
  ChartGrid1.Grid3DMode:=not ChartGrid1.Grid3DMode;
end;

initialization
  RegisterClass(TChartGrid3D);
end.
