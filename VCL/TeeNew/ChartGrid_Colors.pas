unit ChartGrid_Colors;
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
  TeeProcs, Chart;

type
  TChartGridColors = class(TChartGridForm)
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartGridColors.CheckBox3Click(Sender: TObject);
begin
  ChartGrid1.ShowColors:=CheckBox3.Checked;
end;

procedure TChartGridColors.CheckBox4Click(Sender: TObject);
begin
  ChartGrid1.ShowFields:=CheckBox4.Checked;
end;

initialization
  RegisterClass(TChartGridColors);
end.
