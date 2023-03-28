unit DBChart_SingleRecord;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Buttons, Grids, DBGrids, DBCtrls, DBTables,
  Base_DBChart, Db, TeeProcs, TeEngine, Chart, Series, DBChart;

type
  TDBChartSingleRecord = class(TBaseDBChart)
    DBGrid1: TDBGrid;
    Table1: TTable;
    Series1: TBarSeries;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDBChartSingleRecord.CheckBox1Click(Sender: TObject);
begin
  Table1.Active:=CheckBox1.Checked;
end;

procedure TDBChartSingleRecord.FormShow(Sender: TObject);
begin
  inherited;
  CheckTable(Table1);
end;

initialization
  RegisterClass(TDBChartSingleRecord);
end.
