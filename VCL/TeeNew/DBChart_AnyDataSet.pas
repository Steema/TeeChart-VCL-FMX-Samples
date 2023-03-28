unit DBChart_AnyDataSet;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  DBTables,
  Base_DBChart, TeeProcs, TeEngine, Chart, DBChart, Series, Db;

type
  TDBChartAny = class(TBaseDBChart)
    Table1: TTable;
    Series1: TPieSeries;
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

procedure TDBChartAny.CheckBox1Click(Sender: TObject);
begin
  Table1.Active:=CheckBox1.Checked;
end;

procedure TDBChartAny.FormShow(Sender: TObject);
begin
  inherited;
  CheckTable(Table1);
end;

initialization
  RegisterClass(TDBChartAny);
end.
