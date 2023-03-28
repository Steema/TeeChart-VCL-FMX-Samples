unit DBChart_Order;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  DBTables,
  Base_DBChart, TeeProcs, TeEngine, Chart, DBChart, Db, Series;

type
  TDBChartOrder = class(TBaseDBChart)
    Table1: TTable;
    RadioGroup1: TRadioGroup;
    Series1: TBarSeries;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDBChartOrder.FormShow(Sender: TObject);
begin
  inherited;
  CheckTable(Table1);
end;

procedure TDBChartOrder.RadioGroup1Click(Sender: TObject);
begin
  inherited;
  // Change Series Order
  Case RadioGroup1.ItemIndex of
    0: Series1.YValues.Order:=loNone;
    1: Series1.YValues.Order:=loAscending;
    2: Series1.YValues.Order:=loDescending;
  end;
  DBChart1.RefreshData;
end;

procedure TDBChartOrder.CheckBox1Click(Sender: TObject);
begin
  // Open - Close table
  Table1.Active:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TDBChartOrder);
end.
