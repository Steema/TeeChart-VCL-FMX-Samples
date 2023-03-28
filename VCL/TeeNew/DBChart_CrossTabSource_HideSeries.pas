unit DBChart_CrossTabSource_HideSeries;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  DB, DBTables,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas,
  DBChart_CrossTabSource, TeeDBEdit, TeeDBCrossTab, DBChart;

type
  TDBCrossTabSourceDemoHideSeries = class(TDBCrossTabSourceDemo)
    cbHideSeries: TCheckBox;
    bEdit: TButton;
    procedure cbHideSeriesClick(Sender: TObject);
    procedure bEditClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar;

procedure TDBCrossTabSourceDemoHideSeries.cbHideSeriesClick(
  Sender: TObject);
begin
  DBCrossTabSource1.HideSeries:=cbHideSeries.Checked;
  DBCrossTabSource1.Refresh;
end;

procedure TDBCrossTabSourceDemoHideSeries.bEditClick(Sender: TObject);
begin
  EditChart(Self,DBChart1);
end;

initialization
  RegisterClass(TDBCrossTabSourceDemoHideSeries);
end.
