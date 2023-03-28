unit Tools_Assign;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeTools, TeeMagnifyTool;

type
  TToolsAssign = class(TBaseForm)
    Splitter1: TSplitter;
    Chart2: TChart;
    Series1: TPointSeries;
    Series2: TBarSeries;
    BitBtn3: TBitBtn;
    ChartTool3: TGridBandTool;
    ChartTool1: TGridBandTool;
    bEdit: TButton;
    bEdit2: TButton;
    procedure BitBtn3Click(Sender: TObject);
    procedure bEditClick(Sender: TObject);
    procedure bEdit2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar;

procedure TToolsAssign.BitBtn3Click(Sender: TObject);
var i : integer;
begin
  for i:=0 to Chart2.Tools.Count-1 do
      Chart2.Tools[i].Assign(Chart1.Tools[i]);
end;

procedure TToolsAssign.bEditClick(Sender: TObject);
begin
  EditChartTool(Self,Chart1.Tools[0]);
end;

procedure TToolsAssign.bEdit2Click(Sender: TObject);
begin
  EditChartTool(Self,Chart2.Tools[0]);
end;

initialization
  RegisterClass(TToolsAssign);
end.
