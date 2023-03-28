unit Legend_Scrollbar;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeeLegendScrollBar;

type
  TLegendScrollBarDemo = class(TBaseForm)
    Button1: TButton;
    Series1: TPointSeries;
    ChartTool1: TLegendScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChartTool1Scrolled(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses EditChar;

procedure TLegendScrollBarDemo.Button1Click(Sender: TObject);
begin
  EditChartTool(Self,ChartTool1);
end;

procedure TLegendScrollBarDemo.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(50);
end;

procedure TLegendScrollBarDemo.ChartTool1Scrolled(Sender: TObject);
begin
  Label2.Caption:=IntToStr(Chart1.Legend.FirstValue);
end;

initialization
  RegisterClass(TLegendScrollBarDemo);
end.
