unit Series_Violin;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,

  Base, TeeGDIPlus, TeEngine, Series, TeeBoxPlot, TeeProcs, Chart;

type
  TViolinSeriesForm = class(TBaseForm)
    Series1: TViolinSeries;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Series2: TBeeSwarmSeries;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TViolinSeriesForm.CheckBox1Click(Sender: TObject);
begin
  Series1.Pointer.Visible:=CheckBox1.Checked;
end;

procedure TViolinSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;

  Series1.FillSampleValues;
end;

procedure TViolinSeriesForm.CheckBox2Click(Sender: TObject);
begin
  Series1.PointerBehind:=CheckBox2.Checked;
end;

procedure TViolinSeriesForm.Chart1AfterDraw(Sender: TObject);
begin
  inherited;

  // Bee Swarm needs the chart to be repainted once
  if Series2.Count=0 then
     Series2.FillSampleValues;
end;

initialization
  RegisterClass(TViolinSeriesForm);
end.
