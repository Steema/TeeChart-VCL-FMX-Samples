unit Function_Histogram;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, 
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeHistogram, 
  TeeSeriesBandTool;

type
  THistogramFunctionForm = class(TBaseForm)
    Series1: TBarSeries;
    TeeFunction1: THistogramFunction;
    ChartTool1: TSeriesBandTool;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Series2: TBarSeries;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure THistogramFunctionForm.CheckBox1Click(Sender: TObject);
begin
  // Source
  Series1.Visible:=CheckBox1.Checked;

  // Histogram
  Series2.Visible:=not Series1.Visible;
end;

procedure THistogramFunctionForm.CheckBox2Click(Sender: TObject);
begin
  TeeFunction1.Cumulative:=CheckBox2.Checked;
end;

procedure THistogramFunctionForm.Edit1Change(Sender: TObject);
begin
  TeeFunction1.NumBins:=UpDown1.Position;
end;

initialization
  RegisterClass(THistogramFunctionForm);
end.
