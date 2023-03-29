unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, TeeTools, Series, TeeDonut, ExtCtrls,
  TeeProcs, Chart, StdCtrls;

type
  TActivityGaugeForm = class(TForm)
    Chart1: TChart;
    Series1: TDonutSeries;
    ChartTool1: TAnnotationTool;
    Series2: TDonutSeries;
    Timer1: TTimer;
    Timer2: TTimer;
    Series3: TDonutSeries;
    Timer3: TTimer;
    Panel1: TPanel;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    procedure ResetValues;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ActivityGaugeForm: TActivityGaugeForm;

implementation

{$R *.dfm}

procedure TActivityGaugeForm.Button1Click(Sender: TObject);
begin
  ResetValues;
  Timer1.Enabled := true;
  Button1.Enabled := false;
end;

procedure TActivityGaugeForm.ResetValues;
var i : integer;
begin
  ChartTool1.Text := '0 %';
  Chart1.Hover.Visible := false;

  for i := 0 to Chart1.SeriesCount-1 do
  begin
    Chart1[i].FillSampleValues(2);
    Chart1[i].YValue[0] := 100;
    Chart1[i].YValue[1] := 0;
    Chart1[i].ValueColor[0] := Series1.Pen.Color;
  end;
end;

procedure TActivityGaugeForm.FormCreate(Sender: TObject);
begin
  ResetValues;
end;

procedure TActivityGaugeForm.Timer1Timer(Sender: TObject);
begin
  Series1.YValue[0] := Series1.YValue[0]-1;
  Series1.YValue[1] := Series1.YValue[1]+1;

  ChartTool1.Text := IntToStr(Round(Series1.YValue[1])) + ' %';

  if (Series1.YValue[1] = 90) then
  begin
    Timer1.Enabled := false;
    Timer2.Enabled := true;
  end;
end;

procedure TActivityGaugeForm.Timer2Timer(Sender: TObject);
begin
  Series2.YValue[0] := Series2.YValue[0]-1;
  Series2.YValue[1] := Series2.YValue[1]+1;

  ChartTool1.Text := IntToStr(Round(Series2.YValue[1])) + ' %';

  if (Series2.YValue[1] = 75) then
  begin
    Timer2.Enabled := false;
    Timer3.Enabled := true;
  end;
end;

procedure TActivityGaugeForm.Timer3Timer(Sender: TObject);
begin
  Series3.YValue[0] := Series3.YValue[0]-1;
  Series3.YValue[1] := Series3.YValue[1]+1;

  ChartTool1.Text := IntToStr(Round(Series3.YValue[1])) + ' %';

  if (Series3.YValue[1] = 60) then
  begin
    Timer3.Enabled := false;
    Button1.Enabled := true;
  end;
end;

procedure TActivityGaugeForm.Panel1Resize(Sender: TObject);
begin
  Button1.Width:=Panel1.Width;
end;

end.
