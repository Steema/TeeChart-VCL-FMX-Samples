unit Axis_Custom;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TAxisCustom = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses EditChar;

procedure TAxisCustom.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(20);
  Series2.FillSampleValues(20);
  Series3.FillSampleValues(20);
end;

procedure TAxisCustom.CheckBox1Click(Sender: TObject);
var t:Integer;
begin
  With Chart1.CustomAxes do
  for t:=0 to Count-1 do Items[t].Visible:=CheckBox1.Checked
end;

procedure TAxisCustom.Button1Click(Sender: TObject);
begin
  if Chart1.CustomAxes.Count=0 then
     EditChartAxis(Self,Chart1.LeftAxis)
  else
     EditChartAxis(Self,Chart1.CustomAxes[0]);
end;

initialization
  RegisterClass(TAxisCustom);
end.
