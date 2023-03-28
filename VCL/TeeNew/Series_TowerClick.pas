unit Series_TowerClick;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeSurfa;

type
  TSeriesTowerClick = class(TBaseForm)
    Series1: TTowerSeries;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Series1Click(Sender: TChartSeries; ValueIndex: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    oldColor : TColor;
    oldIndex : Integer;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesTowerClick.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.FillSampleValues(5);
  oldIndex:=-1;
end;

procedure TSeriesTowerClick.Series1Click(Sender: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Label1.Caption:=' Clicked Tower :  ' + IntToStr(ValueIndex) +
  '   X :  ' + FormatFloat('0.##',Series1.XValue[ValueIndex]) +
  '   Y :  ' + FormatFloat('0.##',Series1.YValue[ValueIndex]) +
  '   Z :  ' + FormatFloat('0.##',Series1.ZValue[ValueIndex]);

  if ValueIndex<>oldIndex then
  begin
    if oldIndex <> -1 then
       Series1.ValueColor[oldIndex]:=oldColor;

    oldColor:=Series1.ValueColor[ValueIndex];
    oldIndex:=ValueIndex;

    Series1.ValueColor[ValueIndex]:=clRed;
  end;
end;

initialization
  RegisterClass(TSeriesTowerClick);
end.
