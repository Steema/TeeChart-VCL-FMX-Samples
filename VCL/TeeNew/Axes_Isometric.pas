unit Axes_Isometric;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Math,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Base, TeeProcs, TeEngine, Chart, Series;

type
  TAxesIsometric = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    Label1: TLabel;
    CBMode: TComboBox;
    BReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CBModeChange(Sender: TObject);
    procedure BResetClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TAxesIsometric.FormCreate(Sender: TObject);
var t : Integer;
    Angle : Double;
begin
  inherited;

  // A circle and a square reveal unequal horizontal and vertical scales.
  Series1.Clear;
  for t:=0 to 360 do
  begin
    Angle:=t*Pi/180;
    Series1.AddXY(40*Cos(Angle),40*Sin(Angle));
  end;

  Series2.Clear;
  Series2.AddXY(-40,-40);
  Series2.AddXY(-40,40);
  Series2.AddXY(40,40);
  Series2.AddXY(40,-40);
  Series2.AddXY(-40,-40);

  with Chart1.Axes.Isometric do
  begin
    HorizontalAxis:=Chart1.Axes.Bottom;
    VerticalAxis:=Chart1.Axes.Left;
  end;

  CBMode.ItemIndex:=1;
  CBModeChange(Self);
  BResetClick(Self);
end;

procedure TAxesIsometric.CBModeChange(Sender: TObject);
begin
  case CBMode.ItemIndex of
    0: Chart1.Axes.Isometric.Mode:=iaNone;
    1: Chart1.Axes.Isometric.Mode:=iaRescale;
    2: Chart1.Axes.Isometric.Mode:=iaFit;
  end;
end;

procedure TAxesIsometric.BResetClick(Sender: TObject);
begin
  Chart1.UndoZoom;
  Chart1.Axes.Bottom.SetMinMax(-60,60);
  Chart1.Axes.Left.SetMinMax(-60,60);
end;

initialization
  RegisterClass(TAxesIsometric);
end.
