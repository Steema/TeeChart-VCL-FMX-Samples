unit Axis_DepthDateTime;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeeSurfa, TeeProcs, Chart;

type
  TDepthAxisDateTime = class(TBaseForm)
    Series1: TTowerSeries;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDepthAxisDateTime.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.XValues.DateTime:=True;
  Series1.ZValues.DateTime:=True;
  Series1.FillSampleValues;

  Chart1.Axes.Bottom.DateTimeFormat:='mmm-dd';
  Chart1.Axes.Depth.DateTimeFormat:='mmm-dd';
  Chart1.Axes.DepthTop.Visible:=True;
  Chart1.Axes.DepthTop.DateTimeFormat:='mmm-dd';
end;

procedure TDepthAxisDateTime.CheckBox1Click(Sender: TObject);
begin
  inherited;

  Series1.ZValues.DateTime:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TDepthAxisDateTime);
end.
