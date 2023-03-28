unit Axis_Behind;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series;

type
  TAxisBehind = class(TBaseForm)
    CheckBox1: TCheckBox;
    Series1: TBarSeries;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAxisBehind.CheckBox1Click(Sender: TObject);
begin
  Chart1.Axes.Behind:=CheckBox1.Checked;
end;

procedure TAxisBehind.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);
  Chart1.LeftAxis.Grid.Color:=clWhite;
  Chart1.BottomAxis.Grid.Color:=clBlue;
end;

initialization
  RegisterClass(TAxisBehind);
end.
