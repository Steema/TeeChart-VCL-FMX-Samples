unit Axis_MinorGrid;
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
  TAxisMinorGrid = class(TBaseForm)
    Series1: TAreaSeries;
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

procedure TAxisMinorGrid.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(4);

  Chart1.BottomAxis.MinorGrid.Visible := True;
  Chart1.BottomAxis.MinorTickCount := 3;
end;

procedure TAxisMinorGrid.CheckBox1Click(Sender: TObject);
begin
  Chart1.BottomAxis.MinorGrid.Visible := CheckBox1.Checked;
end;

initialization
  RegisterClass(TAxisMinorGrid);
end.
