unit ErrorBar_Negative;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, ErrorBar, TeeProcs, Chart, TeeTools;

type
  TErrorBarNegativeForm = class(TBaseForm)
    Series1: TErrorBarSeries;
    ChartTool1: TColorLineTool;
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

procedure TErrorBarNegativeForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.Clear;
  Series1.AddErrorBar(0,-123,23,'',clTeeColor);
  Series1.AddErrorBar(1,432,65,'',clTeeColor);
  Series1.AddErrorBar(2,-88,13,'',clTeeColor);
  Series1.AddErrorBar(3,222,44,'',clTeeColor);
  Series1.AddErrorBar(4,-321,49,'',clTeeColor);
end;

procedure TErrorBarNegativeForm.CheckBox1Click(Sender: TObject);
begin
  Chart1.View3D:=CheckBox1.Checked
end;

initialization
  RegisterClass(TErrorBarNegativeForm);
end.
