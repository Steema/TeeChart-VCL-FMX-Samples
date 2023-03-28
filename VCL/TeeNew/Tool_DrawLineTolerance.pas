unit Tool_DrawLineTolerance;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeTools;

type
  TDrawLineToleranceForm = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Series1: TFastLineSeries;
    ChartTool1: TDrawLineTool;
    CheckBox1: TCheckBox;
    cbSelect: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure cbSelectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDrawLineToleranceForm.FormCreate(Sender: TObject);
begin
  inherited;
  ChartTool1.Lines.AddLine(Series1.MinXValue,Series1.MinYValue,Series1.MaxXValue,Series1.MaxYValue);
end;

procedure TDrawLineToleranceForm.Edit1Change(Sender: TObject);
begin
  Charttool1.ClickTolerance:=strtoint(edit1.Text);
end;

procedure TDrawLineToleranceForm.CheckBox1Click(Sender: TObject);
begin
  ChartTool1.EnableDraw:=CheckBox1.Checked;
end;

procedure TDrawLineToleranceForm.cbSelectClick(Sender: TObject);
begin
  ChartTool1.SelectNewLines:=cbSelect.Checked;
end;

initialization
  RegisterClass(TDrawLineToleranceForm);
end.
