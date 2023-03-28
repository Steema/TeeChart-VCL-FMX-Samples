unit Tool_RotateOutline;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeEngine, TeeTools, Series, TeCanvas, TeePenDlg, TeeProcs, Chart;

type
  TRotateOutline = class(TBaseForm)
    ButtonPen1: TButtonPen;
    CheckBox1: TCheckBox;
    Series1: THorizAreaSeries;
    ChartTool1: TRotateTool;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TRotateOutline.CheckBox1Click(Sender: TObject);
begin
  // Show or Hide Rotate outline
  ChartTool1.Pen.Visible:=CheckBox1.Checked;
end;

procedure TRotateOutline.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  // Enable Rotate tool "Outline"
  ChartTool1.Pen.Visible:=True;

  // set-up button
  ButtonPen1.LinkPen(ChartTool1.Pen);
end;

initialization
  RegisterClass(TRotateOutline);
end.
