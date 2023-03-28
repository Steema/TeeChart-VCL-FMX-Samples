unit Tool_ImagePen;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg, 

  jpeg,

  TeeTools, GanttCh;

type
  TToolImagePen = class(TBaseForm)
    bPen: TButtonPen;
    Series1: TGanttSeries;
    ChartTool1: TChartImageTool;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TToolImagePen.FormCreate(Sender: TObject);
begin
  inherited;

  with ChartTool1.Pen do
  begin
    Visible:=True;
    Width:=6;
    Color:=clRed;
  end;

  bPen.LinkPen(ChartTool1.Pen);
end;

initialization
  RegisterClass(TToolImagePen);
end.
