unit Canvas_PyramidTrunc;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeComma, TeeProcs, TeEngine, Chart, TeCanvas;

type
  TTruncPyramidForm = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure Chart1AfterDraw(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTruncPyramidForm.Chart1AfterDraw(Sender: TObject);
var R : TRectF;
begin
  R.Left:=Chart1.ChartXCenter;
  R.Top:=Chart1.ChartYCenter;
  R.Right:=R.Left+60;
  R.Bottom:=R.Top+70;

  With Chart1.Canvas do
  begin
    Brush.Color:=clMoneyGreen;
    PyramidTrunc(R,0,50,ScrollBar1.Position,ScrollBar2.Position);
  end;
end;

procedure TTruncPyramidForm.ScrollBar1Change(Sender: TObject);
begin
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TTruncPyramidForm);
end.
