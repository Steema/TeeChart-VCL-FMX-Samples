unit ColorBand_Drag;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  ColorBand_Clicked, TeEngine, TeeTools, TeeProcs, Chart, TeCanvas,
  TeePenDlg;

type
  TColorBandDrag = class(TColorBandClicked)
    ButtonPen1: TButtonPen;
    ButtonPen2: TButtonPen;
    procedure FormCreate(Sender: TObject);
    procedure ButtonPen1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TColorBandDrag.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.View3D:=False;

  ButtonPen1.LinkPen(ChartTool1.StartLine.Pen);
  ButtonPen2.LinkPen(ChartTool1.EndLine.Pen);

  ChartTool1.OnClick:=nil;
end;

procedure TColorBandDrag.ButtonPen1Click(Sender: TObject);
begin
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TColorBandDrag);
end.
 
