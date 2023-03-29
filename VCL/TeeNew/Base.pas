unit Base;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  TeEngine, Series, TeeProcs, Chart;

type
  TBaseForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    BaseSplitter1: TSplitter;
    Chart1: TChart;
    procedure Memo1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TNewCanvasStyle=(ncGDI, ncOpenGL, ncGDIPlus);

var
  TeeNewCanvas : TNewCanvasStyle = ncGDIPlus;

implementation

{$R *.DFM}

Uses
  {$IFNDEF TEELITE}
  TeeOpenGL,
  TeeAntiAlias,
  {$ENDIF}
  TeCanvas, TeeGDIPlus, TeeGLCanvas, EditChar;

// Trick:
// Double-clicking any Memo on any form shows the chart editor
procedure TBaseForm.Memo1DblClick(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

procedure TBaseForm.FormCreate(Sender: TObject);
{$IFNDEF TEELITE}
var GL : TTeeOpenGL;
{$ENDIF}
begin
  case TeeNewCanvas of
  {$IFNDEF TEELITE}
    ncOpenGL    : if not (Chart1.Canvas is TGLCanvas) then
                  begin
                    GL:=TTeeOpenGL.Create(Self);
                    GL.TeePanel:=Chart1;
                    GL.Active:=True;

                    // Force zoom
                    Chart1.View3DOptions.Zoom:=40;
                    Chart1.View3DOptions.Perspective:=50;
                  end;
  {$ENDIF}

    ncGDIPlus   : if not (Chart1.Canvas is TGLCanvas) then // <-- For demos with default OpenGL Canvas, do not change it !
                     if not (Chart1.Canvas is TGDIPlusCanvas) then // <-- Do not assign a GDI+ canvas if it already is !
                         Chart1.Canvas:=TGDIPlusCanvas.Create;

    ncGDI       :  if not (Chart1.Canvas is TTeeCanvas3D) then
                      Chart1.Canvas:=TTeeCanvas3D.Create;
  else
    if not (Chart1.Canvas is TGDIPlusCanvas) then
       Chart1.Canvas:=TGDIPlusCanvas.Create;
  end;
end;

end.
