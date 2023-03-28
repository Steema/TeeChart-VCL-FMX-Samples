unit Tool_RectangleDraggingResizing;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeTools,
  Tool_RectangleAllowDragResize, BubbleCh;

type
  TToolRectangleDraggingResizing = class(TToolRectangleAllowDragResize)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ToolDragging(Sender: TObject);
    procedure ToolDragged(Sender: TObject);
    procedure ToolResizing(Sender: TObject);
    procedure ToolResized(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TToolRectangleDraggingResizing.FormCreate(Sender: TObject);
begin
  inherited;

  with Chart1.Gradient do
  begin
    EndColor:=clRed;
    Visible:=true;
  end;

  with Tool do
  begin
    Shape.Color:=clYellow;
    OnDragging:=ToolDragging;
    OnDragged:=ToolDragged;
    OnResizing:=ToolResizing;
    OnResized:=ToolResized;
    TextAlignment := taCenter;
    Shape.Font.Style:=[fsBold,fsItalic];
    Shape.Font.Size:=15;
  end;

end;

procedure TToolRectangleDraggingResizing.ToolDragging(Sender: TObject);
begin
  Tool.Text:='Dragging...';
end;

procedure TToolRectangleDraggingResizing.ToolDragged(Sender: TObject);
begin
  Tool.Text:='Dragged !';
end;

procedure TToolRectangleDraggingResizing.ToolResizing(Sender: TObject);
begin
  Tool.Text:='Resizing...';
end;

procedure TToolRectangleDraggingResizing.ToolResized(Sender: TObject);
begin
  Tool.Text:='Resized !';
end;

initialization
  RegisterClass(TToolRectangleDraggingResizing);
end.
