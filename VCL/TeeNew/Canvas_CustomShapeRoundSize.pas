unit Canvas_CustomShapeRoundSize;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, BubbleCh;

type
  TCanvasCustomShapeRoundSize = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Series1: TBubbleSeries;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCanvasCustomShapeRoundSize.FormCreate(Sender: TObject);
begin
  inherited;

  with Series1.Marks do
  begin
    Transparent:=False;
    Pen.Visible:=True;
    ShapeStyle:=fosRoundRectangle;
    RoundSize:=UpDown1.Position;
  end;
end;

procedure TCanvasCustomShapeRoundSize.Edit1Change(Sender: TObject);
begin
  Series1.Marks.RoundSize:=UpDown1.Position;
end;

initialization
  RegisterClass(TCanvasCustomShapeRoundSize);
end.
