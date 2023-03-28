unit Pie_MarksVertCenter;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, Pie_MarksLegSize;

type
  TPieMarksVertCenter = class(TPieMarksLegSize)
    cbVertCenter: TCheckBox;
    procedure cbVertCenterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPieMarksVertCenter.cbVertCenterClick(Sender: TObject);
begin
  Series1.PieMarks.VertCenter:=cbVertCenter.Checked;
end;

procedure TPieMarksVertCenter.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.View3D:=True;

  with Series1 do
  begin
    Pen.Style:=psDot;
    Marks.Arrow.Color:=clblack;
    Marks.Arrow.Width:=2;
    PieMarks.VertCenter:=True;
    PieMarks.LegSize:= ScrollBar1.Position;
  end;
end;

initialization
  RegisterClass(TPieMarksVertCenter);
end.
