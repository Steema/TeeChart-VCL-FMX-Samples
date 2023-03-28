unit Buttonpen_Editor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TButtonpenEditor = class(TBaseForm)
    ButtonPen1: TButtonPen;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TButtonpenEditor.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.Walls.Back.Pen.Color:=clRed;
  Chart1.Walls.Back.Pen.Width:=4;

  ButtonPen1.LinkPen(Chart1.Walls.Back.Pen);
end;

initialization
  RegisterClass(TButtonpenEditor);
end.
