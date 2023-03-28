unit Point3D_BaseLine;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Base, TeEngine, TeeSurfa, TeePoin3, TeCanvas, TeePenDlg, TeeProcs, Chart;

type
  TPoint3DBaseLine = class(TBaseForm)
    ButtonPen1: TButtonPen;
    Series1: TPoint3DSeries;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPoint3DBaseLine.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
  Series1.BaseLine.Visible:=True;
  Series1.BaseLine.Color:=clNavy;

  ButtonPen1.LinkPen(Series1.BaseLine);
end;

initialization
  RegisterClass(TPoint3DBaseLine);
end.
