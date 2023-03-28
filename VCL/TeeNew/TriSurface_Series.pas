unit TriSurface_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, TeeSurfa, TeeTriSurface, TeeProcs,
  Chart, TeePenDlg;

type
  TTriSurfaceForm = class(TBaseForm)
    Series1: TTriSurfaceSeries;
    ButtonPen1: TButtonPen;
    ButtonPen2: TButtonPen;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTriSurfaceForm.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.FillSampleValues(30);
  ButtonPen1.LinkPen(Series1.Border);
  ButtonPen2.LinkPen(Series1.Pen);
end;

initialization
  RegisterClass(TTriSurfaceForm);
end.
