unit Histogram_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, StatChar, TeeProcs, Chart, TeePenDlg;

type
  THistogramForm = class(TBaseForm)
    Series1: THistogramSeries;
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

procedure THistogramForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);
  ButtonPen1.LinkPen(Series1.LinePen);
  ButtonPen2.LinkPen(Series1.LinesPen);
end;

initialization
  RegisterClass(THistogramForm);
end.
