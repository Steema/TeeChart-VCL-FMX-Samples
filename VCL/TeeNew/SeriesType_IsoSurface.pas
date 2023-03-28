unit SeriesType_IsoSurface;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeeSurfa, TeeLegendPalette,
  TeePenDlg, TeeTools;

type
  TSeriesTypeIsoSurface = class(TBaseForm)
    bEdit: TButton;
    ChartTool1: TLegendPaletteTool;
    ButtonPen1: TButtonPen;
    ButtonPen2: TButtonPen;
    ChartTool2: TRotateTool;
    procedure FormCreate(Sender: TObject);
    procedure bEditClick(Sender: TObject);
  private
    { Private declarations }
    IsoSurface : TIsoSurfaceSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses
  EditChar;

procedure TSeriesTypeIsoSurface.FormCreate(Sender: TObject);
begin
  inherited;

  IsoSurface:=TIsoSurfaceSeries.Create(self);

  with IsoSurface do
  begin
    ParentChart:=Chart1;

    HideCells:=True;  // Improve visual display of hidden cells when rotating

    FillSampleValues;

    PaletteStyle:=psRainbow;
  end;

  ChartTool1.Series:=IsoSurface;

  Chart1.View3D := True;

  // Setup buttons:
  ButtonPen1.LinkPen(IsoSurface.BandPen);
  ButtonPen2.LinkPen(IsoSurface.SideLines);
end;

procedure TSeriesTypeIsoSurface.bEditClick(Sender: TObject);
begin
  EditSeries(Self, IsoSurface);
end;

initialization
  RegisterClass(TSeriesTypeIsoSurface);
end.
