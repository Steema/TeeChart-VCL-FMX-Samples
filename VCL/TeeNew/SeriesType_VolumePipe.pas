unit SeriesType_VolumePipe;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QComCtrls,
  QButtons,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  {$ENDIF}
  Base, TeEngine, Series, TeeProcs, Chart, TeeVolumePipe, TeeGDIPlus;

type
  TSeriesTypeVolumePipe = class(TBaseForm)
    bEdit: TButton;
    Series1: TVolumePipeSeries;
    procedure bEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$IFNDEF CLX}
{$R *.dfm}
{$ELSE}
{$R *.xfm}
{$ENDIF}

Uses
  EditChar;

procedure TSeriesTypeVolumePipe.bEditClick(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TSeriesTypeVolumePipe.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.Gradient.Visible := False;
end;

initialization
  RegisterClass(TSeriesTypeVolumePipe);
end.
