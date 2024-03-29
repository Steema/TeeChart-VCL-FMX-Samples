unit Filter_Flip;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeAntiAlias,
  TeeFilters, TeeCalendar;

type
  TFilterFlip = class(TBaseForm)
    cbFlip: TCheckBox;
    Series1: TCalendarSeries;
    ChartTool1: TAntiAliasTool;
    procedure FormCreate(Sender: TObject);
    procedure cbFlipClick(Sender: TObject);
  private
    { Private declarations }
    FlipFilter: TFlipFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterFlip.FormCreate(Sender: TObject);
begin
  inherited;

  FlipFilter := TFlipFilter.Create(ChartTool1.Filters);
end;

procedure TFilterFlip.cbFlipClick(Sender: TObject);
begin
  FlipFilter.Enabled:=cbFlip.Checked;
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TFilterFlip);
end.

