unit ChartGrid_NullPoints;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Grids,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, Area_TreatNulls,
  TeeChartGrid;

type
  TChartGridNullPoints = class(TAreaTreatNulls)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  FastLine_TreatNulls;

initialization
  RegisterClass(TChartGridNullPoints);
end.
