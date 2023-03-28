unit Surface_HideCells;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeSurfa, TeeTools;

type
  TSurfaceHideCells = class(TBaseForm)
    cbHideCells: TCheckBox;
    Series1: TSurfaceSeries;
    ChartTool1: TRotateTool;
    procedure cbHideCellsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSurfaceHideCells.cbHideCellsClick(Sender: TObject);
begin
  Series1.HideCells:=cbHideCells.Checked;
end;

initialization
  RegisterClass(TSurfaceHideCells);
end.
