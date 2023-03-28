unit Filter_Reverse;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeFilters,
  TeeAntiAlias;

type
  TFilterReverse = class(TBaseForm)
    Series1: THorizBarSeries;
    cbReverse: TCheckBox;
    ChartTool1: TAntiAliasTool;
    procedure FormCreate(Sender: TObject);
    procedure cbReverseClick(Sender: TObject);
  private
    { Private declarations }
    ReverseFilter: TReverseFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterReverse.FormCreate(Sender: TObject);
begin
  inherited;

  ReverseFilter := TReverseFilter.Create(ChartTool1.Filters);
end;

procedure TFilterReverse.cbReverseClick(Sender: TObject);
begin
  ChartTool1.Active:=cbReverse.Checked;
end;

initialization
  RegisterClass(TFilterReverse);
end.
