unit Filter_Mirror;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeFilters, TeeAntiAlias,
  TeePenDlg;

type
  TFilterMirror = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    ChartTool1: TAntiAliasTool;
    Label1: TLabel;
    cbDirection: TComboFlat;
    procedure FormCreate(Sender: TObject);
    procedure cbDirectionChange(Sender: TObject);
  private
    { Private declarations }
    MirrorFilter: TMirrorFilter;    
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterMirror.FormCreate(Sender: TObject);
begin
  inherited;

  MirrorFilter := TMirrorFilter.Create(ChartTool1.Filters);
  MirrorFilter.Direction:=mdLeft;
  cbDirection.ItemIndex := 3;
end;

procedure TFilterMirror.cbDirectionChange(Sender: TObject);
begin
  MirrorFilter.Direction:=TMirrorDirection(cbDirection.ItemIndex);
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TFilterMirror);
end.
