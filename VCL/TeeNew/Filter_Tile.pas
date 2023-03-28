unit Filter_Tile;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeAntiAlias, TeeFilters;

type
  TFilterTile = class(TBaseForm)
    Label1: TLabel;
    Label2: TLabel;
    EditColumns: TEdit;
    EditRows: TEdit;
    UDRows: TUpDown;
    UDColumns: TUpDown;
    Series1: TPieSeries;
    ChartTool1: TAntiAliasTool;
    cbActive: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbActiveClick(Sender: TObject);
    procedure EditColumnsChange(Sender: TObject);
    procedure EditRowsChange(Sender: TObject);
  private
    { Private declarations }
    TileFilter: TTileFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterTile.FormCreate(Sender: TObject);
begin
  inherited;

  TileFilter := TTileFilter.Create(ChartTool1.Filters);

  with TileFilter do
  begin
    NumCols:=UDColumns.Position;
    NumRows:=UDRows.Position;
  end;
end;

procedure TFilterTile.cbActiveClick(Sender: TObject);
begin
  TileFilter.Enabled:=cbActive.Checked;
  Chart1.Invalidate;
end;

procedure TFilterTile.EditColumnsChange(Sender: TObject);
begin
  TileFilter.NumCols:=UDColumns.Position;
  Chart1.Invalidate;
end;

procedure TFilterTile.EditRowsChange(Sender: TObject);
begin
  TileFilter.NumRows:=UDRows.Position;
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TFilterTile);
end.
