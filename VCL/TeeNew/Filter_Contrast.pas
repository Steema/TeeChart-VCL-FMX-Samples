unit Filter_Contrast;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeFilters, TeeAntiAlias;

type
  TFilterContrast = class(TBaseForm)
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    Label2: TLabel;
    cbPercent: TCheckBox;
    Series1: TBarSeries;
    ChartTool1: TAntiAliasTool;
    bFilters: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure cbPercentClick(Sender: TObject);
    procedure bFiltersClick(Sender: TObject);
  private
    { Private declarations }
    ContrastFilter: TContrastFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeFiltersEditor;

procedure TFilterContrast.FormCreate(Sender: TObject);
begin
  inherited;

  ContrastFilter := TContrastFilter.Create(ChartTool1.Filters);

  with ContrastFilter do
  begin
    Amount := 10;
    Percent:= False;
    ScrollBar1.Position:=Amount;
  end;
end;

procedure TFilterContrast.ScrollBar1Change(Sender: TObject);
begin
  ContrastFilter.Amount:=ScrollBar1.Position;
  Label2.Caption:=IntToStr(ScrollBar1.Position);
  Chart1.Invalidate;
end;

procedure TFilterContrast.cbPercentClick(Sender: TObject);
begin
  ContrastFilter.Percent:=cbPercent.Checked;
  Chart1.Invalidate;
end;

procedure TFilterContrast.bFiltersClick(Sender: TObject);
begin
  if TFiltersEditor.ShowEditor(Self, ChartTool1.Bitmap, ChartTool1.Filters) then
     ChartTool1.Repaint;
end;

initialization
  RegisterClass(TFilterContrast);
end.

