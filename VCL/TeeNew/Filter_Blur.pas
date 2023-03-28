unit Filter_Blur;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, GanttCh, 

  jpeg,

  TeeFilters;

type
  TFilterBlur = class(TBaseForm)
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    Label2: TLabel;
    Label3: TLabel;
    ScrollBar2: TScrollBar;
    Label4: TLabel;
    Series1: TGanttSeries;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
  private
    { Private declarations }
    BlurFilter: TBlurFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterBlur.FormCreate(Sender: TObject);
begin
  inherited;

  BlurFilter:=TBlurFilter.Create(Chart1.BackImage.Filters);

  with BlurFilter do
  begin
    Amount:=10;
    Steps:=10;
    ScrollBar1.Position:=Amount;
    ScrollBar2.Position:=Steps;
  end;
end;

procedure TFilterBlur.ScrollBar1Change(Sender: TObject);
begin
  BlurFilter.Amount:=ScrollBar1.Position;
  Label2.Caption:=IntToStr(ScrollBar1.Position);
  Chart1.Invalidate;
end;

procedure TFilterBlur.ScrollBar2Change(Sender: TObject);
begin
  BlurFilter.Steps:=ScrollBar2.Position;
  Label4.Caption:=IntToStr(ScrollBar2.Position);
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TFilterBlur);
end.
