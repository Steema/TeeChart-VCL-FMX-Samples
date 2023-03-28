unit Filter_Rotate;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeAntiAlias,
  TeeFilters, TeeDonut, TeePenDlg;

type
  TFilterRotate = class(TBaseForm)
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    Label2: TLabel;
    cbAutosize: TCheckBox;
    Series1: TDonutSeries;
    ChartTool1: TAntiAliasTool;
    bBackColor: TButtonColor;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure bBackColorClick(Sender: TObject);
    procedure cbAutosizeClick(Sender: TObject);
  private
    { Private declarations }
    RotateFilter: TRotateFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterRotate.FormCreate(Sender: TObject);
begin
  inherited;

  RotateFilter := TRotateFilter.Create(ChartTool1.Filters);

  with RotateFilter do
  begin
    Angle := 10;
    AutoSize:= True;
    ScrollBar1.Position:=Round(Angle);
    BackColor:=clWhite;
  end;

  bBackColor.LinkProperty(RotateFilter,'BackColor');
end;

procedure TFilterRotate.ScrollBar1Change(Sender: TObject);
begin
  RotateFilter.Angle:=ScrollBar1.Position;
  Label2.Caption:=IntToStr(ScrollBar1.Position);
  Chart1.Invalidate;
end;

procedure TFilterRotate.bBackColorClick(Sender: TObject);
begin
  Chart1.Invalidate;
end;

procedure TFilterRotate.cbAutosizeClick(Sender: TObject);
begin
  RotateFilter.AutoSize:=cbAutosize.Checked;
  Chart1.Invalidate;
end;

initialization
  RegisterClass(TFilterRotate);
end.
