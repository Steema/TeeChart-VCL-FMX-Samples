unit Series_ShowInEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TSeriesShowInEditor = class(TBaseForm)
    Label1: TLabel;
    cbSeries: TComboFlat;
    cbShowInEditor: TCheckBox;
    cbShowInLegend: TCheckBox;
    bEditor: TButton;
    Series1: TBarSeries;
    Series2: TLineSeries;
    Series3: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure cbSeriesChange(Sender: TObject);
    procedure cbShowInEditorClick(Sender: TObject);
    procedure cbShowInLegendClick(Sender: TObject);
    procedure bEditorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  EditChar;

procedure TSeriesShowInEditor.FormCreate(Sender: TObject);
var i : integer;
begin
  inherited;

  for i:=0 to Chart1.SeriesCount-1 do
      cbSeries.Items.Add(Chart1[i].Name);

  cbSeries.ItemIndex:=0;
end;

procedure TSeriesShowInEditor.cbSeriesChange(Sender: TObject);
begin
  cbShowInEditor.Checked:=Chart1[cbSeries.ItemIndex].ShowInEditor;
  cbShowInLegend.Checked:=Chart1[cbSeries.ItemIndex].ShowInLegend;
end;

procedure TSeriesShowInEditor.cbShowInEditorClick(Sender: TObject);
begin
  Chart1[cbSeries.ItemIndex].ShowInEditor:=cbShowInEditor.Checked;
end;

procedure TSeriesShowInEditor.cbShowInLegendClick(Sender: TObject);
begin
  Chart1[cbSeries.ItemIndex].ShowInLegend:=cbShowInLegend.Checked;
end;

procedure TSeriesShowInEditor.bEditorClick(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

initialization
  RegisterClass(TSeriesShowInEditor);
end.
