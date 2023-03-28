unit ChartEditor_Runtime;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeEdit, TeeProcs, Chart;

type
  TChartEditorRuntime = class(TBaseForm)
    ChartEditor1: TChartEditor;
    Button1: TButton;
    Series1: TLineSeries;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartEditorRuntime.Button1Click(Sender: TObject);
begin
  ChartEditor1.RememberPosition:=CheckBox1.Checked;
  ChartEditor1.HighLightTabs:=CheckBox2.Checked;

  ChartEditor1.Execute;
end;

procedure TChartEditorRuntime.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);
end;

initialization
  RegisterClass(TChartEditorRuntime);
end.
