unit Tool_Annotation;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools;

type
  TAnnotationToolForm = class(TBaseForm)
    Series1: TFastLineSeries;
    Button1: TButton;
    ChartTool1: TAnnotationTool;
    ChartTool2: TAnnotationTool;
    ChartTool3: TAnnotationTool;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses EditChar;

procedure TAnnotationToolForm.Button1Click(Sender: TObject);
begin
  EditChartTool(Self,ChartTool1);
end;

procedure TAnnotationToolForm.CheckBox1Click(Sender: TObject);
begin
  ChartTool1.Active:=CheckBox1.Checked;
  ChartTool2.Active:=CheckBox1.Checked;
  ChartTool3.Active:=CheckBox1.Checked;
end;

procedure TAnnotationToolForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(30);
end;

initialization
  RegisterClass(TAnnotationToolForm);
end.
