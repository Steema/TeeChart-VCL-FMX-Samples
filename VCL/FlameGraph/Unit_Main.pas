unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,

  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart,

  TeeFlameSeries;

type
  TFormFlame = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    CBLegend: TCheckBox;
    LabelMouse: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CBLegendClick(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }

    Flame1 : TFlameSeries;
  public
    { Public declarations }
  end;

var
  FormFlame: TFormFlame;

implementation

{$R *.dfm}

uses
  Unit_Pro;

procedure TFormFlame.CBLegendClick(Sender: TObject);
begin
  Chart1.Legend.Visible:=CBLegend.Checked;
end;

procedure TFormFlame.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var tmp : Integer;
begin
  tmp:=Flame1.Clicked(X,Y);

  if tmp=TeeNoPointClicked then
     LabelMouse.Caption:=''
  else
     LabelMouse.Caption:=Flame1.XLabel[tmp];
end;

procedure TFormFlame.FormCreate(Sender: TObject);
begin
  Flame1:=TFlameSeries.Create(Self);
  Flame1.ParentChart:=Chart1;

  Flame1.FillSampleValues;

  Flame1.Brush.Gradient.StartColor:=clYellow;

  CBLegendClick(Self);

  Chart1.Axes.Bottom.Texts.Style:=talValue;

  TryAdding_ChartEditors(Chart1,Self);
end;

end.
