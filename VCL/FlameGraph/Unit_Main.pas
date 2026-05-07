unit Unit_Main;

interface

uses
  Winapi.Windows, Winapi.Messages,

  // RTL
  System.SysUtils, System.Variants, System.Classes,

  // VCL
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,

  // VCL TeeChart
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeCanvas,
  VCLTee.TeePenDlg,

  // Flame
  TeeFlameSeries;

type
  TFormFlame = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    CBLegend: TCheckBox;
    LabelMouse: TLabel;
    ButtonColor1: TButtonColor;
    ButtonColor2: TButtonColor;
    ButtonPen1: TButtonPen;
    procedure FormCreate(Sender: TObject);
    procedure CBLegendClick(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }

    Flame1 : TFlameSeries;

    procedure AddSampleData;
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

procedure TFormFlame.AddSampleData;
var tmp1, tmp2, tmp3 : Integer;
begin
  tmp1:=Flame1.Add(-1,'TApplication.Run',1290);

    tmp2:=Flame1.Add(tmp1,'TApplication.HandleMessage',870);
           tmp3:=Flame1.Add(tmp2,'TApplication.Idle',423);
            Flame1.Add(tmp3,'TApplication.DoMouseIdle',423);
           Flame1.Add(tmp2,'TApplication.ProcessMessage',447);

    tmp2:=Flame1.Add(tmp1,'TCustomForm.SetVisible',320);
           tmp3:=Flame1.Add(tmp2,'TControl.SetVisible',320);
            Flame1.Add(tmp3,'TControl.Perform',300);
            Flame1.Add(tmp3,'TCustomForm.RequestAlign',20);

    tmp2:=Flame1.Add(tmp1,'TObject.Free',100);
           Flame1.Add(tmp2,'TApplication.Destroy',90);
           Flame1.Add(tmp2,'TCustomForm.Destroy',10);
end;

procedure TFormFlame.FormCreate(Sender: TObject);
begin
  Flame1:=TFlameSeries.Create(Self);
  Flame1.ParentChart:=Chart1;

  AddSampleData;

  //Flame1.FillSampleValues;

  Flame1.Brush.Gradient.StartColor:=clYellow;

  TryAdding_ChartEditors(Chart1,Self);

  // Cosmetic
  CBLegendClick(Self);

  ButtonColor1.LinkProperty(Flame1.Brush.Gradient,'StartColor');
  ButtonColor2.LinkProperty(Flame1.Brush.Gradient,'EndColor');
  ButtonPen1.LinkPen(Flame1.Pen);
end;

end.
