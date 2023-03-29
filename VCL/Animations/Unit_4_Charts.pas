unit Unit_4_Charts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Series, TeEngine, TeeProcs, Chart, ExtCtrls,
  StdCtrls, TeeAnimate, TeeAnimations, TeeTools, TeCanvas, TeeEditAnimations,
  TeePenDlg, EditChar;

type
  TFormFourCharts = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Chart1: TChart;
    Chart2: TChart;
    Chart3: TChart;
    Chart4: TChart;
    Series1: TLineSeries;
    Series2: TBarSeries;
    Series3: TAreaSeries;
    Series4: TPieSeries;
    Panel3: TPanel;
    ChartTool1: TRepaintMonitor;
    ChartTool2: TRepaintMonitor;
    ChartTool3: TRepaintMonitor;
    ChartTool4: TRepaintMonitor;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Button1: TButton;
    CheckBox1: TCheckBox;
    ChartAnimation1: TTeeAnimationTool;
    ChartAnimation2: TTeeAnimationTool;
    ChartAnimation3: TTeeAnimationTool;
    ChartAnimation4: TTeeAnimationTool;
    Button2: TButton;
    CheckBox4: TCheckBox;
    ChartAnimation5: TSeriesAnimationTool;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Chart1DblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Chart1Click(Sender: TObject);
  private
    { Private declarations }
    IAnims : TFormTeeAnimations;

    procedure RepaintChart(Index:Integer);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormFourCharts.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues;
  Series2.FillSampleValues;
  Series3.FillSampleValues;
  Series4.FillSampleValues;
end;

procedure TFormFourCharts.CheckBox2Click(Sender: TObject);
begin
  Chart1.View3D:=CheckBox2.Checked;
  Chart2.View3D:=CheckBox2.Checked;
  Chart3.View3D:=CheckBox2.Checked;
  Chart4.View3D:=CheckBox2.Checked;
end;

procedure TFormFourCharts.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
  begin
    Chart1.Canvas:=TGDIPlusCanvas.Create;
    Chart2.Canvas:=TGDIPlusCanvas.Create;
    Chart3.Canvas:=TGDIPlusCanvas.Create;
    Chart4.Canvas:=TGDIPlusCanvas.Create;
  end
  else
  begin
    Chart1.Canvas:=TTeeCanvas3D.Create;
    Chart2.Canvas:=TTeeCanvas3D.Create;
    Chart3.Canvas:=TTeeCanvas3D.Create;
    Chart4.Canvas:=TTeeCanvas3D.Create;
  end;

  CheckBox1.Enabled:=CheckBox3.Checked;
end;

procedure TFormFourCharts.RepaintChart(Index:Integer);
var tmp : TCustomChart;
begin
  case Index of
    0: tmp:=Chart1;
    1: tmp:=Chart2;
    2: tmp:=Chart3;
  else
    tmp:=Chart4;
  end;

  //tmp.Canvas.ReferenceCanvas.Lock;
  try
    //TSeriesTransformAnimation(tmp.Animations[0]).Animate:=nil;
    //TSeriesTransformAnimation(tmp.Animations[0]).Play(False);

    tmp.Draw;
  finally
    //tmp.Canvas.ReferenceCanvas.Unlock;
  end;
end;

procedure TFormFourCharts.Button1Click(Sender: TObject);
begin
  repeat
    TTeeCPU.ParallelFor(0,3,2,RepaintChart);
    //CheckSynchronize(0);
    //Application.ProcessMessages;
  until Application.Terminated;
end;

procedure TFormFourCharts.CheckBox1Click(Sender: TObject);
begin
  TGDIPlusCanvas(Chart1.Canvas).AntiAlias:=CheckBox1.Checked;
  TGDIPlusCanvas(Chart2.Canvas).AntiAlias:=CheckBox1.Checked;
  TGDIPlusCanvas(Chart3.Canvas).AntiAlias:=CheckBox1.Checked;
  TGDIPlusCanvas(Chart4.Canvas).AntiAlias:=CheckBox1.Checked;
end;

procedure TFormFourCharts.Chart1DblClick(Sender: TObject);
begin
  EditChart(Self,TCustomChart(Sender));
end;

procedure TFormFourCharts.FormShow(Sender: TObject);
begin
  IAnims:=TFormTeeAnimations.Create(Self);
  IAnims.Align:=alClient;
  TTeeVCL.AddFormTo(IAnims,Panel4,Chart1.Animations);
end;

procedure TFormFourCharts.Button2Click(Sender: TObject);
begin
  ChartAnimation1.Loop:=CheckBox4.Checked;
  ChartAnimation2.Loop:=CheckBox4.Checked;
  ChartAnimation3.Loop:=CheckBox4.Checked;
  ChartAnimation4.Loop:=CheckBox4.Checked;

  if not ChartAnimation1.Running then
     ChartAnimation1.Play;

  if not ChartAnimation2.Running then
     ChartAnimation2.Play;

  if not ChartAnimation3.Running then
     ChartAnimation3.Play;

  if not ChartAnimation4.Running then
     ChartAnimation4.Play;
end;

procedure TFormFourCharts.Chart1Click(Sender: TObject);
begin
  IAnims.RefreshAnimations(TCustomChart(Sender).Animations);
end;

end.
