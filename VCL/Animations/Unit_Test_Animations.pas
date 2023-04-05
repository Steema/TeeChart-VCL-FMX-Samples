unit Unit_Test_Animations;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeEngine, TeeAnimations, Series, TeeProcs,
  Chart, ComCtrls, ExtCtrls, TeeComma, TeeAnimate, TeeTools, TeCanvas,
  
  // Using TreeAnimate purpose is to test anim items from TeeTree, to verify
  // they are not appearing in TChart animations editor:
  TreeAnimate, BubbleCh;

type
  TAnimationTests = class(TForm)
    TeeCommander1: TTeeCommander;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Chart1: TChart;
    Series1: TAreaSeries;
    Series2: TAreaSeries;
    Series3: TAreaSeries;
    ChartAnimation1: TSeriesAnimationTool;
    Button1: TButton;
    CheckBox1: TCheckBox;
    ChartTool1: TRepaintMonitor;
    TabSheet2: TTabSheet;
    Chart2: TChart;
    ChartTool2: TRepaintMonitor;
    ChartAnimation2: TTeeAnimationTool;
    Series4: TPieSeries;
    CheckBox2: TCheckBox;
    TabSheet3: TTabSheet;
    Chart3: TChart;
    Series5: TPointSeries;
    ChartAnimation3: TTeeAnimationTool;
    ChartTool3: TRepaintMonitor;
    TabSheet4: TTabSheet;
    Chart4: TChart;
    Series6: TBubbleSeries;
    ChartAnimation4: TTeeAnimationTool;
    ChartTool4: TRepaintMonitor;
    CheckBox3: TCheckBox;
    ComboFlat1: TComboFlat;
    TabSheet5: TTabSheet;
    Chart5: TChart;
    Series7: TBarSeries;
    ChartAnimation5: TTeeAnimationTool;
    Button2: TButton;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Chart6: TChart;
    Series8: TLineSeries;
    Series9: TLineSeries;
    Series10: TLineSeries;
    ChartAnimation6: TTeeAnimationTool;
    Chart7: TChart;
    Series11: TBarSeries;
    Series12: TBarSeries;
    Series13: TBarSeries;
    ChartAnimation7: TTeeAnimationTool;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboFlat1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    function Current:TTeeAnimationTool;
    procedure PlayBarTransition(const ATransition:TSeriesTransitionAnimation);
  public
    { Public declarations }
  end;

var
  AnimationTests : TAnimationTests;

implementation

{$R *.dfm}

uses
  Unit_4_Charts, TeeGDIPlus;

function TAnimationTests.Current:TTeeAnimationTool;
begin
  case PageControl1.ActivePageIndex of
    0: result:=ChartAnimation1;
    1: result:=ChartAnimation2;
    2: result:=ChartAnimation3;
    3: result:=ChartAnimation4;
    4: result:=ChartAnimation5;
    5: result:=ChartAnimation6;
  else
    result:=ChartAnimation7;
  end;
end;

procedure TAnimationTests.Button1Click(Sender: TObject);
begin
  // Bar Transitions animation is special, it needs more code than just
  // calling "Play":
  if Current=ChartAnimation7 then
     PlayBarTransition(ChartAnimation7.Animations[0] as TSeriesTransitionAnimation)
  else
     Current.Play;
end;

procedure TAnimationTests.PlayBarTransition(const ATransition:TSeriesTransitionAnimation);
begin
  // "Before" and "After" calls are mandatory
  ATransition.Before;

  // Here we just change the MultiBar property as one example.
  // The animation will also work if we change any other property in the Series
  // that makes the Bar point "rectangle" bounds to be different than the original ones.
  if Series11.MultiBar=mbSelfStack then
     Series11.MultiBar:=mbNone
  else
     Series11.MultiBar:=TMultiBar(Ord(Series11.MultiBar)+1);

  ATransition.After;
end;

procedure TAnimationTests.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;

  Chart1.SeriesList.FillSampleValues();
  Chart6.SeriesList.FillSampleValues();

  // Bar Transitions:
  Chart7.SeriesList.FillSampleValues();

  Series11.Marks.Hide;
  Series12.Marks.Hide;
  Series13.Marks.Hide;

  ChartAnimation7.Animations[0].Duration:=500;
end;

procedure TAnimationTests.CheckBox1Click(Sender: TObject);
begin
  Current.Animate.Loop:=CheckBox1.Checked;
end;

procedure TAnimationTests.PageControl1Change(Sender: TObject);
begin
  TeeCommander1.Panel:=Current.ParentChart;

  CheckBox1.Checked:=Current.Animate.Loop;
  CheckBox3.Checked:=Current.Animations[0].TwoWay;

  ComboFlat1.ItemIndex:=ComboFlat1.Items.IndexOf(FloatToStr(Current.Animate.SpeedFactor)+'x');
end;

procedure TAnimationTests.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    Chart1.Canvas:=TGDIPlusCanvas.Create;
    Chart2.Canvas:=TGDIPlusCanvas.Create;
    Chart3.Canvas:=TGDIPlusCanvas.Create;
    Chart4.Canvas:=TGDIPlusCanvas.Create;
    Chart5.Canvas:=TGDIPlusCanvas.Create;
  end
  else
  begin
    Chart1.Canvas:=TTeeCanvas3D.Create;
    Chart2.Canvas:=TTeeCanvas3D.Create;
    Chart3.Canvas:=TTeeCanvas3D.Create;
    Chart4.Canvas:=TTeeCanvas3D.Create;
    Chart5.Canvas:=TTeeCanvas3D.Create;
  end;
end;

procedure TAnimationTests.CheckBox3Click(Sender: TObject);
begin
  Current.Animations[0].TwoWay:=CheckBox3.Checked;
end;

procedure TAnimationTests.ComboFlat1Change(Sender: TObject);
var s : String;
begin
  s:=ComboFlat1.Items[ComboFlat1.ItemIndex];
  Delete(s,Length(s),1);
  Current.Animate.SpeedFactor:=StrToFloat(s);
end;

procedure TAnimationTests.Button2Click(Sender: TObject);
begin
  with TFormFourCharts.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
