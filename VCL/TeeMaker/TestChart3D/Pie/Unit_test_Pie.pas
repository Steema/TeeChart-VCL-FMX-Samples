unit Unit_test_Pie;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeCanvas, TeeBlocks, TeeChartBlock, ExtCtrls, TeeProcs,
  TeeMakerEditor, TeeChart3D, TeEngine, Chart, Series, TeeClipBlock,
  TeeGLCanvas, TeeBlockCanvas, TeeBlockEditor, TeeAnimate, TeeThemes,
  StdCtrls, TeeEdiFont, TeeNumberanimation,
  TeeAnimateEditor, TeeEdit, ComCtrls, TeeMakerControl;

type
  TForm1 = class(TForm)
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    ScrollBar1: TScrollBar;
    Series1: TPieSeries;
    LabelFPS: TLabel;
    StatusBar1: TStatusBar;
    ChartEditor1: TChartEditor;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Chart3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Chart3D1Animates0Stop(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    Animate : TAnimateItem;

    procedure CreateAnimation0;
    procedure CreateAnimation1;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeeBlockAnimations;
  
procedure TForm1.FormCreate(Sender: TObject);
var Pie : TPieSeries;
    t   : Integer;
begin
 // ChartBlock1.TextAsPictures:=True;

  Pie:=TPieSeries.Create(Self);
  Pie.Circled:=True;
  Pie.Shadow.Visible:=False;

  Chart3D1.Chart.AddSeries(Pie).FillSampleValues();

  //ApplyChartTheme(TFactsTheme,Chart3D1.Chart,13);

  Chart3D1.Chart.ColorPaletteIndex:=13;

  Chart3D1.Options.Floor.Reflection:=60;
  Chart3D1.Options.Floor.Format.Texture.Picture:=nil;

  Chart3D1.Options.DrawShadows:=True;
  Chart3D1.Blocks.Shadows.Transparency:=5;
  Chart3D1.Blocks.Shadows.Color:=RGB(200,200,200);

  Chart1[0].DataSource:=Pie;

  Animate:=Chart3D1.Animates[0];

  LabelFPS.Caption:=IntToStr(Animate.Speed);

  ChartBlock1.CreateItems;

  CreateAnimation0;
  CreateAnimation1;

  ComboBox1.Clear;
  for t:=0 to Chart3D1.Animates.Count-1 do
      ComboBox1.Items.Add(Chart3D1.Animates[t].Description);

  ComboBox1.ItemIndex:=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Chart3D1.Render.Antialias:=CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Chart3D1.Blocks.HideBorders:=not CheckBox2.Checked;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ChartEditor1.Execute;
  Animate.Animate.Stop;
  Animate.Animations.Clear;
end;

procedure TForm1.CreateAnimation0;
var tmpDuration : Integer;
    tmpOverlap  : Integer;
    tmp         : Integer;
begin
  Animate.Animations.Clear;

  tmpDuration:=50;
  tmpOverlap:=50;

  tmp:=(tmpDuration-(tmpDuration-tmpOverlap))*Series1.Count;

  with TFadeBlocksAnimation.Create(Self) do
  begin
    Name:='FadeLegendRect';
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Legend.Rectangle';

    Duration:=tmp;
    Prepare;
  end;

  with TFadeBlocksAnimation.Create(Self) do
  begin
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Legend.Shadow';
    Duration:=tmp;
    Prepare;
  end;

  with TFadeBlocksAnimation.Create(Self) do
  begin
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Overlap:=tmpOverlap;
    Duration:=tmpDuration;
    Prepare;
  end;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Number.Duration:=tmpDuration;
    Overlap:=tmpOverlap;
    Number.PropertyName:='Scale.Value';
    Number.StartValue:=2;
    Number.EndValue:=1;
    Prepare;
  end;

  with TFadeBlocksAnimation.Create(Self) do
  begin
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Legend.Items';
    Overlap:=tmpOverlap;
    Duration:=tmpDuration;
    Prepare;
  end;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Self.Animate.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Legend.Items';
    Overlap:=tmpOverlap;
    Number.Duration:=tmpDuration;
    Number.PropertyName:='Rotation.X';
    Number.StartValue:=200;
    Number.EndValue:=360;
    Prepare;
  end;
end;

procedure TForm1.CreateAnimation1;
var tmp : TAnimateItem;
begin
  tmp:=Chart3D1.Animates.Add;
  tmp.OnStop:=Chart3D1Animates0Stop;
  tmp.Description:='Far Away';

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=tmp.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series';

    with Number do
    begin
      Duration:=100;
      PropertyName:='Location.Y';
      Curve.Y[0]:=1;
      Curve.Y[1]:=-1;
      StartValue:=4000;
      UseEndValue:=False;
    end;

    Prepare;
  end;


  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=tmp.Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Overlap:=0;

    Number.Duration:=100;
    Number.PropertyName:='Rotation.Z';
    Number.StartValue:=0;
    Number.EndValue:=360;

    Prepare;
  end;
  
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Animate.AnimationCount=0 then
  begin
    CreateAnimation0;
    CreateAnimation1;
  end;

  Animate.Animate.Speed:=ScrollBar1.Position;
  Button3.Enabled:=False;
  Animate.Animate.Play;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Animate.Animate.Speed:=ScrollBar1.Position;
  LabelFPS.Caption:=IntToStr(ScrollBar1.Position);
end;

procedure TForm1.Chart3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var tmp : TCustomBlock;
begin
  if not Chart3D1.Options.Navigate.Dragging then
  begin
    tmp:=Chart3D1.Blocks.ClickedBlock(X,Y,True);

    if Assigned(tmp) then
       StatusBar1.SimpleText:=BlockTitlePath(tmp)
    else
       StatusBar1.SimpleText:='';
  end;
end;

procedure TForm1.Chart3D1Animates0Stop(Sender: TObject);
begin
  if not Animate.Loop then
     Button3.Enabled:=True;
end;

type
  TPanelAccess=class(TCustomTeePanel);

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
     TPanelAccess(Chart3D1).DebugPaint:=TTeeDebugPaint.Create
  else
     FreeAndNil(TPanelAccess(Chart3D1).DebugPaint);
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  Animate.Loop:=CheckBox4.Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then
     Chart3D1.Options.Floor.Reflection:=60
  else
     Chart3D1.Options.Floor.Reflection:=0;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  Chart3D1.Options.DrawShadows:=CheckBox5.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Animate:=Chart3D1.Animates[ComboBox1.ItemIndex];
end;

end.
