unit Unit_LineSeries_Tape;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, TeCanvas, TeeBlocks, TeeChartBlock, ExtCtrls,
  TeeProcs, TeeMakerControl, TeeChart3D, StdCtrls, TeeComma, TeeExtruded,
  Chart, TeeBlockEditor, TeeMakerEditor, TeeGLEditor, TeeBlockCanvas,
  TeeThemes, TeeThemeEditor, TeeRoundRect;

type
  TForm1 = class(TForm)
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Series1: TLineSeries;
    TeeCommander1: TTeeCommander;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    ButtonColor1: TButtonColor;
    Button2: TButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ScrollBar1: TScrollBar;
    CheckBox5: TCheckBox;
    Series2: TLineSeries;
    Series3: TBarSeries;
    CheckBox6: TCheckBox;
    Series4: TAreaSeries;
    ScrollBar2: TScrollBar;
    Label1: TLabel;
    Chart2: TChartBlock;
    Series5: TPieSeries;
    Ellipsoid2: TEllipsoidBlock;
    ScrollBar3: TScrollBar;
    Label2: TLabel;
    CheckBox7: TCheckBox;
    Label3: TLabel;
    ScrollBar4: TScrollBar;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Image2: TImageBlock;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Chart3D1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure ScrollBar4Change(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
  private
    { Private declarations }
    Tape : TTapeBlock;

    //Extru : TExtrudedBlock;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TVisualAccess=class(TVisualBlock);

procedure TForm1.FormCreate(Sender: TObject);
begin
  Tape:=TTapeBlock.Create(Self);
  TVisualAccess(Tape).IsCloneable:=False;
  Tape.Radius:=20;
  Tape.Tape3D:=6;

  Series1.Depth:=-1;
  Series2.Depth:=-1;
  Series3.Depth:=-1;
  Series4.Depth:=-1;

  Series4.Visuals.Template:=Tape.Clone;
  TTapeBlock(Series4.Visuals.Template).IsArea:=True;
  TTapeBlock(Series4.Visuals.Template).Corners.Enabled:=True;

  ButtonColor1.LinkProperty(Chart3D1.Chart[0],'SeriesColor');

  ScrollBar1.Position:=Chart3D1.Chart.Chart3DPercent;

  Series1.ColorEachPoint:=True;

  Series2.Visuals.Template:=Tape.Clone;

  CheckBox1Click(Self);

  Series2.Visible:=False;
  Series3.Visible:=False;
  Series4.Visible:=False;
  Chart3D1.Chart.Walls.Visible:=False;

  Series1.ColorEachPoint:=False;
//  Series1.Stairs:=True;

  Series1.Delete(9,10);
  //Chart3D1.Render.Wireframe:=True;

  Chart2.KeepLeft:=True;
  Chart2.Location.X:=-1000;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
     Chart3D1.Chart[0].Visuals.Template:=Tape
  else
  begin
    Chart3D1.Chart[0].Visuals.Template:=nil;
    Tape.Parent:=nil;
  end;

  Button1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TBlockEditor.ModalShow(Self,Tape);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

procedure TForm1.Chart3D1Click(Sender: TObject);
begin
  if Chart3D1.CanFocus then
     Chart3D1.SetFocus;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
var tmp : TChartSeries;
begin
  tmp:=Chart3D1.Chart[0];

  if CheckBox2.Checked then
     ChangeSeriesType(tmp,THorizLineSeries)
  else
     ChangeSeriesType(tmp,TLineSeries);

  CheckBox1Click(Self);
  ButtonColor1.LinkProperty(Chart3D1.Chart[0],'SeriesColor');
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if Chart3D1.Chart[0] is TCustomSeries then
     TCustomSeries(Chart3D1.Chart[0]).Stairs:=CheckBox3.Checked;

  CheckBox4.Enabled:=CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  if Chart3D1.Chart[0] is TCustomSeries then
     TCustomSeries(Chart3D1.Chart[0]).InvertedStairs:=CheckBox4.Checked;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Chart3D1.Chart.Chart3DPercent:=ScrollBar1.Position;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  Chart3D1.Render.Wireframe:=CheckBox5.Checked;
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  CheckBox8.Enabled:=CheckBox6.Checked;

  Tape.IsArea:=CheckBox6.Checked;
  TBlockAccess(Tape).DeleteLists;

  if Tape.IsArea then
  begin
    Chart3D1.Chart.Walls.Visible:=False;
  end;
end;

procedure TForm1.ScrollBar2Change(Sender: TObject);
begin
//  Pepe1:=ScrollBar2.Position*Pi/180;
  Label1.Caption:=IntToStr(ScrollBar2.Position);

//  Pepe2:=ScrollBar3.Position*Pi/180;
  Label2.Caption:=IntToStr(ScrollBar3.Position);

  TBlockAccess(Tape).DeleteLists;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
  Tape.Corners.Enabled:=CheckBox7.Checked;
end;

procedure TForm1.ScrollBar4Change(Sender: TObject);
begin
  Tape.Tape3D:=ScrollBar4.Position;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin
  Tape.ConcaveArea:=CheckBox8.Checked;
  TBlockAccess(Tape).DeleteLists;
end;

procedure TForm1.CheckBox9Click(Sender: TObject);
begin
  Tape.AreaRound:=CheckBox9.Checked;

  if Tape.AreaRound then
  begin
    CheckBox7.Checked:=True;
    CheckBox7Click(Self);
  end;
  
  TBlockAccess(Tape).DeleteLists;
end;

end.
