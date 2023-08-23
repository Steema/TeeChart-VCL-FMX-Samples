unit Unit_Test;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TeeProcs, TeeMakerControl, TeCanvas, TeeBlocks,
  TeeMakerEditor, TeeExtruded, StdCtrls, TeEngine, TeeDragPoint, Series,
  Chart, TeeGLUT, TeeBlockEditor, ComCtrls, TeeChartBlock;

type
  TForm1 = class(TForm)
    Maker1: TMaker;
    CheckBox2: TCheckBox;
    ScrollBar1: TScrollBar;
    Button1: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    ChartTool1: TDragPointTool;
    ScrollBar2: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Ellipsoid2: TEllipsoidBlock;
    Tape1: TTapeBlock;
    Button3: TButton;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    Label3: TLabel;
    ScrollBar5: TScrollBar;
    Label4: TLabel;
    ScrollBar6: TScrollBar;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Label5: TLabel;
    ScrollBar7: TScrollBar;
    ButtonColor1: TButtonColor;
    CheckBox3: TCheckBox;
    procedure CheckBox2Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ChartTool1DragPoint(Sender: TDragPointTool; Index: Integer);
    procedure ScrollBar2Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
    procedure ScrollBar4Change(Sender: TObject);
    procedure Maker1Click(Sender: TObject);
    procedure ScrollBar5Change(Sender: TObject);
    procedure ScrollBar6Change(Sender: TObject);
    procedure ScrollBar7Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Maker1.Render.WireFrame:=CheckBox2.Checked;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Tape1.Tape3D:=ScrollBar1.Position;
end;

procedure TForm1.FormCreate(Sender: TObject);
var t : Integer;
begin
  Maker1.Blocks.LoadFromFile('tapeblock2.hou');

  //Tape1.Format.Texture.PictureLink:='$(TEEMaker)\Rocks.bmp';

  Chart1.Axes.Left.SetMinMax(-200,200);
  Chart1.Axes.Bottom.SetMinMax(-200,200);

  ButtonColor1.LinkProperty(Tape1.Format,'Color');

  Tape1.Tape3D:=6;
  Tape1.Radius:=20;
  Tape1.Format.Color:=clRed;

//  Tape1.Format.VisibleInterior:=True;

  Tape1.Size.Y:=20;

  Maker1.Options.BoundingBox:=True;
  Maker1.Selected:=Tape1;

//  Series1.Clear;
//  Series1.AddXY(-100,-100);
//  Series1.AddXY(100,100);
//  Series1.AddXY(100,-100);

  Button3Click(Self);

  ChartTool1DragPoint(ChartTool1,0);

  Series1.Pointer.Visible:=True;
  Chart1.View3D:=False;
  Series1.Cursor:=crHandPoint;
  ChartTool1.Series:=Series1;

  Maker1.Options.Floor.Limit:=lfNo;

  ScrollBar1.Position:=Round(Tape1.Tape3D);
  ScrollBar2.Position:=Round(Tape1.Size.Y);
  ScrollBar5.Position:=Round(Tape1.Slices);
  ScrollBar6.Position:=Round(Tape1.Slices3D);
  ScrollBar7.Position:=Round(Tape1.Radius);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Maker1);
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TForm1.ChartTool1DragPoint(Sender: TDragPointTool; Index: Integer);
var t : Integer;
begin
  Tape1.Points.Clear;

  for t:=0 to Series1.Count-1 do
      Tape1.Points.Add(Series1.XValues[t],Series1.YValues[t]);

  TBlockAccess(Tape1).DeleteLists;
end;

procedure TForm1.ScrollBar2Change(Sender: TObject);
begin
  Tape1.Size.Y:=ScrollBar2.Position;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TBlockEditor.ModalShow(Self,Tape1);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
     Tape1.Pointer:=Ellipsoid2
  else
     Tape1.Pointer:=nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
var tmp,t : Integer;
begin
  tmp:=UpDown1.Position;

  Series1.Clear;
  for t:=0 to tmp-1 do
      Series1.AddXY(-100+t*(200/tmp),-100+Random(200));

  ChartTool1DragPoint(ChartTool1,0);
end;

procedure TForm1.ScrollBar3Change(Sender: TObject);
begin
  TBlockAccess(Tape1).DeleteLists;
end;

procedure TForm1.ScrollBar4Change(Sender: TObject);
begin
  TBlockAccess(Tape1).DeleteLists;
end;

procedure TForm1.Maker1Click(Sender: TObject);
begin
  if Visible and Enabled and Active then
  if Maker1.CanFocus then
     Maker1.SetFocus;
end;

procedure TForm1.ScrollBar5Change(Sender: TObject);
begin
  Tape1.Slices:=ScrollBar5.Position;

  if Maker1.Blocks.Count>2 then
     TTapeBlock(Maker1.Blocks[2]).Slices:=Tape1.Slices;
end;

procedure TForm1.ScrollBar6Change(Sender: TObject);
begin
  Tape1.Slices3D:=ScrollBar6.Position;

  if Maker1.Blocks.Count>2 then
     TTapeBlock(Maker1.Blocks[2]).Slices3D:=Tape1.Slices3D;
end;

procedure TForm1.ScrollBar7Change(Sender: TObject);
begin
  Tape1.Radius:=ScrollBar7.Position;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Tape1.Corners.Enabled:=CheckBox3.Checked;

  if Maker1.Blocks.Count>2 then
     TTapeBlock(Maker1.Blocks[2]).Corners.Enabled:=Tape1.Corners.Enabled;
end;

end.
