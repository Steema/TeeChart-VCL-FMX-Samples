unit Unit_Test_Collision_Blocks;

interface

uses
  Windows, Messages, SysUtils, Variants, Math, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeeBlocks, TeCanvas, TeeChartBlock, ExtCtrls, TeePenDlg,
  TeeProcs, TeePointEditor, TeeKinematics, TeeMakerControl, TeeChart3D,
  TeeAnimate, TeeBlockEditor, TeEngine, Chart, TeeGDIPlus;

type
  TForm1 = class(TForm)
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Cube1: TCubeBlock;
    Ellipsoid1: TEllipsoidBlock;
    Cube2: TCubeBlock;
    Cube3: TCubeBlock;
    Cube4: TCubeBlock;
    Cube5: TCubeBlock;
    Panel1: TPanel;
    Button2: TButton;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Button1: TButton;
    CBDirections: TCheckBox;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Cylinder1: TCylinderBlock;
    Ellipsoid2: TEllipsoidBlock;
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Chart3D1AfterDraw(Sender: TObject);
    procedure Chart3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBDirectionsClick(Sender: TObject);
  private
    { Private declarations }
    k : TKinematics;

    Cube : TCubeBlock;
    IEditor : TKinematicsEditor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeeMakerEditor, TeeAnimateEditor;

procedure TForm1.ScrollBar1Change(Sender: TObject);
var tmp : TPoint3DFloat;
begin
  with Ellipsoid1.Location.Point do
       tmp:=PointFloat(ScrollBar1.Position,ScrollBar2.Position,Z);

  if Collide(Ellipsoid1,tmp)=nil then
  begin
    Ellipsoid1.Format.Color:=clBlue;
    Ellipsoid1.Location.SetPoint(tmp);
  end
  else
  begin
    Ellipsoid1.Format.Color:=clRed;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Cube1.Visible:=False;

  Cube:=TCubeBlock.Create(nil);
  Cube.Format.Transparency:=100;
  Cube.Format.Border.Width:=3;

  k:=TKinematics.Create(Chart3D1);

  with k.Movements.AddMovement do
  begin
    Block:=Ellipsoid1;
    Direction.Point.X:=0.1;
    Direction.Point.Y:=0.3;
    Speed:=20;
    Material.Bounce:=0.7;
    Acceleration:=0;

    Circular:=True;
    
    Center.X:=400;
    Center.Y:=400;
    Center.Z:=100;

    CenterLink.Instance:=Cube1;
    CenterLink.PropertyName:='Location';

    Radius.X:=200;
    Radius.Y:=50;
    Radius.Z:=0;
  end;

  with k.Movements.AddMovement do
  begin
    Block:=Cube1;
    Direction.Point.X:=0.3;
    Direction.Point.Y:=0;
    Speed:=20;
    Material.Bounce:=0.7;
    Acceleration:=0.1;
  end;

  Chart3D1.Options.View3DAxes:=True;

  IEditor:=TKinematicsEditor.Create(Self);
  IEditor.Align:=alClient;
  AddFormTo(IEditor,Panel2,k);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Cube.Free;
  k.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  k.Movements[0].Speed:=20;
  k.Movements[1].Speed:=20;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

procedure TForm1.Chart3D1AfterDraw(Sender: TObject);
var min,max : TPoint3DFloat;
begin
  if Assigned(Chart3D1.Selected) then
  begin
    Chart3D1.Selected.CalcBounds(min,max);

    Cube.Location.Point:=Center(Min,Max);
    Cube.Size.Point:=Subtract(Max,Min);

    Chart3D1.Blocks.HideBorders:=False;
    Cube.DrawBlock(Chart3D1.Blocks);
    Chart3D1.Blocks.HideBorders:=True;
  end;

  if CBDirections.Checked then
     k.Movements.DrawDirections;
end;

procedure TForm1.Chart3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Chart3D1.Selected:=Chart3D1.Blocks.ClickedBlock(X,Y);
end;

procedure TForm1.CBDirectionsClick(Sender: TObject);
begin
  Chart3D1.Invalidate;
end;

end.
