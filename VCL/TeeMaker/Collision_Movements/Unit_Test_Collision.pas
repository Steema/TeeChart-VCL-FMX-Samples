unit Unit_Test_Collision;

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  TeCanvas, TeeProcs, TeeBlocks, TeeMakerControl, TeePhysics, TeeMakerEditor,
  TeePlayMP3, TeeRain, TeeWater;

type
  TForm1 = class(TForm)
    Maker1: TMaker;
    Panel1: TPanel;
    ScrollCount: TScrollBar;
    ComboClass: TComboFlat;
    Label1: TLabel;
    Label2: TLabel;
    ScrollForce: TScrollBar;
    ComboNavigate: TComboFlat;
    CBSound: TCheckBox;
    Object1: TObjectBlock;
    Label3: TLabel;
    ScrollDecelaration: TScrollBar;
    Button1: TButton;
    PoolTable: TObjectBlock;
    Cube1: TCubeBlock;
    Cube2: TCubeBlock;
    Cube3: TCubeBlock;
    Cube4: TCubeBlock;
    Label4: TLabel;
    ScrollGravity: TScrollBar;
    Object2: TObjectBlock;
    Cylinder1: TCylinderBlock;
    Cylinder2: TCylinderBlock;
    Cylinder3: TCylinderBlock;
    CBAxes: TCheckBox;
    B0: TSphereBlock;
    B1: TSphereBlock;
    B2: TSphereBlock;
    t0: TeeBlocks.TTeeTextBlock ;
    Stars1: TStarsBlock;
    procedure FormCreate(Sender: TObject);
    procedure Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Maker1ClickedBlock(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollCountChange(Sender: TObject);
    procedure ComboClassChange(Sender: TObject);
    procedure ComboNavigateChange(Sender: TObject);
    procedure CBSoundClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollDecelarationChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScrollGravityChange(Sender: TObject);
    procedure CBAxesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Physics : TBlockPhysics;

    procedure AddToPhysics;
    procedure CreateObjects;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Math;

procedure TForm1.CreateObjects;
var t : Integer;
    tmp : TCustomBlock;
    tmpClass : TBlockClass;
begin
  // Remove excess
  with Object1.Items do
  while Count>ScrollCount.Position do
        Remove(Block[Count-1]);

  case ComboClass.ItemIndex of
    0: tmpClass:=TCubeBlock;
    1: tmpClass:=TSphereBlock;
  else
    tmpClass:=TCylinderBlock;
  end;

  // Create new
  for t:=Object1.Items.Count to ScrollCount.Position-1 do
  begin
    tmp:=tmpClass.Create(nil);

    // Set format
    tmp.Format.Border.Visible:=False;

    if t=2 then
       tmp.Format.Color:=clBlack
    else
       tmp.Format.Color:=RGB(Random(255),Random(255),Random(255));

    tmp.Size.Point:=PointFloat(10,10,10);

    // Set location
    repeat

      with tmp.Location.Point do
      begin
        X:=Random(180)-90;
        Y:=Random(380)-190;
        Z:=0.1;
      end;

    until not Assigned(Physics.Collision(tmp,Object1.Items));

    Object1.Items.Add(tmp);
  end;

  AddToPhysics;
end;

procedure TForm1.AddToPhysics;
begin
  Physics.Clear;
  Physics.Add(Object1);
  Physics.AddStatic(PoolTable.Items);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Maker1.Options.Floor.Limit:=lfNo;

  Physics:=TBlockPhysics.Create(nil);

  Physics.T0:=t0;
  Physics.B0:=B0;
  Physics.B1:=B1;
  Physics.B2:=B2;

  CreateObjects;
end;

procedure TForm1.Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var tmp : TCustomBlock;
begin
  tmp:=Object1.Items.ClickedBlock(X,Y);

  if Assigned(tmp) then
     Maker1.Cursor:=crHandPoint
  else
     Maker1.Cursor:=crDefault;

  Maker1.OriginalCursor:=Maker1.Cursor;
end;

procedure TForm1.Maker1ClickedBlock(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if TCustomBlock(Sender).Parent=Object1.Items then
  begin
    if Physics.Sound then
       TPlayMP3Sound.PlayFile('$(TEEMaker)\Effects\Pool Shot.mp3');

    Physics.Push(TCustomBlock(Sender),ScrollForce.Position*0.1,
                 Maker1.View3DOptions.RotationFloat*TeePiStep,0);
  end;
end;

procedure TForm1.ScrollCountChange(Sender: TObject);
begin
  CreateObjects;
  Maker1.SetFocus;
end;

procedure TForm1.ComboClassChange(Sender: TObject);
begin
  CreateObjects;
  Maker1.SetFocus;
end;

procedure TForm1.ComboNavigateChange(Sender: TObject);
begin
  if ComboNavigate.ItemIndex=0 then
     Maker1.Options.Navigate.Mode:=nmExplore
  else
     Maker1.Options.Navigate.Mode:=nmWalk;

  Maker1.SetFocus;
end;

procedure TForm1.CBSoundClick(Sender: TObject);
begin
  Physics.Sound:=CBSound.Checked;
  Maker1.SetFocus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Physics.Free;
end;

procedure TForm1.ScrollDecelarationChange(Sender: TObject);
begin
  Physics.Deceleration:=ScrollDecelaration.Position;
  Maker1.SetFocus;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Maker1);
  AddToPhysics;
end;

procedure TForm1.ScrollGravityChange(Sender: TObject);
begin
  Physics.Gravity:=ScrollGravity.Position*0.1;
  Maker1.SetFocus;
end;

procedure TForm1.CBAxesClick(Sender: TObject);
begin
  Maker1.Options.View3DAxes:=CBAxes.Checked;
  Maker1.SetFocus;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Maker1.SetFocus;
  Physics.ShowDebug:=True;
end;

end.
