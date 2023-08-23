unit TeeWater;
{$I TeeDefs.inc}

interface

uses
  Windows, Classes, SysUtils,
  Forms, Controls, StdCtrls, ComCtrls, Graphics,
  TeeBlocks, TeeAnimate,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeCanvas;

type
  TAnimatedBlock=class(TCustomBlock)
  private
    FAutoPlay : Boolean;

    IAnimate      : TTeeAnimate;
    IAutoPlayDone : Boolean;

    function GetSpeed: Integer;
    procedure SetSpeed(const Value: Integer);
  protected
    procedure DoAnimation(Sender: TTeeAnimation; const Fraction:Single); virtual; abstract;
    procedure SetVisible(const Value:Boolean); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    property Animate:TTeeAnimate read IAnimate;
  published
    property AutoPlay:Boolean read FAutoPlay write FAutoPlay default True;
    property Speed:Integer read GetSpeed write SetSpeed default 10;
  end;

  TWaterBlock=class(TAnimatedBlock)
  private
    FGrid     : TPointXYFloat;
    FSpeed    : TPointXYFloat;
    FWaves    : TPointXYFloat;

    IOffset   : TFloatPoint;
    IPrevious : TDoubleArray;

    procedure SetGrid(const Value: TPointXYFloat);
    procedure SetSpeed(const Value: TPointXYFloat);
    procedure SetWaves(const Value: TPointXYFloat);
  protected
    procedure DoAnimation(Sender: TTeeAnimation; const Fraction:Single); override;
    function GetEditor:String; override;
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Draw; override;
  published
    property Grid:TPointXYFloat read FGrid write SetGrid;
    property Speed:TPointXYFloat read FSpeed write SetSpeed;
    property Waves:TPointXYFloat read FWaves write SetWaves;
  end;

  TWaterEditor = class(TForm)
    CBPlay: TCheckBox;
    Label2: TLabel;
    SBWavesX: TScrollBar;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Edit1: TEdit;
    UDGridX: TUpDown;
    Label4: TLabel;
    Edit2: TEdit;
    UDGridZ: TUpDown;
    Label1: TLabel;
    SBSpeedX: TScrollBar;
    SBWavesY: TScrollBar;
    SBSpeedY: TScrollBar;
    CBAutoPlay: TCheckBox;
    LSpeedX: TLabel;
    LSpeedY: TLabel;
    LWavesX: TLabel;
    LWavesY: TLabel;
    procedure FormShow(Sender: TObject);
    procedure CBPlayClick(Sender: TObject);
    procedure SBWavesXChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure SBSpeedXChange(Sender: TObject);
    procedure SBWavesYChange(Sender: TObject);
    procedure SBSpeedYChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBAutoPlayClick(Sender: TObject);
  private
    { Private declarations }

    Block : TWaterBlock;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math, OpenGL2;

{ TWaterBlock }

Constructor TWaterBlock.Create(AOwner: TComponent);
begin
  inherited;

  Size.Point.Z:=3;

  FGrid:=TPointXYFloat.Create(Self,32);
  FWaves:=TPointXYFloat.Create(Self,5);
  FSpeed:=TPointXYFloat.Create(Self,30);

  Animate.Loop:=True;
end;

Destructor TWaterBlock.Destroy;
begin
  IPrevious:=nil;

  FWaves.Free;
  FSpeed.Free;
  FGrid.Free;

  inherited;
end;

procedure TWaterBlock.DoAnimation(Sender: TTeeAnimation; const Fraction:Single);
var tmp : Single;
begin
  tmp:=0.000001*Max(2,Round(FGrid.Point.X))*Max(2,Round(FGrid.Point.Y));

  with IOffset do
  begin
    X:=X+FSpeed.Point.X*tmp;
    Y:=Y+FSpeed.Point.Y*tmp;
  end;

  Repaint;
end;

procedure TWaterBlock.Draw;
const
  PiRandom=HalfPi*0.000001;

var t,
    tt  : Integer;

    s,
    t1,t2,
    vt1,vt2,
    x,
    tmpFreqt2,
    dXX,dYY,
    tmp  : Single;

    tmpFreq,
    tmpPhase,
    tmpOff,
    d : TFloatPoint;

    mode : GLint;
    xx, zz : Integer;
begin
  tmpOff:=IOffset;

  tmpPhase.X:=FSpeed.Point.X*0.000001;
  tmpPhase.Y:=FSpeed.Point.Y*0.000001;

  xx:=Max(2,Round(FGrid.Point.X));
  zz:=Max(2,Round(FGrid.Point.Y));

  d.X:=1/zz;
  dXX:=2*d.X;

  d.Y:=1/xx;
  dYY:=2*d.Y;

  tmpFreq.X:=FWaves.Point.X;
  tmpFreq.Y:=FWaves.Point.Y;

  t1:=0;
  vt1:=-1;

  SetLength(IPrevious,zz+1);

  for t:=0 to xx-1 do
  begin
    t2:=t1+d.Y;
    vt2:=vt1+dYY;
    tmpFreqt2:=tmpFreq.Y*t2;

    glBegin(GL_QUAD_STRIP);

    glNormal3i(0,1,0);

    s:=0;
    x:=-1;

    for tt:=0 to zz-1 do
    begin
      tmp:=Sin(tmpFreq.X*s+tmpOff.X);

      if t=0 then
         IPrevious[tt]:=Sin(tmpFreq.Y*t1+tmpOff.Y)*tmp;

      //glNormal3f(x,1-IPrevious[tt],vt1);

      glTexCoord2f(s, t1);
      glVertex3f(x, IPrevious[tt], vt1);

      IPrevious[tt]:=Sin(tmpFreqt2+tmpOff.Y)*tmp;

      //glNormal3f(x,1-IPrevious[tt],vt2);

      glTexCoord2f(s, t2);
      glVertex3f(x, IPrevious[tt], vt2);

      s:=s+d.X;
      x:=x+dXX;

      with tmpOff do
      begin
        X:=X+tmpPhase.X;
        Y:=Y+tmpPhase.Y;
      end;
    end;

    glEnd;

    t1:=t2;
    vt1:=vt2;
  end;

  glGetIntegerv(GL_RENDER_MODE,@mode);

  if mode=GL_RENDER then
    if (not Animate.Playing) and AutoPlay and (not IAutoPlayDone) then
    begin
      IAutoPlayDone:=True;
      Animate.Play;
    end;
end;

function TWaterBlock.GetEditor: String;
begin
  result:='TWaterEditor';
end;

procedure TWaterBlock.PrepareForGallery;
begin
  inherited;

  Size.Point.Z:=30;

  Format.Color:=clWhite;
  Format.Texture.PictureLink:=TeeMakerLibraryTag+'Basic\Pool.jpg';
end;

procedure TWaterEditor.FormShow(Sender: TObject);
begin
  Block:=TWaterBlock(Tag);

  if Assigned(Block) then
  with Block do
  begin
    CBPlay.Checked:=Animate.Playing;
    CBAutoPlay.Checked:=AutoPlay;

    SBWavesX.Position:=Round(Waves.X);
    SBWavesY.Position:=Round(Waves.Y);

    LWavesX.Caption:=FloatToStr(Waves.X);
    LWavesY.Caption:=FloatToStr(Waves.Y);

    SBSpeedX.Position:=Round(Speed.X);
    LSpeedX.Caption:=IntToStr(SBSpeedX.Position);

    SBSpeedY.Position:=Round(Speed.Y);
    LSpeedY.Caption:=IntToStr(SBSpeedY.Position);

    UDGridX.Position:=Round(Grid.X);
    UDGridZ.Position:=Round(Grid.Y);
  end;
end;

procedure TWaterEditor.CBPlayClick(Sender: TObject);
begin
  with Block.Animate do
  if CBPlay.Checked then Play
                    else Stop;
end;

procedure TWaterEditor.SBWavesXChange(Sender: TObject);
begin
  Block.Waves.X:=SBWavesX.Position;
  LWavesX.Caption:=FloatToStr(Block.Waves.X);
end;

procedure TWaterBlock.SetGrid(const Value: TPointXYFloat);
begin
  FGrid.Assign(Value);
end;

procedure TWaterEditor.Edit1Change(Sender: TObject);
begin
  if Showing then
     Block.Grid.X:=UDGridX.Position;
end;

procedure TWaterEditor.Edit2Change(Sender: TObject);
begin
  if Showing then
     Block.Grid.Y:=UDGridZ.Position;
end;

procedure TWaterEditor.SBSpeedXChange(Sender: TObject);
begin
  Block.Speed.X:=SBSpeedX.Position;
  LSpeedX.Caption:=IntToStr(SBSpeedX.Position);
end;

procedure TWaterBlock.SetWaves(const Value: TPointXYFloat);
begin
  FWaves.Assign(Value);
end;

procedure TWaterBlock.SetSpeed(const Value: TPointXYFloat);
begin
  FSpeed.Assign(Value);
end;

procedure TWaterEditor.SBWavesYChange(Sender: TObject);
begin
  Block.Waves.Y:=SBWavesY.Position;
  LWavesY.Caption:=FloatToStr(Block.Waves.Y);
end;

procedure TWaterEditor.SBSpeedYChange(Sender: TObject);
begin
  Block.Speed.Y:=SBSpeedY.Position;
  LSpeedY.Caption:=IntToStr(SBSpeedY.Position);
end;

procedure TWaterEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ModalResult:=mrOk;
end;

procedure TWaterEditor.CBAutoPlayClick(Sender: TObject);
begin
  Block.AutoPlay:=CBAutoPlay.Checked;
end;

{ TAnimatedBlock }

Constructor TAnimatedBlock.Create(AOwner: TComponent);
var tmp : TTeeAnimation;
begin
  inherited;

  IAnimate:=TTeeAnimate.Create(nil);
  IAnimate.Speed:=10; // Frames Per Second

  tmp:=TTeeAnimation.Create(nil);
  tmp.OnFrame:=DoAnimation;
  IAnimate.Animations.Add(tmp);

  FAutoPlay:=True;
end;

Destructor TAnimatedBlock.Destroy;
begin
  //Animate.Animations.FreeAll;
  Animate.Free;

  inherited;
end;

function TAnimatedBlock.GetSpeed: Integer;
begin
  result:=Animate.Speed;
end;

procedure TAnimatedBlock.SetSpeed(const Value: Integer);
begin
  Animate.Speed:=Value;
end;

procedure TAnimatedBlock.SetVisible(const Value: Boolean);
begin
  inherited;

  if not Visible then
  begin
    IAutoPlayDone:=False;

    if Animate.Playing then
       Animate.Stop;
  end;
end;

initialization
  RegisterBlock(TWaterBlock);
  RegisterClass(TWaterEditor);
end.

