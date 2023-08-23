unit TeeRain;
{$I TeeDefs.inc}

interface

uses
  Windows,
  Classes, SysUtils,
  {$IFDEF D16}
  System.UITypes,
  {$ENDIF}
  Forms,
  TeCanvas, TeeBlocks,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeAnimate, TeeWater, StdCtrls, Controls;

type
  TElement=record
    Location : TPoint3DFloat;
    Size     : TPoint3DFloat;
    Speed    : TPoint3DFloat;
  end;

  TElementsBlock=class(TAnimatedBlock)
  private
    FCount    : Integer;

    IElements : Array of TElement;

    procedure SetCount(const Value: Integer);
  protected
    procedure InitElements(StartIndex:Integer); virtual;
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property ElementCount:Integer read FCount write SetCount;
  end;

  TRainBlock=class(TElementsBlock)
  private
    FLength : Integer;
    FSound  : Boolean;

    IPlayingSound : Boolean;
    FRandomLength: Boolean;

    procedure SetDropLength(const Value: Integer);
    procedure SetSound(const Value: Boolean);
    procedure SetRandomLength(const Value: Boolean);
  protected
    procedure DoAnimation(Sender: TTeeAnimation; const Fraction:Single); override;
    procedure InitElements(StartIndex:Integer); override;
    procedure InitLengths(StartIndex:Integer);
    procedure SetVisible(const Value: Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;

    procedure PlaySound;
    procedure StopSound;
  published
    property DropLength:Integer read FLength write SetDropLength default 15;
    property ElementCount default 100;
    property RandomLength:Boolean read FRandomLength write SetRandomLength default True;
    property Sound:Boolean read FSound write SetSound default True;
  end;

  TStarsBlock=class(TElementsBlock)
  private
    FSize   : Integer;

    IColors : Array of TRGB;

    procedure SetSize(const Value: Integer);
  protected
    procedure InitElements(StartIndex:Integer); override;
    function ShouldDraw(After:Boolean=False):Boolean; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Draw; override;
  published
    property ElementCount default 1000;
    property Size:Integer read FSize write SetSize default 1;
  end;

  TSkydomeBlock=class(TSphereBlock)
  public
    Constructor Create(AOwner: TComponent); override;
  end;

  TRainEditor = class(TForm)
    LRainDrops: TLabel;
    LRainLength: TLabel;
    Label10: TLabel;
    BlockRainCount: TScrollBar;
    BlockRainLength: TScrollBar;
    BRainPlay: TButton;
    BlockRainSound: TCheckBox;
    BlockRainSpeed: TScrollBar;
    LDropCount: TLabel;
    LDropLength: TLabel;
    CBRandomLengths: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure BlockRainCountChange(Sender: TObject);
    procedure BlockRainLengthChange(Sender: TObject);
    procedure BRainPlayClick(Sender: TObject);
    procedure BlockRainSoundClick(Sender: TObject);
    procedure BlockRainSpeedChange(Sender: TObject);
    procedure CBRandomLengthsClick(Sender: TObject);
  private
    { Private declarations }
    Rain : TRainBlock;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math, Graphics,
  TeeGLCanvas, OpenGL2, MMSystem, TeeMakerConst;

Destructor TElementsBlock.Destroy;
begin
  IElements:=nil;
  inherited;
end;

procedure TElementsBlock.SetCount(const Value: Integer);
var Old : Integer;
begin
  Old:=Length(IElements);
  FCount:=Max(0,Value);
  SetLength(IElements,FCount);

  if FCount>Old then
     InitElements(Old);

  Repaint;
end;

procedure TElementsBlock.InitElements(StartIndex:Integer);
begin
end;

procedure TElementsBlock.Assign(Source: TPersistent);
var t : Integer;
begin
  if Source is TElementsBlock then
  with TElementsBlock(Source) do
  begin
    Self.FCount:=FCount;

    SetLength(Self.IElements,FCount);
    for t:=0 to FCount-1 do
        Self.IElements[t]:=IElements[t];
  end;

  inherited;
end;

{ TRain }

Constructor TRainBlock.Create(AOwner:TComponent);
begin
  inherited;

  FLength:=15;
  FRandomLength:=True;

  Format.Transparency:=30;
  Format.Color:=RGB(215,253,255);

  Animate.Loop:=True;

  ElementCount:=100;

  FSound:=True;
end;

Destructor TRainBlock.Destroy;
begin
  StopSound;
  inherited;
end;

procedure TRainBlock.PlaySound;
var tmp : String;
begin
  if Sound and (not IPlayingSound) then
  begin
    tmp:=TBlocks.ParseFileName(TeeMsg_SoundsLibrary,TeeMakerLibraryTag+'Weather\rain.wav');

    sndPlaySound({$IFDEF D12}PWideChar{$ELSE}PAnsiChar{$ENDIF}(tmp),
                 SND_ASYNC or SND_LOOP or SND_NODEFAULT);

    IPlayingSound:=True;
  end;
end;

procedure TRainBlock.StopSound;
begin
  if Sound and IPlayingSound then
  begin
    sndPlaySound(nil,SND_ASYNC or SND_LOOP);
    IPlayingSound:=False;
  end;
end;

procedure TRainBlock.InitElements(StartIndex:Integer);
var t : Integer;
begin
  for t:=StartIndex to FCount-1 do
  with IElements[t] do
  begin
    Location.X:=(Random(10000)-5000)*0.0002;
    Location.Z:=(Random(10000)-5000)*0.0002;
    Location.Y:=-(Random(10000)-5000)*0.0002;

    Speed.Z:=0.5+(Random(10000)*0.00005);
  end;

  InitLengths(StartIndex);
end;

procedure TRainBlock.InitLengths(StartIndex:Integer);
var t : Integer;
begin
  for t:=StartIndex to FCount-1 do
  with IElements[t] do
    if RandomLength then
       Size.Z:=Random(FLength)
    else
       Size.Z:=FLength;
end;

procedure TRainBlock.DoAnimation(Sender: TTeeAnimation; const Fraction:Single);
var t : Integer;
begin
  for t:=0 to FCount-1 do
  with IElements[t],Location do
  begin
    Z:=Z-Animate.Speed*0.001*Speed.Z;
    if Z<-1 then Z:=1;
  end;

  Repaint;
end;

procedure TRainBlock.Assign(Source: TPersistent);
begin
  if Source is TRainBlock then
  with TRainBlock(Source) do
  begin
    Self.FLength:=FLength;
    Self.FRandomLength:=FRandomLength;
    Self.FSound:=FSound;
    Self.Animate.Speed:=Animate.Speed;
  end;

  inherited;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

procedure TRainBlock.Draw;
var t : Integer;
begin
  TBlockFormatAccess(Format).PreparePen;

  glBegin(GL_LINES);

  for t:=0 to FCount-1 do
  with IElements[t],Location do
  begin
    glVertex3d(X,Z,Y);
    glVertex3d(X,Z+Size.Z*0.0025,Y);
  end;

  glEnd;
end;

procedure TRainBlock.SetDropLength(const Value: Integer);
begin
  if FLength<>Value then
  begin
    FLength:=Max(1,Value);
    InitLengths(0);
    Repaint;
  end;
end;

procedure TRainBlock.SetSound(const Value: Boolean);
begin
  if FSound<>Value then
  begin
    if Animate.Playing and (not Value) then
       StopSound;

    FSound:=Value;

    if Animate.Playing and FSound then
       PlaySound;
  end;
end;

procedure TRainBlock.SetVisible(const Value: Boolean);
begin
  inherited;

  if not Visible then
     if Animate.Playing then
     begin
       Animate.Stop;
       StopSound;
     end;
end;

{ TStarsBlock }

Constructor TStarsBlock.Create(AOwner: TComponent);
begin
  inherited;
  Format.Color:=clWhite;
  ElementCount:=1000;
  FSize:=1;
end;

Destructor TStarsBlock.Destroy;
begin
  IColors:=nil;
  inherited;
end;

procedure TStarsBlock.Draw;
var t : Integer;
begin
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);

  TBlockFormatAccess(Format).PrepareColor;

  glPointSize(FSize);

  glBegin(GL_POINTS);

  for t:=0 to FCount-1 do
  begin
    glColor3ubv(@IColors[t]);
    glVertex3fv(@IElements[t]);
  end;

  glEnd;

  glEnable(GL_LIGHTING);
end;

procedure TStarsBlock.InitElements(StartIndex: Integer);

  function RandomAngle(const MaxAngle:Single):Single;
  begin
    result:=MaxAngle*Random(1000000)*0.000001;
  end;

const
  tmpR=20000;

var t : Integer;
    tmp : Integer;
    tmpSin1,
    tmpCos1 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    tmpSin2,
    tmpCos2 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  SetLength(IColors,FCount);

  for t:=StartIndex to FCount-1 do
  with IElements[t] do
  begin
    SinCos(RandomAngle(Pi),tmpSin1,tmpCos1);
    SinCos(RandomAngle(Pi),tmpSin2,tmpCos2);

    with Location do
    begin
      X:=tmpR*tmpSin1*tmpCos2;
      Y:=tmpR*tmpSin1*tmpSin2;
      Z:=tmpR*tmpCos1;
    end;

    tmp:=64+Random(192);

    with IColors[t] do
    begin
      if Random(10)<5 then
         Red:=Min(255,tmp+Random(64))
      else
         Red:=tmp;

      Green:=tmp;

      if Random(10)<5 then
         Blue:=Min(255,tmp+Random(64))
      else
         Blue:=tmp;
    end;
  end;
end;

procedure TStarsBlock.SetSize(const Value: Integer);
begin
  FSize := Value;
  Repaint;
end;

function TStarsBlock.ShouldDraw(After:Boolean=False):Boolean;
begin
  result:=After and Visible;
end;

procedure TRainEditor.FormShow(Sender: TObject);
begin
  Rain:=TRainBlock(Tag);

  if Assigned(Rain) then
  with Rain do
  begin
    BlockRainCount.Position:=ElementCount;
    BlockRainLength.Position:=DropLength;
    BlockRainSound.Checked:=Sound;
    BlockRainSpeed.Position:=Speed;

    LDropCount.Caption:=IntToStr(BlockRainCount.Position);
    LDropLength.Caption:=IntToStr(BlockRainLength.Position);

    CBRandomLengths.Checked:=RandomLength;
  end;
end;

procedure TRainEditor.BlockRainCountChange(Sender: TObject);
begin
  if Showing then
  begin
    Rain.ElementCount:=BlockRainCount.Position;
    LDropCount.Caption:=IntToStr(BlockRainCount.Position);
    //MarkDirty;
  end;
end;

procedure TRainEditor.BlockRainLengthChange(Sender: TObject);
begin
  if Showing then
  begin
    Rain.DropLength:=BlockRainLength.Position;
    LDropLength.Caption:=IntToStr(BlockRainLength.Position);
    //MarkDirty;
  end;
end;

procedure TRainEditor.BRainPlayClick(Sender: TObject);
begin
  with Rain.Animate do
  begin
    if Playing then
    begin
      Stop;

      Rain.StopSound;
      BRainPlay.Caption:='&Play';
    end
    else
    begin
      Play;

      Rain.PlaySound;
      BRainPlay.Caption:='&Stop';
    end;
  end;
end;

procedure TRainEditor.BlockRainSoundClick(Sender: TObject);
begin
  if Showing then
  begin
    Rain.Sound:=BlockRainSound.Checked;
    //MarkDirty;
  end;
end;

procedure TRainEditor.BlockRainSpeedChange(Sender: TObject);
begin
  if Showing then
  begin
    Rain.Speed:=BlockRainSpeed.Position;
    //MarkDirty;
  end;
end;

function TRainBlock.GetEditor: String;
begin
  result:='TRainEditor';
end;

procedure TRainBlock.SetRandomLength(const Value: Boolean);
begin
  if FRandomLength<>Value then
  begin
    FRandomLength:=Value;
    InitLengths(0);
    Repaint;
  end;
end;

procedure TRainEditor.CBRandomLengthsClick(Sender: TObject);
begin
  Rain.RandomLength:=CBRandomLengths.Checked;
end;

{ TSkydomeBlock }

constructor TSkydomeBlock.Create(AOwner: TComponent);
begin
  inherited;
  Radius:=10000;
  Total:=50;
  Rotation.Y:=180;
  Location.Z:=-150;
  Format.Bright:=True;
  Format.Texture.PictureLink:='$(TEEMAKER)\Clouds\sky1.jpg';
  Format.Texture.Scale.Y:=-0.5;
  Format.Texture.Translate.Z:=0.5;
end;

initialization
  RegisterBlocks([TRainBlock,TStarsBlock,TSkydomeBlock]);
  RegisterClass(TRainEditor);
end.
