{********************************************}
{ TeeMaker 2.0                               }
{ Copyright (c) 2010-2023 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeMakerControl;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}

  Classes, SysUtils,

  {$IFDEF D6}
  Variants,
  {$ENDIF}
  {$IFDEF D10}
  Types,
  {$ENDIF}

  {$IFDEF CLX}
  QGraphics, QControls, QDialogs,
  {$ELSE}
  Graphics, Controls, Dialogs,
  {$ENDIF}

  TypInfo, MMSystem,

  OpenGL2, TeCanvas, TeeAnimate, TeeProcs, TeeOpenGL, TeeBlocks, TeeCamera,
  TeeGLSLShaders;

type
  TFogStyle=(fsLinear, fsExp, fsExp2);

  TFogBlock=class(TPersistent)
  private
    FColor : TColor;
  public
    Density  : Double;
    Style    : TFogStyle;
    StartPos : Double;
    EndPos   : Double;
    Fast     : Boolean;
    Enabled  : Boolean;

    Constructor Create;
    procedure Setup;
  published
    property Color:TColor read FColor write FColor default clGray;
  end;

  TJoystick=class(TPersistent)
  private
    FActive : Boolean;

    ICaps   : TJoyCaps;
    ICapsOk : Boolean;
    IJoyID  : Byte;
    IOwner  : TWinControl;
    IPresent: Boolean;

    procedure CheckCaps;
    function GetPresent: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    Position : TPoint3DFloat;

    Constructor Create(AOwner:TWinControl; JoystickID:Byte);
    property Present:Boolean read GetPresent;
  published
    property Active:Boolean read FActive write SetActive default False;
  end;

  TNavigateMode=(nmObserve, nmWalk, nmFly, nmExplore);

  TWheelAction=(waNone,waZoom);

  TMaker=class;

  TNavigate=class(TPersistent)
  private
    FJoy1      : TJoystick;
    FInertia   : Integer;
    FMode      : TNavigateMode;
    FWalkSpeed : Single;
    FWheel     : TWheelAction;

    FOldX    : Integer;
    FOldY    : Integer;

    IDifX    : Single;
    IDifY    : Single;

    FDragging    : Boolean;
    IMouseButton : TMouseButton;

    IOwner   : TMaker;
    IDragged : Boolean;
    IMovingRotating: Boolean;
    IShift   : TShiftState;

    procedure CheckPendingInertia;
    procedure DoMove(const MoveDelta:Double);
    Procedure DoMouseMove(X,Y:Integer; Shift:TShiftState);
    function DoMouseWheel(WheelDelta: Integer):Boolean;
    procedure DoRotate(X,Y:Integer; Shift:TShiftState);
    Procedure DoRotation(const IDifX,IDifY:Double);
    function FlyEnabled(const Sender:TTimerEvent):Boolean;
    procedure ForceInvalidate;
    function GetJoy1:TJoystick;
    function HasPendingInertia:Boolean;
    function IsWalkSpeedStored:Boolean;
    procedure SetJoystick1(const Value: TJoystick);
    procedure SetMode(const Value:TNavigateMode);
    procedure TimerFly(Sender:TObject);
  protected
    function IsPanning:Boolean;
  public
    AllowRotationX  : Boolean;
    AllowRotationY  : Boolean;
    FlySpeed        : Double;

    Constructor Create(AOwner:TMaker);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function MouseToLocation(const AX,AY:Single):TPoint3DFloat;

    property Dragging:Boolean read FDragging write FDragging;
  published
    property Joystick1:TJoystick read GetJoy1 write SetJoystick1;
    property Mode:TNavigateMode read FMode write SetMode default nmObserve;
    property MouseWheel:TWheelAction read FWheel write FWheel default waZoom;
    property RotateInertia:Integer read FInertia write FInertia default 30;
    property WalkSpeed:Single read FWalkSpeed write FWalkSpeed stored IsWalkSpeedStored;
  end;

  TMakerOptions=class;

  TMakerLimitFloor=(lfAuto,lfYes,lfNo);

  TMakerFloor=class(TPersistent)
  private
    FDistance   : Integer;
    FLimit      : TMakerLimitFloor;
    FReflection : Byte;

    IFloor   : TRectangleBlock;

    function GetFormat: TBlockFormat;
    function GetRotation: TRotationXYZ;
    function GetSize: TPointXYZFloat;
    function GetTile: TTile;
    function GetVisible: Boolean;
    procedure InitDefaults;
    procedure SetDistance(const Value: Integer);
    procedure SetFormat(const Value: TBlockFormat);
    procedure SetLimit(const Value: TMakerLimitFloor);
    procedure SetReflection(const Value:Byte);
    procedure SetRotation(const Value: TRotationXYZ);
    procedure SetSize(const Value: TPointXYZFloat);
    procedure SetTile(const Value: TTile);
    procedure SetVisible(const Value: Boolean);
  public
    Constructor Create(AOwner:TComponent);
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    class function DefaultTexture:String;

    property Block:TRectangleBlock read IFloor;
  published
    property Distance:Integer read FDistance write SetDistance default 0;
    property Format:TBlockFormat read GetFormat write SetFormat;
    property Limit:TMakerLimitFloor read FLimit write SetLimit default lfAuto;
    property Reflection:Byte read FReflection write SetReflection default 20;
    property Rotation:TRotationXYZ read GetRotation write SetRotation;
    property Size:TPointXYZFloat read GetSize write SetSize;
    property Tile:TTile read GetTile write SetTile;
    property Visible:Boolean read GetVisible write SetVisible default True;
  end;

  TAnaglyph=(haNone, haRedCyan, haCyanRed);

  TMakerOptions=class(TPersistent)
  private
    FBoundingBox    : Boolean;
    FCameras        : TMakerCameras;
    FClickToFocus   : Boolean;
    FDrawShadows    : Boolean;
    FFloor          : TMakerFloor;
    FNavigate       : TNavigate;
    FSelectMode     : Boolean;
    FShowLightLamps : Boolean;
    FView3DAxes     : Boolean;

    IMaker : TMaker;

    procedure CheckLimits;
    procedure DrawAnaglyph;
    procedure DrawAxes;
    procedure DrawBlurShadows;
    function GetHideBorders:Boolean;
    function GetUseThreads: Boolean;
    function IsCamerasStored:Boolean;
    procedure SetBoundingBox(const Value: Boolean);
    procedure SetCameras(const Value:TMakerCameras);
    procedure SetDrawShadows(const Value: Boolean);
    procedure SetFloor(const Value: TMakerFloor);
    procedure SetHideBorders(const Value: Boolean);
    procedure SetNavigate(const Value: TNavigate);
    procedure SetShowLightLamps(const Value: Boolean);
    procedure SetUseThreads(const Value: Boolean);
    procedure SetView3DAxes(const Value: Boolean);
  protected
    procedure AfterDraw;
    procedure BeforeDraw;
    procedure DrawBoundingBox(const ABlock:TCustomBlock; const AColor:TColor; DrawCoords:Boolean);
    procedure DrawReflection(const AReflect:TMakerFloor);
  public
    Anaglyph     : TAnaglyph;
    AnaglyphDistance : Integer;
    Fog          : TFogBlock;

    SelectModeBlocks   : TBlocks;
    ShowBoundPositions : Boolean;
    BoundTitle         : String;

    Constructor Create(ABlocks:TBlocks);
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property BoundingBox:Boolean read FBoundingBox write SetBoundingBox default False;
    property Cameras:TMakerCameras read FCameras write SetCameras stored IsCamerasStored;
    property ClickToFocus:Boolean read FClickToFocus write FClickToFocus default True;
    property DrawShadows:Boolean read FDrawShadows write SetDrawShadows default False;
    property Floor:TMakerFloor read FFloor write SetFloor;
    property HideBorders:Boolean read GetHideBorders write SetHideBorders default True;
    property Navigate:TNavigate read FNavigate write SetNavigate;
    property SelectMode:Boolean read FSelectMode write FSelectMode default False;
    property ShowLightLamps:Boolean read FShowLightLamps write SetShowLightLamps default False;
    property UseThreads:Boolean read GetUseThreads write SetUseThreads default False;
    property View3DAxes:Boolean read FView3DAxes write SetView3DAxes default False;
  end;

  TLoadMakerEvent=procedure(Sender:TMaker; const FileName:String) of object;

  TDragBlockInfo=packed record
    CheckMinMax : Boolean;
    HasMin      : Boolean;
    HasMax      : Boolean;
    Invert      : Boolean;
    IsWheel     : Boolean;
    MaxDrag     : Double;
    MinDrag     : Double;
    PreDragging : Boolean;
    Prop        : PPropInfo;
    Target      : TObject;
    Source      : TCustomBlock;
    Action      : String;
  end;

  TBlockEvent=procedure(Sender:TCustomBlock; const Event:String) of object;

  TMaker=class(TCustomTeePanelExtended)
  private
    FBlocks         : TBlocks;

    FOnBeforeDraw   : TNotifyEvent;
    FOnBlockEvent   : TBlockEvent;
    FOnClickedBlock : TMouseEvent;
    FOnDoLoad       : TLoadMakerEvent;

    FCurrentOver    : TCustomBlock;
    FOptions        : TMakerOptions;
    FShader         : TProgramShader;
    FTeeOpenGL      : TTeeOpenGL;
    FUnderMouse     : TCustomBlock;

    Current         : TCustomBlock;

    IDrag           : TDragBlockInfo;

    IFrame          : Cardinal;
    ILastTime       : Cardinal;
    IFPS            : Cardinal;

    IBlockToLoad    : String;
    OldPos          : TPoint;

    function BlockParentWithAction(const AEvent:String; ABlock:TCustomBlock):TCustomBlock;
    procedure CanvasProjection(Sender: TObject);

    (*
    procedure CollisionEvent(Sender:TObject; const ActionID:String;
                                             const ABlock:TCustomBlock;
                                             var ACollided:TCustomBlock;
                                             var APoint:TPoint3DFloat);
    *)
    procedure DoDragProp(DeltaX,DeltaY:Integer);
    procedure ExploreCurrent;
    function NewBlocks:TBlocks;

    function GetAnimates: TAnimates;
    procedure SetAnimates(const Value: TAnimates);
    function GetEvents: TStrings;
    function GetProperties: TObjectProperties;
    function HasEvents: Boolean;
    function HasProperties: Boolean;
    function IsAnimatesStored: Boolean;
    procedure SetEvents(const Value: TStrings);
    procedure SetProperties(const Value: TObjectProperties);

    function OnGetPicBits(AGraphic:TGraphic; var Bits:PByteArray):Boolean;
    procedure OpenGLInit(Sender: TObject);
    procedure ProcessEvent(const ABlock:TCustomBlock; Action:String);
    procedure RemovedBlock(Sender: TObject);
    function SelectedClickedBlock(X,Y:Integer; SubObjects:Boolean=False):TCustomBlock;
    procedure SetCurrentOver(const Value: TCustomBlock);
    procedure SetOptions(const Value: TMakerOptions);
    procedure SetSelected(const Value: TCustomBlock);
    procedure SetUnderMouse(const Value: TCustomBlock);
    procedure UpdatePosition(const ADelta:Single);
  protected
    procedure CMBlockBirth(var Message: TMessage); message CM_BLOCKBIRTH;

    procedure DoContextPopup({$IFDEF CLX}const {$ENDIF}MousePos: TPoint; var Handled: Boolean); override;
    procedure DoInitDrag(X,Y:Integer);
    Procedure InternalDraw(Const UserRectangle:TRect); override;

    procedure DeleteAllLists;
    {$IFNDEF D11}
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    {$ENDIF}

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WriteState(Writer: TWriter); override;

    // Joystick
    {$IFNDEF CLX}
    procedure JoyButtonDown(var Message: TMessage); message MM_JOY1BUTTONDOWN;
    procedure JoyButtonUp(var Message: TMessage); message MM_JOY1BUTTONUP;
    procedure JoyMove(var Message: TMessage); message MM_JOY1MOVE;
    procedure JoyZMove(var Message: TMessage); message MM_JOY1ZMOVE;
    {$ENDIF}

    // Mouse
    {$IFDEF CLX}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                          {$IFNDEF UCL}const{$ENDIF} MousePos: TPoint): Boolean; override;
    {$ELSE}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                          MousePos: TPoint): Boolean; override;
    {$ENDIF}

    procedure DragCanceled; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property DragInfo:TDragBlockInfo read IDrag;

    function ParentIsSelected(ABlock:TCustomBlock):Boolean;
    procedure ProcessTimerAction(Sender:TObject);

    {$IFDEF CLX}
    Procedure SetParent(const AParent: TWidgetControl); override;
    {$ELSE}
    Procedure SetParent(AParent: TWinControl); override;
    {$ENDIF}

    property BlockUnderMouse:TCustomBlock read FUnderMouse write SetUnderMouse;
    property CurrentOver:TCustomBlock read FCurrentOver write SetCurrentOver;
    property DragPosition:TPoint read OldPos write OldPos;
  public
    RepaintMonitor : Boolean;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function DoBlockAction(const ABlock:TCustomBlock; AEvent:String):TCustomBlock;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    procedure DoSingleAction(const ABlock:TCustomBlock; AAction:String);
    Procedure DrawPanelBevels(Rect:TRect); override;

    {$IFDEF D11}
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    {$ENDIF}

    property Render:TTeeOpenGL read FTeeOpenGL;
    property Selected:TCustomBlock read Current write SetSelected;
  published
    { TCustomTeePanelExtended properties }
    property BackImage;
    property BackImageMode;
    property Gradient;
    property OnAfterDraw;

    { TCustomTeePanel properties }
    property BufferedDisplay default True;
    property View3D;
    property View3DOptions;

    { TPanel properties }
    property Align;
    property Color default clWhite;

    {$IFNDEF CLX}
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    {$ENDIF}

    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Anchors;

    {$IFNDEF CLX}
    property AutoSize;
    {$ENDIF}

    property Constraints;

    {$IFNDEF CLX}
    property DragKind;
    {$IFNDEF LCL}
    property Locked;
    {$ENDIF}
    {$ENDIF}

    { TMaker properties }

    // TBlocks properties:
    property Animates:TAnimates read GetAnimates write SetAnimates
                                stored IsAnimatesStored;
    property Events:TStrings read GetEvents write SetEvents stored HasEvents;
    property Properties:TObjectProperties read GetProperties write SetProperties
                           stored HasProperties;

    property Blocks:TBlocks read FBlocks; // read only
    property Options:TMakerOptions read FOptions write SetOptions;

    { TPanel events }
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;

    {$IFNDEF CLX}
    {$IFNDEF LCL}
    property OnCanResize;
    {$ENDIF}
    {$ENDIF}
    property OnConstrainedResize;
    {$IFNDEF CLX}
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}

    {$IFDEF K3}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ELSE}
    {$IFDEF D10}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    {$ENDIF}

    {$IFDEF D14}
    {$IFNDEF CLX}
    property OnGesture;
    {$ENDIF}
    {$ENDIF}

    property OnBeforeDraw:TNotifyEvent read FOnBeforeDraw write FOnBeforeDraw;
    property OnBlockEvent:TBlockEvent read FOnBlockEvent write FOnBlockEvent;
    property OnClickedBlock:TMouseEvent read FOnClickedBlock write FOnClickedBlock;
    property OnDoLoad:TLoadMakerEvent read FOnDoLoad write FOnDoLoad;
  end;

implementation

uses
  Math, Forms,

  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  
  {$IFDEF CLX}
  TeeGIF,
  {$ELSE}

  {$IFDEF D105}
  GIFImg,
  {$ELSE}
  GIFImage,
  {$ENDIF}

  {$IFDEF D12}
  PNGImage,
  {$ELSE}
  TeePNGImage,
  {$ENDIF}

  {$ENDIF}

  TeeGLCanvas, TeePlayMP3, TeeMakerConst;

{ TFogBlock }

Constructor TFogBlock.Create;
begin
  inherited;

  Style:=fsExp;
  FColor:=clGray;
  Density:=0.001;
  StartPos:=30;
  EndPos:=10000;
end;

procedure TFogBlock.Setup;
var tmp : GLMat;
begin
  if Enabled then
  begin
    ColorToGL(Color,tmp);
    glFogfv(GL_FOG_COLOR,@tmp); // Define the fog colour
    glFogf(GL_FOG_DENSITY,Density);                   // How dense

    case Style of
      fsLinear : glFogi(GL_FOG_MODE,GL_LINEAR);
      fsExp2   : glFogi(GL_FOG_MODE,GL_EXP2);
    else
      glFogi(GL_FOG_MODE,GL_EXP);                   // exponential decay
    end;

    glFogf(GL_FOG_START,StartPos);                   // Where wwe start fogging
    glFogf(GL_FOG_END,EndPos);                       // end

    if Fast then
       glHint(GL_FOG_HINT, GL_FASTEST)
    else
       glHint(GL_FOG_HINT, GL_NICEST);

    glEnable(GL_FOG);
  end
  else
     glDisable(GL_FOG);
end;

{ TMakerFloor }

Constructor TMakerFloor.Create(AOwner:TComponent);
begin
  inherited Create;

  IFloor:=TRectangleBlock.Create(nil);
  IFloor.Title:='Floor';
  
  FReflection:=20;

  if (not Assigned(AOwner)) or (not (csLoading in AOwner.ComponentState)) then
     InitDefaults;
end;

Destructor TMakerFloor.Destroy;
begin
  IFloor.Free;
  inherited;
end;

class function TMakerFloor.DefaultTexture:String;
begin
  result:=TeeMakerReadRegistry('Floor','FloorTexture',TeeMakerLibraryTag+'Basic\parket.bmp')
end;

procedure TMakerFloor.InitDefaults;
begin
  Format.Border.Color:=5460819;
  Size.Point.X:=13000;
  Size.Point.Z:=13000;

  with Format.Texture do
  begin
    PictureLink:=DefaultTexture;
    Scale.Point.X:=10;
    Scale.Point.Y:=10;
  end;

  with IFloor.Location.Point do
  begin
    X:= 0;
    Y:= 0;
    Z:= -150;
  end;

  IFloor.Rotation.Point.Y:=90;
  IFloor.Size.Y:=0;
end;

procedure TMakerFloor.Assign(Source: TPersistent);
begin
  if Source is TMakerFloor then
  with TMakerFloor(Source) do
  begin
    Self.FDistance:=FDistance;
    Self.FLimit:=FLimit;
    Self.FReflection:=FReflection;
    Self.IFloor.Assign(IFloor);

    Self.IFloor.Repaint;
  end
  else
    inherited;
end;

function TMakerFloor.GetFormat: TBlockFormat;
begin
  result:=IFloor.Format;
end;

function TMakerFloor.GetRotation: TRotationXYZ;
begin
  result:=IFloor.Rotation;
end;

function TMakerFloor.GetSize: TPointXYZFloat;
begin
  result:=IFloor.Size;
end;

function TMakerFloor.GetTile: TTile;
begin
  result:=IFloor.Tile;
end;

function TMakerFloor.GetVisible: Boolean;
begin
  result:=IFloor.Visible;
end;

procedure TMakerFloor.SetDistance(const Value: Integer);
begin
  FDistance := Value;
  IFloor.Repaint;
end;

procedure TMakerFloor.SetFormat(const Value: TBlockFormat);
begin
  IFloor.Format:=Value;
end;

procedure TMakerFloor.SetReflection(const Value:Byte);
begin
  FReflection:=Value;
  IFloor.Repaint;
end;

procedure TMakerFloor.SetRotation(const Value: TRotationXYZ);
begin
  IFloor.Rotation:=Value;
end;

procedure TMakerFloor.SetSize(const Value: TPointXYZFloat);
begin
  IFloor.Size:=Value;
end;

procedure TMakerFloor.SetTile(const Value: TTile);
begin
  IFloor.Tile:=Value;
end;

procedure TMakerFloor.SetVisible(const Value: Boolean);
begin
  IFloor.Visible:=Value;
end;

type
  TBlocksAccess=class(TBlocks);
  TBlockAccess=class(TCustomBlock);
  TRectangleAccess=class(TRectangleBlock);

procedure TMakerFloor.SetLimit(const Value: TMakerLimitFloor);
begin
  FLimit := Value;
  IFloor.Repaint;
end;

{ TMakerOptions }

Constructor TMakerOptions.Create(ABlocks:TBlocks);

  procedure CreateReflection;
  begin
    FFloor:=TMakerFloor.Create(IMaker);

    TRectangleAccess(FFloor.IFloor).IBlocks:=ABlocks;
    TRectangleAccess(FFloor.IFloor).ICanvas:=TGLCanvas(IMaker.Canvas);
  end;

begin
  inherited Create;

  IMaker:=TMaker(ABlocks.Parent);
  UseThreads:=False;
  CreateReflection;
  FNavigate:=TNavigate.Create(IMaker);
  Fog:=TFogBlock.Create;
  AnaglyphDistance:=1;
  ShowBoundPositions:=True;
  FClickToFocus:=True;

  FCameras:=TMakerCameras.Create(Self,TMakerCamera);
end;

type
  TGLCanvasAccess=class(TGLCanvas);

procedure TMakerOptions.DrawAnaglyph;
begin
  // Prepare
  glDrawBuffer(GL_BACK);
  glReadBuffer(GL_BACK);

//  glClear(GL_DEPTH_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
//  glBlendFunc(GL_ONE, GL_ONE);

  // Left eye
  with IMaker.View3Doptions do
       HorizOffsetFloat:=HorizOffsetFloat+AnaglyphDistance;

  TGLCanvasAccess(IMaker.Canvas).SetModelView;

  if Anaglyph=haRedCyan then
     glColorMask(GL_TRUE,GL_FALSE,GL_TRUE,GL_TRUE)
  else
     glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_TRUE);

  IMaker.FBlocks.Draw;

//  glClear(GL_DEPTH_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
//  glBlendFunc(GL_ONE, GL_ONE);

  // Right eye
  with IMaker.View3Doptions do
       HorizOffsetFloat:=HorizOffsetFloat-2*AnaglyphDistance;

  TGLCanvasAccess(IMaker.Canvas).SetModelView;

  if Anaglyph=haRedCyan then
     glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_TRUE)
  else
     glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_TRUE);

  IMaker.FBlocks.Draw;

  // Reset
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);

  with IMaker.View3Doptions do
       HorizOffsetFloat:=HorizOffsetFloat+AnaglyphDistance;

  TGLCanvasAccess(IMaker.Canvas).SetModelView;

  glDisable(GL_BLEND);
//  glClear(GL_DEPTH_BUFFER_BIT);
end;

procedure TMakerOptions.AfterDraw;
begin
  if Anaglyph<>haNone then
     DrawAnaglyph;

  FNavigate.CheckPendingInertia;
end;

procedure TMakerOptions.DrawAxes;
const
  AxisStep=100;
  AxisSize=1000;

var t,
    xx, yy, zz : Integer;
    tmpX,tmpY,tmpZ : Single;
begin
  with IMaker.Canvas do
  begin
    ResetState;

    Pen.Style:=psSolid;

    if IMaker.Current=nil then
    begin
      Pen.Color:=clWhite;

      xx:=Round(RotationCenter.X);
      yy:=Round(-RotationCenter.Z);
      zz:=Round(-RotationCenter.Y);
    end
    else
    begin
      TBlockAccess(IMaker.Current).GetRotationCenter(tmpX,tmpY,tmpZ,True);

      xx:=Round(tmpX);
      yy:=Round(tmpY);
      zz:=-Round(tmpZ);

      Pen.Color:=clNavy;
    end;

    Font.Name:='Arial';
    Font.Color:=clBlack;

    HorizLine3D(-AxisSize+xx,AxisSize+xx,zz,yy);
    Font.Size:=100;
    TextOut3D(AxisSize+xx,zz,yy,'X');

    Font.Size:=10;

    t:=-AxisSize+xx;
    while t<AxisSize+xx-AxisStep do
    begin
      Inc(t,AxisStep);
      VertLine3D(t,zz-10,zz+10,yy);
      TextOut3D(t,zz,yy,IntToStr(t));
    end;

    VertLine3D(xx,-AxisSize+zz,AxisSize+zz,yy);
    Font.Size:=100;
    TextOut3D(xx,AxisSize+zz,yy,'Z');
    TextOut3D(xx,-AxisSize-zz,yy,'Z');

    Font.Size:=10;

    t:=-AxisSize+zz;
    while t<AxisSize+zz-AxisStep do
    begin
      Inc(t,AxisStep);
      HorizLine3D(xx-10,xx+10,t,yy);
      TextOut3D(xx,t,yy,IntToStr(t));
    end;

    ZLine3D(xx,zz,-AxisSize+yy,AxisSize+yy);
    Font.Size:=100;
    TextOut3D(xx,zz,AxisSize+yy,'Y');

    Font.Size:=10;

    t:=-AxisSize+yy;
    while t<AxisSize+yy-AxisStep do
    begin
      Inc(t,AxisStep);
      VertLine3D(xx,zz-10,zz+10,t);
      TextOut3D(xx,zz,t,IntToStr(t));
    end;
  end;
end;

type
  TCustomBlockAccess=class(TCustomBlock);

procedure TMakerOptions.DrawBoundingBox(const ABlock:TCustomBlock; const AColor:TColor; DrawCoords:Boolean);

  function ToCoord(const Value:Single):String;
  begin
    result:=FormatFloat('0.###',Value);
  end;

var Min,
    Max : TPoint3DFloat;
    tmp : String;
//    t   : Integer;
    tmpSize : TSize;
begin
  with IMaker do
  if Assigned(ABlock) then
  begin
    if not ABlock.BoundingBox(Min,Max) then
       Exit;

    glPushMatrix;

    TBlockAccess(ABlock).InternalTransform;
  end
  else
  if not Blocks.CalcBounds(Min,Max) then
     Exit;

  with IMaker,TGLCanvas(Canvas) do
  begin
    Pen.Style:=psSolid;
    Pen.Color:=AColor;
    Brush.Style:=bsClear;

    if (not Assigned(ABlock)) {or (ABlock is TCustomObjectBlock)} then
       TGLCanvas(Canvas).Cube(Min.X,Max.X,-Min.Z,-Max.Z,Min.Y,Max.Y,True)
    else
    begin
      TBlockAccess(ABlock).ICanvas:=TGLCanvas(Canvas);
      TBlockAccess(ABlock).DrawSelected;
    end;

    TGLCanvas(Canvas).ResetState;

    if DrawCoords then
    begin
      Font.Name:='Arial';
      Font.Style:=[];
      Font.Size:=6;
      Font.Color:=RGB(200,200,255);
      FontExtrusion:=1; // 2.1;

      if Assigned(ABlock) {and (not (ABlock is TCustomObjectBlock))} then
         glScalef(2/(Max.X-Min.X), 2/(Max.Z-Min.Z), 2/Math.Max(1,(Max.Y-Min.Y)));

      if ShowBoundPositions then
      begin
        TextOut3DFloat(Min.X,-Min.Z,Min.Y,ToCoord(Min.X));
        TextOut3DFloat(Max.X,-Min.Z,Min.Y,ToCoord(Max.X));
        TextOut3DFloat(Max.X,-Max.Z,Min.Y,ToCoord(Min.Y));
        TextOut3DFloat(Max.X,-Max.Z,Max.Y,ToCoord(Max.Y));
        TextOut3DFloat(Min.X,-Min.Z,Max.Y,ToCoord(Min.Z));
        TextOut3DFloat(Min.X,-Max.Z,Max.Y,ToCoord(Max.Z));
      end;

      if Assigned(ABlock) then
      begin
        tmp:=TBlockAccess(ABlock).TitleOrName;

        tmpSize:=ReferenceCanvas.TextExtent(tmp);

        TextOut3DFloat((Min.X+Max.X-tmpSize.cx)*0.5,-Max.Z-tmpSize.cy*2,0.5*(Min.Y+Max.Y),tmp);

        if ABlock.Size.Y<>0 then
        begin
          glPushMatrix;
          glTranslatef(0,0,tmpSize.cx*0.5);
          glRotatef(90,0,1,0);
          TextOut3DFloat(0,-Max.Z-tmpSize.cy*3.2,0,tmp);
          glPopMatrix;
        end;

        if BoundTitle<>'' then
        begin
          Font.Color:=clRed;
          TextOut3DFloat((Min.X+Max.X-tmpSize.cx)*0.5,-Max.Z-tmpSize.cy*4,0.5*(Min.Y+Max.Y),BoundTitle);
        end;

      end;
    end;
  end;

  if Assigned(ABlock) then
     TBlockAccess(IMaker.Current).EndTransform;
end;

function TMakerOptions.IsCamerasStored:Boolean;
begin
  result:=FCameras.Count>0;
end;

procedure TMakerOptions.SetCameras(const Value:TMakerCameras);
begin
  FCameras.Assign(Value);
end;

procedure TMakerOptions.SetFloor(const Value: TMakerFloor);
begin
  FFloor.Assign(Value);
end;

procedure TMakerOptions.SetHideBorders(const Value:Boolean);
begin
  IMaker.Blocks.HideBorders:=Value;
end;

procedure TMakerOptions.Assign(Source: TPersistent);
begin
  if Source is TMakerOptions then
  with TMakerOptions(Source) do
  begin
    Self.Floor:=Floor;
    Self.FBoundingBox:=FBoundingBox;
    Self.FCameras.Assign(FCameras);
    Self.FClickToFocus:=FClickToFocus;
    Self.FDrawShadows:=FDrawShadows;
    Self.HideBorders:=HideBorders;
    Self.Navigate:=FNavigate;
    Self.FSelectMode:=FSelectMode;
    Self.FShowLightLamps:=FShowLightLamps;
    Self.UseThreads:=UseThreads;
    Self.FView3DAxes:=FView3DAxes;
  end
  else
    inherited;
end;

type
  TTeeOpenGLAccess=class(TTeeOpenGL);
  TTeeGradientAccess=class(TCustomTeeGradient);

{ TMaker }

Constructor TMaker.Create(AOwner: TComponent);
begin
  inherited;

  AutoRepaint:=False;

  View3DOptions.Perspective:=30;

  Gradient.Visible:=True;
  TTeeGradientAccess(Gradient).IDefVisible:=True;

  if (not Assigned(AOwner)) or (not (csLoading in AOwner.ComponentState)) then
  begin
    Gradient.EndColor := 13556735;
    Gradient.MidColor := 14739177;
    Gradient.StartColor := 16774122;

    with TTeeGradientAccess(Gradient) do
    begin
      IDefEnd:=EndColor;
      IDefMid:=MidColor;
      IDefStart:=StartColor;
    end;
  end;

  MarginLeft:=0;
  MarginTop:=0;
  MarginRight:=0;
  MarginBottom:=0;

  Shadow.Visible := False;
  View3DOptions.Orthogonal := False;
  BevelOuter := bvNone;
  Color := clWhite;

  FTeeOpenGL:=TTeeOpenGL.Create(Self);
  FTeeOpenGL.TeePanel:=Self;

  // Special case for TeeMaker only:
  TTeeOpenGLAccess(FTeeOpenGL).IAlwaysActive:=True;

  FTeeOpenGL.OnInit:=OpenGLInit;

  // TeeFullLightModel:=GL_TRUE;
  with FTeeOpenGL.Light0.Position do
  begin
    X:=-685;
    Y:=-1582;
    Z:=-1278;
  end;

  with FTeeOpenGL.Light1.Position do
  begin
    X:=0;
    Y:=-1582;
    Z:=-100;
  end;

  with FTeeOpenGL.Light2.Position do
  begin
    X:=685;
    Y:=-1582;
    Z:=-100;
  end;

  FTeeOpenGL.Light.Color:=clSilver;
  FTeeOpenGL.TextureQuality:=True;

  FTeeOpenGL.Active:=True;

  TGLCanvasAccess(FTeeOpenGL.Canvas).IZeroAtCenter:=False;
  TGLCanvasAccess(FTeeOpenGL.Canvas).FOnGetPicBits:=OnGetPicBits;

  FBlocks:=NewBlocks;
  FBlocks.Parent:=Self;
  FBlocks.OnRemoved:=RemovedBlock;

  FOptions:=TMakerOptions.Create(FBlocks);
  TBlocksAccess(FBlocks).IFloor:=FOptions.Floor.IFloor;

  TabStop:=True;

  AutoRepaint:=True;
end;

function TMaker.OnGetPicBits(AGraphic:TGraphic; var Bits:SysUtils.PByteArray):Boolean;
var tmpPos : PByte;
    t,tt,
    tmpW   : Integer;
    tmpH   : Integer;
    tmpLine : PRGBs;
    tmpAlpha : PByteArray;
begin
  result:=(AGraphic is
           {$IFDEF D12}TPngImage{$ELSE}{$IFDEF CLX}TBitmap{$ELSE}TPNGObject{$ENDIF}{$ENDIF})
           and AGraphic.Transparent;

  if result then
  begin
    tmpH:=AGraphic.Height;
    tmpW:=AGraphic.Width;

    GetMem(Bits,tmpH*tmpW*4);

    tmpPos:=@Bits[0];

    for t:=tmpH-1 downto 0 do
    begin
      tmpLine:={$IFDEF D12}TPngImage{$ELSE}{$IFDEF CLX}TBitmap{$ELSE}TPNGObject{$ENDIF}{$ENDIF}(AGraphic).Scanline[t];

      tmpAlpha:={$IFDEF CLX}nil{$ELSE}{$IFDEF D12}TPngImage{$ELSE}TPNGObject{$ENDIF}(AGraphic).AlphaScanline[t]{$ENDIF};

      if not Assigned(tmpAlpha) then
      begin
        for tt:=0 to tmpW-1 do
        begin
          {$IFOPT R+}
          {$DEFINE WASRANGE}
          {$R-}
          {$ENDIF}
          with tmpLine[tt] do
          {$IFDEF WASRANGE}
          {$R+}
          {$ENDIF}
          begin
            tmpPos^:=Red;
            Inc(tmpPos);

            tmpPos^:=Green;
            Inc(tmpPos);

            tmpPos^:=Blue;
            Inc(tmpPos);
          end;

          tmpPos^:=1;
          Inc(tmpPos);
        end;
      end
      else
      begin
        for tt:=0 to tmpW-1 do
        begin
          {$IFOPT R+}
          {$DEFINE WASRANGE}
          {$R-}
          {$ENDIF}

          with tmpLine[tt] do
          begin
            tmpPos^:=Red;
            Inc(tmpPos);

            tmpPos^:=Green;
            Inc(tmpPos);

            tmpPos^:=Blue;
            Inc(tmpPos);
          end;

          tmpPos^:=tmpAlpha[tt];

          {$IFDEF WASRANGE}
          {$R+}
          {$ENDIF}

          Inc(tmpPos);
        end;
      end;
    end;
  end;
end;

procedure TMaker.RemovedBlock(Sender: TObject);
begin
  if Selected=Sender then
     Selected:=nil;
end;

procedure TMaker.OpenGLInit(Sender: TObject);
var t : Integer;
begin
  FTeeOpenGL.Canvas.ScreenSync:=ssNo;  // Default is to avoid monitor VSync (speed)

  TBlocksAccess(Blocks).InitLights;

  // TODO: Replace with ILightList...
  with FBlocks do
  for t:=0 to Count-1 do
      if Block[t].Visible and (Block[t] is TLightBlock) then
         TLightBlock(Block[t]).InitLight;
end;

procedure TMaker.Loaded;
begin
  inherited;
  TBlocksAccess(Blocks).FixAnimationsLoaded;
end;

type
  TAnimatesAccess=class(TAnimates);

procedure TMaker.WriteState(Writer: TWriter);
begin
  TAnimatesAccess(TBlocksAccess(Blocks).Animates).PrepareList;
  inherited;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

Procedure TMaker.InternalDraw(Const UserRectangle:TRect);
var Old : Boolean;
    tmp : TRect;
    OldRect : TRect;
    tmpTick : Cardinal;
begin
  Old:=AutoRepaint;
  AutoRepaint:=False;

  tmp:=UserRectangle;

  RecalcWidthHeight;
  Width3D:=100;

  FOptions.CheckLimits;

  if Options.Navigate.Mode=nmExplore then
     ExploreCurrent;

  InternalCanvas.Projection(Width3D,ChartBounds,ChartRect);

  OldRect:=ChartRect;
  ChartRect:=tmp;
  PanelPaint(UserRectangle);
  ChartRect:=OldRect;

  FOptions.BeforeDraw;

  if Assigned(FOnBeforeDraw) then
     FOnBeforeDraw(Self);

  FBlocks.Draw;

  TBlocksAccess(FBlocks).DrawAfter;

  FOptions.AfterDraw;

  if RepaintMonitor then
  begin
    Inc(IFrame);

    tmpTick:=GetTickCount;
    if tmpTick-ILastTime>1000 then
    begin
      IFPS:=IFrame;
      ILastTime:=tmpTick;
      IFrame:=0;
    end;

    InternalCanvas.TextOut(0,0,IntToStr(IFPS));
  end;

  if Assigned(FOnAfterDraw) then
     FOnAfterDraw(Self);

  Assert(TGLCanvas(Canvas).CheckGLError,'AfterDraw: '+IntToStr(TGLCanvasAccess(Canvas).ISavedError));

  AutoRepaint:=Old;
end;

procedure TMaker.UpdatePosition(const ADelta:Single);
var tmpSin,
    tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  with View3DOptions do
  begin
    SinCos(RotationFloat*Pi/180,tmpSin,tmpCos);

    HorizOffsetFloat:=HorizOffsetFloat+ADelta*tmpSin;

    with FOptions.Navigate do
         ZOffset:=ZOffset+ADelta*tmpCos;
  end;
end;

{$IFDEF CLX}
function TMaker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                      {$IFNDEF UCL}const{$ENDIF} MousePos: TPoint): Boolean;
{$ELSE}
function TMaker.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
                      MousePos: TPoint): Boolean;
{$ENDIF}
var tmp : TCustomBlock;
    tmpDrag : TCustomBlock;
begin
  result:=False;

  if ssCtrl in Shift then
     WheelDelta:=WheelDelta div 10
  else
  if ssShift in Shift then
     WheelDelta:=WheelDelta*2;

  if FOptions.Navigate.FWheel=waZoom then
     result:=FOptions.Navigate.DoMouseWheel(WheelDelta)
  else
  begin
    if Assigned(IDrag.Prop) and IDrag.IsWheel then
    begin
      DoDragProp(WheelDelta div 50,WheelDelta div 50);
      result:=True;
    end
    else
    begin
      // Drag Action:
      with GetCursorPos do
           tmp:=Blocks.ClickedBlock(X,Y,True,True);

      if Assigned(tmp) then
      begin
        tmpDrag:=DoBlockAction(tmp,BlockAction_WheelDrag);

        if Assigned(tmpDrag) then
        begin
          with Options.Navigate do
          begin
            IMouseButton:=mbLeft;
            FDragging:=True;
            IDragged:=False;

            FOldX:=MousePos.X;
            FOldY:=MousePos.Y;

            IDrag.IsWheel:=True;
          end;

          result:=True;
        end;
      end;
    end;
  end;
end;

procedure TMaker.CanvasProjection(Sender: TObject);
const
  PiStep=Pi/180;

var LookCenter : TPoint3DFloat;
    tmpSin,
    tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    offX, offY : Single;
    ILookEye : TPoint3DFloat;
begin
  with View3DOptions do
  begin
    SinCos(RotationFloat*PiStep,tmpSin,tmpCos);

    offX:=HorizOffsetFloat;
    offY:=VertOffsetFloat;

    ILookEye:=PointFloat(offX,offY,ZOffset);
    LookCenter:=PointFloat(offX+10*tmpSin,offY,10*tmpCos+ZOffset);
  end;

  glRotatef(-View3DOptions.ElevationFloat, 1, 0, 0);

  gluLookAt(ILookEye.X,ILookEye.Y,ILookEye.Z,
            LookCenter.X,LookCenter.Y,LookCenter.Z,
            0,1,0);
end;

procedure TMaker.DoKeyDown(var Key: Word; Shift: TShiftState);
var tmp       : Integer;
    tmpX      : Integer;
    tmpY      : Integer;
    tmpWalking: Boolean;
    tmpRotate : Boolean;
begin
  if ssCtrl in Shift then
     tmp:=250
  else
  if ssShift in Shift then
     tmp:=10
  else
     tmp:=100;

  tmpRotate:=ssAlt in Shift;

  tmpX:=0;
  tmpY:=0;

  tmpWalking:=(Options.Navigate.Mode=nmWalk) or (Options.Navigate.Mode=nmFly);

  if tmpWalking then
     tmpRotate:=not tmpRotate;

  with View3DOptions do
  case Key of
    VK_NEXT  : if tmpRotate then
               begin
                 VertOffset:=VertOffset-tmp;
                 Options.CheckLimits;
               end
               else Tilt:=Tilt-Round(tmp*0.1);

    VK_PRIOR : if tmpRotate then
               begin
                 VertOffset:=VertOffset+tmp;
                 Options.CheckLimits;
               end
               else Tilt:=Tilt+Round(tmp*0.1);

    VK_LEFT  : if tmpRotate then tmpX:=tmp div 10
                            else HorizOffset:=HorizOffset+tmp;

    VK_RIGHT : if tmpRotate then tmpX:=-tmp div 10
                            else HorizOffset:=HorizOffset-tmp;

    VK_UP    : if tmpWalking then
                  Options.Navigate.DoMove(100)
               else
               if tmpRotate then
                  tmpY:=-tmp div 10
               else
                  ZoomFloat:=ZoomFloat*(1+(tmp*0.001));

    VK_DOWN  : if tmpWalking then
                  Options.Navigate.DoMove(-100)
               else
               if tmpRotate then
                  tmpY:=tmp div 10
               else
                  ZoomFloat:=ZoomFloat*(1-(tmp*0.001));
  end;

  if tmpRotate then
     if (tmpX<>0) or (tmpY<>0) then
        Options.Navigate.DoRotate(tmpX,tmpY,Shift);
end;

function TMaker.NewBlocks:TBlocks;
begin
  result:=TBlocks.Create(Self);

  TBlocksAccess(result).ICanvas:=TGLCanvas(Canvas);

  result.Name:='Blocks';
end;

Destructor TMaker.Destroy;
begin
  FShader.Free;
  FOptions.Free;
  FreeAndNil(FBlocks);
  inherited;
end;

procedure TMaker.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  TBlocksAccess(Blocks).GetChildren(Proc,Root);
end;

procedure TMaker.DragCanceled;
begin
  inherited;

  if Assigned(OnEndDrag) then
     OnEndDrag(Self,nil,0,0);
end;

function TMaker.SelectedClickedBlock(X,Y:Integer; SubObjects:Boolean=False):TCustomBlock;
begin
  result:=nil;

  if Options.SelectMode then
  begin
    if Assigned(Options.SelectModeBlocks) then
       result:=Options.SelectModeBlocks.ClickedBlock(X,Y,True,SubObjects)
  end
  else
    result:=Blocks.ClickedBlock(X,Y,True,SubObjects);
end;

procedure TMaker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp     : TCustomBlock;
    tmpDrag : TCustomBlock;
begin
  inherited;

  if Options.ClickToFocus then
     if not Focused then
        if CanFocus then
           SetFocus;

  if not CancelMouse then
  begin
    tmp:=SelectedClickedBlock(X,Y,True);

    if Assigned(tmp) then
    begin
      if Button=mbLeft then
         tmpDrag:=DoBlockAction(tmp,BlockAction_LeftDrag)
      else
         tmpDrag:=DoBlockAction(tmp,BlockAction_RightDrag);

      if not Assigned(tmpDrag) then
      begin
        if ssDouble in Shift then
           if Button=mbLeft then
              DoBlockAction(tmp,BlockAction_LeftDoubleClick)
           else
           if Button=mbRight then
              DoBlockAction(tmp,BlockAction_RightDoubleClick);
      end;
    end;

    with Options.Navigate do
    begin
      IMouseButton:=Button;
      FDragging:=True;
      IDragged:=False;
      IDrag.IsWheel:=False;

      FOldX:=X;
      FOldY:=Y;
    end;
  end;

  //SetCaptureControl(Self);
end;

procedure TMaker.DoDragProp(DeltaX,DeltaY:Integer);

  function GetDragValue(const Value:Variant; ADelta:Integer):Variant;
  begin
    if IDrag.Invert then
       result:=Value-ADelta
    else
       result:=Value+ADelta;

    if IDrag.CheckMinMax then
      if IDrag.HasMin and (result<IDrag.MinDrag) then
         result:=IDrag.MinDrag
      else
      if IDrag.HasMax and (result>IDrag.MaxDrag) then
         result:=IDrag.MaxDrag
  end;

var tmp : Variant;
    tmpObject : TObject;
begin
  Options.Navigate.FDragging:=True;

  if PropIsType(IDrag.Target,String(IDrag.Prop.Name),tkClass) then
  begin
    tmpObject:=GetObjectProp(IDrag.Target,IDrag.Prop{$IFNDEF D9}.Name{$ENDIF});

    if tmpObject is TPointXYZFloat then
    begin
      with TPointXYZFloat(tmpObject) do
      begin
        X:=GetDragValue(X,DeltaX);
        Y:=GetDragValue(Y,-DeltaY);
      end;
    end
    else
    if tmpObject is TPointXYFloat then
    begin
      with TPointXYFloat(tmpObject) do
      begin
        X:=GetDragValue(X,DeltaX);
        Y:=GetDragValue(Y,-DeltaY);
      end;
    end
  end
  else
  begin
    tmp:=GetPropValue(IDrag.Target,IDrag.Prop{$IFNDEF D9}.Name{$ENDIF});
    tmp:=GetDragValue(tmp,DeltaX+DeltaY);
    SetPropValue(IDrag.Target,IDrag.Prop{$IFNDEF D9}.Name{$ENDIF},tmp);

    if Assigned(IDrag.Source) and (IDrag.Action<>'') then
       DoSingleAction(IDrag.Source,IDrag.Action);
  end;

  if Assigned(IDrag.Source) then
     TBlockAccess(IDrag.Source).Dragged;

  CurrentOver:=nil;
end;

procedure TMaker.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure DoRuntimeMode;
  var tmpBlock : TCustomBlock;
  begin
    if Assigned(IDrag.Prop) and (not IDrag.IsWheel) then
    begin
      DoDragProp((X-OldPos.X),(Y-OldPos.Y));

      OldPos.X:=X;
      OldPos.Y:=Y;

      CancelMouse:=True;
    end
    else
    begin
      tmpBlock:=SelectedClickedBlock(X,Y);

      if tmpBlock<>BlockUnderMouse then
      begin
        if Assigned(tmpBlock) then
           Cursor:=tmpBlock.Cursor
        else
           Cursor:=crDefault;
      end;

      if Assigned(tmpBlock) then
      begin
        //Cursor:=tmpBlock.Cursor;
        BlockUnderMouse:=tmpBlock;
      end
      else
      begin
        //Cursor:=crDefault;
        BlockUnderMouse:=nil;
      end;
    end;
  end;

begin
  TGLCanvasAccess(Render.Canvas).CheckContext;

  inherited;

  if {(not Options.SelectMode) and} (not Options.Navigate.IMovingRotating) and (not CancelMouse) then
     DoRuntimeMode;

  if (not CancelMouse) and Options.Navigate.FDragging then
     Options.Navigate.DoMouseMove(X,Y,Shift);
end;

procedure TMaker.DoContextPopup({$IFDEF CLX}const {$ENDIF}MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=Options.Navigate.IsPanning;
  inherited;
end;

procedure TMaker.DoInitDrag(X,Y:Integer);
begin
  CurrentOver:=nil;
  OldPos.X:=X;
  OldPos.Y:=Y;
  IDrag.PreDragging:=True;
  CancelMouse:=True;
end;

function TMaker.BlockParentWithAction(const AEvent:String; ABlock:TCustomBlock):TCustomBlock;
begin
  result:=ABlock;

  repeat
    if TBlockAccess(result).HasActions(AEvent) then
       break
    else
       result:=TBlocksAccess(result.Parent).IObject;

  until not Assigned(result);
end;

procedure TMaker.ProcessEvent(const ABlock:TCustomBlock; Action:String);
begin
  Action:=Trim(Action); // (double "::"?)

  if Copy(Action,1,1)=':' then
     Delete(Action,1,1);

  if Action='' then
  begin
    if Assigned(FOnBlockEvent) then
       FOnBlockEvent(ABlock,Action);
  end
  else
     DoBlockAction(ABlock,Action);
end;

  function GetPropTarget(AExp:String; ASource:TObject; var AObject,ATarget:TObject;
               var AProp:PPropInfo;
               out APropName:String):Boolean;
  var i : Integer;
      tmpProp,
      tmpName : String;
  begin
    result:=False;
    APropName:='';

    i:=Pos('.',AExp);

    if i>0 then
    begin
      tmpName:=Copy(AExp,1,i-1);

      if ASource is TCustomBlock then
      begin
        ATarget:=TBlocksAccess(TCustomBlock(ASource).Parent).DoFindName(tmpName);

        if not Assigned(ATarget) then
           if ASource is TCustomObjectBlock then
              ATarget:=TBlocksAccess(TCustomObjectBlock(ASource).Items).DoFindName(tmpName);
      end
      else
         ATarget:=nil;

      if not Assigned(ATarget) then
      begin
        ATarget:=ASource;
        //tmpName:='';
      end;
    end
    else
      ATarget:=ASource;

    if Assigned(ATarget) then
    begin
      if tmpName<>'' then
         tmpName:=tmpName+'.';

      tmpProp:=tmpName+Copy(AExp,i+1,Length(AExp));

      AObject:=ATarget;

      AProp:=TPropertyAnimation.Fixup(ATarget,tmpProp);

      if not Assigned(AProp) then
         if tmpProp<>'' then
            AProp:=GetPropInfo(ATarget,tmpProp);

      result:=Assigned(AProp);

      APropName:=tmpProp;
    end;
  end;

  function PropertyValue(AObject:TObject; AExp:String):Double;
  var tmpTarget : TObject;
      tmpSource : TObject;
      tmpProp   : PPropInfo;
      tmpSt     : String;
      tmpPropName : String;
  begin
    if AObject is TCustomObjectBlock then
    begin
      tmpSt:=TCustomObjectBlock(AObject).Properties.PropertyValue(AExp);

      if tmpSt<>'' then
         AExp:=tmpSt
      else
      begin
        tmpSt:=TCustomObjectBlock(AObject).Items.Properties.PropertyValue(AExp);

        if tmpSt<>'' then
           AExp:=tmpSt
      end;
    end
    else
    if (AObject is TCustomBlock) and Assigned(TBlocksAccess(TCustomBlock(AObject).Parent).IObject) then
    begin
      tmpSt:=TBlocksAccess(TCustomBlock(AObject).Parent).IObject.Properties.PropertyValue(AExp);
      if tmpSt<>'' then
         AExp:=tmpSt;
    end
    else
    begin
      tmpSt:=TBlocksAccess(TCustomBlock(AObject).Parent).Properties.PropertyValue(AExp);
      if tmpSt<>'' then
         AExp:=tmpSt;
    end;

    if GetPropTarget(AExp,AObject,tmpSource,tmpTarget,tmpProp,tmpPropName) then
       result:=GetPropValue(tmpTarget,tmpPropName)
    else
    if Assigned(tmpTarget) and (tmpTarget is TCustomObjectBlock) then
       result:=PropertyValue(tmpTarget,tmpPropName)
    else
       result:=StrToFloat(AExp);
  end;

type
  TTeeAnimateAccess=class(TTeeAnimate);

procedure TMaker.DoSingleAction(const ABlock:TCustomBlock; AAction:String);

  procedure PlayAnimation(const AName:String);

    function GetDefaultBlocks:TBlocks;
    begin
      if Assigned(ABlock) then
      begin
        result:=ABlock.Parent;

        while not result.HasAnimations do
        if Assigned(TBlocksAccess(result).IObject) then
        begin
          result:=TBlocksAccess(result).IObject.Parent;

          if not Assigned(result) then
             break;
        end
        else
          break;
      end
      else
        result:=Blocks;
    end;

  var i : Integer;
      tmpBlock  : TCustomBlock;
      tmpObject : TBlocks;
      tmpOb,
      tmpName   : String;
      tmpItem   : TAnimateItem;
  begin
    i:=Pos(':',AName);

    if i=0 then
    begin
      tmpObject:=GetDefaultBlocks;
      tmpName:=AName;
    end
    else
    begin
      tmpOb:=Trim(Copy(AName,1,i-1));

      if tmpOb='' then
         tmpObject:=GetDefaultBlocks
      else
      begin
        tmpBlock:=TBlocksAccess(ABlock.Parent).DoFindName(tmpOb);

        if Assigned(tmpBlock) and (tmpBlock is TCustomObjectBlock) then
           tmpObject:=TCustomObjectBlock(tmpBlock).Items
        else
           tmpObject:=nil;
      end;

      tmpName:=Copy(AName,i+1,Length(AName));

      if Copy(tmpName,1,1)='"' then
         Delete(tmpName,1,1);

      if Copy(tmpName,Length(tmpName),1)='"' then
         Delete(tmpName,Length(tmpName),1);
    end;

    if Assigned(tmpObject) then
    begin
      tmpItem:=tmpObject.Animates.IndexOf(tmpName);

      if Assigned(tmpItem) then
         tmpItem.Animate.Play;
    end;
  end;

  procedure DragProperty(AExp:String);

    procedure ParseMinMax(const ASource:TObject; AExp:String);
    var i2 : Integer;
        tmpValue : String;
    begin
      i2:=Pos(';',AExp);

      if i2>0 then
      begin
        tmpValue:=Trim(Copy(AExp,1,i2-1));
        Delete(AExp,1,i2);
      end
      else
      begin
        tmpValue:=Trim(AExp);
        AExp:='';
      end;

      IDrag.HasMin:=(tmpValue<>'');

      if IDrag.HasMin then
         IDrag.MinDrag:=PropertyValue(ASource,tmpValue);


      i2:=Pos(',',AExp);

      if i2=0 then
         tmpValue:=Trim(AExp)
      else
         tmpValue:=Trim(Copy(AExp,1,i2-1));

      IDrag.HasMax:=(tmpValue<>'');

      if IDrag.HasMax then
         IDrag.MaxDrag:=PropertyValue(ASource,tmpValue);
    end;

  const
    DragInvert =',INVERT';
    DragAction =',ACTION:';
    DragMinMax =',MINMAX:';

  var i,i2 : Integer;
      tmpMinMax,
      tmpProp : String;
      ASource : TObject;
  begin
    AExp:=Trim(AExp);

    i:=Pos(DragInvert,UpperCase(AExp));

    if i>0 then
    begin
      IDrag.Invert:=True;
      Delete(AExp,i,Length(DragInvert));
    end
    else
      IDrag.Invert:=False;

    i:=Pos(DragMinMax,UpperCase(AExp));

    IDrag.CheckMinMax:=(i>0);

    if i>0 then
    begin
      Delete(AExp,i,Length(DragMinMax));

      i2:=Pos(DragAction,UpperCase(AExp));
      if i2=0 then
      begin
        tmpMinMax:=Copy(AExp,i,Length(AExp));
        Delete(AExp,i,Length(AExp));
      end
      else
      begin
        tmpMinMax:=Copy(AExp,i,i2-i);
        Delete(AExp,i,i2-i);
      end;
    end;

    i:=Pos(DragAction,UpperCase(AExp));
    if i>0 then
    begin
      Delete(AExp,i,Length(DragAction));
      IDrag.Action:=Copy(AExp,i,Length(AExp));
      Delete(AExp,i,Length(AExp));
    end;

    if GetPropTarget(AExp,ABlock,ASource,IDrag.Target,IDrag.Prop,tmpProp) then
    begin
      if IDrag.CheckMinMax then
         ParseMinMax(ABlock,tmpMinMax);

      IDrag.Source:=ABlock;

      with GetCursorPos do
           DoInitDrag(X,Y);

      Cursor:=crHandPoint;
      OriginalCursor:=crHandPoint;
      CancelMouse:=True;
    end;
  end;

  procedure ProcessAction(Action:String);
  var i : Integer;
      tmpEvent : String;
      tmpBlock : TCustomBlock;
  begin
    Action:=Trim(Action);

    i:=LastPosOf('.',Action);

    if i>0 then
    begin
      tmpEvent:=Trim(Copy(Action,i+1,Length(Action)));
      Action:=Trim(Copy(Action,1,i-1));
    end
    else
      tmpEvent:='';

    tmpBlock:=TBlocksAccess(ABlock.Parent).DoFindName(Action);

    if Assigned(tmpBlock) then
       DoBlockAction(tmpBlock,tmpEvent);
  end;

  procedure ProcessDelay(Action:String);
  var tmpDelay : Cardinal;
      t1       : Cardinal;
  begin
    tmpDelay:=Round(PropertyValue(ABlock,Action));

    // Replace with a Timer with "no Repeat" action?

    t1:=GetTickCount+Cardinal(tmpDelay);

    while GetTickCount<t1 do
    begin
      Application.ProcessMessages;

      if Application.Terminated then
         break;
    end;
  end;

  procedure TryLoadFile(FileName,Original:String);
  var tmp : String;
  begin
    // Try to load file:
    tmp:=ExtractFileExt(FileName);

    if tmp=UpperCase(TeeMakerExtension) then
       IBlockToLoad:=Original
    else
       if TeeIsURL(Original) then
          TeeGotoURL(Handle,Original)
       else
       if FileExists(Original+TeeMakerExtension) then
          IBlockToLoad:=Original;
  end;

  procedure ProcessTimer(Exp:String);
  var Interval : Cardinal;
      tmpRepeat : Boolean;
      tmpInterval : String;
      tmpAction : String;
      i         : Integer;
      tmp       : TMultiTimer;
  begin
    i:=Pos(',',Exp);
    if i>0 then
    begin
      tmpInterval:=Copy(Exp,1,i-1);
      Interval:=Round(PropertyValue(ABlock,tmpInterval));
      Delete(Exp,1,i);

      tmpInterval:=tmpInterval+',';
    end
    else
    begin
      Interval:=1000;
      tmpInterval:='';
    end;

    tmpRepeat:=UpperCase(Copy(Exp,1,6))='REPEAT';

    if tmpRepeat then
       Delete(Exp,1,6);

    if Copy(Exp,1,1)=',' then
       Delete(Exp,1,1);

    if Copy(Exp,1,7)='ACTION:' then
    begin
      tmpAction:=Copy(Exp,8,Length(Exp));
    end
    else
      tmpAction:='';

    tmp:=TTeeAnimateAccess(TTeeAnimate).GlobalTimer;
    tmp.AddEvent(ProcessTimerAction,nil,Interval,tmpRepeat,ABlock,tmpInterval+tmpAction);
  end;

var tmp : String;
begin
  tmp:=UpperCase(Trim(AAction));

  case TBlockActions.ActionToIndex(tmp) of
    0: PlayAnimation(Copy(tmp,11,Length(tmp)));
    1: ProcessAction(Copy(tmp,8,Length(tmp)));
    2: ProcessEvent(ABlock,Copy(tmp,7,Length(tmp)));
    3: TBlocksAccess(ABlock.Parent).DoSetProperty(ABlock,Copy(tmp,5,Length(tmp)));
    4: DragProperty(Copy(tmp,6,Length(tmp)));
    5: TPlayMP3Sound.PlayFile(Copy(tmp,7,Length(tmp)));
    6: ProcessDelay(Copy(tmp,7,Length(tmp)));
    7: ABlock.Repaint;
    8: ProcessTimer(Copy(tmp,7,Length(tmp)));
  else
    TryLoadFile(tmp,AAction);
  end;
end;

procedure TMaker.ProcessTimerAction(Sender:TObject);
var tmpExp : String;
    tmpInterval : String;
    i : Integer;
    tmp : Cardinal;
begin
  with PTimerEvent(Sender)^ do
  begin
    if (FData<>'') and (FSource is TCustomBlock) then
    begin
      tmpExp:=FData;

      i:=Pos(',',tmpExp);
      if i>0 then
      begin
        tmpInterval:=Copy(tmpExp,1,i-1);

        tmp:=Max(1,Round(PropertyValue(FSource,tmpInterval)));

        if tmp<>FInterval then
        begin
          FInterval:=tmp;
          FOldTick:=0;
        end;

        Delete(tmpExp,1,i);
      end;

      DoSingleAction(TCustomBlock(FSource),tmpExp);
    end;
  end;
end;

function TMaker.DoBlockAction(const ABlock:TCustomBlock; AEvent:String):TCustomBlock;

  procedure DoABlock(const ABlock:TCustomBlock);
  var t : Integer;
  begin
    IBlockToLoad:='';

    with ABlock.Actions.OfEvent(AEvent).Actions do
    for t:=0 to Count-1 do
        DoSingleAction(ABlock,Strings[t]);
  end;

  procedure DoToLoad;
  begin
    // If some action sets a new file to load, do load it:
    if IBlockToLoad<>'' then
       if Assigned(FOnDoLoad) then
          FOnDoLoad(Self,IBlockToLoad)
       else
       begin
         if TeeIsURL(IBlockToLoad) then
            Blocks.LoadFromURL(IBlockToLoad)
         else
            Blocks.LoadFromFile(IBlockToLoad);
       end;
  end;

begin
  // Traverse hierarchy up...
  result:=BlockParentWithAction(AEvent,ABlock);

  if Assigned(result) then
  begin
    DoABlock(result);
    DoToLoad;
  end;
end;

function TMaker.ParentIsSelected(ABlock:TCustomBlock):Boolean;
var tmp : TBlocks;
begin
  result:=False;

  repeat
    tmp:=ABlock.Parent;

    if tmp=Options.SelectModeBlocks then
    begin
      result:=True;
      break;
    end
    else
    if Assigned(tmp) then
       ABlock:=TBlocksAccess(tmp).IObject;

  until not Assigned(ABlock);
end;

procedure TMaker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp : TCustomBlock;
begin
  inherited;

  CancelMouse:=IDrag.PreDragging and Options.Navigate.IDragged;

  with Options.Navigate do
  begin
    if IMovingRotating or IDragged then
    begin
      if FDragging and HasPendingInertia then
         Self.Invalidate;
    end
    else
    if not Self.CancelMouse then
    begin
      tmp:=Blocks.ClickedBlock(X,Y,True);

      if Assigned(tmp) then
      begin
        if Options.SelectMode and (not ParentIsSelected(tmp)) then
        begin
          if Assigned(tmp.OnClick) then
             tmp.OnClick(tmp,Button,Shift,X,Y);

          if Assigned(FOnClickedBlock) then
             FOnClickedBlock(tmp,Button,Shift,X,Y);
        end
        else
        if not Self.CancelMouse then
           if Button=mbLeft then
              DoBlockAction(TCustomBlock(tmp),BlockAction_LeftClick)
           else
           if Button=mbRight then
              DoBlockAction(TCustomBlock(tmp),BlockAction_RightClick);
      end;
    end;

    IMovingRotating:=False;
    FDragging:=False;
    IDragged:=False;
  end;

  // Dragging

  if Assigned(IDrag.Prop) then
  begin
    Cursor:=crDefault;
    OriginalCursor:=crDefault;
  end;

  // Block drag

  IDrag.PreDragging:=False;

  IDrag.Target:=nil;
  IDrag.Source:=nil;
  IDrag.Prop:=nil;
  IDrag.Invert:=False;
  IDrag.Action:='';

  CurrentOver:=nil;
  
  Options.Navigate.FDragging:=False;
  Options.BoundTitle:='';

  //SetCaptureControl(nil);
end;

procedure TMaker.DrawPanelBevels(Rect: TRect);
begin
end;

// Joystick
{$IFNDEF CLX}
procedure TMaker.JoyButtonDown(var Message: TMessage);
var tmpX : Integer;
    tmpY : Integer;
begin
  with Options.Navigate.Joystick1.Position do
  begin
    tmpX:=Round(X);
    tmpY:=Round(Y);
  end;

  if (Message.WParam and JOY_BUTTON1)=JOY_BUTTON1 then
     MouseDown(mbLeft,[],tmpX,tmpY)
  else
  if (Message.WParam and JOY_BUTTON2)=JOY_BUTTON2 then
     MouseDown(mbRight,[],tmpX,tmpY)
  else
     MouseDown(mbMiddle,[],tmpX,tmpY);
end;

procedure TMaker.JoyButtonUp(var Message: TMessage);
var tmpX : Integer;
    tmpY : Integer;
begin
  with Options.Navigate.Joystick1.Position do
  begin
    tmpX:=Round(X);
    tmpY:=Round(Y);
  end;

  if (Message.WParam and JOY_BUTTON1CHG)=JOY_BUTTON1CHG then
     MouseUp(mbLeft,[],tmpX,tmpY)
  else
  if (Message.WParam and JOY_BUTTON2CHG)=JOY_BUTTON2CHG then
     MouseUp(mbRight,[],tmpX,tmpY)
  else
     MouseUp(mbMiddle,[],tmpX,tmpY);
end;

procedure TMaker.JoyMove(var Message: TMessage);
var tmpX : Integer;
    tmpY : Integer;
    DifX : Double;
    DifY : Double;
begin
  with Options.Navigate.Joystick1,Position do
  begin
    CheckCaps;

    DifX:=(2*((1+Message.LParamLo)/(1+ICaps.wXmax))-1);
    DifY:=(2*((1+Message.LParamHi)/(1+ICaps.wYmax))-1);

    X:=X+DifX;
    Y:=Y+DifY;

    tmpX:=Round(X);
    tmpY:=Round(Y);

    if (tmpX<>0) or (tmpY<>0) then
       MouseMove([],tmpX,tmpY);
  end;
end;

procedure TMaker.JoyZMove(var Message: TMessage);
begin
  with Options.Navigate.Joystick1 do
  begin
    CheckCaps;

    Position.Z:=-(Message.LParamLo/ICaps.wZmax)*2+1;
  end;
end;
{$ENDIF}

procedure TMaker.ExploreCurrent;
var tmpX : Single;
    tmpY : Single;
    tmpZ : Single;
begin
  with Canvas do
  if Assigned(Current) then
  begin
    TBlockAccess(Current).GetRotationCenter(tmpX,tmpY,tmpZ,True);
    RotationCenter.X:=tmpX;
    RotationCenter.Y:=-tmpZ;
    RotationCenter.Z:=-tmpY;

    if Assigned(View3DOptions) then
    with View3DOptions do
    begin
      HorizOffsetFloat:=-tmpX;
      VertOffsetFloat:=tmpZ;
      ZOffset:=tmpY;
    end;
  end
  else
  begin
    RotationCenter.X:=0;
    RotationCenter.Y:=0;
    RotationCenter.Z:=0;
  end;
end;

procedure TMaker.SetSelected(const Value: TCustomBlock);
begin
  if Current<>Value then
  begin
    if Assigned(Current) then
       Current.RemoveFreeNotification(Self);

    Current:=Value;

    if Assigned(Current) then
       Current.FreeNotification(Self);
       
    Invalidate;
  end;
end;

procedure TMaker.SetOptions(const Value: TMakerOptions);
begin
  FOptions.Assign(Value);
end;

procedure TMaker.DeleteAllLists;
begin
  if Assigned(Blocks) then
     TBlocksAccess(Blocks).DeleteLists;

  with Options do
  if Assigned(FFloor) then
     TRectangleAccess(FFloor.IFloor).DeleteLists;
end;

{$IFDEF CLX}
Procedure TMaker.SetParent(const AParent: TWidgetControl);
{$ELSE}
Procedure TMaker.SetParent(AParent: TWinControl);
{$ENDIF}
var tmp : Boolean;
    Old : Boolean;
begin
  if Parent<>AParent then
  begin
    inherited;

    if not (csDestroying in ComponentState) then
    begin
      Old:=AutoRepaint;
      AutoRepaint:=False;

      DeleteAllLists;

      // Trick: Force deleting all cached GL Fonts and Textures
      tmp:=Canvas.UseBuffer;
      Canvas.UseBuffer:=not tmp;
      Canvas.UseBuffer:=tmp;

      AutoRepaint:=Old;
    end;
  end;
end;

(*
procedure TMaker.CollisionEvent(Sender:TObject; const ActionID:String;
                                                const ABlock:TCustomBlock;
                                              var ACollided:TCustomBlock;
                                              var APoint:TPoint3DFloat);
var tmp : TCustomBlock;
begin
  tmp:=BlockParentWithAction(ActionID,ABlock);

  if Assigned(tmp) then
     DoBlockAction(ABlock,ActionID);
end;
*)

procedure TMaker.CMBlockBirth(var Message: TMessage);
var tmp : TCustomBlock;
begin
  tmp:=BlockParentWithAction(BlockAction_LifeBirth,TCustomBlock(Message.WParam));

  if Assigned(tmp) and (not Options.SelectMode) then
     DoBlockAction(TCustomBlock(Message.WParam),BlockAction_LifeBirth);
end;

procedure TMaker.Assign(Source: TPersistent);
begin
  if Source is TMaker then
  with TMaker(Source) do
  begin
    Self.Options:=Options;

    // Self.Render:=Render;
  end;

  inherited;
end;

procedure TMaker.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(AComponent) then
  begin
    if AComponent=FUnderMouse then
       FUnderMouse:=nil;

    if AComponent=FCurrentOver then
       FCurrentOver:=nil;

    if AComponent=Current then
       Current:=nil;
  end;
end;

procedure TMaker.SetCurrentOver(const Value: TCustomBlock);
begin
  if FCurrentOver<>Value then
  begin
    if Assigned(FCurrentOver) then
       FCurrentOver.RemoveFreeNotification(Self);

    FCurrentOver:=Value;

    if Assigned(FCurrentOver) then
       FCurrentOver.FreeNotification(Self);
  end;
end;

procedure TMaker.SetUnderMouse(const Value: TCustomBlock);
//var tmp : TCustomBlock;
begin
 { if Assigned(Value) then
     if Options.SelectMode then
     begin
       if Assigned(Options.SelectModeBlocks) and
          (Value.Parent=Options.SelectModeBlocks) then
            tmp:=Value
       else
          tmp:=nil
     end
     else
       tmp:=BlockParentWithAction(BlockAction_MouseEnter,Value)
  else
     tmp:=nil;}


  if FUnderMouse<>Value then
  begin
    if Assigned(FUnderMouse) and (not (csDestroying in FUnderMouse.ComponentState)) then
    begin
      FUnderMouse.RemoveFreeNotification(Self);

      if (not Options.SelectMode) or ParentIsSelected(FUnderMouse) then
         DoBlockAction(FUnderMouse,BlockAction_MouseExit);
    end;

    FUnderMouse:=Value;

    if Assigned(FUnderMouse) then
    begin
      FUnderMouse.FreeNotification(Self);

      if (not Options.SelectMode) or ParentIsSelected(FUnderMouse) then
         DoBlockAction(BlockUnderMouse,BlockAction_MouseEnter);
    end;
  end;
end;

// Redirected properties --> Blocks...

function TMaker.GetAnimates: TAnimates;
begin
  result:=Blocks.Animates;
end;

function TMaker.GetEvents: TStrings;
begin
  result:=Blocks.Events;
end;

function TMaker.GetProperties: TObjectProperties;
begin
  result:=Blocks.Properties;
end;

function TMaker.HasEvents: Boolean;
begin
  result:=TBlocksAccess(Blocks).HasEvents;
end;

function TMaker.HasProperties: Boolean;
begin
  result:=Blocks.HasProperties;
end;

function TMaker.IsAnimatesStored: Boolean;
begin
  result:=TBlocksAccess(Blocks).IsAnimatesStored;
end;

procedure TMaker.SetAnimates(const Value: TAnimates);
begin
  Blocks.Animates:=Value;
end;

procedure TMaker.SetEvents(const Value: TStrings);
begin
  Blocks.Events:=Value;
end;

procedure TMaker.SetProperties(const Value: TObjectProperties);
begin
  Blocks.Properties:=Value;
end;

{ TNavigate }

Constructor TNavigate.Create(AOwner:TMaker);
begin
  inherited Create;

  IOwner:=AOwner;

  AllowRotationX:=True;
  AllowRotationY:=True;

  FWheel:=waZoom;
  FInertia:=30;

  WalkSpeed:=100;
end;

function TNavigate.IsWalkSpeedStored:Boolean;
begin
  result:=WalkSpeed<>100;
end;

function TNavigate.IsPanning:Boolean;
begin
  result:=FDragging and IDragged and (IMouseButton=IOwner.ScrollMouseButton);
end;

function TNavigate.GetJoy1:TJoystick;
begin
  if not Assigned(FJoy1) then
     FJoy1:=TJoystick.Create(IOwner,JOYSTICKID1);

  result:=FJoy1;
end;

procedure TNavigate.TimerFly(Sender:TObject);
begin
  if FlySpeed<>0 then
  begin
    IOwner.UpdatePosition(FlySpeed);
    ForceInvalidate;
  end;
end;

function TNavigate.MouseToLocation(const AX,AY:Single):TPoint3DFloat;
var viewport   : THomogeneousIntVector;
    mvmatrix,
    projmatrix : THomogeneousDblMatrix;
    AZ         : Single;
begin
  glGetIntegerv(GL_VIEWPORT, @viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, @mvmatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);

  with result do
  begin
    //TGLCanvasAccess(IOwner.Canvas).CheckContext;

    if glIsEnabled(GL_DEPTH_TEST)=GL_TRUE then
    begin
      glReadBuffer(GL_BACK);
      glReadPixels(Round(AX), Round(viewport[3]-AY-1), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @AZ);
      Assert(TGLCanvas(IOwner.Canvas).CheckGLError,'ReadPixels: '+IntToStr(TGLCanvasAccess(IOwner.Canvas).ISavedError));

      if gluUnProject(AX,viewport[3]-AY-1,AZ,mvmatrix,projmatrix,viewport,@X,@Y,@Z)=GL_TRUE then
      begin
        SwapSingle(Y,Z);
        Y:=-Y;
      end
      else
      begin
        X:=0;
        Y:=0;
        Z:=0;
      end;
    end
    else
    begin
      X:=0;
      Y:=0;
      Z:=0;
    end;

  end;
end;

function TNavigate.FlyEnabled(const Sender:TTimerEvent):Boolean;
begin
  result:=FlySpeed<>0;
end;

procedure TNavigate.SetMode(const Value: TNavigateMode);
var tmp : TCanvas3D;
begin
  if FMode<>Value then
  begin
    FMode:=Value;

    tmp:=IOwner.Canvas;

    if FMode<>nmExplore then
    with tmp do
    begin
      RotationCenter.X:=0;
      RotationCenter.Y:=0;
      RotationCenter.Z:=0;
    end;

    with TGLCanvasAccess(tmp) do
    if (Self.FMode=nmObserve) or (Self.FMode=nmExplore) then
    begin
      FOnProjection:=nil;
      IOwner.SetSelected(IOwner.Current);
    end
    else
      FOnProjection:=IOwner.CanvasProjection;

    FlySpeed:=0;

    if FMode=nmFly then
       TTeeAnimateAccess(TTeeAnimate).GlobalTimer.AddEvent(TimerFly,FlyEnabled,1)
    else
       TTeeAnimateAccess(TTeeAnimate).GlobalTimer.RemoveEvent(TimerFly);

    IOwner.Invalidate;
  end;
end;

const
  InertiaLevel = 0.01;

function TNavigate.HasPendingInertia:Boolean;
begin
  result:=(FInertia>0) and
          ((Abs(IDifX)>InertiaLevel) or (Abs(IDifY)>InertiaLevel));
end;

procedure TNavigate.CheckPendingInertia;

  function ChangeInertia:Boolean;
  var tmp : Single;
  begin
    result:=HasPendingInertia;

    if result then
    begin
      tmp:=(100-FInertia)*0.001;

      if Abs(IDifX)>InertiaLevel then
         IDifX:=IDifX-(IDifX*tmp)
      else
         IDifX:=0;

      if Abs(IDifY)>InertiaLevel then
         IDifY:=IDifY-(IDifY*tmp)
      else
         IDifY:=0;
    end;
  end;

begin
  if ChangeInertia then
  begin
    DoRotation(IDifX,IDifY);
    ForceInvalidate;
  end;
end;

procedure TNavigate.ForceInvalidate;
var Old : Boolean;
begin
  with IOwner do
  begin
    Old:=AutoRepaint;
    AutoRepaint:=True;
    Invalidate;
    AutoRepaint:=Old;
  end;
end;

Procedure TNavigate.DoRotation(const IDifX,IDifY:Double);

  Function CorrectAngle(Const AAngle:Double):Double;
  begin
    result:=AAngle;

    if result>360 then result:=result-360
    else
    if result<0 then result:=360+result;
  end;

begin
  with IOwner.View3DOptions do
  begin
    if ssShift in Self.IShift then
       Tilt:=Round(CorrectAngle(Tilt+IDifY))
    else
    begin
      RotationFloat:=CorrectAngle(RotationFloat+IDifX);
      ElevationFloat:=CorrectAngle(ElevationFloat+IDifY);
    end;
  end;
end;

procedure TNavigate.DoRotate(X,Y:Integer; Shift:TShiftState);
var tmpStep : Double;
begin
  if ssCtrl in Shift then
     tmpStep:=9
  else
     tmpStep:=30;

  with IOwner,View3DOptions do
  begin
    if Zoom>1 then
       tmpStep:=Ln(Zoom)*tmpStep;

    if AllowRotationX then
       IDifX:=tmpStep*X/Width
    else
       IDifX:=0;

    if AllowRotationY then
       IDifY:=tmpStep*Y/Height
    else
       IDifY:=0;
  end;

  IShift:=Shift;

  DoRotation(IDifX,IDifY);
end;

Procedure TNavigate.DoMouseMove(X,Y:Integer; Shift:TShiftState);

  Procedure DoMove;
  var tmpStep : Double;
  begin
    if ssCtrl in Shift then
       tmpStep:=0.1
    else
       tmpStep:=1;

    with IOwner.View3DOptions do
    begin
      HorizOffsetFloat:=HorizOffsetFloat+((X-FOldX)*tmpStep);
      VertOffsetFloat:=VertOffsetFloat+((Y-FOldY)*tmpStep);
    end;
  end;

begin
  if FDragging then
  begin
    FDragging:=False;
    IMovingRotating:=False;

    if Assigned(FJoy1) and FJoy1.FActive then
    begin
      DoMouseWheel(Y);
      Y:=FOldY;
    end;

    if IMouseButton=mbRight then
       DoMove
    else
    if IOwner.View3D then
       DoRotate(X-FOldX,FOldY-Y,Shift);

    IMovingRotating:=True;

    FOldX:=X;
    FOldY:=Y;

    FDragging:=True;
    IDragged:=True;
  end;
end;

procedure TNavigate.DoMove(const MoveDelta:Double);
var tmp : Double;
begin
  tmp:=MoveDelta;

  if Mode=nmFly then
  begin
    FlySpeed:=FlySpeed+tmp*0.005;

    if Abs(FlySpeed)<0.00001 then
       FlySpeed:=0;

    tmp:=FlySpeed;

    TTeeAnimateAccess(TTeeAnimate).GlobalTimer.TryToEnable;
  end;

  IOwner.UpdatePosition(tmp);
  IOwner.Invalidate;
end;

function TNavigate.DoMouseWheel(WheelDelta: Integer):Boolean;
const
  WheelStep = 1/(WHEEL_DELTA*10);

var tmp : Double;
begin
  result:=FWheel=waZoom;

  if result then
     if (Mode=nmWalk) or (Mode=nmFly) then
        DoMove(300*(WheelDelta*WheelStep))
     else
     with IOwner.View3DOptions do
     begin
       tmp:=ZoomFloat*WheelDelta*WheelStep;

       if (tmp>0) or (ZoomFloat>=0.1) then
          ZoomFloat:=ZoomFloat+tmp;
     end;
end;

procedure TNavigate.SetJoystick1(const Value: TJoystick);
begin
  if Assigned(Value) then Joystick1.Assign(Value)
                     else FreeAndNil(FJoy1);
end;

Destructor TNavigate.Destroy;
begin
  FJoy1.Free;
  inherited;
end;

procedure TNavigate.Assign(Source:TPersistent);
begin
  if Source is TNavigate then
  with TNavigate(Source) do
  begin
    Self.FInertia:=FInertia;
    Self.Joystick1:=FJoy1;
    Self.FMode:=FMode;
    Self.FWheel:=FWheel;
  end
  else
    inherited;
end;

{ TJoystick }

Constructor TJoystick.Create(AOwner:TWinControl; JoystickID:Byte);
begin
  inherited Create;

  IOwner:=AOwner;
  IJoyID:=JoystickID;
end;

procedure TJoystick.CheckCaps;
begin
  if not ICapsOk then
  begin
    IPresent:=joyGetDevCaps(IJoyID, @ICaps, SizeOf(ICaps))=JOYERR_NOERROR;
    ICapsOk:=True;
  end;
end;

function TJoystick.GetPresent: Boolean;
begin
  CheckCaps;
  result:=IPresent;
end;

procedure TJoystick.SetActive(const Value: Boolean);
begin
  FActive:=Value;

  {$IFNDEF CLX}
  if FActive then
     joySetCapture(IOwner.Handle,JOYSTICKID1,1,True)
  else
     joyReleaseCapture(JOYSTICKID1);
  {$ENDIF}
end;

{ TMakerOptions }

Destructor TMakerOptions.Destroy;
begin
  FCameras.Free;
  Fog.Free;
  Floor.Free;
  FNavigate.Free;
  inherited;
end;

procedure TMakerOptions.DrawReflection(const AReflect:TMakerFloor);

  procedure DrawFloor(Blend:Boolean);
  begin
    if Blend then
       TBlockFormatAccess(AReflect.Format).InitTransparency(AReflect.Reflection);

     TBlocksAccess(IMaker.Blocks).DoDrawItem(AReflect.IFloor);

    if Blend then
       TBlockFormatAccess(AReflect.Format).FinishTransparency(AReflect.Reflection);
  end;

  procedure BeginStencil;
  begin
    glDisable(GL_DEPTH_TEST);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

    glEnable(GL_STENCIL_TEST);
    glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
    glStencilFunc(GL_ALWAYS, 1, $ffffffff);
  end;

  procedure EndStencil;
  begin
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glEnable(GL_DEPTH_TEST);

    glStencilFunc(GL_EQUAL, 1, $ffffffff);  // draw if stencil==1
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  end;

  procedure ClipPlane;
  var tmp : packed Array[0..3] of Double;
  begin
    tmp[0]:=0;
    tmp[1]:=1;
    tmp[2]:=0;

    tmp[3]:=-Floor.IFloor.Location.Z;

    glClipPlane(GL_CLIP_PLANE0,@tmp);
    glEnable(GL_CLIP_PLANE0);
  end;

begin
  if AReflect.Visible then
  begin
    //glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE_ARB);

    if AReflect.Reflection<>0 then
    begin
      BeginStencil;
      DrawFloor(False);
      EndStencil;

      glPushMatrix;

      // Horizontal
      if AReflect.Rotation.Y=90 then
      begin
        glScalef(1.0, -1.0, 1.0);
        glTranslatef(0, -2*AReflect.IFloor.Location.Point.Z+AReflect.Distance, 0);
      end
      else
      begin
        glScalef(-1.0, 1.0, 1.0);
        glTranslatef(-2*AReflect.IFloor.Location.Point.X+AReflect.Distance, 0, 0);
      end;

      ClipPlane;

      glCullFace(GL_FRONT);
      TBlocksAccess(IMaker.FBlocks).IDrawingReflection:=True;
      IMaker.FBlocks.Draw;
      TBlocksAccess(IMaker.FBlocks).IDrawingReflection:=False;
      glCullFace(GL_BACK);

      glDisable(GL_CLIP_PLANE0);

      glPopMatrix;

      glDisable(GL_STENCIL_TEST);
    end;

    DrawFloor(True);

    //glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE_ARB);
  end;
end;

procedure TMakerOptions.DrawBlurShadows;
var tmpBitmap : TBitmap;
begin
  tmpBitmap:=TAlphaBitmap.Create;
  try
    with TFrameBuffer.Create(IMaker.Width,IMaker.Height,False) do
    try
      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glDisable(GL_DEPTH_TEST);

      glShadeModel(GL_FLAT);

      IMaker.Blocks.Draw;

      //glReadBuffer(GL_DEPTH_BUFFER);
      //Assert(TGLCanvasAccess(IMaker.InternalCanvas).CheckGLError,'glReadBuffer (DEPTH): '+IntToStr(TGLCanvasAccess(IMaker.InternalCanvas).ISavedError));

      //glRasterPos2i(0,0);
      //with IMaker.ClientRect do
      //     glCopyPixels(Left,Top,Right-Left,Bottom-Top,GL_COLOR);

      TGLCanvasAccess(IMaker.InternalCanvas).BufferToBitmap(tmpBitmap,IMaker.ClientRect);

      Assert(TGLCanvasAccess(IMaker.InternalCanvas).CheckGLError,'BufferToBitmap (DEPTH): '+IntToStr(TGLCanvasAccess(IMaker.InternalCanvas).ISavedError));
    finally
      Free;
    end;

    with IMaker.ClientRect do
      TeeShadowSmooth(tmpBitmap,nil,Left,Top,Right-Left,Bottom-Top,IMaker.Blocks.Shadows.SmoothSize,1,nil,False);

    // Alpha:
    tmpBitmap.TransparentColor:=clWhite;
    tmpBitmap.Transparent:=True;

    with TGLCanvasAccess(IMaker.Canvas) do
    begin
      DisableRotation;

      BeginBlending(IMaker.ClientRect,Round(IMaker.Blocks.Shadows.Transparency/2.55));

      //TeeTextureEnvMode:=GL_BLEND;

      Draw(0,0,tmpBitmap);

      //TeeTextureEnvMode:=GL_MODULATE;

      TGLCanvasAccess(IMaker.Canvas).RemoveTexture(tmpBitmap);

      EndBlending(nil);
      
      EnableRotation;
    end;

  finally
    tmpBitmap.Free;
  end;
end;

procedure TMakerOptions.CheckLimits;

  procedure LimitVertical;
  begin
    with IMaker.View3DOptions do
    if VertOffsetFloat<Floor.IFloor.Location.Z+10 then
       VertOffsetFloat:=Floor.IFloor.Location.Z+10;
  end;

  procedure LimitPlane;

    Function Cull(const P0,P1,P2:TPoint3DFloat):Boolean;
    begin
      result:=( ((P0.x-P1.x) * (P2.y-P1.y)) -
                ((P2.x-P1.x) * (P0.y-P1.y))
              ) < 0;
    end;

var viewport   : TVector4i;
    mvmatrix,
    projmatrix : TMatrix4d;

    function Calculate2DPosition(const x,y,z:Double):TPoint3DFloat;
    var wx, wy, wz : GLDouble;
    begin
      gluProject(x, y, z, mvmatrix, projmatrix, viewport, @wx, @wy, @wz);
      Assert(glGetError=GL_NO_ERROR,'gluProject');

      result.X:=wx;
      result.Y:=ViewPort[3]-wy;
      result.Z:=wz;
    end;

    function FloorCull:Boolean;
    var P0,P1,P2 : TPoint3DFloat;
        y0       : Double;
    begin
      y0:=Floor.Block.Location.Z;

      P0:=Calculate2DPosition(-50,y0,50);
      P1:=Calculate2DPosition(50,y0,50);
      P2:=Calculate2DPosition(-50,y0,-50);

      result:=not Cull(P0,P1,P2);
    end;

  begin
    repeat
      glGetIntegerv(GL_VIEWPORT, @viewport);
      Assert(glGetError=GL_NO_ERROR,'glGetIntegerv GL_VIEWPORT');

      glGetDoublev(GL_MODELVIEW_MATRIX, @mvmatrix);
      glGetDoublev(GL_PROJECTION_MATRIX, @projmatrix);

      if FloorCull then
         break
      else
      begin
        IMaker.Options.Navigate.IDifY:=0;
        
        with IMaker.View3DOptions do
        begin
          ElevationFloat:=ElevationFloat-1;

          if ElevationFloat<0 then
             ElevationFloat:=360+ElevationFloat;
        end;

        with TGLCanvasAccess(IMaker.Render.Canvas) do
        begin
          DoProjection;
          SetModelView;
        end;
      end;
    until False;
  end;

begin
  if (Floor.Limit=lfYes) or ((Floor.Limit=lfAuto) and Floor.Visible) then
     if (FNavigate.Mode=nmWalk) or (FNavigate.Mode=nmFly) then
        LimitVertical
     else
        LimitPlane;
end;

type
  TLightPositionAccess=class(TGLPosition);

procedure TMakerOptions.BeforeDraw;

  procedure DoDrawShadows(const ALight:TGLLightSource);

    procedure CreateShadowMatrix(const l,e,n:TPoint3DFloat);
    var d   : Single;
        c   : Single;
        mat : Array[0..15] of Single;
    begin
      d := n.X*l.X + n.y*l.y + n.z*l.z;
      c := e.X*n.X + e.y*n.y + e.z*n.z - d;

      mat[0]  := l.x*n.x+c;
      mat[4]  := n.y*l.x;
      mat[8]  := n.z*l.x;
      mat[12] := -l.x*c-l.x*d;

      mat[1]  := n.x*l.y;
      mat[5]  := l.y*n.y+c;
      mat[9]  := n.z*l.y;
      mat[13] := -l.y*c-l.y*d;

      mat[2]  := n.x*l.z;
      mat[6]  := n.y*l.z;
      mat[10] := l.z*n.z+c;
      mat[14] := -l.z*c-l.z*d;

      mat[3]  := n.x;
      mat[7]  := n.y;
      mat[11] := n.z;
      mat[15] := -d;

      glMultMatrixf(@mat);
    end;

    function Point3DFloat(const P:{$IFDEF D20}TeCanvas.{$ENDIF}TPoint3D):TPoint3DFloat;
    begin
      result.X:=P.x;
      result.Y:=P.y;
      result.Z:=P.z;
    end;

  var tmpLightPosition  : TPoint3DFloat;
      tmpShadowPosition : TPoint3DFloat;
      tmpPlaneNormal    : TPoint3DFloat;
  begin
    tmpLightPosition:=TLightPositionAccess(ALight.Position).Point;
    tmpLightPosition.X:= -tmpLightPosition.X;

    with Floor.IFloor.Location.Point do
    begin
      tmpShadowPosition.X:=X;
      tmpShadowPosition.Y:=Z+4;
      tmpShadowPosition.Z:=Y;
    end;

    glPushAttrib(GL_LIGHTING or GL_DEPTH_TEST or GL_DEPTH_WRITEMASK or GL_SHADE_MODEL);

    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(GL_FALSE);
    glShadeModel(GL_FLAT);

    tmpPlaneNormal:=Point3DFloat(Point3D(0,1,0));

    glPushMatrix;

    CreateShadowMatrix(tmpLightPosition, tmpShadowPosition, tmpPlaneNormal);

    IMaker.FBlocks.Shadows.Visible:=True;

    if IMaker.FBlocks.Shadows.Smooth and Assigned(glBindRenderbufferEXT) then
       DrawBlurShadows
    else
       IMaker.FBlocks.Draw;

    IMaker.FBlocks.Shadows.Visible:=False;

    glPopMatrix;

    glPopAttrib;

    with TGLCanvasAccess(IMaker.Canvas) do
         Assert(CheckGLError,'DrawShadows: '+IntToStr(ISavedError));
  end;

begin
  Fog.Setup;

  if FView3DAxes then
     DrawAxes;

  if FBoundingBox then
  begin
    if IMaker.Current=IMaker.CurrentOver then
       DrawBoundingBox(IMaker.Current,clYellow,True)
    else
       DrawBoundingBox(IMaker.Current,clRed,True);

    if Assigned(IMaker.CurrentOver) then
       DrawBoundingBox(IMaker.CurrentOver,clLime,False);
  end;

  TGLCanvasAccess(IMaker.Canvas).UsesStencil:=Floor.Visible;

  DrawReflection(Floor);

  // Light lamps
  if FShowLightLamps then
     IMaker.FTeeOpenGL.DrawLamps;

  if DrawShadows then
  with IMaker.FTeeOpenGL do
  begin
    if Light0.Visible then
       DoDrawShadows(Light0);

    if Light1.Visible then
       DoDrawShadows(Light1);

    if Light2.Visible then
       DoDrawShadows(Light2);
  end;
end;

function TMakerOptions.GetHideBorders:Boolean;
begin
  result:=IMaker.Blocks.HideBorders;
end;

function TMakerOptions.GetUseThreads: Boolean;
begin
  result:=not TeeNoThreads;
end;

procedure TMakerOptions.SetBoundingBox(const Value: Boolean);
begin
  if FBoundingBox<>Value then
  begin
    FBoundingBox:=Value;
    IMaker.Invalidate;
  end;
end;

procedure TMakerOptions.SetDrawShadows(const Value: Boolean);
begin
  if FDrawShadows<>Value then
  begin
    FDrawShadows:=Value;
    IMaker.Invalidate;
  end;
end;

procedure TMakerOptions.SetNavigate(const Value: TNavigate);
begin
  FNavigate.Assign(Value);
end;

procedure TMakerOptions.SetShowLightLamps(const Value: Boolean);
begin
  FShowLightLamps:=Value;
  IMaker.Invalidate;
end;

procedure TMakerOptions.SetUseThreads(const Value: Boolean);
begin
  TeeNoThreads:=not Value;
end;

procedure TMakerOptions.SetView3DAxes(const Value: Boolean);
begin
  FView3DAxes:=Value;
  IMaker.Invalidate;
end;

end.

