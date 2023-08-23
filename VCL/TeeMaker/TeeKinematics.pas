unit TeeKinematics;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons,
  TeCanvas, TeeBlocks, TeeAnimate, TeeMakerControl, TeePointEditor;

type
  TMaterial=class(TPersistent)
  private
    FFriction: Double;
    FDensity: Double;
    FBounce: Double;
  public
    Constructor Create;
  published
    property Bounce:Double read FBounce write FBounce;
    property Density:Double read FDensity write FDensity;
    property Friction:Double read FFriction write FFriction;
  end;

  TMovement=class(TCollectionItem)
  private
    FAccel    : Double;
    FBlock    : TCustomBlock;
    FCenter   : TPointXYZFloat;
    FCenterLink: TPropertyLink;
    FCircular : Boolean;
    FDirection: TPointXYZFloat;
    FMaterial : TMaterial;
    FRadius   : TPointXYZFloat;
    FSpeed    : Double;

    Angle     : Double;

    IOwner : TComponent;

    function CenterPoint:TPoint3DFloat;
    function GetCenterLink: TPropertyLink;
    function HasLink(ALink:TPropertyLink):Boolean;
    procedure SetCenter(const Value: TPointXYZFloat);
    procedure SetCenterLink(const Value: TPropertyLink);
    procedure SetDirection(const Value: TPointXYZFloat);
    procedure SetRadius(const Value: TPointXYZFloat);
    procedure SetMaterial(const Value: TMaterial);
  public
    Constructor Create(ACollection:TCollection); override;
    Destructor Destroy; override;
  published
    property Acceleration:Double read FAccel write FAccel;
    property Block:TCustomBlock read FBlock write FBlock;
    property Material:TMaterial read FMaterial write SetMaterial;
    property Center:TPointXYZFloat read FCenter write SetCenter;
    property CenterLink:TPropertyLink read GetCenterLink write SetCenterLink;
    property Circular:Boolean read FCircular write FCircular default False;
    property Direction:TPointXYZFloat read FDirection write SetDirection;
    property Radius:TPointXYZFloat read FRadius write SetRadius;
    property Speed:Double read FSpeed write FSpeed;
  end;

  TMovements=class;

  TGravity=class(TPersistent)
  private
    FAccel   : Double;
    FEnabled : Boolean;

    IBlocks  : TMovements;

    function IsAccelStored:Boolean;
    procedure SetAccel(const Value:Double);
  public
    Constructor Create;
    Destructor Destroy; override;
  published
    property Acceleration:Double read FAccel write SetAccel stored IsAccelStored;
    property Enabled:Boolean read FEnabled write FEnabled default True;
  end;

  TCollisionEvent=procedure(Sender:TMovement; const ABlock:TCustomBlock;
                                              var ACollided:TCustomBlock;
                                              var APoint:TPoint3DFloat) of object;

  TKinematics=class;

  TMovements=class(TOwnedCollection)
  private
    FCollisions: Boolean;
    FEnabled   : Boolean;
    FGravity   : TGravity;
    FOnCollision: TCollisionEvent;

    IMaker : TMaker;
    IKinematics : TKinematics;

    function Get(Index: Integer): TMovement;
    procedure Process(AutoSpeed:Boolean=False);
    procedure Put(Index: Integer; const Value: TMovement);
    procedure SetEnabled(const Value: Boolean);
    procedure SetGravity(const Value:TGravity);
  public
    Constructor Create(AOwner:TComponent);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function AddMovement:TMovement;
    procedure DrawDirections;

    property Maker:TMaker read IMaker write IMaker;
    property Item[Index:Integer]:TMovement read Get write Put; default;
  published
    property Collisions:Boolean read FCollisions write FCollisions default True;
    property Enabled:Boolean read FEnabled write SetEnabled default False;
    property Gravity:TGravity read FGravity write SetGravity;

    // Events
    property OnCollision:TCollisionEvent read FOnCollision write FOnCollision;
  end;

  TRotationLimits=class(TPersistent)
  private
    FMinX: Double;
    FMaxX: Double;
    FMinZ: Double;
    FMinY: Double;
    FMaxZ: Double;
    FMaxY: Double;
  public
    Constructor Create;
  published
    property MinX:Double read FMinX write FMinX;
    property MaxX:Double read FMaxX write FMaxX;
    property MinY:Double read FMinY write FMinY;
    property MaxY:Double read FMaxY write FMaxY;
    property MinZ:Double read FMinZ write FMinZ;
    property MaxZ:Double read FMaxZ write FMaxZ;
  end;

  TJoint=class(TCollectionItem)
  private
    FBlock: TCustomBlock;
    FParent: TCustomBlock;
    FParentPosition: TPointXYZFloat;
    FPosition: TPointXYZFloat;
    FLimits: TRotationLimits;

    function GetLimits:TRotationLimits;
    function GetRotation:TPoint3DFloat;
    procedure SetBlock(const Value:TCustomBlock);
    procedure SetLimits(const Value: TRotationLimits);
    procedure SetPosition(const Value: TPointXYZFloat);
    procedure SetParentPosition(const Value: TPointXYZFloat);
  public
    Constructor Create(AOwner:TCollection); override;
    Destructor Destroy; override;
  published
    property Block:TCustomBlock read FBlock write SetBlock;
    property Parent:TCustomBlock read FParent write FParent;
    property ParentPosition:TPointXYZFloat read FParentPosition write SetParentPosition;
    property Position:TPointXYZFloat read FPosition write SetPosition;
    property RotationLimits:TRotationLimits read GetLimits write SetLimits;
  end;

  TJoints=class(TOwnedCollection)
  private
    function Get(Index: Integer): TJoint;
    procedure Put(Index: Integer; const Value: TJoint);
  public
    function AddJoint:TJoint;
    procedure Process;

    property Item[Index:Integer]:TJoint read Get write Put; default;
  end;

  TKinematics=class(TPersistent)
  private
    FJoints    : TJoints;
    FMovements : TMovements;

    procedure SetJoints(const Value: TJoints);
    procedure SetMovements(const Value: TMovements);
    procedure Timer(Sender: TObject);
    procedure TryToEnable;
  public
    Constructor Create(AMaker:TMaker);
    Destructor Destroy; override;
  published
    property Joints: TJoints read FJoints write SetJoints;
    property Movements : TMovements read FMovements write SetMovements;
  end;

  TKinematicsEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CBEnableGravity: TCheckBox;
    Label1: TLabel;
    EAccelGravity: TEdit;
    Label2: TLabel;
    Panel1: TPanel;
    CBCollisions: TCheckBox;
    ListMoves: TListBox;
    Panel2: TPanel;
    TabSheet3: TTabSheet;
    SBAdd: TSpeedButton;
    SBRemove: TSpeedButton;
    CBEnabled: TCheckBox;
    ListJoints: TListBox;
    Panel4: TPanel;
    SBAddJoint: TSpeedButton;
    SBRemoveJoint: TSpeedButton;
    PageJoints: TPageControl;
    TabJointBlock: TTabSheet;
    TabJointParent: TTabSheet;
    TabSheet6: TTabSheet;
    GroupX: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    TBXMin: TTrackBar;
    TBXMax: TTrackBar;
    LXMin: TLabel;
    LXMax: TLabel;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    LYMin: TLabel;
    LYMax: TLabel;
    TBYMin: TTrackBar;
    TBYMax: TTrackBar;
    GroupBox2: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    LZMin: TLabel;
    LZMax: TLabel;
    TBZMin: TTrackBar;
    TBZMax: TTrackBar;
    Panel5: TPanel;
    Label6: TLabel;
    LabelParentBlock: TLabel;
    Button2: TButton;
    Panel6: TPanel;
    Label5: TLabel;
    LabelBlock: TLabel;
    Button1: TButton;
    Splitter1: TSplitter;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    TabDirection: TTabSheet;
    TabCircular: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    ESpeed: TEdit;
    UDSpeed: TUpDown;
    EAccel: TEdit;
    UDAccel: TUpDown;
    Label7: TLabel;
    EBounce: TEdit;
    UDBounce: TUpDown;
    Panel3: TPanel;
    CBCircular: TCheckBox;
    Splitter2: TSplitter;
    PageControl3: TPageControl;
    TabCenter: TTabSheet;
    TabSheet7: TTabSheet;
    Button3: TButton;
    BClearCenterLink: TButton;
    LCenterLink: TLabel;
    Memo1: TMemo;
    TabRadius: TTabSheet;
    Label12: TLabel;
    EFriction: TEdit;
    UDFriction: TUpDown;
    Label13: TLabel;
    EDensity: TEdit;
    UDDensity: TUpDown;
    procedure CBEnableGravityClick(Sender: TObject);
    procedure EAccelGravityChange(Sender: TObject);
    procedure CBCollisionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListMovesClick(Sender: TObject);
    procedure ESpeedChange(Sender: TObject);
    procedure EAccelChange(Sender: TObject);
    procedure SBRemoveClick(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure TBXMinChange(Sender: TObject);
    procedure TBXMaxChange(Sender: TObject);
    procedure TBYMinChange(Sender: TObject);
    procedure TBYMaxChange(Sender: TObject);
    procedure TBZMinChange(Sender: TObject);
    procedure TBZMaxChange(Sender: TObject);
    procedure ListJointsClick(Sender: TObject);
    procedure SBAddJointClick(Sender: TObject);
    procedure SBRemoveJointClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EBounceChange(Sender: TObject);
    procedure CBCircularClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BClearCenterLinkClick(Sender: TObject);
    procedure EFrictionChange(Sender: TObject);
    procedure EDensityChange(Sender: TObject);
  private
    { Private declarations }
    Movements : TMovements;
    Kinematics: TKinematics;

    // Movements
    IDirection : TPointEditor;
    ICircular  : TPointEditor;
    IRadius    : TPointEditor;

    // Joints
    IPosition : TPointEditor;
    IParentPosition : TPointEditor;

    function Joint:TJoint;
    function Movement:TMovement;
    procedure SetLinkLabel(ALabel:TLabel; ALink:TPropertyLink);
  public
    { Public declarations }
    procedure RefreshKinematics(Value:TKinematics);
  end;

const
  BlockAction_KinematicsCollision='11';

function Collide(const ABlock:TCustomBlock; var ANewLocation:TPoint3DFloat):TCustomBlock;

implementation

{$R *.dfm}

uses
  TeePenDlg, Math, OpenGL2, TeeBlockGallery, TeeBlockEditor, TeeActionGallery,
  TypInfo, TeeProcs;

{
function Intersect(const PosA,PosB,SizeA,SizeB:TPoint3DFloat):Boolean; overload;
begin
  with AbsSubtract(PosA,PosB) do
       result:= (X<=0.5*(SizeA.X+SizeB.X)) and
                (Y<=0.5*(SizeA.Y+SizeB.Y)) and
                (Z<=0.5*(SizeA.Z+SizeB.Z));
end;
}

function Intersect(const Bounds0,Bounds1:TBounds):Boolean; overload;

  function CornerInCube(const P:TPoint3DFloat):Boolean; overload;
  begin
    with Bounds0,P do
       result:=(X>=Min.X) and (X<=Max.X) and
               (Y>=Min.Y) and (Y<=Max.Y) and
               (Z>=Min.Z) and (Z<=Max.Z);
  end;

  function CornerInCube(const X,Y,Z:Single):Boolean; overload;
  begin
    with Bounds0 do
    result:=(X>=Min.X) and (X<=Max.X) and
            (Y>=Min.Y) and (Y<=Max.Y) and
            (Z>=Min.Z) and (Z<=Max.Z);
  end;

begin
  with Bounds1 do
  result:=CornerInCube(Min) or
          CornerInCube(Max) or
          CornerInCube(Max.X,Min.Y,Min.Z) or
          CornerInCube(Min.X,Max.Y,Min.Z) or
          CornerInCube(Min.X,Min.Y,Max.Z) or
          CornerInCube(Min.X,Max.Y,Max.Z) or
          CornerInCube(Max.X,Min.Y,Max.Z) or
          CornerInCube(Max.X,Max.Y,Min.Z);
end;

function Collide(const ABlock:TCustomBlock; var ANewLocation:TPoint3DFloat):TCustomBlock;
var t : Integer;
    bounds0,
    bounds1 : TBounds;
    tmpZ : Single;
    tmpFloor : TCustomBlock;
begin
  tmpFloor:=TMaker(ABlock.Parent.Parent).Options.Floor.Block;
  tmpZ:=ANewLocation.Z-ABlock.Size.Point.Z*0.5;

  if tmpFloor.Visible and (tmpZ<tmpFloor.Location.Z) then
  begin
    ANewLocation.Z:=tmpFloor.Location.Z+ABlock.Size.Point.Z*0.5;
    result:=tmpFloor;
  end
  else
  begin
    result:=nil;

    ABlock.CalcBounds(bounds0.Min,bounds0.Max);
    bounds0:=AddPoints(bounds0,Subtract(ANewLocation,ABlock.Location.Point));

    with ABlock.Parent do
    for t:=0 to Count-1 do
        if Block[t]<>ABlock then
        with Block[t] do
        if Visible then
        begin
          CalcBounds(bounds1.Min,bounds1.Max);

          if Intersect(bounds0,bounds1) or Intersect(bounds1,bounds0) then
          begin
            result:=Block[t];
            break;
          end;
        end;
  end;
end;

{ TMovements }

Constructor TMovements.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,TMovement);
  FCollisions:=True;

  if Assigned(AOwner) then
  begin
    FGravity:=TGravity.Create;
    FGravity.IBlocks.IMaker:=IMaker;
  end;
end;

Destructor TMovements.Destroy;
begin
  FGravity.Free;
  inherited;
end;

function TMovements.AddMovement: TMovement;
begin
  result:=Add as TMovement;
  result.IOwner:=Maker;
end;

function TMovements.Get(Index: Integer): TMovement;
begin
  result:=TMovement(inherited Items[Index]);
end;

function RotatePoint(const Angle:Single; Center,Radius:TPoint3DFloat):TPoint3DFloat;
var tmpSin,
    tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  SinCos(Angle*TeePiStep,tmpSin,tmpCos);

  with result do
  begin
    X:=Center.X+( Radius.X*tmpCos + Radius.Y*tmpSin );
    Y:=Center.Y+( -Radius.X*tmpSin + Radius.Y*tmpCos );
    Z:=Center.Z;
  end;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

procedure TMovements.DrawDirections;
var t,
    tmpAngle : Integer;
    tmpR,
    tmpV : TPoint3DFloat;
begin
  for t:=0 to Count-1 do
  with Item[t] do
  begin
    TBlockFormatAccess(Block.Format).InternalPreparePen;

    if Circular then
    begin
      with TEllipsoidBlock.Create(nil) do
      try
        Size.Value:=Item[t].Block.Size.Value/20;
        Format.Color:=clRed;
        Location.Point:=CenterPoint;
        DrawBlock(Maker.Blocks);
      finally
        Free;
      end;

      glBegin(GL_LINE_LOOP);
      glColor4ub(255,0,0,0);

      tmpV:=CenterPoint;
      tmpR:=Radius.Point;

      for tmpAngle:=0 to 359 do
      with RotatePoint(tmpAngle,tmpV,tmpR) do
           glVertex3f(X,Z,-Y);

      glEnd;
    end
    else
    begin
      glBegin(GL_LINES);
      glColor4ub(255,0,0,0);

      tmpV:=Block.Location.Point;
      glVertex3f(tmpV.X,tmpV.Z,-tmpV.Y);

      tmpV:=AddMultiply(tmpV,Direction.Point,4*Block.Size.Value);
      glVertex3f(tmpV.X,tmpV.Z,-tmpV.Y);

      glEnd;
    end;

    TBlockFormatAccess(Block.Format).FinishPen;
  end;
end;

{
function TMovements.FloorPosition:Double;
begin
  with IMaker.Options.Floor.Block do
       result:=Location.Z;
end;

procedure TMovements.ProcessGravity;
var t : Integer;
    tmpG : Integer;
    tmpZ : Double;
begin
  for t:=0 to IMaker.Blocks.Count-1 do
  with IMaker.Blocks[t] do
  begin
    tmpZ:=Location.Point.Z-Size.Point.Z*0.5;

    tmpG:=FGravity.IndexOfBlock(IMaker.Blocks[t]);

    if tmpZ>FloorPosition then
    begin
      if tmpG=-1 then
      begin
        tmpG:=Length(FGravity.IBlocks);
        SetLength(FGravity.IBlocks,tmpG+1);
        FGravity.IBlocks[tmpG].Block:=IMaker.Blocks[t];
        FGravity.IBlocks[tmpG].Speed:=FGravity.FAccel*0.01; // Wrong
      end;

      if tmpZ-FGravity.IBlocks[tmpG].Speed<FloorPosition then
      begin
        Location.Point.Z:=FloorPosition+Size.Point.Z*0.5;
        FGravity.IBlocks[tmpG].Speed:=0;
      end
      else
      begin
        with Location.Point do
             Z:=Z-FGravity.IBlocks[tmpG].Speed;

        with FGravity.IBlocks[tmpG] do
             Speed:=Max(0,Speed+FGravity.Acceleration*0.1);
      end;
    end
    else
    if tmpZ<FloorPosition then
       Location.Point.Z:=FloorPosition+Size.Point.Z*0.5;
  end;
end;
}

procedure TMovements.Process(AutoSpeed:Boolean=False);
var t : Integer;
    tmp : TPoint3DFloat;
    tmpColl : TCustomBlock;
begin
  for t:=0 to Count-1 do
  with Item[t] do
  begin
    if AutoSpeed or (Speed>0) then
    begin
      if Circular then
         tmp:=RotatePoint(Angle+0.01*Speed,CenterPoint,Radius.Point)
      else
         tmp:=AddMultiply(Block.Location.Point,Direction.Point,Speed);

      if FCollisions then
      begin
        tmpColl:=Collide(Block,tmp);

        if Assigned(tmpColl) then
        begin
          if Assigned(FOnCollision) and Assigned(tmpColl) then
             FOnCollision(Item[t],Block,tmpColl,tmp);

          Block.Location.SetPoint(tmp);

          if FMaterial.Bounce=0 then  // Heavy rock
             Speed:=0
          else
          begin
            // Invert current direction (rebound)
            with Direction.Point do
            begin
              X:=-X;
              Y:=-Y;
              Z:=-Z;
            end;

            // Slowdown due to collision
            Speed:=Speed*FMaterial.Bounce;

            Acceleration:=0;
          end;
        end
        else
        begin
          Block.Location.SetPoint(tmp);

          if Circular then
             if Direction.X>0 then
                Angle:=Angle+0.01*Speed
             else
                Angle:=Angle-0.01*Speed;

          Speed:=Max(0,Speed+Acceleration-FMaterial.Friction);
        end;
      end
      else
      begin
        Block.Location.SetPoint(tmp);

        if Circular then
           if Direction.X>0 then
              Angle:=Angle+0.01*Speed
           else
              Angle:=Angle-0.01*Speed;

        Speed:=Max(0,Speed+Acceleration-FMaterial.Friction);
      end;
    end;
  end;
end;

procedure TMovements.Put(Index: Integer; const Value: TMovement);
begin
  inherited Items[Index]:=Value;
end;

procedure TMovements.Assign(Source: TPersistent);
begin
  if Source is TMovements then
  with TMovements(Source) do
  begin
    Self.FGravity.Assign(FGravity);
  end;

  inherited;
end;

procedure TMovements.SetGravity(const Value: TGravity);
begin

end;

procedure TMovements.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;

    if FEnabled then
       IKinematics.TryToEnable;
  end;
end;

{ TMovement }

Constructor TMovement.Create;
begin
  inherited;
  FDirection:=TPointXYZFloat.Create;
  FCenter:=TPointXYZFloat.Create;
  FRadius:=TPointXYZFloat.Create;

  FMaterial:=TMaterial.Create;
end;

Destructor TMovement.Destroy;
begin
  FMaterial.Free;
  FRadius.Free;
  FCenterLink.Free;
  FCenter.Free;
  FDirection.Free;
  inherited;
end;

function TMovement.GetCenterLink: TPropertyLink;
begin
  if not Assigned(FCenterLink) then
     FCenterLink:=TPropertyLink.Create(IOwner);

  result:=FCenterLink;
end;

procedure TMovement.SetCenter(const Value: TPointXYZFloat);
begin
  FCenter.Assign(Value);
end;

procedure TMovement.SetCenterLink(const Value: TPropertyLink);
begin
  if Assigned(Value) then CenterLink.Assign(Value)
                     else FreeAndNil(FCenterLink);
end;

procedure TMovement.SetDirection(const Value: TPointXYZFloat);
begin
  FDirection.Assign(Value);
end;

function TMovement.CenterPoint: TPoint3DFloat;
var tmp : TPointXYZFloat;
begin
  if HasLink(FCenterLink) then
  begin
    tmp:=GetObjectProp(FCenterLink.Instance,FCenterLink.PropertyName) as TPointXYZFloat;
    result:=tmp.Point;
  end
  else
    result:=FCenter.Point;
end;

function TMovement.HasLink(ALink:TPropertyLink):Boolean;
begin
  result:=Assigned(ALink) and (Assigned(ALink.Instance) or (ALink.PropertyName<>''));
end;

procedure TMovement.SetRadius(const Value: TPointXYZFloat);
begin
  FRadius.Assign(Value);
end;

procedure TMovement.SetMaterial(const Value: TMaterial);
begin
  FMaterial.Assign(Value);
end;

{ TGravity }

Constructor TGravity.Create;
begin
  inherited;
  FEnabled:=True;
  FAccel:=9.8;
  IBlocks:=TMovements.Create(nil);
end;

Destructor TGravity.Destroy;
begin
  IBlocks.Free;
  inherited;
end;

function TGravity.IsAccelStored: Boolean;
begin
  result:=FAccel<>9.8;
end;

procedure TKinematicsEditor.CBEnableGravityClick(Sender: TObject);
begin
  Movements.Gravity.Enabled:=CBEnableGravity.Checked; 
end;

procedure TKinematicsEditor.EAccelGravityChange(Sender: TObject);
var tmp : Double;
begin
  if TryStrToFloat(EAccelGravity.Text,tmp) then
     Movements.Gravity.Acceleration:=tmp;
end;

procedure TKinematicsEditor.CBCollisionsClick(Sender: TObject);
begin
  Movements.Collisions:=CBCollisions.Checked;
end;

procedure TKinematicsEditor.FormShow(Sender: TObject);

  procedure PrepareEditor(AEditor:TPointEditor);

    procedure SetScroll(AScroll:TScrollBar);
    begin
      AScroll.Min:=-100;
      AScroll.Max:=100;
    end;

  begin
    with AEditor do
    begin
      Align:=alClient;

      Factor:=0.01;

      SetScroll(BlockPathX);
      SetScroll(BlockPathY);
      SetScroll(BlockPathZ);
    end;
  end;

begin
  IDirection:=TPointEditor.Create(Self);
  PrepareEditor(IDirection);
  TTeeVCL.AddFormTo(IDirection,TabDirection);

  ICircular:=TPointEditor.Create(Self);
  TTeeVCL.AddFormTo(ICircular,TabCenter);

  IRadius:=TPointEditor.Create(Self);
  TTeeVCL.AddFormTo(IRadius,TabRadius);

  Kinematics:=TKinematics(Tag);

  if Assigned(Kinematics) then
     RefreshKinematics(Kinematics);
end;

procedure TKinematicsEditor.RefreshKinematics(Value:TKinematics);
var t : Integer;
begin
  Kinematics:=Value;

  if Assigned(Kinematics) then
  begin
    ListJoints.Items.Clear;

    for t:=0 to Kinematics.Joints.Count-1 do
        ListJoints.Items.Add(Kinematics.Joints[t].Block.Title);

    Movements:=Kinematics.Movements;

    if Assigned(Movements) then
    with Movements do
    begin
      CBEnableGravity.Checked:=Gravity.Enabled;
      CBCollisions.Checked:=Collisions;
      EAccelGravity.Text:=FloatToStr(Gravity.Acceleration);

      CBEnabled.Checked:=Enabled;

      ListMoves.Items.Clear;

      for t:=0 to Count-1 do
          ListMoves.Items.Add(Item[t].Block.Title);

      if Count>0 then
         ListMoves.ItemIndex:=0;

      ListMovesClick(Self);
    end;
  end;
end;

procedure TKinematicsEditor.SetLinkLabel(ALabel:TLabel; ALink:TPropertyLink);
begin
  Memo1.Lines.Clear;

  if Movement.HasLink(ALink) then
  begin
    ALabel.Caption:=TActionGallery.PropertyText(ALink);

    with Movement.CenterPoint,Memo1.Lines do
    begin
      Add('X: '+FloatToStr(X));
      Add('Y: '+FloatToStr(Y));
      Add('Z: '+FloatToStr(Z));
    end;
  end
  else
    ALabel.Caption:='';
end;

procedure TKinematicsEditor.ListMovesClick(Sender: TObject);
begin
  SBRemove.Enabled:=ListMoves.ItemIndex<>-1;

  if ListMoves.ItemIndex=-1 then
     PageControl2.Enabled:=False
  else
  begin
    PageControl2.Enabled:=True;

    with Movement do
    begin
      UDSpeed.Position:=Round(Speed);
      UDAccel.Position:=Round(Acceleration);
      UDBounce.Position:=Round(100*Material.Bounce);
      UDFriction.Position:=Round(100*Material.Friction);

      CBCircular.Checked:=Circular;

      IDirection.SelectPoint(Direction);
      ICircular.SelectPoint(Center);
      IRadius.SelectPoint(Radius);

      BClearCenterLink.Enabled:=HasLink(CenterLink);
      SetLinkLabel(LCenterLink,CenterLink);
    end;
  end;
end;

procedure TKinematicsEditor.ESpeedChange(Sender: TObject);
begin
  if Showing then
     Movement.Speed:=UDSpeed.Position;
end;

procedure TKinematicsEditor.EAccelChange(Sender: TObject);
begin
  if Showing then
     Movement.Acceleration:=UDAccel.Position;
end;

procedure TGravity.SetAccel(const Value: Double);
var t : Integer;
begin
  if FAccel<>Value then
  begin
    FAccel:=Value;

    for t:=0 to IBlocks.Count-1 do
        IBlocks[t].FAccel:=FAccel;
  end;
end;

{ TJoint }

Constructor TJoint.Create(AOwner: TCollection);
begin
  inherited;
  FPosition:=TPointXYZFloat.Create;
  FParentPosition:=TPointXYZFloat.Create;
end;

type
  TRotationXYZAccess=class(TRotationXYZ);

Destructor TJoint.Destroy;
begin
  if Assigned(FBlock) then
     TRotationXYZAccess(FBlock.Rotation).FOnGet:=nil;

  FPosition.Free;
  FParentPosition.Free;
  FLimits.Free;
  inherited;
end;

function TJoint.GetLimits: TRotationLimits;
begin
  if not Assigned(FLimits) then
     FLimits:=TRotationLimits.Create;

  result:=FLimits;
end;

procedure TJoint.SetBlock(const Value: TCustomBlock);
begin
  if FBlock<>Value then
  begin
    if Assigned(FBlock) then
       TRotationXYZAccess(FBlock.Rotation).FOnGet:=nil;

    FBlock:=Value;

    if Assigned(FBlock) then
       TRotationXYZAccess(FBlock.Rotation).FOnGet:=GetRotation;
  end;
end;

function TJoint.GetRotation:TPoint3DFloat;
begin
  if Assigned(FParent) then
     result:=AddPoints(FParent.Rotation.Point,FBlock.Rotation.Point)
  else
     result:=FBlock.Rotation.Point;
end;

procedure TJoint.SetLimits(const Value: TRotationLimits);
begin
  FLimits.Assign(Value);
end;

procedure TJoint.SetParentPosition(const Value: TPointXYZFloat);
begin
  FParentPosition.Assign(Value);
end;

procedure TJoint.SetPosition(const Value: TPointXYZFloat);
begin
  FPosition.Assign(Value);
end;

{ TRotationLimits }

Constructor TRotationLimits.Create;
begin
  inherited;
  FMaxX:=360;
  FMaxY:=360;
  FMaxZ:=360;
end;

function TJoints.AddJoint: TJoint;
begin
  result:=TJoint(Add);
end;

{ TKinematics }

type
  TTeeAnimateAccess=class(TTeeAnimate);

Constructor TKinematics.Create(AMaker:TMaker);
begin
  inherited Create;

  FJoints:=TJoints.Create(nil,TJoint);

  FMovements:=TMovements.Create(AMaker);
  FMovements.Maker:=AMaker;
  FMovements.IKinematics:=Self;

  TTeeAnimateAccess(TTeeAnimate).GlobalTimer.AddEvent(Timer,nil,10);
end;

Destructor TKinematics.Destroy;
begin
  TTeeAnimateAccess(TTeeAnimate).GlobalTimer.RemoveEvent(Timer);

  FMovements.Free;
  FJoints.Free;
  inherited;
end;

procedure TKinematics.Timer(Sender: TObject);
var t : Integer;
begin
  if Assigned(FMovements.IMaker) and
     (FMovements.IMaker.Render.Canvas.Handle<>0) then
       if Movements.Enabled then
       begin
         Movements.Process;

         if Movements.Gravity.Enabled then
         begin
           with Movements.Gravity.IBlocks do
           begin
             if Count=0 then
             for t:=0 to FMovements.IMaker.Blocks.Count-1 do
             begin
               with AddMovement do
               begin
                 FMaterial.FBounce:=0;
                 FBlock:=Movements.IMaker.Blocks[t];
                 FAccel:=Movements.Gravity.Acceleration;
                 FDirection.Point.Z:=-1;
                 FSpeed:=0.01;
               end;
             end;

             Process(True);

             for t:=0 to Count-1 do
                 Item[t].FAccel:=Movements.Gravity.Acceleration;
           end;
         end;
       end;
end;

function TJoints.Get(Index: Integer): TJoint;
begin
  result:=TJoint(inherited Items[Index]);
end;

procedure TJoints.Process;
var t : Integer;
begin
  for t:=0 to Count-1 do
  with Item[t] do
  if Assigned(Block) and Assigned(Parent) then
  begin
    Block.Location.Z:=Parent.Location.Z+(Parent.Size.Z*0.5)+(Block.Size.Z*0.5);
    Block.Location.Y:=Parent.Location.Y;
    Block.Location.X:=Parent.Location.X;


    with Block.Rotation,RotationLimits do
    begin
      if MinX<MaxX then
      begin
        if X<MinX then X:=MinX else if X>MaxX then X:=MaxX;
      end
      else
      begin
        if X+(360-MinX)<0 then X:=MinX else if X>MaxX then X:=MaxX;
      end;

      if Y<MinY then Y:=MinY else if Y>MaxY then Y:=MaxY;
      if Z<MinZ then Z:=MinZ else if Z>MaxZ then Z:=MaxZ;
    end;
  end;
end;

procedure TJoints.Put(Index: Integer; const Value: TJoint);
begin
  inherited Items[Index]:=Value;
end;

procedure TKinematicsEditor.SBRemoveClick(Sender: TObject);
begin
  Movements.Delete(ListMoves.ItemIndex);
  ListMoves.Items.Delete(ListMoves.ItemIndex);
  ListMovesClick(Self);
end;

procedure TKinematicsEditor.SBAddClick(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=TBlockGallery.ChooseBlock(Self,Movements.Maker.Blocks);

  if Assigned(tmp) then
  begin
    Movements.AddMovement.Block:=tmp;
    ListMoves.Items.Add(tmp.Title);
    ListMoves.ItemIndex:=ListMoves.Items.Count-1;
    ListMovesClick(Self);
  end;
end;

procedure TKinematicsEditor.CBEnabledClick(Sender: TObject);
begin
  Movements.Enabled:=CBEnabled.Checked;
end;

procedure TKinematicsEditor.TBXMinChange(Sender: TObject);
begin
  Joint.RotationLimits.MinX:=TBXMin.Position;
  LXMin.Caption:=IntToStr(TBXMin.Position);
end;

procedure TKinematicsEditor.TBXMaxChange(Sender: TObject);
begin
  Joint.RotationLimits.MaxX:=TBXMax.Position;
  LXMax.Caption:=IntToStr(TBXMax.Position);
end;

function TKinematicsEditor.Joint: TJoint;
begin
  result:=Kinematics.Joints[ListJoints.ItemIndex];
end;

procedure TKinematicsEditor.TBYMinChange(Sender: TObject);
begin
  Joint.RotationLimits.MinY:=TBYMin.Position;
  LYMin.Caption:=IntToStr(TBYMin.Position);
end;

procedure TKinematicsEditor.TBYMaxChange(Sender: TObject);
begin
  Joint.RotationLimits.MaxY:=TBYMax.Position;
  LYMax.Caption:=IntToStr(TBYMax.Position);
end;

procedure TKinematicsEditor.TBZMinChange(Sender: TObject);
begin
  Joint.RotationLimits.MinZ:=TBZMin.Position;
  LZMin.Caption:=IntToStr(TBZMin.Position);
end;

procedure TKinematicsEditor.TBZMaxChange(Sender: TObject);
begin
  Joint.RotationLimits.MaxZ:=TBZMax.Position;
  LZMax.Caption:=IntToStr(TBZMax.Position);
end;

procedure TKinematicsEditor.ListJointsClick(Sender: TObject);
begin
  SBRemoveJoint.Enabled:=ListJoints.ItemIndex<>-1;

  PageJoints.Enabled:=SBRemoveJoint.Enabled;
    
  if ListJoints.ItemIndex<>-1 then
  with Joint do
  begin
    if Assigned(Block) then
       LabelBlock.Caption:=Block.Title
    else
       LabelBlock.Caption:='';

    if Assigned(Parent) then
       LabelParentBlock.Caption:=Parent.Title
    else
       LabelParentBlock.Caption:='';

    if not Assigned(IPosition) then
    begin
      IPosition:=TPointEditor.Create(Self);
      IPosition.Factor:=0.01;
      IPosition.Align:=alClient;
      TTeeVCL.AddFormTo(IPosition,TabJointBlock);
    end;

    IPosition.SelectPoint(Position);

    if not Assigned(IParentPosition) then
    begin
      IParentPosition:=TPointEditor.Create(Self);
      IParentPosition.Factor:=0.01;
      IParentPosition.Align:=alClient;
      TTeeVCL.AddFormTo(IParentPosition,TabJointParent);
    end;

    IParentPosition.SelectPoint(ParentPosition);

    with RotationLimits do
    begin
      TBXMin.Position:=Round(MinX);
      TBXMax.Position:=Round(MaxX);
      TBYMin.Position:=Round(MinY);
      TBYMax.Position:=Round(MaxY);
      TBZMin.Position:=Round(MinZ);
      TBZMax.Position:=Round(MaxZ);
    end;
  end;
end;

procedure TKinematicsEditor.SBAddJointClick(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=TBlockGallery.ChooseBlock(Self,Movements.Maker.Blocks);

  if Assigned(tmp) then
  begin
    Kinematics.Joints.AddJoint.Block:=tmp;
    ListJoints.Items.Add(tmp.Title);
    ListJoints.ItemIndex:=ListJoints.Items.Count-1;
    ListJointsClick(Self);
  end;
end;

procedure TKinematicsEditor.SBRemoveJointClick(Sender: TObject);
begin
  Kinematics.Joints.Delete(ListJoints.ItemIndex);
  ListJoints.Items.Delete(ListJoints.ItemIndex);
  ListJointsClick(Self);
end;

procedure TKinematicsEditor.Button1Click(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=TBlockGallery.ChooseBlock(Self,Movements.Maker.Blocks,Joint.Block);

  if Assigned(tmp) then
  begin
    Joint.Block:=tmp;
    LabelBlock.Caption:=tmp.Title;
  end;
end;

procedure TKinematicsEditor.Button2Click(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=TBlockGallery.ChooseBlock(Self,Movements.Maker.Blocks,Joint.Parent);

  if Assigned(tmp) then
  begin
    Joint.Parent:=tmp;
    LabelParentBlock.Caption:=tmp.Title;
  end;
end;

procedure TKinematicsEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;
  PageJoints.Enabled:=False;

  LabelBlock.Caption:='';
  LabelParentBlock.Caption:='';
end;

procedure TKinematics.SetMovements(const Value: TMovements);
begin
  FMovements.Assign(Value);
end;

procedure TKinematics.SetJoints(const Value: TJoints);
begin
  FJoints.Assign(Value);
end;

function TKinematicsEditor.Movement:TMovement;
begin
  result:=Movements[ListMoves.ItemIndex];
end;

procedure TKinematicsEditor.EBounceChange(Sender: TObject);
begin
  if Showing then
     Movement.Material.Bounce:=UDBounce.Position*0.01;
end;

procedure TKinematicsEditor.CBCircularClick(Sender: TObject);
begin
  Movement.Circular:=CBCircular.Checked;
end;

procedure TKinematicsEditor.Button3Click(Sender: TObject);

  function AskLink(ALink:TPropertyLink):Boolean;
  var tmpInst : TObject;
      tmpProp : String;
  begin
    tmpInst:=ALink.Instance;
    tmpProp:=ALink.PropertyName;

    result:=TMakerPropertySelector.ModalShow(Self,Movements.Maker.Blocks,
                                             tmpInst,tmpProp);

    if result then
    with ALink do
    begin
      Instance:=tmpInst as TComponent;
      PropertyName:=tmpProp;
    end;
  end;

begin
  BClearCenterLink.Enabled:=AskLink(Movement.CenterLink);
  SetLinkLabel(LCenterLink,Movement.CenterLink);
end;

procedure TKinematicsEditor.BClearCenterLinkClick(Sender: TObject);
begin
  Movement.CenterLink:=nil;
  BClearCenterLink.Enabled:=False;
  SetLinkLabel(LCenterLink,Movement.CenterLink);
end;

procedure TKinematics.TryToEnable;
begin
  TTeeAnimateAccess(TTeeAnimate).GlobalTimer.TryToEnable;
end;

{ TMaterial }

Constructor TMaterial.Create;
begin
  inherited;
  FBounce:=0.5;
  FDensity:=2;
  FFriction:=0.1;
end;

procedure TKinematicsEditor.EFrictionChange(Sender: TObject);
begin
  if Showing then
     Movement.Material.Friction:=UDFriction.Position*0.01;
end;

procedure TKinematicsEditor.EDensityChange(Sender: TObject);
begin
  if Showing then
     Movement.Material.Density:=UDDensity.Position*0.01;
end;

initialization
  MakerEvents.AddEvent(BlockAction_KinematicsCollision,'Kinematics.Collision');
end.
