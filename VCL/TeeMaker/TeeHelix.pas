unit TeeHelix;
{$I TeeDefs.inc}

interface

uses
  Classes,
  {$IFDEF D6}
  Types,
  {$ENDIF}
  SysUtils, Graphics, Forms,
  TeeBlocks,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeCanvas, Controls, StdCtrls;

type
  THelixBlock=class(TCustomCoverBlock)
  private
    FSlices    : Integer;
    FStacks    : Integer;
    FTwists    : Double;
    FTwistSize : Double;

    IList      : Integer;

    function IsTwistsStored: Boolean;
    function IsTwistSizeStored: Boolean;
    procedure SetSlices(const Value: Integer);
    procedure SetStacks(const Value: Integer);
    procedure SetTwists(const Value: Double);
    procedure SetTwistSize(const Value: Double);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner:TComponent); override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;
  published
    property Slices:Integer read FSlices write SetSlices default 32;
    property Stacks:Integer read FStacks write SetStacks default 32;
    property Twists:Double read FTwists write SetTwists stored IsTwistsStored;
    property TwistSize:Double read FTwistSize write SetTwistSize stored IsTwistSizeStored;
  end;

  THelixEditor = class(TVisualEditor)
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    LHelixSlices: TLabel;
    LHelixStacks: TLabel;
    LHelixTwists: TLabel;
    Label89: TLabel;
    LHelixTwistSize: TLabel;
    BlockHelixTwists: TScrollBar;
    BlockHelixSlices: TScrollBar;
    BlockHelixStacks: TScrollBar;
    BlockHelixTwistSize: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure BlockHelixTwistsChange(Sender: TObject);
    procedure BlockHelixSlicesChange(Sender: TObject);
    procedure BlockHelixStacksChange(Sender: TObject);
    procedure BlockHelixTwistSizeChange(Sender: TObject);
  private
    { Private declarations }
    Helix : THelixBlock;
    IModifying : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math,
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  OpenGL2, TeeProcs, TeeGLCanvas;

{ THelixBlock }

Constructor THelixBlock.Create(AOwner: TComponent);
begin
  inherited;
  FSlices:=32;
  FStacks:=32;
  FTwists:=3;
  FTwistSize:=1;
end;

procedure THelixBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
end;

function THelixBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(0.5,1,0),'Twists,MinMax:0;1000').Format.Color:=clYellow;
    AddHandle(Point3D(1,1,0.5),'TwistSize,MinMax:0;1000').Format.Color:=clAqua;
  end;
end;

procedure THelixBlock.Assign(Source: TPersistent);
begin
  if Source is THelixBlock then
  with THelixBlock(Source) do
  begin
    Self.FSlices:=FSlices;
    Self.FStacks:=FStacks;
    Self.FTwists:=FTwists;
    Self.FTwistSize:=FTwistSize;
  end;

  inherited;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

function THelixBlock.GetEditor:String;
begin
  result:='THelixEditor';
end;

procedure THelixBlock.Draw;

type
  THelixVertex=record
    Normal0,
    Normal1,
    Vertex0,
    Vertex1  : TPoint3DFloat;
  end;

var
  thetaInc,
  phiInc : Single;

  procedure CalcHelix;
  var
    tmpMaxTheta : Integer;
    IVertexes   : Array of Array of THelixVertex;

    procedure CalcVertexes;
    var
      ZPos : Single;

      procedure CalcVertex(out Vertex:TPoint3DFloat; const Phi,Theta:TPointSinCos);
      begin
        with Vertex do
        begin
          X:=Theta.Y*(2+Phi.Y);
          Y:=Theta.X*(2+Phi.Y);
          Z:=ZPos+TwistSize*Phi.X;
        end;
      end;

    var theta0,
        phi0,
        theta1,
        phi1,
        phi2 : TPointSinCos;

        tmpVertex1,
        tmpVertex2 : TPoint3DFloat;

        t,tt : Integer;
    begin
      phi0.X:=0;
      phi0.Y:=1;

      SetLength(IVertexes,FSlices,tmpMaxTheta+1);

      for t:=0 to FSlices-1 do
      begin
        SinCos((t+1)*phiInc,phi1.X,phi1.Y);
        SinCos((t+2)*phiInc,phi2.X,phi2.Y);

        SinCos(tmpMaxTheta,theta1.X,theta1.Y);

        for tt:=tmpMaxTheta+1 downto 1 do
        begin
          ZPos:=(tt-1)*thetaInc;

          SinCos(ZPos,theta0.X,theta0.Y);

          CalcVertex(IVertexes[t,tt-1].Vertex0,phi0,theta0);
          CalcVertex(tmpVertex1,phi0,theta1);
          CalcVertex(tmpVertex2,phi1,theta1);

          IVertexes[t,tt-1].Normal0:=CalculateNormal(IVertexes[t,tt-1].Vertex0,tmpVertex1,tmpVertex2);

          CalcVertex(IVertexes[t,tt-1].Vertex1,phi1,theta0);
          CalcVertex(tmpVertex1,phi2,theta1);

          IVertexes[t,tt-1].Normal1:=CalculateNormal(IVertexes[t,tt-1].Vertex1,tmpVertex2,tmpVertex1);

          theta1:=theta0;
        end;

        phi0:=phi1;
      end;
    end;

  var t,tt : Integer;
      tmpTeX2,
      tmpTeY2 : Single;
  begin
    tmpMaxTheta:=Round(Twists*(FStacks-1));

    CalcVertexes;

    tmpTeX2:=1/FSlices;
    tmpTeY2:=Twists/(tmpMaxTheta+1);

    for t:=0 to FSlices-1 do
    begin
      glBegin(GL_QUAD_STRIP);

      for tt:=tmpMaxTheta downto 0 do
      with IVertexes[t,tt] do
      begin
        glNormal3fv(@normal0);
        glTexCoord2f(t*tmpTeX2,tt*tmpTeY2);
        glVertex3fv(@vertex0);

        glNormal3fv(@normal1);
        glTexCoord2f((t+1)*tmpTeX2,tt*tmpTeY2);
        glVertex3fv(@vertex1);
      end;

      glEnd;
    end;

    IVertexes:=nil;
  end;

  procedure DoHelix;
  begin
    if IList=0 then
    begin
      IList:=CreateNewList;
      CalcHelix;
      glEndList;
    end
    else
      glCallList(IList);
  end;

  procedure DrawCover(const AFormat:TBlockFormat; const APos:Single; Invert:Boolean);
  var sinU,
      cosU : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};

    procedure DrawTriangle(const Index:Integer);
    var tmpSin,
        tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        tmp    : Single;
    begin
      SinCos(Index*phiInc,tmpSin,tmpCos);

      tmp:=2+tmpCos;

      glTexCoord2f(tmpSin,tmpCos);
      glVertex3f(cosU*tmp,sinU*tmp,APos+TwistSize*tmpSin);
    end;

  var t : Integer;
  begin
    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(AFormat).Start;
    end;

    SinCos(APos,sinU,cosU);

    glBegin(GL_TRIANGLE_FAN);

    glVertex3f(cosU*3,sinU*3,APos);

    if Invert then
    for t:=FSlices downto 0 do
        DrawTriangle(t)
    else
    for t:=0 to FSlices do
        DrawTriangle(t);

    glEnd;

    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(AFormat).Finish;
      TBlockFormatAccess(Format).Start;
    end;
  end;

var OldStyle : TTeeCanvasSurfaceStyle;
    CanCull  : Boolean;
    Draw1    : Boolean;
    Draw2    : Boolean;
begin
  glTranslatef(0,0,-1);
  glScalef(1/Pi,1/Pi,2/((TwoPi*Twists)+1));

  phiInc:=TwoPi/Max(1,FSlices);
  thetaInc:=TwoPi/Max(1,FStacks);

  if Format.Solid then
  begin
    Draw1:=(not Assigned(FBrush1)) or FBrush1.Solid;
    Draw2:=(not Assigned(FBrush2)) or FBrush2.Solid;

    CanCull:=(not ShouldDrawInterior) and Draw1 and Draw2;

    if CanCull then
       glEnable(GL_CULL_FACE);

    DoHelix;

    if Draw1 then
       DrawCover(FBrush1,0,False);

    if Draw2 then
       DrawCover(FBrush2,Round(Twists*(Stacks-1))*thetaInc,True);

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if TBlockFormatAccess(Format).PreparePen then
  begin
    {$IFDEF BLOCKS}
    OldStyle:=ICanvas.getDrawStyle;
    ICanvas.setDrawStyle(tcsWire);
    {$ELSE}
    OldStyle:=ICanvas.DrawStyle;
    ICanvas.DrawStyle:=tcsWire;
    {$ENDIF}

    DoHelix;

    {$IFDEF BLOCKS}
    ICanvas.setDrawStyle(OldStyle);
    {$ELSE}
    ICanvas.DrawStyle:=OldStyle;
    {$ENDIF}
    TBlockFormatAccess(Format).FinishPen;
  end;
end;

function THelixBlock.IsTwistsStored: Boolean;
begin
  result:=FTwists<>3;
end;

function THelixBlock.IsTwistSizeStored: Boolean;
begin
  result:=FTwistSize<>1;
end;

procedure THelixBlock.SetSlices(const Value: Integer);
begin
  FSlices:=Value;
  DeleteLists;
end;

procedure THelixBlock.SetStacks(const Value: Integer);
begin
  FStacks:=Value;
  DeleteLists;
end;

procedure THelixBlock.SetTwists(const Value: Double);
begin
  FTwists:=Value;
  DeleteLists;
end;

procedure THelixBlock.SetTwistSize(const Value: Double);
begin
  FTwistSize:=Value;
  DeleteLists;
end;

procedure THelixEditor.FormShow(Sender: TObject);
begin
  Helix:=THelixBlock(Tag);

  if Assigned(Helix) then
  with Helix do
  begin
    IModifying:=True;

    BlockHelixTwists.Position:=Round(Twists*100);
    LHelixTwists.Caption:=FormatFloat('#.##',Twists);

    BlockHelixSlices.Position:=Slices;
    LHelixSlices.Caption:=TeeStr(BlockHelixSlices.Position);

    BlockHelixStacks.Position:=Stacks;
    LHelixStacks.Caption:=TeeStr(BlockHelixStacks.Position);

    BlockHelixTwistSize.Position:=Round(500*TwistSize);
    LHelixTwistSize.Caption:=FormatFloat('#.##',TwistSize);

    IModifying:=False;
  end;
end;

procedure THelixEditor.BlockHelixTwistsChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Helix.Twists:=BlockHelixTwists.Position*0.01;
    LHelixTwists.Caption:=FormatFloat('#.##',Helix.Twists);
    MarkDirty;
  end;
end;

procedure THelixEditor.BlockHelixSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Helix.Slices:=BlockHelixSlices.Position;
    LHelixSlices.Caption:=TeeStr(BlockHelixSlices.Position);
    MarkDirty;
  end;
end;

procedure THelixEditor.BlockHelixStacksChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Helix.Stacks:=BlockHelixStacks.Position;
    LHelixStacks.Caption:=TeeStr(BlockHelixStacks.Position);
    MarkDirty;
  end;
end;

procedure THelixEditor.BlockHelixTwistSizeChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Helix.TwistSize:=(BlockHelixTwistSize.Position/500);
    LHelixTwistSize.Caption:=FormatFloat('#.##',Helix.TwistSize);
    MarkDirty;
  end;
end;

initialization
  RegisterBlock(THelixBlock);
  RegisterClass(THelixEditor);
end.
