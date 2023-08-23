unit TeeRoundRect;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeBlocks, TeCanvas, TeeProcs;

type
  TRoundPoints=packed record
    Points  : TPoint3DArray;
    Normals : TPoint3DArray;
    TextureCoords : Array of Single;
  end;

  TRoundCorners=class(TPersistent)
  private
    FLeftBottom  : TBlockEdge;
    FLeftTop     : TBlockEdge;
    FOnChanged   : TNotifyEvent;
    FRightBottom : TBlockEdge;
    FRightTop    : TBlockEdge;

    procedure CornerChanged(Sender:TObject);
    function GetRoundSize: TPointFloat;
    function GetSlices:Integer;

    procedure SetLeftBottom(const Value:TBlockEdge);
    procedure SetLeftTop(const Value:TBlockEdge);
    procedure SetRightBottom(const Value:TBlockEdge);
    procedure SetRightTop(const Value:TBlockEdge);
    procedure SetRoundSize(const Value: TPointFloat);
    procedure SetSlices(const Value: Integer);
    function GetRoundHeight: Double;
    procedure SetRoundHeight(const Value: Double);
    function GetRoundWidth: Double;
    procedure SetRoundWidth(const Value: Double);
  protected
    IOwner : TCustomBlock;

    procedure CalcPoints(const AWidth,ADepth:Single; var P:TRoundPoints);
    procedure FreePoints(var P:TRoundPoints);
  public
    Constructor Create(AOwner:TCustomBlock; const ASize:Single=10);
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property RoundSize:TPointFloat read GetRoundSize write SetRoundSize;
  published
    property LeftBottom:TBlockEdge read FLeftBottom write SetLeftBottom;
    property LeftTop:TBlockEdge read FLeftTop write SetLeftTop;
    property RightBottom:TBlockEdge read FRightBottom write SetRightBottom;
    property RightTop:TBlockEdge read FRightTop write SetRightTop;

    property RoundHeight:Double read GetRoundHeight write SetRoundHeight stored False;
    property RoundWidth:Double read GetRoundWidth write SetRoundWidth stored False;

    property Slices:Integer read GetSlices write SetSlices default 16;

    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TRoundRectBlock=class(TCustomCoverBlock)
  private
    FCorners : TRoundCorners;

    IList       : Integer;
    IListCover1 : Integer;
    IListCover2 : Integer;
    IListPen1   : Integer;
    IListPen2   : Integer;

    IPoints     : TRoundPoints;

    procedure CornersChanged(Sender:TObject);
    procedure DrawSides(const P:TRoundPoints; Invert:Boolean=False);
    procedure SetCorners(const Value: TRoundCorners);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Draw; override;
  published
    property Corners:TRoundCorners read FCorners write SetCorners;
  end;

  TImageBlock=class(TRoundRectBlock)
  protected
    procedure PrepareForGallery; override;
    procedure ReadState(Reader: TReader); override;
  public
    Constructor Create(AOwner: TComponent); override;
  end;

  TRoundRectEditor = class(TVisualEditor)
    Label41: TLabel;
    LabelRoundWidth: TLabel;
    Label43: TLabel;
    LabelRoundHeight: TLabel;
    Label67: TLabel;
    BlockRoundWidth: TScrollBar;
    BlockRoundHeight: TScrollBar;
    RGCorner: TRadioGroup;
    BlockCornerStyle: TComboFlat;
    BlockRoundSlices: TScrollBar;
    ChangeAll: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure RGCornerClick(Sender: TObject);
    procedure BlockRoundWidthChange(Sender: TObject);
    procedure BlockRoundHeightChange(Sender: TObject);
    procedure BlockCornerStyleChange(Sender: TObject);
    procedure BlockRoundSlicesChange(Sender: TObject);
    procedure ChangeAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    IModifying : Boolean;

    function CurrentCorner:TBlockEdge;
  public
    { Public declarations }
    Corners : TRoundCorners;
  end;

  THoleStyle=(hsRoundRect,hsSquare);

  THole=class(TPersistent)
  private
    FCenter   : TPointXYFloat;
    FCorners  : TRoundCorners;
    FFormat   : TBlockFormat;
    FSize     : TPointXYFloat;
    FStyle    : THoleStyle;

    IOwner    : TCustomBlock;

    procedure DataChanged(Sender:TObject);
    procedure SetCenter(const Value:TPointXYFloat);
    procedure SetCorners(const Value: TRoundCorners);
    procedure SetFormat(const Value:TBlockFormat);
    procedure SetSize(const Value:TPointXYFloat);
    procedure SetStyle(const Value:THoleStyle);
  public
    Constructor Create(AOwner:TCustomBlock);
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Center:TPointXYFloat read FCenter write SetCenter;
    property Corners:TRoundCorners read FCorners write SetCorners;
    property Format:TBlockFormat read FFormat write SetFormat;
    property Size:TPointXYFloat read FSize write SetSize;
    property Style:THoleStyle read FStyle write SetStyle default hsRoundRect;
  end;

  THoleBlock=class(TRoundRectBlock)
  private
    FHole : THole;

    IListSides : Integer;

    procedure SetHole(const Value:THole);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
  published
    property Hole:THole read FHole write SetHole;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  Math, TeeGLCanvas,
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  {$IFDEF D6}
  Types,
  {$ENDIF}
  OpenGL2;

type
  TCustomBlockAccess=class(TCustomBlock);
  TCylinderBlockAccess=class(TCylinderBlock);

{ THole }

Constructor THole.Create(AOwner:TCustomBlock);
begin
  inherited Create;
  IOwner:=AOwner;

  FCorners:=TRoundCorners.Create(IOwner);
  FCorners.OnChanged:=DataChanged;

  FCenter:=TPointXYFloat.Create(IOwner,50,DataChanged);
  FSize:=TPointXYFloat.Create(IOwner,50,DataChanged);

  FFormat:=TBlockFormat.Create(IOwner);
end;

Destructor THole.Destroy;
begin
  FFormat.Free;

  FSize.Free;
  FCenter.Free;
  FCorners.Free;

  inherited;
end;

procedure THole.DataChanged(Sender:TObject);
begin
  TCustomBlockAccess(IOwner).DeleteLists;
end;

procedure THole.Assign(Source: TPersistent);
begin
  if Source is THole then
  with THole(Source) do
  begin
    Self.FCenter.Assign(FCenter);
    Self.FCorners.Assign(FCorners);
    Self.FSize.Assign(FSize);
    Self.FStyle:=FStyle;
    Self.Format:=Format;
  end
  else
    inherited;
end;

procedure THole.SetCenter(const Value:TPointXYFloat);
begin
  FCenter.Assign(Value);
  IOwner.Repaint;
end;

procedure THole.SetSize(const Value:TPointXYFloat);
begin
  FSize.Assign(Value);
  IOwner.Repaint;
end;

procedure THole.SetStyle(const Value:THoleStyle);
begin
  FStyle:=Value;
  TCustomBlockAccess(IOwner).DeleteLists;
end;

procedure THole.SetCorners(const Value:TRoundCorners);
begin
  FCorners.Assign(Value);
  IOwner.Repaint;
end;

procedure THole.SetFormat(const Value:TBlockFormat);
begin
  FFormat.Assign(Value);
end;

{ THoleBlock }

Constructor THoleBlock.Create(AOwner: TComponent);
begin
  inherited;
  FHole:=THole.Create(Self);
end;

Destructor THoleBlock.Destroy;
begin
  FreeAndNil(FHole);
  inherited;
end;

procedure THoleBlock.Assign(Source: TPersistent);
begin
  if Source is THoleBlock then
     FHole.Assign(THoleBlock(Source).FHole);

  inherited;
end;

type
  TBlocksAccess=class(TBlocks);
  TGLCanvasAccess=class(TGLCanvas);
  TBlockTextureAccess=class(TBlockTexture);
  TBlockFormatAccess=class(TBlockFormat);

procedure THoleBlock.Draw;

  procedure DrawRoundOutline(const P:TPoint3DArray);

    procedure InternalDraw(const APos:Single);
    var t : Integer;
    begin
      glBegin(GL_LINE_LOOP);

      for t:=0 to Length(P)-1 do
          glVertex3f(P[t].X,-P[t].Y,APos);

      glEnd;
    end;

  begin
    InternalDraw(-1);

    if Size.Point.Y<>0 then
       InternalDraw(1);
  end;

  procedure DrawOutline(const R:TFloatRect); overload;
  begin
    with R do
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3f( Left,  Bottom, 1);
        glVertex3f( Right, Bottom, 1);
        glVertex3f( Right, Top,    1);
        glVertex3f( Left,  Top,    1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Left,  Top,    -1);
        glVertex3f( Left,  Bottom, -1);
        glVertex3f( Right, Bottom, -1);
        glVertex3f( Right, Top,    -1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Right, Top,    1);
        glVertex3f( Right, Top,    -1);
        glVertex3f( Right, Bottom, -1);
        glVertex3f( Right, Bottom, 1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Left, Top,    1);
        glVertex3f( Left, Bottom, 1);
        glVertex3f( Left, Bottom, -1);
        glVertex3f( Left, Top,    -1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Right, Top, -1);
        glVertex3f( Right, Top, 1);
        glVertex3f( Left,  Top, 1);
        glVertex3f( Left,  Top, -1);
      glEnd;
    end;
  end;

  procedure DrawOutline(const Left,Top,Right,Bottom:Integer); overload;
  var tmp : TFloatRect;
  begin
    tmp.Left:=Left;
    tmp.Top:=Top;
    tmp.Right:=Right;
    tmp.Bottom:=Bottom;
    DrawOutline(tmp);
  end;

var
  tmpHole : TFloatRect;

  procedure DrawCoverRect(var AList:Integer; AFormat:TBlockFormat; const ZPos:Single; Invert:Boolean);
  begin
    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(AFormat).Start;
    end;

    if AList=0 then
    begin
      AList:=CreateNewList;

      glBegin(GL_QUADS);

      if Invert then
      begin
        glNormal3i( 0, 0, -1);

        glVertex3f( -1, -1, ZPos);
        glVertex3f( -1, 1, ZPos);
        glVertex3f( tmpHole.Left, 1, ZPos);
        glVertex3f( tmpHole.Left, -1, ZPos);

        glVertex3f( tmpHole.Right, 1, ZPos);
        glVertex3f( 1, 1, ZPos);
        glVertex3f( 1, -1, ZPos);
        glVertex3f( tmpHole.Right, -1, ZPos);

        glVertex3f( tmpHole.Left, -1, ZPos);
        glVertex3f( tmpHole.Left, tmpHole.Bottom, ZPos);
        glVertex3f( tmpHole.Right, tmpHole.Bottom, ZPos);
        glVertex3f( tmpHole.Right, -1, ZPos);

        glVertex3f( tmpHole.Left, tmpHole.Top, ZPos);
        glVertex3f( tmpHole.Left, 1, ZPos);
        glVertex3f( tmpHole.Right, 1, ZPos);
        glVertex3f( tmpHole.Right, tmpHole.Top, ZPos);
      end
      else
      begin
        glNormal3i( 0, 0, 1);

        glVertex3f( tmpHole.Left, 1, ZPos);
        glVertex3f( -1, 1, ZPos);
        glVertex3f( -1, -1, ZPos);
        glVertex3f( tmpHole.Left, -1, ZPos);

        glVertex3f( 1, -1, ZPos);
        glVertex3f( 1, 1, ZPos);
        glVertex3f( tmpHole.Right, 1, ZPos);
        glVertex3f( tmpHole.Right, -1, ZPos);

        glVertex3f( tmpHole.Right, tmpHole.Bottom, ZPos);
        glVertex3f( tmpHole.Left, tmpHole.Bottom, ZPos);
        glVertex3f( tmpHole.Left, -1, ZPos);
        glVertex3f( tmpHole.Right, -1, ZPos);

        glVertex3f( tmpHole.Right, 1, ZPos);
        glVertex3f( tmpHole.Left, 1, ZPos);
        glVertex3f( tmpHole.Left, tmpHole.Top, ZPos);
        glVertex3f( tmpHole.Right, tmpHole.Top, ZPos);
      end;

      glEnd;

      glEndList;
    end
    else
      glCallList(AList);

    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(AFormat).Finish;
      TBlockFormatAccess(Format).Start;
    end;
  end;

  procedure DrawTubeRect(const R:TFloatRect);
  begin
    with R do
    begin
      glBegin(GL_QUADS);

      glNormal3i(-1,  0,  0);
       glTexCoord2s(0,0);
       glVertex3f( Left, Bottom, 1);
       glTexCoord2s(0,1);
       glVertex3f( Left, Bottom, -1);
       glTexCoord2s(1,1);
       glVertex3f( Left, Top,    -1);
       glTexCoord2s(1,0);
       glVertex3f( Left, Top,    1);

      glNormal3i( 1,  0,  0);
       glTexCoord2s(0,0);
       glVertex3f( Right, Top,    -1);
       glTexCoord2s(0,1);
       glVertex3f( Right, Bottom, -1);
       glTexCoord2s(1,1);
       glVertex3f( Right, Bottom, 1);
       glTexCoord2s(1,0);
       glVertex3f( Right, Top,    1);

      glNormal3i( 0, 1,  0);
       glTexCoord2s(0,0);
       glVertex3f( Right, Top, 1);
       glTexCoord2s(0,1);
       glVertex3f( Left,  Top, 1);
       glTexCoord2s(1,1);
       glVertex3f( Left,  Top, -1);
       glTexCoord2s(1,0);
       glVertex3f( Right, Top, -1);

      glNormal3i( 0, -1,  0);
       glTexCoord2s(0,0);
       glVertex3f( Left,  Bottom, -1);
       glTexCoord2s(0,1);
       glVertex3f( Left,  Bottom, 1);
       glTexCoord2s(1,1);
       glVertex3f( Right, Bottom, 1);
       glTexCoord2s(1,0);
       glVertex3f( Right, Bottom, -1);

      glEnd;
    end;
  end;

var
  Inner : TRoundPoints;
  tmpHasTextures : Boolean;

  procedure DrawRoundCover(var AList:Integer; AFormat:TBlockFormat; const ZPos:Single; Invert:Boolean);
  var
    tmpMax  : TPoint3DFloat;
    tmpMin  : TPoint3DFloat;
    IRangeX : Single;
    IRangeY : Single;

    procedure Vertex(Index:Integer);
    begin
      with IPoints.Points[Index] do
      begin
        if tmpHasTextures then
           glTexCoord2f((x-tmpMin.X)*IRangeX,(y-tmpMin.Y)*IRangeY);

        glVertex3f(X,-Y,-ZPos);
      end;

      with Inner.Points[Index] do
      begin
        if tmpHasTextures then
           glTexCoord2f((x-tmpMin.X)*IRangeX,(y-tmpMin.Y)*IRangeY);

        glVertex3f(X,-Y,-ZPos);
      end;
    end;

  var t : Integer;
  begin
    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(AFormat).Start;
    end;

    if AList=0 then
    begin
      AList:=CreateNewList;

      if tmpHasTextures then
      begin
        TGLCanvasAccess(ICanvas).CalcMinMax(IPoints.Points,tmpMin,tmpMax);

        if tmpMax.X=tmpMin.X then
           IRangeX:=1
        else
           IRangeX:=1/(tmpMax.X-tmpMin.X);

        if tmpMax.Y=tmpMin.Y then
           IRangeY:=1
        else
           IRangeY:=1/(tmpMax.Y-tmpMin.Y);
      end;

      glBegin(GL_QUAD_STRIP);

      if Invert then
         glNormal3i( 0, 0, -1)
      else
         glNormal3i( 0, 0, 1);

      Vertex(0);

      if Invert then
      for t:=Length(IPoints.Points)-1 downto 1 do
          Vertex(t)
      else
      for t:=1 to Length(IPoints.Points)-1 do
          Vertex(t);

      Vertex(0);

      glEnd;

      glEndList;
    end
    else
      glCallList(AList);

    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(AFormat).Finish;
      TBlockFormatAccess(Format).Start;
    end;
  end;

var Draw1 : Boolean;
    Draw2 : Boolean;
    CanCull : Boolean;
    tmpW    : Single;
    tmpH    : Single;
    tmpCenterX : Single;
    tmpCenterY : Single;
begin
  with FHole.Size.Point do
  begin
    tmpW:=X*0.01;
    tmpH:=Y*0.01;
  end;

  with FHole.Center.Point do
  begin
    tmpCenterX:=(X-50)*0.01;
    tmpCenterY:=(Y-50)*0.01;
  end;

  with tmpHole do
  begin
    Left:=tmpCenterX-tmpW;
    Top:=tmpCenterY+tmpH;
    Right:=tmpCenterX+tmpW;
    Bottom:=tmpCenterY-tmpH;
  end;

  if Format.Solid then
  begin
    Draw1:=(not Assigned(FBrush1)) or FBrush1.Solid;
    Draw2:=(not Assigned(FBrush2)) or FBrush2.Solid;

    CanCull:=(not ShouldDrawInterior) and Draw1 and Draw2;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if FHole.Style=hsSquare then
    begin
      if Draw1 then
         DrawCoverRect(IListCover1,FBrush1,1,False);

      if Draw2 then
         DrawCoverRect(IListCover2,FBrush2,-1,True);
    end;

    if IListSides=0 then
    begin
      IListSides:=CreateNewList;

      FCorners.CalcPoints(1,1,IPoints);
      DrawSides(IPoints,False);

      glEndList;
    end
    else
      glCallList(IListSides);

    if FHole.Style=hsSquare then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(FHole.Format).Start;

      DrawTubeRect(tmpHole);
    end
    else
    begin
      FHole.Corners.CalcPoints(tmpW,tmpH,Inner);

      tmpHasTextures:=(not Parent.DrawBlocks.Shadows.Visible) and
                      TBlockTextureAccess(Hole.Format.Texture).HasTexture;

      if Draw1 then // Cover Z0
         DrawRoundCover(IListCover1,FBrush1,1,True);

      if Draw2 then // Cover Z1
         DrawRoundCover(IListCover2,FBrush2,-1,False);

      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(FHole.Format).Start;

      // Inner rounded sides
      DrawSides(Inner,True);

      if TBlockFormatAccess(Format).PreparePen then
         DrawRoundOutline(IPoints.Points);

      if TBlockFormatAccess(Hole.Format).PreparePen then
         DrawRoundOutline(Inner.Points);

      FHole.FCorners.FreePoints(Inner);
    end;

    if CanCull then
       glDisable(GL_CULL_FACE);

    TBlockFormatAccess(FHole.Format).Finish;
    TBlockFormatAccess(Format).Start;
  end;

  if FHole.Style<>hsRoundRect then
     if TBlockFormatAccess(Format).PreparePen then
        DrawOutline(-1,-1,1,1);

  if FHole.Style=hsSquare then
     if TBlockFormatAccess(FHole.Format).PreparePen then
        DrawOutline(tmpHole);

  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Hole');
  {$ENDIF}
end;

procedure THoleBlock.SetHole(const Value:THole);
begin
  FHole.Assign(Value);
end;

procedure THoleBlock.DeleteLists;
begin
  inherited;

  DeleteList(IListSides);
end;

function THoleBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(0.5,1,0),'Hole.Size.X,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(Point3D(1,1,0.5),'Hole.Size.Y,MinMax:0;100').Format.Color:=clAqua;
  end;
end;

{ TRoundCorners }

Constructor TRoundCorners.Create(AOwner:TCustomBlock; const ASize:Single=10);
begin
  inherited Create;

  IOwner:=AOwner;

  FLeftTop:=TBlockEdge.Create(IOwner,ASize,CornerChanged);
  FRightTop:=TBlockEdge.Create(IOwner,ASize,CornerChanged);
  FLeftBottom:=TBlockEdge.Create(IOwner,ASize,CornerChanged);
  FRightBottom:=TBlockEdge.Create(IOwner,ASize,CornerChanged);
end;

Destructor TRoundCorners.Destroy;
begin
  FRightBottom.Free;
  FRightTop.Free;
  FLeftBottom.Free;
  FLeftTop.Free;

  inherited;
end;

procedure TRoundCorners.CornerChanged(Sender:TObject);
begin
  if Assigned(FOnChanged) then
     FOnChanged(Self);
end;

procedure TRoundCorners.Assign(Source: TPersistent);
begin
  if Source is TRoundCorners then
  with TRoundCorners(Source) do
  begin
    Self.FLeftTop.Assign(FLeftTop);
    Self.FRightTop.Assign(FRightTop);
    Self.FLeftBottom.Assign(FLeftBottom);
    Self.FRightBottom.Assign(FRightBottom);
  end
  else
    inherited;
end;

procedure TRoundCorners.SetLeftTop(const Value:TBlockEdge);
begin
  FLeftTop.Assign(Value);
end;

procedure TRoundCorners.SetRightTop(const Value:TBlockEdge);
begin
  FRightTop.Assign(Value);
end;

procedure TRoundCorners.SetLeftBottom(const Value:TBlockEdge);
begin
  FLeftBottom.Assign(Value);
end;

procedure TRoundCorners.SetRightBottom(const Value:TBlockEdge);
begin
  FRightBottom.Assign(Value);
end;

procedure TRoundCorners.SetSlices(const Value: Integer);
begin
  FLeftTop.Slices:=Value;
  FLeftBottom.Slices:=Value;
  FRightTop.Slices:=Value;
  FRightBottom.Slices:=Value;
end;

function TRoundCorners.GetRoundSize: TPointFloat;
begin
  result:=FLeftTop.Point;
end;

function TRoundCorners.GetSlices:Integer;
begin
  result:=FLeftTop.Slices;
end;

procedure TRoundCorners.SetRoundSize(const Value: TPointFloat);
begin
  FLeftTop.Point:=Value;
  FRightTop.Point:=Value;
  FLeftBottom.Point:=Value;
  FRightBottom.Point:=Value;

  IOwner.Repaint;
end;

procedure TRoundCorners.CalcPoints(const AWidth,ADepth:Single; var P:TRoundPoints);

  function CalcSize(const Value:TPointXYFloat):TFloatPoint;
  begin
    result.x:=AWidth * Value.Point.X*0.01;
    result.y:=ADepth * Value.Point.Y*0.01;
  end;

var Centers,
    tmpSize : Array[0..3] of TFloatPoint;

    t   : Integer;
    s,c : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    tmpT,
    tmpSliceTex,
    tmp : Single;
    tmpIndex,
    FSlices : Integer;
    tmpL : Integer;
begin
  tmpL:=4*Slices;

  with P do
  begin
    if Length(Points)<>tmpL then
       SetLength(Points,tmpL);

    if Length(Normals)<>tmpL then
       SetLength(Normals,tmpL);

    if Length(TextureCoords)<>tmpL then
       SetLength(TextureCoords,tmpL);
  end;

  tmpSize[0]:=CalcSize(LeftTop);
  Centers[0].X:=-AWidth+tmpSize[0].x;
  Centers[0].Y:=-ADepth+tmpSize[0].y;

  tmpSize[1]:=CalcSize(RightTop);
  Centers[1].X:=AWidth-tmpSize[1].x;
  Centers[1].Y:=-ADepth+tmpSize[1].y;

  tmpSize[2]:=CalcSize(RightBottom);
  Centers[2].X:=AWidth-tmpSize[2].x;
  Centers[2].Y:=ADepth-tmpSize[2].y;

  tmpSize[3]:=CalcSize(LeftBottom);
  Centers[3].X:=-AWidth+tmpSize[3].x;
  Centers[3].Y:=ADepth-tmpSize[3].y;

  FSlices:=Slices;

  tmp:=HalfPi/FSlices;

  tmpSliceTex:=tmpL*0.05;
  tmpT:=1/(tmpL+(AWidth)+(ADepth));

  with P do
  for t:=0 to FSlices-1 do
  begin
    SinCos(t*tmp,s,c);

    // Left-Top corner
    tmpIndex:=FSlices-t-1;

    with Normals[tmpIndex] do
    begin
      X:=s;
      Y:=c;
      Z:=0;
    end;

    with Points[tmpIndex] do
    begin
      X:=Centers[0].X-tmpSize[0].x*s;
      Y:=Centers[0].Y-tmpSize[0].y*c;
    end;

    TextureCoords[tmpIndex]:=tmpIndex*tmpT;

    // Right-Top corner
    tmpIndex:=FSlices+t;

    with Normals[tmpIndex] do
    begin
      X:=s;
      Y:=c;
      Z:=0;
    end;

    with Points[tmpIndex] do
    begin
      X:=Centers[1].X+tmpSize[1].x*s;
      Y:=Centers[1].Y-tmpSize[1].y*c;
    end;

    TextureCoords[tmpIndex]:=tmpSliceTex+(tmpIndex*tmpT);

    // Right-Bottom corner
    tmpIndex:=3*FSlices-t-1;

    with Normals[tmpIndex] do
    begin
      X:=s;
      Y:=c;
      Z:=0;
    end;

    with Points[tmpIndex] do
    begin
      X:=Centers[2].X+tmpSize[2].x*s;
      Y:=Centers[2].Y+tmpSize[2].y*c;
    end;

    TextureCoords[tmpIndex]:=(2*tmpSliceTex)+(tmpIndex*tmpT);

    // Left-Bottom corner
    tmpIndex:=3*FSlices+t;

    with Normals[tmpIndex] do
    begin
      X:=s;
      Y:=c;
      Z:=0;
    end;

    with Points[tmpIndex] do
    begin
      X:=Centers[3].X-tmpSize[3].x*s;
      Y:=Centers[3].Y+tmpSize[3].y*c;
    end;

    TextureCoords[tmpIndex]:=(3*tmpSliceTex)+(tmpIndex*tmpT);
  end;
end;

type
  TBorderAccess=class(TBlockBorder);

procedure TRoundCorners.FreePoints(var P: TRoundPoints);
begin
  P.Points:=nil;
  P.Normals:=nil;
  P.TextureCoords:=nil;
end;

function TRoundCorners.GetRoundHeight: Double;
begin
  result:=LeftTop.Y;
end;

procedure TRoundCorners.SetRoundHeight(const Value: Double);
begin
  LeftTop.Y:=Value;
  LeftBottom.Y:=Value;
  RightTop.Y:=Value;
  RightBottom.Y:=Value;
end;

function TRoundCorners.GetRoundWidth: Double;
begin
  result:=LeftTop.X;
end;

procedure TRoundCorners.SetRoundWidth(const Value: Double);
begin
  LeftTop.X:=Value;
  LeftBottom.X:=Value;
  RightTop.X:=Value;
  RightBottom.X:=Value;
end;

{ TRoundRectBlock }

Constructor TRoundRectBlock.Create(AOwner: TComponent);
begin
  inherited;

  FCorners:=TRoundCorners.Create(Self);
  FCorners.OnChanged:=CornersChanged;

 // BAD SOLUTION: (see Stairs IStepBlock.Format:=Format, Border.Assign calls
 // IChanged too many times...
 // TBorderAccess(Format.Border).IChanged:=CornersChanged;
end;

Destructor TRoundRectBlock.Destroy;
begin
  FCorners.FreePoints(IPoints);
  FCorners.Free;
  inherited;
end;

procedure TRoundRectBlock.CornersChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TRoundRectBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListCover1);
  DeleteList(IListCover2);
  DeleteList(IListPen1);
  DeleteList(IListPen2);
end;

function TRoundRectBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(0.5,1,0),'Corners.RoundWidth,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(Point3D(1,1,0.5),'Corners.RoundHeight,MinMax:0;100').Format.Color:=clAqua;
  end;
end;

procedure TRoundRectBlock.DrawSides(const P:TRoundPoints;
                                    Invert:Boolean=False);

  procedure AddPoint(const Index:Integer);
  var tmpTexture : Single;
  begin
    with P do
    begin
      with Normals[Index] do
           glNormal3f(X,Y,Z);

      tmpTexture:=TextureCoords[Index];

      with Points[Index] do
      begin
        glTexCoord2f(tmpTexture,0);
        glVertex3f(X,-Y,1);

        glTexCoord2f(tmpTexture,1);
        glVertex3f(X,-Y,-1);
      end;
    end;
  end;

var t    : Integer;
    tmpL : Integer;
begin
  glBegin(GL_QUAD_STRIP);

  tmpL:=Length(P.Points)-1;

  if Invert then
  begin
    for t:=0 to tmpL do
        AddPoint(t);

    AddPoint(0);
  end
  else
  begin
    for t:=tmpL downto 0 do
        AddPoint(t);

    AddPoint(tmpL);
  end;

  glEnd;
end;

procedure TRoundRectBlock.Draw;

  procedure DrawCover(var AList,AListPen:Integer; AFormat:TBlockFormat; const YPos:Single; Invert:Boolean);
  var t : Integer;
  begin
    if AList=0 then
    begin
      with IPoints do
      begin
        for t:=0 to Length(Points)-1 do
            Points[t].Z:=YPos;

        TBlockFormatAccess(AFormat).ConvexPolygon(AList,Points,Invert);
        TBlockFormatAccess(AFormat).PolylineList(AListPen,Points);
      end;
    end
    else
      glCallList(AList);

    if TBlockFormatAccess(AFormat).PreparePen then
    begin
      glCallList(AListPen);
      TBlockFormatAccess(AFormat).FinishPen;
    end;
  end;

var
  Draw1 : Boolean;
  Draw2 : Boolean;
  CanCull : Boolean;
  tmp : TBlockFormat;
begin
  if Format.Solid then
  begin
    Draw1:=(not Assigned(FBrush1)) or FBrush1.Solid;
    Draw2:=(not Assigned(FBrush2)) or FBrush2.Solid;

    CanCull:=(Size.Y<>0) and (not ShouldDrawInterior) and Draw1 and Draw2;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if Assigned(FBrush1) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(FBrush1).Start;
      tmp:=FBrush1;
    end
    else
      tmp:=Format;

    if IList=0 then
       FCorners.CalcPoints(1,1,IPoints);

    if Draw1 then
       DrawCover(IListCover1,IListPen1,tmp,1,True);

    if Assigned(FBrush1) then
    begin
      TBlockFormatAccess(FBrush1).Finish;
      TBlockFormatAccess(Format).Start;
    end;

    if Size.Point.Y<>0 then
    begin
      if Assigned(FBrush2) then
      begin
        TBlockFormatAccess(Format).Finish;
        TBlockFormatAccess(FBrush2).Start;
        tmp:=FBrush2;
      end
      else
        tmp:=Format;

      if Draw2 then
         DrawCover(IListCover2,IListPen2,tmp,-1,False);

      if Assigned(FBrush2) then
      begin
        TBlockFormatAccess(FBrush2).Finish;
        TBlockFormatAccess(Format).Start;
      end;

      if IList=0 then
      begin
        IList:=CreateNewList;
        DrawSides(IPoints);
        glEndList;
      end
      else
        glCallList(IList);
    end;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;
end;

procedure TRoundRectBlock.SetCorners(const Value:TRoundCorners);
begin
  FCorners.Assign(Value);
  Repaint;
end;

function TRoundRectEditor.CurrentCorner:TBlockEdge;
begin
  with Corners do
  case RGCorner.ItemIndex of
    0: result:=LeftTop;
    1: result:=RightTop;
    2: result:=LeftBottom;
  else
    result:=RightBottom;
  end;
end;

procedure TRoundRectEditor.FormShow(Sender: TObject);
begin
  if Assigned(Corners) then
  begin
    RGCorner.ItemIndex:=0;
    RGCornerClick(Self);

    BlockRoundSlices.Position:=Corners.Slices;
  end;
  
  ChangeAllClick(Self);

  IModifying:=False;
end;

procedure TRoundRectEditor.RGCornerClick(Sender: TObject);
var Old : Boolean;
begin
  Old:=IModifying;
  IModifying:=True;

  with CurrentCorner do
  begin
    BlockRoundWidth.Position:=Round(X);
    BlockRoundHeight.Position:=Round(Y);
    BlockCornerStyle.ItemIndex:=Ord(Style);
  end;

  LabelRoundWidth.Caption:=IntToStr(BlockRoundWidth.Position);
  LabelRoundHeight.Caption:=IntToStr(BlockRoundHeight.Position);

  IModifying:=Old;
end;

procedure TRoundRectEditor.BlockRoundWidthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if ChangeAll.Checked then
    with Corners do
    begin
      LeftTop.X:=BlockRoundWidth.Position;
      RightTop.X:=BlockRoundWidth.Position;
      LeftBottom.X:=BlockRoundWidth.Position;
      RightBottom.X:=BlockRoundWidth.Position;
    end
    else
       CurrentCorner.X:=BlockRoundWidth.Position;

    LabelRoundWidth.Caption:=IntToStr(BlockRoundWidth.Position);
    MarkDirty;
  end;
end;

procedure TRoundRectEditor.BlockRoundHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if ChangeAll.Checked then
    with Corners do
    begin
      LeftTop.Y:=BlockRoundHeight.Position;
      RightTop.Y:=BlockRoundHeight.Position;
      LeftBottom.Y:=BlockRoundHeight.Position;
      RightBottom.Y:=BlockRoundHeight.Position;
    end
    else
      CurrentCorner.Y:=BlockRoundHeight.Position;

    LabelRoundHeight.Caption:=IntToStr(BlockRoundHeight.Position);
    MarkDirty;
  end;
end;

procedure TRoundRectEditor.BlockCornerStyleChange(Sender: TObject);
var tmp : TBlockEdgeStyle;
begin
  if not IModifying then
  begin
    tmp:=TBlockEdgeStyle(BlockCornerStyle.ItemIndex);

    if ChangeAll.Checked then
    with Corners do
    begin
      LeftTop.Style:=tmp;
      RightTop.Style:=tmp;
      LeftBottom.Style:=tmp;
      RightBottom.Style:=tmp;
    end
    else
      CurrentCorner.Style:=tmp;

    MarkDirty;
  end;
end;

procedure TRoundRectEditor.BlockRoundSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Corners.Slices:=BlockRoundSlices.Position;
    MarkDirty;
  end;
end;

procedure TRoundRectEditor.ChangeAllClick(Sender: TObject);
begin
  RGCorner.Enabled:=not ChangeAll.Checked;
end;

procedure TRoundRectEditor.FormCreate(Sender: TObject);
begin
  IModifying:=True;
end;

{ TImageBlock }

Constructor TImageBlock.Create(AOwner: TComponent);
begin
  inherited;

  if Assigned(AOwner) and (csDesigning in AOwner.ComponentState) and
     (not (csLoading in AOwner.ComponentState)) then
          PrepareForGallery;
end;

procedure TImageBlock.PrepareForGallery;
begin
  inherited;

  Format.Texture.PictureLink:=TeeMakerLibraryTag+'Basic\Rocks.bmp';
  Format.Color:=clDefault;

  Size.Point.Y:=0;
end;

procedure TImageBlock.ReadState(Reader: TReader);
begin
  inherited;

  Size.Point.Y:=0;
end;

initialization
  RegisterBlocks( [
    THoleBlock,
    TRoundRectBlock,
    TImageBlock ]);
end.
