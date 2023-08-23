unit TeeMesh;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls, QButtons, QStdCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  {$ENDIF}
  TeCanvas, TeeBlocks, TeeExtruded, TeePointEditor,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeProcs, TeeTerrain, TeeTriangulate;

type
  TTrianglePoint=packed record
    Color    : TRGBAlpha;
    Point    : TPoint3DFloat;
    UseColor : Boolean;
  end;

  TTriangle=packed record
    Point0 : Integer;
    Point1 : Integer;
    Point2 : Integer;
  end;

  TMeshBlock=class(TPointerBlock)
  private
    FSelectedTriangle : Integer;

    IList     : Integer;
    IListPen  : Integer;
    IRange    : TPoint3DFloat;

    function GetX(Index:Integer):TTriValue;
    function GetZ(Index:Integer):TTriValue;
    procedure SetSelectedTriangle(const Value: Integer);
    procedure SetXYZ(Dest,Source:Integer);

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    Picking : Boolean;

    procedure DataChanged;
    procedure DefineProperties(Filer:TFiler); override;
    procedure DeleteLists; override;
    function GetEditor:String; override;
    procedure PrepareForGallery; override;
    function SelectedPoint:TPoint3DFloat; override;
  public
    Points    : packed Array of TTrianglePoint;
    Triangles : packed Array of TTriangle;

    Destructor Destroy; override;

    function AddPoint(const P:TPoint3DFloat):Integer; overload;
    function AddPoint(const X,Y,Z:Single): Integer; overload;
    function AddPoint(const X,Y,Z:Single; const Color:TColor): Integer; overload;
    function ClickedPoint(X,Y:Integer):Integer;
    procedure DeletePoint(Index: Integer);
    procedure MovePoint(Index:Integer; const Delta:TPoint3DFloat; LimitBounds:Boolean=True);
    procedure SetPointColor(Index:Integer; const AColor:TColor);

    function AddTriangle(Index0,Index1,Index2:Integer):Integer; overload;
    function AddTriangle(const P0,P1,P2:TPoint3DFloat):Integer; overload;
    procedure DeleteTriangle(TriangleIndex:Integer);

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
    procedure SubDivide(TriangleIndex:Integer);
    function TriangleCenter(TriangleIndex:Integer):TPoint3DFloat;
    procedure Triangulate;

    property SelectedTriangle:Integer read FSelectedTriangle write SetSelectedTriangle;
  end;

  TMeshEditor = class(TForm)
    PageControl1: TPageControl;
    TabTriangles: TTabSheet;
    TabPointer: TTabSheet;
    Label61: TLabel;
    BlockPointer: TComboFlat;
    BlockColorEach: TCheckBox;
    TabSheet4: TTabSheet;
    LabelTotalTriangles: TLabel;
    LabelTotalPoints: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    TabPoints: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    EPoint: TEdit;
    UDPoint: TUpDown;
    Label1: TLabel;
    SpeedButton2: TSpeedButton;
    ETriangle: TEdit;
    UDTriangle: TUpDown;
    Button1: TButton;
    Label5: TLabel;
    EPoint0: TEdit;
    UDPoint0: TUpDown;
    Label6: TLabel;
    EPoint1: TEdit;
    UDPoint1: TUpDown;
    Label7: TLabel;
    EPoint2: TEdit;
    UDPoint2: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ETriangleChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure BlockPointerChange(Sender: TObject);
    procedure BlockColorEachClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EPointChange(Sender: TObject);
  private
    { Private declarations }

    IPoints  : TPointEditor;
    ThePoint : TPointXYZColor;

    function FirstTriangleWithPoint(APoint:Integer):Integer;
    procedure PointChanged(Sender: TObject);
    procedure RefreshInfo;
    procedure SetPointData(APoint:Integer; P:TPointXYZColor);
  public
    { Public declarations }

    Mesh : TMeshBlock;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeGLCanvas, OpenGL2, TeePenDlg, TeeBlockEditor, Math, TeeGeometry;

{ TMeshBlock }

Destructor TMeshBlock.Destroy;
begin
  Triangles:=nil;
  Points:=nil;
  inherited;
end;

procedure TMeshBlock.Assign(Source: TPersistent);
var tmpL : Integer;
    t    : Integer;
begin
  if Source is TMeshBlock then
  with TMeshBlock(Source) do
  begin
    tmpL:=Length(Points);
    SetLength(Self.Points,tmpL);

    for t:=0 to tmpL-1 do
        Self.Points[t]:=Points[t];

    tmpL:=Length(Triangles);
    SetLength(Self.Triangles,tmpL);

    for t:=0 to tmpL-1 do
        Self.Triangles[t]:=Triangles[t];

    Self.DataChanged;
  end;

  inherited;
end;

function GLColor(const C:TRGBAlpha):TColor;
begin
  with C do
       result:=(Red or (Green shl 8) or (Blue shl 16));
end;

procedure TMeshBlock.MovePoint(Index:Integer; const Delta:TPoint3DFloat; LimitBounds:Boolean=True);
begin
  with Points[Index].Point do
  begin
    X:=X+Delta.X;

    if X<IMin.X then
       X:=IMin.X
    else
    if X>IMax.X then
       X:=IMax.X;

    Y:=Y+Delta.Y;

    if Y<IMin.Y then
       Y:=IMin.Y
    else
    if Y>IMax.Y then
       Y:=IMax.Y;

    Z:=Z+Delta.Z;

    if Z<IMin.Z then
       Z:=IMin.Z
    else
    if Z>IMax.Z then
       Z:=IMax.Z;
  end;

  DataChanged;
end;

procedure TMeshBlock.SetPointColor(Index:Integer; const AColor:TColor);
begin
  Points[Index].Color:=TBlockFormat.ColorToGL(AColor);
  Points[Index].UseColor:=True;
  DataChanged;
end;

type
  TBlockFormatAccess=class(TBlockFormat);
  TBlockAccess=class(TCustomBlock);
  TBlocksAccess=class(TBlocks);
  TBlockTextureAccess=class(TBlockTexture);

{.$DEFINE TEEPICKSELECT}

function TMeshBlock.ClickedPoint(X,Y:Integer):Integer;
{$IFDEF TEEPICKSELECT}
var
  tmp    : Integer;
  names  : Integer;
  Buffer : TPickBuffer;

  procedure GetResult;
  var t : Integer;
  begin
    result:=Buffer[tmp+3];
    Dec(result);

    for t:=1 to names-1 do
        result:=Buffer[tmp+3+t]-1;
  end;
{$ENDIF}

var
{$IFNDEF TEEPICKSELECT}
    tmpColor : Cardinal;
    viewport : THomogeneousIntVector;
{$ELSE}
    tmpHits: Integer;
    tmpZ   : Single;
    t      : Integer;
    z1,z2  : Single;
{$ENDIF}
begin
  result:=-1;

  TBlocksAccess(Parent.DrawBlocks).PreparePicking({$IFDEF TEEPICKSELECT}Buffer,{$ENDIF}X,Y);

  Picking:=True;
  try
    ICanvas:=TBlocksAccess(Parent.DrawBlocks).ICanvas;

    {$IFDEF TEEPICKSELECT}
    glPushName(0);
    {$ENDIF}

    TBlocksAccess(Parent).DoDrawItem(Self);

    {$IFDEF TEEPICKSELECT}
    glPopName;
    {$ENDIF}
  finally
    Picking:=False;
  end;

  {$IFDEF TEEPICKSELECT}
  tmpHits:=TBlocksAccess(Parent.DrawBlocks).DoPicking;

  if tmpHits>0 then
  begin
    tmp:=0;
    tmpZ:=0;

    for t:=0 to tmpHits-1 do
    begin
      names:=Buffer[tmp];

      z1:=(Buffer[tmp+1] and $7FFFFFFF);
      z2:=(Buffer[tmp+2] and $7FFFFFFF);

      if tmp=0 then
      begin
        GetResult;

        if z1<z2 then
           tmpZ:=z1
        else
           tmpZ:=z2
      end
      else
      if z1<tmpZ then
      begin
        tmpZ:=z1;
        GetResult;
      end
      else
      if z2<tmpZ then
      begin
        tmpZ:=z2;
        GetResult;
      end;

      Inc(tmp,3+names);
    end;
  end;
  {$ELSE}
  TBlocksAccess(Parent.DrawBlocks).DoPicking;

  glGetIntegerv(GL_VIEWPORT, @viewport);
  glReadPixels(Round(X), Round(viewport[3]-Y-1), 1, 1, GL_RGBA, GL_INT, @tmpColor);

  if tmpColor<>$FFFFFFFF then
     result:=tmpColor;
  {$ENDIF}
end;

procedure TMeshBlock.Draw;

  procedure CalcRange;
  var t : Integer;
  begin
    with Points[0].Point do
    begin
      IMin.X:=X;
      IMax.X:=X;

      IMin.Y:=Y;
      IMax.Y:=Y;

      IMin.Z:=Z;
      IMax.Z:=Z;
    end;

    for t:=0 to Length(Points)-1 do
    with Points[t].Point do
    begin
      if X<IMin.X then IMin.X:=X else
      if X>IMax.X then IMax.X:=X;

      if Y<IMin.Y then IMin.Y:=Y else
      if Y>IMax.Y then IMax.Y:=Y;

      if Z<IMin.Z then IMin.Z:=Z else
      if Z>IMax.Z then IMax.Z:=Z;
    end;

    IRange.X:=IMax.X-IMin.X;
    IRange.Y:=IMax.Y-IMin.Y;
    IRange.Z:=IMax.Z-IMin.Z;
  end;

var
  OldColor : TColor;
  tmpChangedColor : Boolean;

  procedure AddVertex(Index:Integer);
  begin
    with Points[Index] do
    begin
      if UseColor then
      begin
        glColor4ubv(@Color);
        tmpChangedColor:=True;
      end
      else
      if tmpChangedColor then
      begin
        TBlockFormatAccess(Format).SetDirectColor(OldColor);
        tmpChangedColor:=False;
      end;

      with Point do
           glVertex3f(X,Z,-Y);
    end;
  end;

var
  tmpL : Integer;

  procedure DrawSimpleTriangles;
  var t : Integer;
  begin
    glBegin(GL_TRIANGLES);

    for t:=0 to tmpL-1 do
    with Triangles[t] do
    begin
      with Points[Point0].Point do
           glVertex3f(X,Z,-Y);

      with Points[Point1].Point do
           glVertex3f(X,Z,-Y);

      with Points[Point2].Point do
           glVertex3f(X,Z,-Y);
    end;

    glEnd;
  end;

  procedure DrawPointers;
  var t         : Integer;
      Old       : TPoint3DFloat;
      OldParent : TBlocks;
      OldColor  : TColor;
  begin
    TBlockFormatAccess(Format).Finish;

    Old:=Pointer.Location.Point;
    OldParent:=Pointer.Parent;

    TBlockAccess(Pointer).IBlocks:=IBlocks;
    TBlockAccess(Pointer).ICanvas:=ICanvas;

    OldColor:=Pointer.Format.Color;

    PrepareCalcPoint;

    glPushMatrix;

    with IMin do
         glTranslatef(X+IRange.X*0.5,Z+IRange.Z*0.5,-Y-IRange.Y*0.5);

    with IRange do
         glScalef(X/Size.Point.X,Z/Size.Point.Z,Y/Size.Point.Y);

    for t:=0 to Length(Points)-1 do
    with Points[t] do
    begin
      Pointer.Location.Point:=CalcPoint(Point);

      if Self.ColorEach then
      begin
        if UseColor then
           Pointer.Format.Color:=GLColor(Color)
        else
           Pointer.Format.Color:=OldColor;

        TBlockFormatAccess(Format).SetDirectColor(Pointer.Format.Color);
      end;

      {$IFDEF TEEPICKSELECT}
      if Picking then
         glPushName(t+1);
      {$ENDIF}

      Pointer.DrawBlock;

      {$IFDEF TEEPICKSELECT}
      if Picking then
         glPopName;
      {$ENDIF}
    end;

    Pointer.Format.Color:=OldColor;

    Pointer.Location.Point:=Old;
    TBlockAccess(Pointer).IBlocks:=OldParent;

    TBlockFormatAccess(Format).Start;

    glPopMatrix;
  end;

var t        : Integer;
    OldStyle : TTeeCanvasSurfaceStyle;
begin
  inherited;

  tmpL:=Length(Triangles);

  if tmpL>0 then
  begin
    if (IList=0) or (IListPen=0) then
       CalcRange;

//    glTranslatef(-0.5,-0.5,-0.5);

    with IRange do
         glScalef(2/X,2/Z,2/Y);

    with IMin do
         glTranslatef(-X,-Z,Y);

    with IRange do
         glTranslatef(-X*0.5,-Z*0.5,Y*0.5);

    if Assigned(Pointer) then
       DrawPointers;

    if Format.Solid then
    begin
      if Parent.DrawBlocks.Shadows.Visible then
         DrawSimpleTriangles
      else
      begin
        if IList=0 then
        begin
          IList:=CreateNewList;

          PrepareCalcPoint;

          OldColor:=TBlockFormatAccess(Format).ICurrentColor;
          tmpChangedColor:=False;

          TBlockTextureAccess(Format.Texture).SetAutomatic(True);

          glBegin(GL_TRIANGLES);

          for t:=0 to tmpL-1 do
          with Triangles[t] do
          begin
            with CalculateNormal(Points[Point0].Point,Points[Point1].Point,Points[Point2].Point) do
                 glNormal3f(X,Y,Z);

            AddVertex(Point0);
            AddVertex(Point1);
            AddVertex(Point2);
          end;

          glEnd;

          glEndList;

          TBlockTextureAccess(Format.Texture).SetAutomatic(False);
        end
        else
          glCallList(IList);
      end;
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

      if IListPen=0 then
      begin
        IListPen:=CreateNewList;
        DrawSimpleTriangles;
        glEndList;
      end
      else
        glCallList(IListPen);

      {$IFDEF BLOCKS}
      ICanvas.setDrawStyle(OldStyle);
      {$ELSE}
      ICanvas.DrawStyle:=OldStyle;
      {$ENDIF}

      TBlockFormatAccess(Format).FinishPen;
    end;
  end;
end;

function TMeshBlock.GetX(Index:Integer):TTriValue;
begin
  result:=Points[Index].Point.X;
end;

function TMeshBlock.GetZ(Index:Integer):TTriValue;
begin
  result:=Points[Index].Point.Z;
end;

procedure TMeshBlock.SetXYZ(Dest,Source:Integer);
begin
  Points[Dest]:=Points[Source];
end;

procedure TMeshBlock.PrepareForGallery;
const
  Range     = 1000000;
  HalfRange =  500000;

  function RandomPoint:TPoint3DFloat;
  begin
    result.X:=Random(Range)-HalfRange;
    result.Y:=Random(Range)-HalfRange;
    result.Z:=Random(Range)-HalfRange;
  end;

var t : Integer;
begin
  inherited;

  Size.Point.Z:=30;

  SetLength(Points,10);

  for t:=0 to Length(Points)-1 do
      Points[t].Point:=RandomPoint;

  Triangulate;
end;

procedure TMeshBlock.Triangulate;
var t : Integer;
    tmpIndex : Integer;
    tmp : TTriangulator;
begin
  tmp:=TTriangulator.Create;
  try
    tmp.ImprovedTriangles:=True;

    tmp.GetX:=GetX;
    tmp.GetZ:=GetZ;
    tmp.SetXYZ:=SetXYZ;

    if tmp.CreateTriangles(Length(Points)) then
    begin
      SetLength(Triangles,tmp.NumTriangles);

      for t:=0 to Length(Triangles)-1 do
      with Triangles[t] do
      begin
        tmpIndex:=3*(t+1);

        Point0:=tmp.IPT[tmpIndex-2];
        Point1:=tmp.IPT[tmpIndex-1];
        Point2:=tmp.IPT[tmpIndex];

        // Colors:
      end;

      DataChanged;
    end;
  finally
    tmp.Free;
  end;
end;

procedure TMeshBlock.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Data',ReadData,WriteData,Length(Triangles)>0);
end;

procedure TMeshBlock.ReadData(Stream: TStream);
var tmpL : Integer;
    t    : Integer;
begin
  Stream.Read(tmpL,SizeOf(tmpL));
  SetLength(Points,tmpL);

  for t:=0 to tmpL-1 do
      Stream.ReadBuffer(Points[t],SizeOf(TTrianglePoint));

  Stream.Read(tmpL,SizeOf(tmpL));
  SetLength(Triangles,tmpL);

  for t:=0 to tmpL-1 do
      Stream.ReadBuffer(Triangles[t],SizeOf(TTriangle));

  DataChanged;
end;

procedure TMeshBlock.DataChanged;
begin
  DeleteLists;
end;

function TMeshBlock.GetEditor:String;
begin
  result:='TMeshEditor';
end;

procedure TMeshBlock.WriteData(Stream: TStream);
var tmpL : Integer;
    t    : Integer;
begin
  tmpL:=Length(Points);
  Stream.Write(tmpL,SizeOf(tmpL));

  for t:=0 to tmpL-1 do
      Stream.WriteBuffer(Points[t],SizeOf(TTrianglePoint));

  tmpL:=Length(Triangles);
  Stream.Write(tmpL,SizeOf(tmpL));

  for t:=0 to tmpL-1 do
      Stream.WriteBuffer(Triangles[t],SizeOf(TTriangle));
end;

procedure TMeshBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListPen);
end;

procedure TMeshEditor.SetPointData(APoint:Integer; P:TPointXYZColor);
begin
  with Mesh.Points[APoint] do
  begin
    UseColor:=P.Color<>clDefault;

    if UseColor then
    with Color do
    begin
      Red:=GetRValue(P.Color);
      Green:=GetGValue(P.Color);
      Blue:=GetBValue(P.Color);
    end;

    Point:=P.Point;
  end;
end;

procedure TMeshEditor.PointChanged(Sender: TObject);
begin
  SetPointData(Mesh.Selected,ThePoint);
  Mesh.DataChanged;
end;

type
  TPointEditorAccess=class(TPointEditor);

procedure TMeshEditor.FormCreate(Sender: TObject);

  procedure AddEditor(var AEditor:TPointEditor; ATab:TTabSheet; var APoint:TPointXYZColor);
  begin
    AEditor:=TPointEditor.Create(Self);
    AEditor.Align:=alClient;
    TPointEditorAccess(AEditor).OnDirty:=PointChanged;
    AEditor.Factor:=1000;

    APoint:=TPointXYZColor.Create;
    TTeeVCL.AddFormTo(AEditor,ATab,APoint);
    AEditor.SelectPoint(APoint);
  end;

begin
  AddEditor(IPoints,TabPoints,ThePoint);
end;

function TMeshEditor.FirstTriangleWithPoint(APoint:Integer):Integer;
var t : Integer;
begin
  result:=-1;

  for t:=0 to Length(Mesh.Triangles)-1 do
  with Mesh.Triangles[t] do
  if (Point0=APoint) or (Point1=APoint) or (Point2=APoint) then
  begin
    result:=t;
    break;
  end;
end;

procedure TMeshEditor.FormShow(Sender: TObject);
begin
  Mesh:=TMeshBlock(Tag);

  if Assigned(Mesh) then
  begin
    UDPoint.Max:=Length(Mesh.Points)-1;
    UDPoint0.Max:=UDPoint.Max;
    UDPoint1.Max:=UDPoint.Max;
    UDPoint2.Max:=UDPoint.Max;

    UDTriangle.Max:=Length(Mesh.Triangles)-1;

    SpeedButton2.Enabled:=Length(Mesh.Triangles)>0;

    if Mesh.Selected<>-1 then
    begin
      UDPoint.Position:=Min(UDPoint.Max,Mesh.Selected);
      UDTriangle.Position:=FirstTriangleWithPoint(Mesh.Selected);
    end
    else
    if Mesh.SelectedTriangle<>-1 then
    begin
      UDTriangle.Position:=Min(UDTriangle.Max,Mesh.SelectedTriangle);

      if Mesh.SelectedTriangle<>-1 then
         if Mesh.SelectedTriangle<Length(Mesh.Triangles) then
            UDPoint.Position:=Min(UDPoint.Max,Mesh.Triangles[Mesh.SelectedTriangle].Point0);
    end;

    ETriangleChange(Self);

    TBlockEditor.AddBlocks(BlockPointer,Mesh,Mesh.Pointer,'(none)');

    BlockColorEach.Checked:=Mesh.ColorEach;
    RefreshInfo;
  end;
end;

procedure TMeshEditor.ETriangleChange(Sender: TObject);

  procedure SetData(P:TPointXYZColor; Index:Integer);
  begin
    with Mesh.Points[Index] do
    begin
      P.Point:=Point;

      if UseColor then
         P.Color:=GLColor(Color)
      else
         P.Color:=clDefault;
    end;
  end;

begin
  if Assigned(Mesh) then
  begin
    Mesh.SelectedTriangle:=UDTriangle.Position;

    if Mesh.SelectedTriangle<>-1 then
    with Mesh.Triangles[Mesh.SelectedTriangle] do
    begin
      UDPoint0.Position:=Point0;
      UDPoint1.Position:=Point1;
      UDPoint2.Position:=Point2;

      // Select Point:
      Mesh.Selected:=Point0;
      UDPoint.Position:=Mesh.Selected;
    end;

    Mesh.Repaint;
  end;
end;

procedure TMeshEditor.FormDestroy(Sender: TObject);
begin
  ThePoint.Free;
end;

procedure TMeshEditor.SpeedButton2Click(Sender: TObject);
begin
  Mesh.DeleteTriangle(UDTriangle.Position);

  UDPoint.Max:=Length(Mesh.Points)-1;
  UDPoint0.Max:=UDPoint.Max;
  UDPoint1.Max:=UDPoint.Max;
  UDPoint2.Max:=UDPoint.Max;

  RefreshInfo;

  UDTriangle.Max:=Length(Mesh.Triangles)-1;

  ETriangleChange(Self);
end;

function TMeshBlock.SelectedPoint:TPoint3DFloat;
begin
  result:=Points[Selected].Point;
end;

procedure TMeshEditor.BlockPointerChange(Sender: TObject);
begin
  Mesh.Pointer:=TCustomBlock(BlockPointer.Items.Objects[BlockPointer.ItemIndex]);
end;

procedure TMeshEditor.BlockColorEachClick(Sender: TObject);
begin
  Mesh.ColorEach:=BlockColorEach.Checked;
end;

procedure TMeshEditor.Button1Click(Sender: TObject);
begin
  Mesh.SubDivide(Mesh.SelectedTriangle);
  RefreshInfo;
end;

procedure TMeshEditor.RefreshInfo;
begin
  LabelTotalPoints.Caption:=IntToStr(Length(Mesh.Points));
  LabelTotalTriangles.Caption:=IntToStr(Length(Mesh.Triangles));
end;

procedure TMeshEditor.Button2Click(Sender: TObject);
begin
  if TeeYesNo('Are you sure? (All triangles will be deleted and recalculated)') then
  begin
    Mesh.Triangulate;
    RefreshInfo;
  end;
end;

procedure TMeshBlock.SetSelectedTriangle(const Value: Integer);
begin
  FSelectedTriangle:=Value;
  Repaint;
end;

procedure TMeshEditor.EPointChange(Sender: TObject);
begin
  if Assigned(Mesh) then
  begin
    Mesh.Selected:=UDPoint.Position;

    Mesh.SelectedTriangle:=FirstTriangleWithPoint(Mesh.Selected);
    UDTriangle.Position:=Mesh.SelectedTriangle;

    Mesh.Repaint;
  end;

  IPoints.SelectPoint(ThePoint);
end;

function TMeshBlock.AddPoint(const X,Y,Z:Single): Integer;
begin
  result:=Length(Points);
  SetLength(Points,result+1);

  with Points[result] do
  begin
    Point.X:=X;
    Point.Y:=Y;
    Point.Z:=Z;
  end;

  Repaint;
end;

function TMeshBlock.AddPoint(const P: TPoint3DFloat): Integer;
begin
  with P do
    result:=AddPoint(X,Y,Z);
end;

function TMeshBlock.TriangleCenter(TriangleIndex: Integer): TPoint3DFloat;
const
  Inv3=1/3;
begin
  with Triangles[TriangleIndex],result do
  begin
    X:=(Points[Point0].Point.X+
        Points[Point1].Point.X+
        Points[Point2].Point.X)*Inv3;

    Y:=(Points[Point0].Point.Y+
        Points[Point1].Point.Y+
        Points[Point2].Point.Y)*Inv3;

    Z:=(Points[Point0].Point.Z+
        Points[Point1].Point.Z+
        Points[Point2].Point.Z)*Inv3;
  end;
end;

function TMeshBlock.AddTriangle(const P0,P1,P2:TPoint3DFloat):Integer;
begin
  result:=AddTriangle(AddPoint(P0),AddPoint(P1),AddPoint(P2));
end;

function TMeshBlock.AddTriangle(Index0, Index1, Index2: Integer): Integer;
begin
  result:=Length(Triangles);
  SetLength(Triangles,result+1);

  with Triangles[result] do
  begin
    Point0:=Index0;
    Point1:=Index1;
    Point2:=Index2;
  end;

  DataChanged;
end;

function TMeshBlock.AddPoint(const X,Y,Z: Single; const Color:TColor): Integer;
begin
  result:=AddPoint(X,Y,Z);

  Points[result].Color:=TBlockFormat.ColorToGL(Color);
  Points[result].UseColor:=Color<>clDefault;
end;

procedure TMeshBlock.SubDivide(TriangleIndex: Integer);
var tmp : Integer;
begin
  tmp:=AddPoint(TriangleCenter(TriangleIndex));

  with Triangles[TriangleIndex] do
       AddTriangle(tmp,Point0,Point1);

  with Triangles[TriangleIndex] do
       AddTriangle(tmp,Point1,Point2);

  with Triangles[TriangleIndex] do
       AddTriangle(tmp,Point2,Point0);

  DeleteTriangle(TriangleIndex);
end;

procedure TMeshBlock.DeletePoint(Index: Integer);
var t    : Integer;
    tmpL : Integer;
begin
  tmpL:=Length(Points)-1;

  for t:=Index to tmpL-1 do
      Points[t]:=Points[t+1];

  SetLength(Points,tmpL);
end;

procedure TMeshBlock.DeleteTriangle(TriangleIndex: Integer);

  procedure TryDeletePoint(APoint:Integer);
  var t : Integer;
      Found : Boolean;
  begin
    Found:=False;

    for t:=0 to Length(Triangles)-1 do
    if t<>TriangleIndex then
    with Triangles[t] do
      if (APoint=Point0) or (APoint=Point1) or (APoint=Point2) then
      begin
        Found:=True;
        break;
      end;

    if not Found then
    begin
      DeletePoint(APoint);

      for t:=0 to Length(Triangles)-1 do
      if t<>TriangleIndex then
      with Triangles[t] do
      begin
        if Point0>APoint then
           Dec(Point0);

        if Point1>APoint then
           Dec(Point1);

        if Point2>APoint then
           Dec(Point2);
      end;
    end;
  end;

var t    : Integer;
    tmpL : Integer;
begin
  with Triangles[TriangleIndex] do
  begin
    TryDeletePoint(Point0);
    TryDeletePoint(Point1);
    TryDeletePoint(Point2);
  end;

  tmpL:=Length(Triangles)-1;

  for t:=TriangleIndex to tmpL-1 do
      Triangles[t]:=Triangles[t+1];

  SetLength(Triangles,tmpL);

  DataChanged;
end;

initialization
  RegisterClass(TMeshEditor);
  RegisterBlocks([ TMeshBlock ]);
end.
