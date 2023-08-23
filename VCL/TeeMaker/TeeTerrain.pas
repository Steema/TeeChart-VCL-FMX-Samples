unit TeeTerrain;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,

  {$IFDEF D17}
  System.Types,
  {$ENDIF}

  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  TeCanvas,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}

  TeeBlocks, OpenGL2;

// Use VBO Vertex Buffer Objects instead of CallLists
// Warning: VBOs seems to be 8x slower than "obsolete" lists.

{.$DEFINE TEEVBO}

type
  TTerrainBlock=class(TCustomBlock)
  private
    FDrawNormals: Boolean;
    FHeightMap : String;
    FUseLists  : Boolean;

    IBoundsLow   : TPoint;
    IBoundsHigh  : TPoint;
    IBoundsZ     : TFloatPoint;
    INormalsDone : Boolean;

    Normals      : packed Array of TPoint3DArray;
    FInvert      : Boolean;

    {$IFDEF TEEVBO}
    NormalBuffer    : GLuint;
    TexCoordsBuffer : GLuint;
    VertexBuffer    : GLuint;

    TexCoordsData   : TFloatPoints;
    VertexBufferData: TPoint3DArray;
    NormalBufferData: TPoint3DArray;

    {$ELSE}
    ICallLists   : packed Array of Integer;
    {$ENDIF}

    procedure FillSampleGrid(AX,AY:Integer);
    procedure InvertGrid;
    procedure ReadData(Stream: TStream);
    procedure SetDrawNormals(const Value: Boolean);
    procedure SetHeightMap(const Value: String);
    procedure SetInvert(const Value: Boolean);
    procedure SetUseLists(const Value:Boolean);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer:TFiler); override;
    procedure DeleteLists; override;
    procedure InternalRecalcBounds;
    procedure PrepareForGallery; override;
  public
    Colors : Array of Array of TRGBAlpha;
    Grid   : Array of Array of Double;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;

    function GetAsBitmap:TBitmap;
    function GridSize:TPoint;
    procedure Load(Graphic:TGraphic); overload;
    procedure Load(const PictureFileName:String); overload;
    procedure RecalcBounds;
  published
    property DrawNormals:Boolean read FDrawNormals write SetDrawNormals default False;
    property HeightMap:String read FHeightMap write SetHeightMap;
    property InvertHeight:Boolean read FInvert write SetInvert default False;
    property OptimizeSpeed:Boolean read FUseLists write SetUseLists default True;
  end;

  TTerrainEditor = class(TVisualEditor)
    LTerrainSize: TLabel;
    GroupBox11: TGroupBox;
    Label81: TLabel;
    LTerrainHeightMap: TLabel;
    Button3: TButton;
    BlockTerrainSpeed: TCheckBox;
    Button5: TButton;
    CBInvert: TCheckBox;
    Button1: TButton;
    CBNormals: TCheckBox;
    procedure CBInvertClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BlockTerrainSpeedClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBNormalsClick(Sender: TObject);
  private
    { Private declarations }
    Terrain : TTerrainBlock;

    procedure ChangeHeightMap(const FileName:String);
  public
    { Public declarations }
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeGLCanvas, Math,
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  TeeProcs, TeeBrushDlg, TeeTextureSelector, TeeMakerConst;

{ TTerrainBlock }

Constructor TTerrainBlock.Create(AOwner: TComponent);
begin
  inherited;
  FUseLists:=True;
end;

Destructor TTerrainBlock.Destroy;
begin
  Normals:=nil;
  Colors:=nil;
  Grid:=nil;

  {$IFDEF TEEVBO}
  NormalBufferData:=nil;
  VertexBufferData:=nil;
  TexCoordsData:=nil;
  {$ENDIF}

  inherited;
end;

procedure TTerrainBlock.DeleteLists;
{$IFNDEF TEEVBO}
var t : Integer;
{$ENDIF}
begin
  inherited;

  {$IFDEF TEEVBO}
  glDeleteBuffersARB(1,@TexCoordsBuffer);
  glDeleteBuffersARB(1,@VertexBuffer);
  glDeleteBuffersARB(1,@NormalBuffer);

  TexCoordsBuffer:=0;
  VertexBuffer:=0;
  NormalBuffer:=0;

  TexCoordsData:=nil;
  VertexBufferData:=nil;
  NormalBufferData:=nil;
  {$ELSE}

  for t:=0 to Length(ICallLists)-1 do
      glDeleteLists(ICallLists[t],1);

  ICallLists:=nil;
  {$ENDIF}
end;

procedure TTerrainBlock.Load(const PictureFileName:String);
var tmp : TPicture;
begin
  tmp:=TPicture.Create;
  try
    tmp.LoadFromFile(TBlocks.ParseFileName(TeeMsg_TexturesLibrary,PictureFileName));
    Load(tmp.Graphic);
  finally
    tmp.Free;
  end;
end;

procedure TTerrainBlock.InvertGrid;
var x,y : Integer;
    tmpX,
    tmpY : Integer;
begin
  with GridSize do
  begin
    tmpX:=X;
    tmpY:=Y;
  end;

  if tmpX>0 then
  begin
    for x:=0 to tmpX-1 do
        for y:=0 to tmpY-1 do
            Grid[x,y]:=-Grid[x,y];

    RecalcBounds;
  end;
end;

procedure TTerrainBlock.Load(Graphic:TGraphic);
var x,y,
    tmpX,
    tmpY : Integer;
    tmp : {$IFDEF CLX}TRGBAArray{$ELSE}TRGBArray{$ENDIF};
    tmpB : TBitmap;
begin
  tmpX:=Graphic.Width;
  tmpY:=Graphic.Height;

  Colors:=nil;
  
  SetLength(Grid,tmpX,tmpY);

  if Graphic is TBitmap then
  begin
    TeeCalcLines(tmp,TBitmap(Graphic));
    tmpB:=nil;
  end
  else
  begin
    tmpB:=TBitmap.Create;
    tmpB.Assign(Graphic);
    TeeCalcLines(tmp,tmpB);
  end;

  {$IFOPT R+}
  {$DEFINE RANGEON}
  {$ENDIF}

  {$R-}
  for x:=0 to tmpX-1 do
      for y:=0 to tmpY-1 do
      with tmp[y,x] do
           Grid[x,y]:=-(Red or (Green shl 8) or (Blue shl 16));

  if InvertHeight then
     InvertGrid;

  {$IFDEF RANGEON}
  {$R+}
  {$ENDIF}

  tmp:=nil;
  tmpB.Free;

  RecalcBounds;
  
  Repaint;
end;

procedure TTerrainBlock.FillSampleGrid(AX,AY:Integer);
Const
  HalfPi=Pi/2;

var x,y : Integer;
begin
  Colors:=nil;
  SetLength(Grid,AX,AY);

  for x:=0 to AX-1 do
      for y:=0 to AY-1 do
          Grid[x,y]:=Cos(Abs(x-HalfPi)*Abs(y-HalfPi));

  RecalcBounds;
end;

procedure TTerrainBlock.PrepareForGallery;
var tmp : TGraphic;
begin
  inherited;
  Format.Border.Visible:=False;

  Size.Point.Z:=20;

  tmp:=TBlockPicture.LoadGraphicResource(TeeMakerLibraryTag+'Terrain\200px-Heightmap.png');

  if Assigned(tmp) then
  try
    Load(tmp);
  finally
    tmp.Free;
  end
  else
    FillSampleGrid(20,20);
end;

function TTerrainBlock.GridSize:TPoint;
begin
  if Length(Grid)>0 then
  begin
    result.X:=High(Grid)-Low(Grid)+1;

    if Length(Grid[0])>0 then
       result.Y:=High(Grid[0])-Low(Grid[0])+1
    else
       result.Y:=0;
  end
  else
    result:=TeePoint(0,0);
end;

procedure TTerrainBlock.RecalcBounds;
begin
  InternalRecalcBounds;
  INormalsDone:=False;
  DeleteLists;
end;

procedure TTerrainBlock.InternalRecalcBounds;
var x, y : Integer;
begin
  IBoundsLow.X:=0;
  IBoundsLow.Y:=0;

  IBoundsHigh.X:=0;
  IBoundsHigh.Y:=0;

  IBoundsZ.X:=0;
  IBoundsZ.Y:=0;

  if Length(Grid)>0 then
  begin
    IBoundsLow.X:=Low(Grid);
    IBoundsHigh.X:=High(Grid);

    if Length(Grid[0])>0 then
    begin
      IBoundsLow.Y:=Low(Grid[0]);
      IBoundsHigh.Y:=High(Grid[0]);

      IBoundsZ.X:=Grid[IBoundsLow.X,IBoundsLow.Y];
      IBoundsZ.Y:=IBoundsZ.X;

      for x:=IBoundsLow.X to IBoundsHigh.X do
          for y:=IBoundsLow.Y to IBoundsHigh.Y do
          begin
            if Grid[x,y]<IBoundsZ.X then
               IBoundsZ.X:=Grid[x,y]
            else
            if Grid[x,y]>IBoundsZ.Y then
               IBoundsZ.Y:=Grid[x,y];
          end;
    end;
  end;
end;

function TTerrainBlock.GetAsBitmap:TBitmap;
var tmpSize : TPoint;
    x       : Integer;
    y       : Integer;
    tmpRange : Single;
begin
  result:=nil;

  tmpSize:=GridSize;

  if (tmpSize.X>0) and (tmpSize.Y>0) then
  begin
    InternalRecalcBounds;

    tmpRange:=(IBoundsZ.Y-IBoundsZ.X);

    if tmpRange<>0 then
    begin
      result:=TBitmap.Create;

      result.Width:=tmpSize.X;
      result.Height:=tmpSize.Y;

      tmpRange:=16777216/tmpRange;  // 256*256*256 / Range

      for x:=0 to result.Width-1 do
          for y:=0 to result.Height-1 do
              TBitmap(result).Canvas.Pixels[x,y]:=Round((Grid[IBoundsLow.X+x,IBoundsLow.Y+y]-IBoundsZ.X)*tmpRange);
    end;
  end;
end;

type
  TBlockFormatAccess=class(TBlockFormat);
  TGLCanvasAccess=class(TGLCanvas);
  TBlockTextureAccess=class(TBlockTexture);

procedure TTerrainBlock.Draw;
var
  tmpNumX : Integer;
  tmpNumY : Integer;

  {$IFDEF TEEVBO}

  procedure DrawBufferObjects;

    procedure InitVBOBuffers;
    var tmpX : Integer;
        tmpY : Integer;
        tmpX2 : Integer;
        tmpY2 : Integer;
        Pos  : Integer;
        tmpTeX : Single;
        tmpTeY : Single;
        tmpCount : Integer;
        t        : Integer;
    begin
      tmpCount:=tmpNumX*tmpNumY*6;

      if Length(VertexBufferData)=0 then
      begin
        SetLength(VertexBufferData,tmpCount);
        SetLength(TexCoordsData,tmpCount);
        SetLength(NormalBufferData,tmpCount);

        Pos:=0;

        tmpTeX:=1/tmpNumX;
        tmpTeY:=1/tmpNumY;

        for tmpX:=0 to tmpNumX-1 do
        begin
          for tmpY:=0 to tmpNumY-1 do
          begin
            for t:=0 to 5 do
            begin
              if (t=1) or (t=2) or (t=5) then
                 tmpX2:=tmpX+1
              else
                 tmpX2:=tmpX;

              if (t=4) or (t=2) or (t=5) then
                 tmpY2:=tmpY+1
              else
                 tmpY2:=tmpY;

              with TexCoordsData[Pos] do
              begin
                X:=tmpTeX*tmpX2;
                Y:=tmpTeY*tmpY2;
              end;

              with VertexBufferData[Pos] do
              begin
                X:=tmpX2;
                Y:=Grid[tmpX2,tmpY2];
                Z:=tmpY2;
              end;

              NormalBufferData[Pos]:=Normals[tmpX2,tmpY2];

              Inc(Pos);
            end;
          end;
        end;
      end;

      glGenBuffersARB(1, @VertexBuffer);
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, VertexBuffer);
      glBufferDataARB(GL_ARRAY_BUFFER_ARB, tmpCount*SizeOf(TPoint3DFloat), VertexBufferData, GL_STATIC_DRAW_ARB);

      glGenBuffersARB(1, @TexCoordsBuffer);
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, TexCoordsBuffer);
      glBufferDataARB(GL_ARRAY_BUFFER_ARB, tmpCount*SizeOf(TFloatPoint), TexCoordsData, GL_STATIC_DRAW_ARB);

      glGenBuffersARB(1, @NormalBuffer);
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, NormalBuffer);
      glBufferDataARB(GL_ARRAY_BUFFER_ARB, tmpCount*SizeOf(TPoint3DFloat), NormalBufferData, GL_STATIC_DRAW_ARB);
    end;

  begin
    if VertexBuffer=0 then
       InitVBOBuffers;

    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnableClientState(GL_VERTEX_ARRAY);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, NormalBuffer);
    glNormalPointer(GL_DOUBLE, 0, nil);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, TexCoordsBuffer);
    glTexCoordPointer(2, GL_DOUBLE, 0, nil);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, VertexBuffer);
    glVertexPointer(3, GL_DOUBLE, 0, nil);

    glDrawArrays(GL_TRIANGLES, 0, tmpNumX*tmpNumY*6);

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
  end;

  {$ELSE}

  procedure DrawGrid(DoUseLists:Boolean);
  var tmpX,
      tmpList,
      tmpAlpha,
      tmpTransp,
      x,
      y : Integer;

      tmpTeX,
      tmpTeY,
      tmpTeY2 : Single;

      tmpTeX1,
      tmpTeX2 : Single;

      HasColors : Boolean;
  begin
    if DoUseLists then
    begin
      tmpList:=glGenLists(IBoundsHigh.X-IBoundsLow.X+1);
      if tmpList=0 then
         raise Exception.Create('Cannot generate Terrain lists.');

      SetLength(ICallLists,IBoundsHigh.X-IBoundsLow.X+1);

      for x:=IBoundsLow.X to IBoundsHigh.X-1 do
          ICallLists[x-IBoundsLow.X]:=tmpList+x-IBoundsLow.X;
    end;

    HasColors:=(not IPicking) and (not IBlocks.DrawBlocks.Shadows.Visible) and (Length(Colors)>0);

    {$IFDEF BLOCKS}
    tmpTransp:=255-Round(ICanvas.getBrushTransparency*255);
    {$ELSE}
    tmpTransp:=255-Round(TGLCanvasAccess(ICanvas).ITransp*255);
    {$ENDIF}

    tmpTeX:=1/tmpNumX;
    tmpTeY:=1/tmpNumY;

    tmpTeX1:=IBoundsLow.X*tmpTeX;

    for x:=IBoundsLow.X to IBoundsHigh.X-1 do
    begin
      tmpX:=x+1;

      tmpTeX2:=tmpTeX1+tmpTeX;

      if DoUseLists then
         glNewList(ICallLists[x-IBoundsLow.X],GL_COMPILE_AND_EXECUTE);

      glBegin(GL_QUAD_STRIP);

      tmpTeY2:=IBoundsLow.Y*tmpTeY;

      for y:=IBoundsLow.Y to IBoundsHigh.Y do
      begin
        if HasColors then
        with Colors[x,y] do
        begin
          tmpAlpha:=Alpha-tmpTransp;
          if tmpAlpha<0 then tmpAlpha:=0;

          glColor4ub(Red,Green,Blue,tmpAlpha);
        end;

        with Normals[x,y] do
             glNormal3f(X,Y,Z);

        glTexCoord2f(tmpTeX1,tmpTeY2);
        glVertex3f(x,Grid[x,y],y);

        if HasColors then
        with Colors[tmpX,y] do
        begin
          tmpAlpha:=Alpha-tmpTransp;
          if tmpAlpha<0 then tmpAlpha:=0;

          glColor4ub(Red,Green,Blue,tmpAlpha);
        end;

        with Normals[tmpX,y] do
             glNormal3f(X,Y,Z);

        glTexCoord2f(tmpTeX2,tmpTeY2);
        glVertex3f(tmpX,Grid[tmpX,y],y);

        tmpTeY2:=tmpTeY2+tmpTeY;
      end;

      glEnd;

      if DoUseLists then
         glEndList;

      tmpTeX1:=tmpTeX2;
    end;
  end;
  {$ENDIF}

  procedure DoDrawNormals;
  const
    PreviewLength=53;

  var x, y : Integer;
      tmpV : TPoint3DFloat;
      tmpL : Single;
  begin
    tmpL:=PreviewLength*0.01;

    glBegin(GL_LINES);

    glColor4ub(255,0,0,0);

    for x:=IBoundsLow.X to IBoundsHigh.X-1 do
    begin
      for y:=IBoundsLow.Y to IBoundsHigh.Y do
      begin
        tmpV.X:=X;
        tmpV.Y:=Grid[x,y];
        tmpV.Z:=y;

        glVertex3fv(@tmpV);

        with Normals[x,y] do
        begin
          tmpV.X:=tmpV.X+X*tmpL;
          tmpV.Y:=tmpV.Y+Y*tmpL;
          tmpV.Z:=tmpV.Z+Z*tmpL;
        end;

        glVertex3fv(@tmpV);
      end;
    end;

    glEnd;
  end;

var
  FaceNormals : Array of TPoint3DArray;

  function AverageNormal(const Ax,Ay:Integer):TPoint3DFloat;
  begin
    with result do
    begin
      X:=(FaceNormals[Ax,Ay].X+FaceNormals[Ax+1,Ay].X+FaceNormals[Ax,Ay+1].X+FaceNormals[Ax+1,Ay+1].X)*0.25;
      Y:=(FaceNormals[Ax,Ay].Y+FaceNormals[Ax+1,Ay].Y+FaceNormals[Ax,Ay+1].Y+FaceNormals[Ax+1,Ay+1].Y)*0.25;
      Z:=(FaceNormals[Ax,Ay].Z+FaceNormals[Ax+1,Ay].Z+FaceNormals[Ax,Ay+1].Z+FaceNormals[Ax+1,Ay+1].Z)*0.25;
    end;
  end;

var OldStyle : TTeeCanvasSurfaceStyle;

    tmpZ : Single;
    x,y  : Integer;

    {$IFNDEF TEEVBO}
    t    : Integer;
    {$ENDIF}
begin
  if Length(Grid)>0 then
  begin
    tmpNumX:=(IBoundsHigh.X-IBoundsLow.X);
    tmpNumY:=(IBoundsHigh.Y-IBoundsLow.Y);

    tmpZ:=IBoundsZ.Y-IBoundsZ.X;

    if tmpZ=0 then tmpZ:=1
              else tmpZ:=2/tmpZ;

    glTranslatef(-1-IBoundsLow.X,-1,-1-IBoundsLow.Y);

    glScalef(2/tmpNumX,tmpZ,2/tmpNumY);
    glTranslatef(0,-IBoundsZ.X,0);

    if not INormalsDone then
    begin
      SetLength(FaceNormals,tmpNumX,tmpNumY);

      for x:=IBoundsLow.X+1 to IBoundsHigh.X-1 do
          for y:=IBoundsLow.Y+1 to IBoundsHigh.Y-1 do
            FaceNormals[x,y]:=CalculateNormal(
                              PointFloat(x,Grid[x,y]-IBoundsZ.X,y),
                              PointFloat(x,Grid[x,y+1]-IBoundsZ.X,y+1),
                              PointFloat(x+1,Grid[x+1,y+1]-IBoundsZ.X,y+1));

      SetLength(Normals,tmpNumX+1,tmpNumY+1);

      for x:=IBoundsLow.X to IBoundsHigh.X-2 do
        for y:=IBoundsLow.Y to IBoundsHigh.Y-2 do
            Normals[x+1,y+1]:=AverageNormal(x,y);

      (*
      for x:=IBoundsLow.X to IBoundsHigh.X-1 do
      begin
        for y:=IBoundsLow.Y to IBoundsHigh.Y-1 do
            Normals[x,y]:=CalculateNormal(
                              PointFloat(x+1,Grid[x+1,y+1]-IBoundsZ.X,y+1),
                              PointFloat(x+1,Grid[x+1,y]-IBoundsZ.X,y),
                              PointFloat(x,Grid[x,y]-IBoundsZ.X,y));

        Normals[x,IBoundsHigh.Y]:=Normals[x,IBoundsHigh.Y-1];
      end;

      {$R-}
      for y:=IBoundsLow.Y to IBoundsHigh.Y do
          Normals[IBoundsHigh.X,y]:=Normals[IBoundsHigh.X-1,y];
      *)

      FaceNormals:=nil;

      INormalsDone:=True;
    end;

    if Format.Solid then
    begin
      {$IFDEF TEEVBO}

      DrawBufferObjects;

      {$ELSE}

      if FUseLists and (not IPicking) then
         if ICallLists=nil then
            DrawGrid(True)
         else
         for t:=0 to Length(ICallLists)-2 do
             glCallList(ICallLists[t])
      else
         DrawGrid(False);

      {$ENDIF}
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

      // TODO: Convert to GL List, or Array of Lists like cells?
      // (it seems Lists have a limit on maximum number of gl instructions)
      for x:=IBoundsLow.X to IBoundsHigh.X-1 do
      begin
        glBegin(GL_QUAD_STRIP);

        for y:=IBoundsLow.Y to IBoundsHigh.Y do
        begin
          glVertex3f(x,Grid[x,y],y);
          glVertex3f(x+1,Grid[x+1,y],y);
        end;

        glEnd;
      end;

      {$IFDEF BLOCKS}
      ICanvas.setDrawStyle(OldStyle);
      {$ELSE}
      ICanvas.DrawStyle:=OldStyle;
      {$ENDIF}

      TBlockFormatAccess(Format).FinishPen;
    end;

    if DrawNormals then
       DoDrawNormals;
  end;
end;

procedure TTerrainBlock.SetHeightMap(const Value: String);
begin
  if FHeightMap<>Value then
  begin
    FHeightMap:=Value;
    Load(FHeightMap);
  end;
end;

procedure TTerrainBlock.DefineProperties(Filer:TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Data',ReadData,WriteData,
                             (HeightMap='') and (Length(Grid)>0));
end;

procedure TTerrainBlock.ReadData(Stream: TStream);
var tmpX : Integer;
    tmpZ : Integer;
    t    : Integer;
    tmp  : Integer;
begin
  Stream.Read(tmpX,SizeOf(tmpX));
  Stream.Read(tmpZ,SizeOf(tmpZ));

  SetLength(Grid,tmpX,tmpZ);

  tmp:=tmpZ*SizeOf(Double);

  for t:=0 to tmpX-1 do
      Stream.ReadBuffer(Grid[t,0],tmp);

  Stream.Read(tmpX,SizeOf(tmpX));
  Stream.Read(tmpZ,SizeOf(tmpZ));

  SetLength(Colors,tmpX,tmpZ);

  tmp:=tmpZ*SizeOf(TRGBAlpha);

  for t:=0 to tmpX-1 do
      Stream.ReadBuffer(Colors[t,0],tmp);

  RecalcBounds;
end;

procedure TTerrainBlock.WriteData(Stream: TStream);
var tmpX : Integer;
    tmpZ : Integer;
    t    : Integer;
    tmp  : Integer;
begin
  tmpX:=Length(Grid);
  tmpZ:=Length(Grid[0]);

  Stream.Write(tmpX,SizeOf(tmpX));
  Stream.Write(tmpZ,SizeOf(tmpZ));

  tmp:=tmpZ*SizeOf(Double);

  for t:=0 to tmpX-1 do
      Stream.WriteBuffer(Grid[t,0],tmp);

  tmpX:=Length(Colors);
  if tmpX=0 then tmpZ:=0
            else tmpZ:=Length(Grid[0]);

  Stream.Write(tmpX,SizeOf(tmpX));
  Stream.Write(tmpZ,SizeOf(tmpZ));

  tmp:=tmpZ*SizeOf(TRGBAlpha);

  for t:=0 to tmpX-1 do
      Stream.WriteBuffer(Colors[t,0],tmp);
end;

procedure TTerrainBlock.Assign(Source: TPersistent);
var tmpX,
    tmpY,
    x,
    y : Integer;
begin
  if Source is TTerrainBlock then
  with TTerrainBlock(Source) do
  begin
    Self.HeightMap:=HeightMap;

    if HeightMap='' then
    begin
      // Grid
      tmpX:=Length(Grid);

      if tmpX>0 then
      begin
        tmpY:=Length(Grid[0]);

        SetLength(Self.Grid,tmpX,tmpY);

        for x:=0 to tmpX-1 do
            for y:=0 to tmpY-1 do
                Self.Grid[x,y]:=Grid[x,y];
      end
      else
        Grid:=nil;

      // Colors
      tmpX:=Length(Colors);

      if tmpX>0 then
      begin
        tmpY:=Length(Colors[0]);

        SetLength(Self.Colors,tmpX,tmpY);

        for x:=0 to tmpX-1 do
            for y:=0 to tmpY-1 do
                Self.Colors[x,y]:=Colors[x,y];
      end
      else
        Colors:=nil;

      Self.RecalcBounds;
    end;

    Self.FUseLists:=FUseLists;
  end;

  inherited;
end;

procedure TTerrainBlock.SetUseLists(const Value: Boolean);
begin
  FUseLists:=Value;

  if not FUseLists then
     DeleteLists;

  Repaint;
end;

procedure TTerrainBlock.SetInvert(const Value: Boolean);
begin
  if FInvert<>Value then
  begin
    FInvert:=Value;
    InvertGrid;
  end;
end;

function TTerrainBlock.GetEditor: String;
begin
  result:='TTerrainEditor';
end;

procedure TTerrainBlock.SetDrawNormals(const Value: Boolean);
begin
  if FDrawNormals<>Value then
  begin
    FDrawNormals:=Value;
    Repaint;
  end;
end;

{ TTerrainEditor }

procedure TTerrainEditor.CBInvertClick(Sender: TObject);
begin
  Terrain.InvertHeight:=CBInvert.Checked;
end;

procedure TTerrainEditor.FormShow(Sender: TObject);
begin
  Terrain:=TTerrainBlock(Tag);

  if Assigned(Terrain) then
  with Terrain do
  begin
    with GridSize do
         LTerrainSize.Caption:='Size: '+TeeStr(X)+' x '+TeeStr(Y);

    LTerrainHeightMap.Caption:=ExtractFileName(HeightMap);
    BlockTerrainSpeed.Checked:=OptimizeSpeed;
    CBInvert.Checked:=InvertHeight;
    CBNormals.Checked:=DrawNormals;
  end;
end;

procedure TTerrainEditor.ChangeHeightMap(const FileName:String);
begin
  if Terrain.HeightMap<>FileName then
  begin
    Terrain.HeightMap:=FileName;

    with Terrain.GridSize do
         LTerrainSize.Caption:='Size: '+TeeStr(X)+' x '+TeeStr(Y);

    LTerrainHeightMap.Caption:=ExtractFileName(FileName);

    MarkDirty;
  end;
end;

procedure TTerrainEditor.Button3Click(Sender: TObject);
var tmp : String;
begin
  tmp:=TeeGetPictureFileName(Self);

  if tmp<>'' then
     ChangeHeightMap(tmp);
end;

procedure TTerrainEditor.BlockTerrainSpeedClick(Sender: TObject);
begin
  Terrain.OptimizeSpeed:=BlockTerrainSpeed.Checked;
  MarkDirty;
end;

procedure TTerrainEditor.Button1Click(Sender: TObject);
var tmpFormat : TBlockFormat;
begin
  tmpFormat:=TBlockFormat.Create(nil);
  try
    if TTextureSelector.ModalShow(Self,Terrain.Parent,tmpFormat) then
    begin
      if tmpFormat.Texture.PictureLink<>'' then
         ChangeHeightMap(tmpFormat.Texture.PictureLink);
    end;

  finally
    tmpFormat.Free;
  end;
end;

procedure TTerrainEditor.CBNormalsClick(Sender: TObject);
begin
  Terrain.DrawNormals:=CBNormals.Checked;
end;

initialization
  RegisterBlocks([ TTerrainBlock ]);
  RegisterClass(TTerrainEditor);
end.


