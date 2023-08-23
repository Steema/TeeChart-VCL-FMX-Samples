unit TeeGLUT;
{$I TeeDefs.inc}

interface

uses
  Windows,
  Classes, Graphics,
  TeCanvas,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeBlocks;

type
  TDodecahedron=class(TCustomBlock)
  private
    // class var
    ICreated : Boolean;
    IData    : Array[0..19] of TPoint3DFloat;
    INormals : Array[0..11] of TPoint3DFloat;

    procedure Dodecahedron(const AType:Integer);
  public
    procedure Draw; override;
  end;

  TTeaPotBlock=class(TCustomBlock)
  private
    IList : Integer;

    procedure BorderChanged(Sender:TObject);
  protected
    procedure DeleteLists; override;
  public
    Constructor Create(AOwner: TComponent); override;

    procedure Draw; override;
  end;

implementation

uses
  OpenGL2, TeeGLCanvas;

const
  GLUTDLL='glut32.dll';

var
  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCube: procedure(size: GLdouble); stdcall;
  glutSolidCube: procedure(size: GLdouble); stdcall;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutWireDodecahedron: procedure; stdcall;
  glutSolidDodecahedron: procedure; stdcall;
  glutWireTeapot: procedure(size: GLdouble); stdcall;
  glutSolidTeapot: procedure(size: GLdouble); stdcall;
  glutWireOctahedron: procedure; stdcall;
  glutSolidOctahedron: procedure; stdcall;
  glutWireTetrahedron: procedure; stdcall;
  glutSolidTetrahedron: procedure; stdcall;
  glutWireIcosahedron: procedure; stdcall;
  glutSolidIcosahedron: procedure; stdcall;

  GLUTHandle : THandle=0;

procedure LoadProcs;
begin
  if GLUTHandle=0 then
  begin
    GLUTHandle:=TeeLoadLibrary(GLUTDLL);

    if GLUTHandle > 0 then
    begin
      @glutWireSphere := GetProcAddress(GLUTHandle, 'glutWireSphere');
      @glutSolidSphere := GetProcAddress(GLUTHandle, 'glutSolidSphere');
      @glutWireCone := GetProcAddress(GLUTHandle, 'glutWireCone');
      @glutSolidCone := GetProcAddress(GLUTHandle, 'glutSolidCone');
      @glutWireCube := GetProcAddress(GLUTHandle, 'glutWireCube');
      @glutSolidCube := GetProcAddress(GLUTHandle, 'glutSolidCube');
      @glutWireTorus := GetProcAddress(GLUTHandle, 'glutWireTorus');
      @glutSolidTorus := GetProcAddress(GLUTHandle, 'glutSolidTorus');
      @glutWireDodecahedron := GetProcAddress(GLUTHandle, 'glutWireDodecahedron');
      @glutSolidDodecahedron := GetProcAddress(GLUTHandle, 'glutSolidDodecahedron');
      @glutWireTeapot := GetProcAddress(GLUTHandle, 'glutWireTeapot');
      @glutSolidTeapot := GetProcAddress(GLUTHandle, 'glutSolidTeapot');
      @glutWireOctahedron := GetProcAddress(GLUTHandle, 'glutWireOctahedron');
      @glutSolidOctahedron := GetProcAddress(GLUTHandle, 'glutSolidOctahedron');
      @glutWireTetrahedron := GetProcAddress(GLUTHandle, 'glutWireTetrahedron');
      @glutSolidTetrahedron := GetProcAddress(GLUTHandle, 'glutSolidTetrahedron');
      @glutWireIcosahedron := GetProcAddress(GLUTHandle, 'glutWireIcosahedron');
      @glutSolidIcosahedron := GetProcAddress(GLUTHandle, 'glutSolidIcosahedron');
    end;
  end;
end;

procedure FreeProcs;
begin
  FreeLibrary(GLUTHandle);

  @glutWireSphere := nil;
  @glutSolidSphere := nil;
  @glutWireCone := nil;
  @glutSolidCone := nil;
  @glutWireCube := nil;
  @glutSolidCube := nil;
  @glutWireTorus := nil;
  @glutSolidTorus := nil;
  @glutWireDodecahedron := nil;
  @glutSolidDodecahedron := nil;
  @glutWireTeapot := nil;
  @glutSolidTeapot := nil;
  @glutWireOctahedron := nil;
  @glutSolidOctahedron := nil;
  @glutWireTetrahedron := nil;
  @glutSolidTetrahedron := nil;
  @glutWireIcosahedron := nil;
  @glutSolidIcosahedron := nil;
end;

type
  TBorderAccess=class(TBlockBorder);

{ TTeaPotBlock }

Constructor TTeaPotBlock.Create(AOwner: TComponent);
begin
  inherited;
  TBorderAccess(Format.Border).IChanged:=BorderChanged;
  Format.Border.Visible:=False;
  Format.VisibleInterior:=True;
end;

procedure TTeaPotBlock.BorderChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TTeaPotBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
end;

type
  TBlockFormatAccess=class(TBlockFormat);
  
procedure TTeaPotBlock.Draw;
var CanCull : Boolean;
begin
  LoadProcs;

  with Size.Point do
       glScalef(0.007, 0.013, 0.011);

  CanCull:=not ShouldDrawInterior;

  if CanCull then
  begin
    glEnable(GL_CULL_FACE);
    glFrontFace(GL_CW);
    //glCullFace(GL_FRONT);
  end;

  if IList=0 then
  begin
    IList:=CreateNewList;

    if Format.Solid then
       glutSolidTeapot(100);

    if TBlockFormatAccess(Format).PreparePen then
       glutWireTeapot(100);

    glEndList;
  end
  else
    glCallList(IList);

  if CanCull then
  begin
    glDisable(GL_CULL_FACE);
    glFrontFace(GL_CCW);
    //glCullFace(GL_BACK);
  end;
end;

{ TDodecahedron }

procedure TDodecahedron.Dodecahedron(const AType:Integer);

  procedure InitDodecahedron;
  var alpha,
      beta,
      factor : Single;
  begin
    factor:=3 + Sqrt(5);
    alpha:= Sqrt(2 / factor);
    beta:= 1 + Sqrt(6 / factor - 2 + 2 * Sqrt(2 / factor));

    IData[0].X := -alpha; IData[0].Y := 0; IData[0].Z := beta;
    IData[1].X := alpha; IData[1].Y := 0; IData[1].Z := beta;
    IData[2].X := -1; IData[2].Y := -1; IData[2].Z := -1;
    IData[3].X := -1; IData[3].Y := -1; IData[3].Z := 1;
    IData[4].X := -1; IData[4].Y := 1; IData[4].Z := -1;
    IData[5].X := -1; IData[5].Y := 1; IData[5].Z := 1;
    IData[6].X := 1; IData[6].Y := -1; IData[6].Z := -1;
    IData[7].X := 1; IData[7].Y := -1; IData[7].Z := 1;
    IData[8].X := 1; IData[8].Y := 1; IData[8].Z := -1;
    IData[9].X := 1; IData[9].Y := 1; IData[9].Z := 1;
    IData[10].X := beta; IData[10].Y := alpha; IData[10].Z := 0;
    IData[11].X := beta; IData[11].Y := -alpha; IData[11].Z := 0;
    IData[12].X := -beta; IData[12].Y := alpha; IData[12].Z := 0;
    IData[13].X := -beta; IData[13].Y := -alpha; IData[13].Z := 0;
    IData[14].X := -alpha; IData[14].Y := 0; IData[14].Z := -beta;
    IData[15].X := alpha; IData[15].Y := 0; IData[15].Z := -beta;
    IData[16].X := 0; IData[16].Y := beta; IData[16].Z := alpha;
    IData[17].X := 0; IData[17].Y := beta; IData[17].Z := -alpha;
    IData[18].X := 0; IData[18].Y := -beta; IData[18].Z := alpha;
    IData[19].X := 0; IData[19].Y := -beta; IData[19].Z := -alpha;

    ICreated:=True;
  end;

const
  Sides:Array[0..11] of Array[0..4] of Byte=(
    (0, 1, 9, 16, 5),
    (1, 0, 3, 18, 7),
    (1, 7, 11, 10, 9),
    (11, 7, 18, 19, 6),
    (8, 17, 16, 9, 10),
    (2, 14, 15, 6, 19),
    (2, 13, 12, 4, 14),
    (2, 19, 18, 3, 13),
    (3, 0, 5, 12, 13),
    (6, 15, 8, 10, 11),
    (4, 17, 8, 15, 14),
    (4, 12, 5, 16, 17)
       );


  procedure Pentagon(Index:Integer);
  begin
    glBegin(AType);
     glNormal3fv(@INormals[Index]);
     glVertex3fv(@IData[Sides[Index,0]]);
     glVertex3fv(@IData[Sides[Index,1]]);
     glVertex3fv(@IData[Sides[Index,2]]);
     glVertex3fv(@IData[Sides[Index,3]]);
     glVertex3fv(@IData[Sides[Index,4]]);
    glEnd;
  end;

var t : Integer;
    CanCull : Boolean;
begin
  if not ICreated then
  begin
    InitDodecahedron;

    for t:=Low(Sides) to High(Sides) do
       INormals[t]:=CalculateNormal(IData[Sides[t,0]], IData[Sides[t,1]],IData[Sides[t,2]]);
  end;

  CanCull:=(not ShouldDrawInterior);

  if CanCull then
     glEnable(GL_CULL_FACE);

  for t:=Low(Sides) to High(Sides) do
      Pentagon(t);

  if CanCull then
     glDisable(GL_CULL_FACE);
end;

type
  TBlockTextureAccess=class(TBlockTexture);

procedure TDodecahedron.Draw;
var tmpTextures : Boolean;
    ISizeFactor : Single;
begin
  ISizeFactor:=2-2*Cos(Pi*0.25);
  glScalef(ISizeFactor,ISizeFactor,ISizeFactor);

  tmpTextures:=(not Parent.DrawBlocks.Shadows.Visible) and TBlockTextureAccess(Format.Texture).HasTexture;

  if tmpTextures then
  begin
    glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
  end;

  if Format.Solid then
     Dodecahedron(GL_TRIANGLE_FAN);

  if tmpTextures then
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
  end;

  if TBlockFormatAccess(Format).PreparePen then
     Dodecahedron(GL_LINE_LOOP);
end;

initialization
  RegisterBlocks([TTeaPotBlock,TDodecahedron]);
finalization
  FreeProcs;
end.
