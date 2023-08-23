unit TeeFacesBlock;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  TeeBlocks, TeCanvas, TeeProcs, TeePointEditor, Buttons;

type
  TVertexNormal=record
    Normal : TPoint3DFloat;
    Count  : Integer;
  end;

  TGroupNormals=record
    Group  : Integer;
    Normal : Integer;
  end;

  TVertexNormals=Array of TVertexNormal;

  TVertexGroups=packed record
    Vertex : TPoint3DFloat;
    NormalGroups : Array of TGroupNormals;
    NormalCount  : Integer;
  end;

  TGeometry=class
  protected
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    Vertex : Array of TVertexGroups;
    VertexCount : Integer;

    Texture : TPoint3DArray;
    TextureCount : Integer;

    Normals : TVertexNormals;
    NormalCount : Integer;

    SmoothGroups : packed Array of Cardinal;

    Destructor Destroy; override;

    procedure AddVertex(const AX,AY,AZ:Single); 
    procedure AverageNormals;
    procedure Clear;
    procedure ClearSmoothGroups;
    function NewSmoothGroup(const Number:Cardinal):Cardinal;
  end;

  TFaceVertex=packed record
    Vertex  : Integer;
    Texture : Integer;
    Normal  : Integer;
  end;

  TFaceMaterial=packed record
    Name : String;
    Ambient,
    Diffuse,
    Specular : TRGB;
  end;

  TFace=packed record
    Data     : packed Array of TFaceVertex;
    Line     : Boolean;
    HasTextures : Boolean;
    SmoothGroup : Integer;
    Material    : Integer;
  end;

  TFaces=packed Array of TFace;

  TFaceOutline=(foNone, foOutline, foOutlineVertex, foOutlineAll, foAllVertexes);

  TFacePreview=record
    Color   : TColor;
    Length  : Integer;
    Normals : Boolean;
    Outline : TFaceOutline;
    SelectedFace : Integer;
    SelectedVertex : Integer;
  end;

  TGetColorProc=function(Material:Integer):TRGB of object;

  TFacesBlock=class(TCustomBlock)
  private
    NotSaveFaces : Boolean;

    IList    : Integer;
    IListPen : Integer;

    //function CalcBoundingBox(var AMin,AMax:TPoint3DFloat):Boolean;
    //procedure NormalizeVertex;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    GetMaterialColor : TGetColorProc;
    OwnsGeometry : Boolean;

    procedure AddFace(const Vertexes: Array of Integer); overload;

    function CalcFaceBounds(Face:Integer; out Min,Max:TPoint3DFloat):Boolean;
    function CalcNormals:Boolean;
    procedure ClearNormals;
    procedure DefineProperties(Filer:TFiler); override;
    procedure DeleteLists; override;
    function GetEditor:String; override;
  public
    Faces      : TFaces;
    FacesCount : Integer;

    Geometry   : TGeometry;
    Preview    : TFacePreview;

    Destructor Destroy; override;
    procedure Draw; override;
    class procedure InitPreview(var Preview:TFacePreview);
  end;

  TLocateVertexEvent=procedure(Sender:TObject; Index:Integer) of object;

  TFacesBlockEditor = class(TForm)
    Panel1: TPanel;
    CBPreviewNormals: TCheckBox;
    Label1: TLabel;
    LFaceCount: TLabel;
    Label2: TLabel;
    TBFace: TTrackBar;
    Label3: TLabel;
    LVertexCount: TLabel;
    CBLine: TCheckBox;
    ESmooth: TEdit;
    UDSmooth: TUpDown;
    EFace: TEdit;
    CBOutline: TComboFlat;
    LSmoothGroup: TLabel;
    CBMaterial: TComboFlat;
    Panel2: TPanel;
    LVertex: TListBox;
    PanelPoint: TPanel;
    Panel3: TPanel;
    Label4: TLabel;
    LVertexIndex: TLabel;
    Button1: TButton;
    Splitter1: TSplitter;
    SBNextFace: TSpeedButton;
    SBPrevFace: TSpeedButton;
    procedure CBPreviewNormalsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TBFaceChange(Sender: TObject);
    procedure CBLineClick(Sender: TObject);
    procedure LVertexClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ESmoothChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBOutlineClick(Sender: TObject);
    procedure EFaceChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBMaterialChange(Sender: TObject);
    procedure SBNextFaceClick(Sender: TObject);
    procedure SBPrevFaceClick(Sender: TObject);
  private
    { Private declarations }
    Face : TFacesBlock;

    XYZ : TPointXYZFloat;
    IPoint : TPointEditor;
    FOnLocateVertex : TLocateVertexEvent;

    function VertexIndex:Integer;
    procedure XYZChanged(Sender: TObject);
  public
    { Public declarations }

    property OnLocateVertex:TLocateVertexEvent read FOnLocateVertex write
                                               FOnLocateVertex;
  end;

implementation

{$R *.dfm}

uses
  {$IFNDEF D6}
  TeeHTML,
  {$ENDIF}
  OpenGL2, TeePenDlg;

{ TFacesBlock }

type
  TBlocksAccess=class(TBlocks);

procedure TFacesBlock.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Data',ReadData,WriteData,
                               Assigned(IBlocks) and (not NotSaveFaces) and (FacesCount>0));

  Filer.DefineBinaryProperty('Geometry',Geometry.ReadData,Geometry.WriteData,OwnsGeometry);
end;

procedure TFacesBlock.DeleteLists;
begin
  inherited;
  DeleteList(IList);
  DeleteList(IListPen);
end;

Destructor TFacesBlock.Destroy;
var t : Integer;
begin
  for t:=0 to FacesCount-1 do
      Faces[t].Data:=nil;

  Faces:=nil;
  FacesCount:=0;

  if OwnsGeometry then
     Geometry.Free;

  inherited;
end;

function TFacesBlock.CalcNormals:Boolean;

  procedure CalcFaceNormal(const Face:Integer);

    function NewNormal:Integer;
    begin
      with Geometry do
      begin
        result:=NormalCount;

        if NormalCount>=Length(Normals) then
           SetLength(Normals,NormalCount+1000);

        Inc(NormalCount);
      end;
    end;

    procedure CalcVertexNormal(const A,B,C:Integer);
    var tmpN : TPoint3DFloat;
        tmpI : Integer;
        t    : Integer;
    begin
      with Geometry,Faces[Face] do
      if Data[A].Normal=-1 then
      begin
        tmpN:=CalculateNormal(Vertex[Data[A].Vertex-1].Vertex,
                              Vertex[Data[B].Vertex-1].Vertex,
                              Vertex[Data[C].Vertex-1].Vertex);

        if SmoothGroup=-1 then
        begin
          tmpI:=NewNormal;

          with Normals[tmpI] do
          begin
            Normal:=tmpN;
            Count:=0;
          end;
        end
        else
        begin
          tmpI:=-1;

          with Vertex[Data[A].Vertex-1] do
          for t:=0 to NormalCount-1 do
              if NormalGroups[t].Group=SmoothGroup then
              begin
                tmpI:=NormalGroups[t].Normal;
                break;
              end;

          if tmpI=-1 then
          begin
            tmpI:=NewNormal;
            Normals[tmpI].Normal:=tmpN;
            Normals[tmpI].Count:=1;

            with Vertex[Data[A].Vertex-1] do
            begin
              if NormalCount=0 then
                 SetLength(NormalGroups,Length(SmoothGroups));

              with NormalGroups[NormalCount] do
              begin
                Group:=SmoothGroup;
                Normal:=tmpI;
              end;

              Inc(NormalCount);
            end;
          end
          else
          begin
            with Normals[tmpI],Normal do
            begin
              X:=X+tmpN.X;
              Y:=Y+tmpN.Y;
              Z:=Z+tmpN.Z;

              Inc(Count);
            end;

            result:=True;
          end;
        end;

        Data[A].Normal:=tmpI+1;
      end;
    end;

  var t : Integer;
      tmpV : Integer;
  begin
    tmpV:=Length(Faces[Face].Data);

    for t:=0 to tmpV-3 do
        CalcVertexNormal(t,t+1,t+2);

    CalcVertexNormal(tmpV-2,tmpV-1,0);
    CalcVertexNormal(tmpV-1,0,1);
  end;

var t : Integer;
begin
  result:=False;

  for t:=0 to FacesCount-1 do
      CalcFaceNormal(t);
end;

type
  TBlockFormatAccess=class(TBlockFormat);

procedure TFacesBlock.Draw;

  procedure DrawFaces(const ForceLine:Boolean=False);
  var t : Integer;
      n : Integer;
      tt : Integer;
      NewMode,
      Old : Integer;
      HasNormals : Boolean;
  begin
    Old:=GL_POINTS;

    HasNormals:=Geometry.NormalCount>0;

    for t:=0 to FacesCount-1 do
    with Faces[t] do
    begin
      n:=Length(Data);

      if ForceLine then
         glBegin(GL_LINE_LOOP)
      else
      begin
        if Line then
           NewMode:=GL_LINE_STRIP
        else
        if n=3 then
           NewMode:=GL_TRIANGLES
        else
        if n=4 then
           NewMode:=GL_QUADS
        else
           NewMode:=GL_POLYGON;

        if NewMode<>Old then
        begin
          if Old<>GL_POINTS then
             glEnd;

          glBegin(NewMode);
          Old:=NewMode;
        end;
      end;

      {
       Can't be done here. Must be done outside the Call List.

      if (not IPicking) and (Material<>-1) then
      with GetMaterialColor(Material) do
           glColor3ub(Red,Green,Blue);
      }

      for tt:=0 to n-1 do
      with Data[tt] do
      begin
        if HasNormals then
           glNormal3fv(@Geometry.Normals[Normal-1]);

        if HasTextures then
           glTexCoord3fv(@Geometry.Texture[Texture-1]);

        glVertex3fv(@Geometry.Vertex[Vertex-1]);
      end;

      if ForceLine then
         glEnd
      else
      if Old=GL_POLYGON then
      begin
        glEnd;
        Old:=GL_POINTS;
      end;
    end;

    if Old<>GL_POINTS then
       glEnd;
  end;

  procedure DrawOutline;
  var t : Integer;
  begin
    if Preview.SelectedFace<>-1 then
    if FacesCount>Preview.SelectedFace then
    begin
      glBegin(GL_LINE_LOOP);

      glColor4ubv(@Preview.Color);

      with Faces[Preview.SelectedFace] do
        for t:=0 to Length(Data)-1 do
        with Data[t] do
             glVertex3fv(@Geometry.Vertex[Vertex-1].Vertex);

      glEnd;
    end;
  end;

  procedure PreviewVertexes(Face,AVertex:Integer);
  var tmpE : TEllipsoidBlock;

    procedure DrawVertex(AFace,AIndex:Integer);
    begin
      if (AFace=Preview.SelectedFace) and (AIndex=Preview.SelectedVertex) then
         tmpE.Format.Color:=clPurple
      else
         tmpE.Format.Color:=clGreen;

      with tmpE,Geometry.Vertex[Faces[AFace].Data[AIndex].Vertex-1].Vertex do
      begin
        Location.Point.X:=X;
        Location.Point.Y:=-Z;
        Location.Point.Z:=Y;

        DrawBlock(IBlocks);
      end;
    end;

    procedure PreviewFaceVertex(AFace:Integer);
    var t : Integer;
    begin
      if AVertex=-1 then
         for t:=0 to Length(Faces[AFace].Data)-1 do
             DrawVertex(AFace,t)
      else
         DrawVertex(AFace,AVertex);
    end;

  var tmpS : TPoint3DFloat;
      tmpMin,
      tmpMax : TPoint3DFloat;
      t      : Integer;
  begin
    if CalcFaceBounds(Face,tmpMin,tmpMax) then
    begin
      tmpE:=TEllipsoidBlock.Create(nil);

      try
        tmpE.Format.Border.Visible:=False;
        tmpE.Format.Color:=clGreen;
        tmpE.Size.Value:=0.01*MaxValue(Subtract(tmpMax,tmpMin));

        if Assigned(TBlocksAccess(Self.Parent).IObject) then
        begin
          tmpS:=TBlocksAccess(Self.Parent).IObject.Scale.Point;

          with tmpE.Scale.Point do
          begin
            X:=1/tmpS.X;
            Y:=1/tmpS.Y;
            Z:=1/tmpS.Z;
          end;
        end;

        if Face=-1 then
        for t:=0 to FacesCount-1 do
            PreviewFaceVertex(t)
        else
            PreviewFaceVertex(Face);
      finally
        tmpE.Free;
      end;
    end;
  end;

  procedure DrawNormals;
  var t, tt: Integer;
      tmpV : TPoint3DFloat;
      tmpL : Single;
  begin
    tmpL:=Preview.Length*0.01;

    glBegin(GL_LINES);

    glColor4ubv(@Preview.Color);

    for t:=0 to FacesCount-1 do
    with Faces[t] do
      for tt:=0 to Length(Data)-1 do
      with Data[tt] do
      begin
        tmpV:=Geometry.Vertex[Vertex-1].Vertex;
        glVertex3fv(@tmpV);

        with Geometry.Normals[Normal-1].Normal do
        begin
          tmpV.X:=tmpV.X+X*tmpL;
          tmpV.Y:=tmpV.Y+Y*tmpL;
          tmpV.Z:=tmpV.Z+Z*tmpL;
        end;

        glVertex3fv(@tmpV);
      end;

    glEnd;
  end;

begin
  inherited;

  if not Assigned(Geometry) then
     Exit;
     
  if Format.Solid then
  begin
    if not ShouldDrawInterior then
       glEnable(GL_CULL_FACE);

    if IList=0 then
    begin
      IList:=CreateNewList;
      DrawFaces;
      glEndList;
    end
    else
      glCallList(IList);

    if not ShouldDrawInterior then
       glDisable(GL_CULL_FACE);
  end;

  if TBlockFormatAccess(Format).PreparePen then
  begin
    if IListPen=0 then
    begin
      IListPen:=CreateNewList;
      DrawFaces(True);
      glEndList;
    end
    else
      glCallList(IListPen);

    TBlockFormatAccess(Format).FinishPen;
  end;

  if not IPicking then
  begin
    if Preview.Normals then
       DrawNormals;

    case Preview.Outline of
      foOutline       : DrawOutline;
      foOutlineVertex : begin
                          DrawOutline;
                          PreviewVertexes(Preview.SelectedFace,Preview.SelectedVertex);
                        end;
      foOutlineAll    : begin
                          DrawOutline;
                          PreviewVertexes(Preview.SelectedFace,-1);
                        end;
      foAllVertexes   : PreviewVertexes(-1,-1);
    end;
  end;
end;

(*
function TFacesBlock.CalcBoundingBox(var AMin,AMax:TPoint3DFloat):Boolean;
var First : Boolean;
    t,
    tt  : Integer;
begin
  First:=True;

  for t:=0 to FacesCount-1 do
  with Faces[t] do
  begin
    for tt:=0 to Length(Data)-1 do
    begin
      with Geometry.Vertex[Data[tt].Vertex-1].Vertex do
      begin
        if First then
        begin
          AMin.X:=X;
          AMin.Y:=Y;
          AMin.Z:=Z;
          AMax:=AMin;

          First:=False;
        end
        else
        begin
          if X<AMin.X then AMin.X:=X else
          if X>AMax.X then AMax.X:=X;

          if Y<AMin.Y then AMin.Y:=Y else
          if Y>AMax.Y then AMax.Y:=Y;

          if Z<AMin.Z then AMin.Z:=Z else
          if Z>AMax.Z then AMax.Z:=Z;
        end;
      end;
    end;
  end;

  result:=True;
end;

procedure TFacesBlock.NormalizeVertex;
var Min,
    Max : TPoint3DFloat;
begin
  if CalcBoundingBox(Min,Max) then
  begin

    with Location.Point do
    begin
      X:=(Max.X+Min.X)*0.5;
      Z:=(Max.Y+Min.Y)*0.5;
      Y:=(Max.Z+Min.Z)*0.5;
    end;

    {
    with Size.Point do
    begin
      X:=Abs(Max.X-Min.X);
      Y:=Abs(Max.Y-Min.Y);
      Z:=Abs(Max.Z-Min.Z);
    end;
    }
  end;
end;
*)

procedure TFacesBlock.ReadData(Stream: TStream);
var t : Integer;
    tmp : Integer;
begin
  Stream.Read(FacesCount,SizeOf(FacesCount));
  SetLength(Faces,FacesCount);

  for t:=0 to FacesCount-1 do
  with Faces[t] do
  begin
    Stream.Read(Line,SizeOf(Line));
    Stream.Read(HasTextures,SizeOf(HasTextures));
    Stream.Read(SmoothGroup,SizeOf(SmoothGroup));
    Stream.Read(Material,SizeOf(Material));

    Stream.Read(tmp,SizeOf(tmp));

    if tmp>0 then
    begin
      SetLength(Data,tmp);
      Stream.ReadBuffer(Data[0],tmp*SizeOf(TFaceVertex));
    end;
  end;
end;

procedure TFacesBlock.WriteData(Stream: TStream);
var t : Integer;
    tmp : Integer;
begin
  Stream.Write(FacesCount,SizeOf(FacesCount));

  for t:=0 to FacesCount-1 do
  with Faces[t] do
  begin
    Stream.Write(Line,SizeOf(Line));
    Stream.Write(HasTextures,SizeOf(HasTextures));
    Stream.Write(SmoothGroup,SizeOf(SmoothGroup));
    Stream.Write(Material,SizeOf(Material));

    tmp:=Length(Data);
    Stream.Write(tmp,SizeOf(tmp));

    if tmp>0 then
       Stream.WriteBuffer(Data[0],tmp*SizeOf(TFaceVertex));
  end;
end;

function TFacesBlock.GetEditor: String;
begin
  result:='TFacesBlockEditor';
end;

class procedure TFacesBlock.InitPreview(var Preview:TFacePreview);
begin
  Preview.Color:=clRed;
  Preview.Length:=10;
  Preview.SelectedFace:=-1;
  Preview.SelectedVertex:=-1;
end;

procedure TFacesBlock.ClearNormals;
var t, tt : Integer;
begin
  for t:=0 to FacesCount-1 do
  with Faces[t] do
       for tt:=0 to Length(Data)-1 do
           Data[tt].Normal:=-1;
end;

function TFacesBlock.CalcFaceBounds(Face: Integer; out Min,
  Max: TPoint3DFloat):Boolean;

var
  First : Boolean;

  procedure FaceBounds(AFace:Integer);
  var t, n : Integer;
  begin
    with Faces[AFace] do
    begin
      n:=Length(Data);

      if n>0 then
        for t:=0 to n-1 do
        with Geometry.Vertex[Data[t].Vertex-1].Vertex do
        if First then
        begin
          Min.X:=X;
          Min.Y:=Y;
          Min.Z:=Z;
          Max:=Min;

          First:=False;
        end
        else
        begin
          if X<Min.X then Min.X:=X else if X>Max.X then Max.X:=X;
          if Y<Min.Y then Min.Y:=Y else if Y>Max.Y then Max.Y:=Y;
          if Z<Min.Z then Min.Z:=Z else if Z>Max.Z then Max.Z:=Z;
        end;
    end;
  end;

var t : Integer;
begin
  First:=True;

  if Face=-1 then
     for t:=0 to FacesCount-1 do
         FaceBounds(t)
  else
     FaceBounds(Face);

  result:=not First;
end;

procedure TFacesBlock.AddFace(const Vertexes: Array of Integer);
var t : Integer;
begin
  SetLength(Faces,FacesCount+1);

  SetLength(Faces[FacesCount].Data,High(Vertexes)-Low(Vertexes)+1);

  with Faces[FacesCount] do
  for t:=Low(Vertexes) to High(Vertexes) do
      with Data[t-Low(Vertexes)] do
      begin
        Vertex:=Vertexes[t];
        Normal:=-1;
        SmoothGroup:=0;
      end;

  Inc(FacesCount);
end;

{ TGeometry }

Destructor TGeometry.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGeometry.Clear;
begin
  Vertex:=nil;
  VertexCount:=0;

  Texture:=nil;
  TextureCount:=0;

  Normals:=nil;
  NormalCount:=0;

  ClearSmoothGroups;

  SmoothGroups:=nil;
end;

procedure TGeometry.AverageNormals;
var t : Integer;
    tmpF : Single;
begin
  for t:=0 to NormalCount-1 do
  with Normals[t] do
     if Count>1 then
     begin
       tmpF:=1/Count;

       with Normal do
       begin
         X:=X*tmpF;
         Y:=Y*tmpF;
         Z:=Z*tmpF;
       end;

       Count:=1;
     end;
end;

procedure TGeometry.ClearSmoothGroups;
var t : Integer;
begin
  for t:=0 to VertexCount-1 do
  with Vertex[t] do
  begin
    NormalGroups:=nil;
    NormalCount:=0;
  end;
end;

function TGeometry.NewSmoothGroup(const Number:Cardinal):Cardinal;
var t : Integer;
    tmpL : Integer;
begin
  tmpL:=Length(SmoothGroups);

  for t:=0 to tmpL-1 do
  if SmoothGroups[t]=Number then
  begin
    result:=t;
    exit;
  end;

  SetLength(SmoothGroups,tmpL+1);
  SmoothGroups[tmpL]:=Number;
  result:=tmpL;
end;

procedure TGeometry.AddVertex(const AX, AY, AZ: Single);
begin
  SetLength(Vertex,VertexCount+1);

  with Vertex[VertexCount].Vertex do
  begin
    X:=AX;
    Y:=AY;
    Z:=AZ;
  end;

  Inc(VertexCount);
end;

procedure TGeometry.ReadData(Stream: TStream);
begin
  Stream.Read(VertexCount,SizeOf(VertexCount));
  SetLength(Vertex,VertexCount);

  if VertexCount>0 then
     Stream.ReadBuffer(Vertex[0],VertexCount*SizeOf(TVertexGroups));

  Stream.Read(TextureCount,SizeOf(TextureCount));
  SetLength(Texture,TextureCount);

  if TextureCount>0 then
     Stream.ReadBuffer(Texture[0],TextureCount*SizeOf(TPoint3DFloat));

  Stream.Read(NormalCount,SizeOf(NormalCount));
  SetLength(Normals,NormalCount);

  if NormalCount>0 then
     Stream.ReadBuffer(Normals[0],NormalCount*SizeOf(TVertexNormal));
end;

procedure TGeometry.WriteData(Stream: TStream);
begin
  Stream.Write(VertexCount,SizeOf(VertexCount));

  if VertexCount>0 then
     Stream.WriteBuffer(Vertex[0],VertexCount*SizeOf(TVertexGroups));

  Stream.Write(TextureCount,SizeOf(TextureCount));

  if TextureCount>0 then
     Stream.WriteBuffer(Texture[0],TextureCount*SizeOf(TPoint3DFloat));

  Stream.Write(NormalCount,SizeOf(NormalCount));

  if NormalCount>0 then
     Stream.WriteBuffer(Normals[0],NormalCount*SizeOf(TVertexNormal));
end;

procedure TFacesBlockEditor.CBPreviewNormalsClick(Sender: TObject);
begin
  Face.Preview.Normals:=CBPreviewNormals.Checked;
  Face.Repaint;
end;

procedure TFacesBlockEditor.XYZChanged(Sender: TObject);
var tmp : Integer;
begin
  tmp:=VertexIndex;
  Face.Geometry.Vertex[tmp-1].Vertex:=XYZ.Point;

  Face.DeleteLists;
end;

procedure TFacesBlockEditor.FormShow(Sender: TObject);
begin
  Face:=TFacesBlock(Tag);

  if Assigned(Face) then
  with Face do
  begin
    CBPreviewNormals.Checked:=Preview.Normals;
    CBOutline.ItemIndex:=Ord(Preview.Outline);

    LFaceCount.Caption:=IntToStr(FacesCount);

    if FacesCount>0 then
    begin
      TBFace.Max:=FacesCount-1;
      TBFace.LineSize:=FacesCount div 100;
      TBFace.PageSize:=FacesCount div 10;
      TBFace.Frequency:=FacesCount div 30;
      TBFace.Position:=0;
      TBFaceChange(Self);
    end
    else
    begin
      TBFace.Enabled:=False;
      SBNextFace.Enabled:=False;
      SBPrevFace.Enabled:=False;
      EFace.Enabled:=False;
    end;
  end;
end;

procedure TFacesBlockEditor.TBFaceChange(Sender: TObject);
var t : Integer;
begin
  EFace.Text:=IntToStr(TBFace.Position);

  SBNextFace.Enabled:=TBFace.Position<Face.FacesCount;
  SBPrevFace.Enabled:=TBFace.Position>0;

  Face.Preview.SelectedFace:=TBFace.Position;

  with Face.Faces[TBFace.Position] do
  begin
    LVertex.Items.Clear;

    for t:=0 to Length(Data)-1 do
        LVertex.Items.Add(IntToStr(t));

    if Length(Data)>0 then
    begin
      LVertex.ItemIndex:=0;
      LVertexClick(Self);
    end;

    CBLine.Checked:=Line;
    LVertexCount.Caption:=IntToStr(Length(Data));
    UDSmooth.Position:=SmoothGroup;
    CBMaterial.ItemIndex:=Material+1;
  end;

  if Face.Preview.Outline<>foAllVertexes then
     Face.Repaint;
end;

procedure TFacesBlockEditor.CBLineClick(Sender: TObject);
begin
  Face.Faces[TBFace.Position].Line:=CBLine.Checked;
  Face.DeleteLists;
end;

function TFacesBlockEditor.VertexIndex:Integer;
begin
  result:=Face.Faces[TBFace.Position].Data[LVertex.ItemIndex].Vertex;
end;

procedure TFacesBlockEditor.LVertexClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=VertexIndex;

  LVertexIndex.Caption:=IntToStr(tmp);

  Face.Preview.SelectedVertex:=LVertex.ItemIndex;

  if Face.Preview.Outline=foOutlineVertex then
     Face.Repaint;

  XYZ.Point:=Face.Geometry.Vertex[tmp-1].Vertex;

  if not Assigned(IPoint) then
  begin
    IPoint:=TPointEditor.Create(Self);
    IPoint.Factor:=0.001;
    IPoint.Align:=alClient;
    TTeeVCL.AddFormTo(IPoint,PanelPoint);
  end;

  IPoint.SelectPoint(XYZ);
end;

procedure TFacesBlockEditor.FormDestroy(Sender: TObject);
begin
  IPoint.Free;
  XYZ.Free;
end;

procedure TFacesBlockEditor.ESmoothChange(Sender: TObject);
begin
  if Showing then
  begin
    Face.Faces[TBFace.Position].SmoothGroup:=UDSmooth.Position;
    Face.DeleteLists;
  end;
end;

procedure TFacesBlockEditor.FormCreate(Sender: TObject);
begin
  XYZ:=TPointXYZFloat.Create(nil,0,XYZChanged);

  Button1.Visible:=Assigned(FOnLocateVertex);
  Button1.Enabled:=Button1.Visible;
end;

procedure TFacesBlockEditor.CBOutlineClick(Sender: TObject);
begin
  Face.Preview.Outline:=TFaceOutline(CBOutline.ItemIndex);

  if Face.Preview.Outline=foOutlineVertex then
     Face.Preview.SelectedVertex:=LVertex.ItemIndex;

  Face.Repaint;
end;

procedure TFacesBlockEditor.EFaceChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(EFace.Text,tmp) then
     if (tmp>=TBFace.Min) and (tmp<=TBFace.Max) then
     begin
       TBFace.Position:=tmp;
       TBFaceChange(Self);
     end;
end;

procedure TFacesBlockEditor.Button1Click(Sender: TObject);
begin
  if Assigned(FOnLocateVertex) then
     FOnLocateVertex(Self,VertexIndex);
end;

procedure TFacesBlockEditor.CBMaterialChange(Sender: TObject);
begin
  if Showing then
  begin
    Face.Faces[TBFace.Position].Material:=CBMaterial.ItemIndex-1;
    Face.DeleteLists;
  end;
end;

procedure TFacesBlockEditor.SBNextFaceClick(Sender: TObject);
begin
  with TBFace do
  begin
    Position:=Position+1;
    SBNextFace.Enabled:=Position<Face.FacesCount;
  end;
end;

procedure TFacesBlockEditor.SBPrevFaceClick(Sender: TObject);
begin
  with TBFace do
  begin
    Position:=Position-1;
    SBPrevFace.Enabled:=Position>0;
  end;
end;

initialization
  RegisterBlock(TFacesBlock);
  RegisterClass(TFacesBlockEditor);
end.
