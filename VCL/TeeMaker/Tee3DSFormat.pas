unit Tee3DSFormat;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, TeCanvas, TeeBlocks, TeeObjFormat, TeeFacesBlock, StdCtrls, ExtCtrls;

type
  T3DSObject=class(TBaseObjBlock)
  protected
    IDebug : TStrings;

    function GetEditor:String; override;
    function GetMaterialColor(Material:Integer):TRGB;
    procedure LoadItems(const ASource,AFile:String); override;
  published
    property Items;
    property LinkFile;
    property Properties;
  end;

  TBlock3DSEditor = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  Tee3DSExtension='.3ds';

implementation

{$R *.dfm}

function RGBColor(const ARGB:TRGB):TColor;
begin
  with ARGB do
       result:=RGB(Red,Green,Blue);
end;

{ T3DSObject }

function T3DSObject.GetEditor: String;
begin
  result:='TObjBlockEditor';
end;

function T3DSObject.GetMaterialColor(Material:Integer):TRGB;
begin
  result:=FMaterials[Material].Diffuse;
end;

type
  TFacesBlockAccess=class(TFacesBlock);

procedure T3DSObject.LoadItems(const ASource, AFile: String);
var tmp : TFileStream;

  function ReadString:String;
  var c : Char;
  begin
    result:='';

    repeat
      tmp.Read(c,1);

      if c=#0 then
         break
      else
         result:=result+c;

    until False;
  end;

var
  LastVertex : Integer;

  procedure ReadVertexes;
  var tmpCount : Word;
      t        : Integer;
      data     : Single;
  begin
    tmp.Read(tmpCount,2);

    with Geometry do
    begin
      LastVertex:=VertexCount;
      Inc(VertexCount,tmpCount);
      SetLength(Vertex,VertexCount);
    end;

    for t:=0 to tmpCount-1 do
    with Geometry.Vertex[LastVertex+t] do
    begin
      tmp.Read(data,SizeOf(data));
      Vertex.X:=data;

      tmp.Read(data,SizeOf(data));
      Vertex.Z:=-data;

      tmp.Read(data,SizeOf(data));
      Vertex.Y:=data;
    end;
  end;

  procedure ReadTexCoords;
  var tmpCount : Word;
      t        : Integer;
      data     : Single;
      Old      : Integer;
  begin
    tmp.Read(tmpCount,2);

    with Geometry do
    begin
      Old:=TextureCount;
      Inc(TextureCount,tmpCount);
      SetLength(Texture,TextureCount);
    end;

    for t:=0 to tmpCount-1 do
    with Geometry.Texture[Old+t] do
    begin
      tmp.Read(data,SizeOf(data));
      X:=data;

      tmp.Read(data,SizeOf(data));
      Y:=data;

      Z:=0;
    end;
  end;

  procedure ReadFaces;
  var tmpCount : Word;
      t        : Integer;
      tmpData     : Word;
  begin
    Group:=TFacesBlock.Create(Owner);
    Group.Geometry:=Geometry;
    Group.InitPreview(Group.Preview);

    TFacesBlockAccess(Group).GetMaterialColor:=GetMaterialColor;

    Group.Title:='Face '+IntToStr(Items.Count);

    Items.Add(Group);

    tmp.Read(tmpCount,2);

    Group.FacesCount:=tmpCount;

    SetLength(Group.Faces,tmpCount);

    for t:=0 to tmpCount-1 do
    with Group.Faces[t] do
    begin
      SetLength(Data,3);

      tmp.Read(tmpData,SizeOf(tmpData));
      Data[0].Vertex:=tmpData+1+LastVertex;

      tmp.Read(tmpData,SizeOf(tmpData));
      Data[1].Vertex:=tmpData+1+LastVertex;

      tmp.Read(tmpData,SizeOf(tmpData));
      Data[2].Vertex:=tmpData+1+LastVertex;

      tmp.Read(tmpData,SizeOf(tmpData));

      HasTextures:=Geometry.TextureCount>0;

      if HasTextures then
      begin
        Data[0].Texture:=Data[0].Vertex;
        Data[1].Texture:=Data[1].Vertex;
        Data[2].Texture:=Data[2].Vertex;
      end;

      Data[0].Normal:=-1;
      Data[1].Normal:=-1;
      Data[2].Normal:=-1;

      SmoothGroup:=0;
      Material:=-1;
    end;
  end;

  procedure AddMaterial;
  var tmp : Integer;
  begin
    tmp:=Length(FMaterials);
    SetLength(FMaterials,tmp+1);
    FMaterials[tmp].Name:=ReadString;
  end;

  procedure ReadFacesMaterial;
  var tmpMat : Integer;
      n     : Word;
      t     : Integer;
      tmpFace : Word;
  begin
    tmpMat:=FindMaterial(ReadString);

    tmp.Read(n,SizeOf(n));

    with TFacesBlock(Items[Items.Count-1]) do
    begin
      for t:=0 to n-1 do
      begin
        tmp.Read(tmpFace,SizeOf(tmpFace));
        Faces[tmpFace].Material:=tmpMat;  // <-- not used yet
      end;

      Format.Color:=RGBColor(FMaterials[tmpMat].Diffuse);
    end;
  end;

  function ReadColor:TRGB;
  var tmpchunkID : Word;
      tmpchunkLength : Integer;
  begin
    tmp.Read(tmpchunkID,2);
    tmp.Read(tmpchunkLength,4);

    tmp.Read(result.Red,1);
    tmp.Read(result.Green,1);
    tmp.Read(result.Blue,1);
  end;

  const
    Chunk_Main3DS        = $4D4D;
    Chunk_DataSize       = $3D3D;

    Chunk_Bottom         = $2;      // 10
    Chunk_Edit3DS        = $3D3E;   // 10
    Chunk_Edit_Config1   = $100;    // 10
    Chunk_Light          = $9;      // 320
    Chunk_Tri_Local      = $4160;   // 54
    Chunk_Tri_Smooth     = $4150;   // 54
    Chunk_KeyF3DS        = $B000;   // 219

    Chunk_Object         = $4000;
    Chunk_Object_TriMesh = $4100;   // 754
    Chunk_Vertexes       = $4110;
    Chunk_Faces          = $4120;
    Chunk_Material       = $4130;
    Chunk_TexCoords      = $4140;

    Chunk_MaterialName   = $A000;
    Chunk_MaterialAmbient= $A010;
    Chunk_MaterialDiffuse= $A020;
    Chunk_MaterialSpecular=$A030;
    Chunk_EditMaterial   = $AFFF;

  procedure DoLoad;
  var chunkID : Word;
      chunkLength : Integer;
      tmpSize : Int64;
  begin
    Clear;

    LastVertex:=0;

    tmpSize:=tmp.Size;

    while tmp.Position < tmpSize do
    begin

      tmp.Read(chunkID,2);
      tmp.Read(chunkLength,4);

      if Assigned(IDebug) then
         IDebug.Add('Chunk ID: $'+IntToHex(chunkID,4)+' Size: '+IntToStr(chunkLength));

      case chunkID of
        Chunk_Main3DS  : ;
        Chunk_DataSize : ;
        Chunk_Object_TriMesh : ;
        Chunk_Object   : Title:=ReadString;
        Chunk_Vertexes : ReadVertexes;
        Chunk_TexCoords: ReadTexCoords;
        Chunk_Faces    : ReadFaces;
        Chunk_EditMaterial : ;
        Chunk_MaterialName : AddMaterial;
        Chunk_Material : ReadFacesMaterial;

        Chunk_MaterialAmbient : FMaterials[Length(FMaterials)-1].Ambient:=ReadColor;
        Chunk_MaterialDiffuse : FMaterials[Length(FMaterials)-1].Diffuse:=ReadColor;
        Chunk_MaterialSpecular : FMaterials[Length(FMaterials)-1].Specular:=ReadColor;
      else
        tmp.Seek(chunkLength-6,{$IFDEF D8}soCurrent{$ELSE}soFromCurrent{$ENDIF});
      end;
    end;

    Repaint;
  end;

var tmpMin,
    tmpMax : TPoint3DFloat;
begin
  if AFile<>'' then
  begin
    tmp:=TFileStream.Create(TBlocks.ParseFileName('',AFile),fmOpenRead);
    try
      DoLoad;

      SetLength(Geometry.SmoothGroups,1);
      Geometry.SmoothGroups[0]:=0;

      NormalizeVertexes;

      CalculateNormals;

      BoundingBox(tmpMin,tmpMax);

      Title:=RemoveFileExtension(ExtractFileName(AFile));
    finally
      tmp.Free;
    end;
  end;
end;

initialization
  RegisterBlock(T3DSObject);
end.
