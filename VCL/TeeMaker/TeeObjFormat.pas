unit TeeObjFormat;
{$I TeeDefs.inc}

{$IFDEF D12}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  ExtCtrls, Menus,
  TeCanvas, TeeBlocks, TeeFacesBlock, TeeGeometry, OpenGL2,
  TeeProcs, TeePointEditor;

type
  TBaseObjBlock=class(TCustomObjectBlock)
  private
    procedure ReadData(Stream: TStream);
    procedure VertexBounds(var Min,Max:TPoint3DFloat);
    procedure WriteData(Stream: TStream);
  protected
    FMaterials : Array of TFaceMaterial;

    Geometry : TGeometry;
    Group    : TFacesBlock;

    procedure CalculateNormals;
    procedure DefineProperties(Filer:TFiler); override;
    function FindMaterial(const AName:String):Integer;
    procedure NormalizeVertexes;
    procedure RecalcNormals;
    procedure ResetPreviewNormals;
  public
    Preview : TFacePreview;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Clear; override;
  end;

  TObjBlock=class(TBaseObjBlock)
  private
    IMaterial : TStringList;

    OldColor : TColor;
    OldPicLink : String;
    OldSmoothGroup : Integer;

    procedure Process(S:String);
  protected
    function GetEditor:String; override;
    procedure LoadItems(const ASource,AFile:String); override;
  public
    Destructor Destroy; override;

    procedure Clear; override;
  published
    property Items;
    property LinkFile;
    property Properties;
  end;

  TObjBlockEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    MemoInfo: TMemo;
    TabNormals: TTabSheet;
    Button2: TButton;
    TabSheet1: TTabSheet;
    MemoMaterial: TMemo;
    CBPreviewNormals: TCheckBox;
    Label1: TLabel;
    BNormalColor: TButtonColor;
    ENormalLength: TEdit;
    UDLength: TUpDown;
    TabGroups: TTabSheet;
    LBGroups: TListBox;
    Panel1: TPanel;
    CBFaceVisible: TCheckBox;
    CBShowCurrent: TCheckBox;
    PopupMenu1: TPopupMenu;
    SortbyTitle1: TMenuItem;
    SortbyIndex1: TMenuItem;
    TabSheet3: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    TBVertex: TTrackBar;
    EVertex: TEdit;
    PageControl2: TPageControl;
    TabVertexFaces: TTabSheet;
    TabVertexPoint: TTabSheet;
    LVertexFaces: TListBox;
    Splitter1: TSplitter;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBPreviewNormalsClick(Sender: TObject);
    procedure ENormalLengthChange(Sender: TObject);
    procedure BNormalColorClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure LBGroupsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBFaceVisibleClick(Sender: TObject);
    procedure CBShowCurrentClick(Sender: TObject);
    procedure SortbyTitle1Click(Sender: TObject);
    procedure SortbyIndex1Click(Sender: TObject);
    procedure TBVertexChange(Sender: TObject);
    procedure EVertexChange(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    ObjBlock    : TBaseObjBlock;
    IFaceEditor : TFacesBlockEditor;
    IPoint      : TPointEditor;
    XYZ         : TPointXYZFloat;

    function CurrentFaceBlock:TFacesBlock;
    procedure FillGroups;
    procedure LocateVertex(Sender: TObject; Index:Integer);
    procedure ResortGroups;
    procedure ShowStats;
    procedure XYZChanged(Sender: TObject);
  public
    { Public declarations }
  end;

const
  TeeObjExtension='.obj';

implementation

uses
  Math, TeeGLCanvas, TeeMakerConst, TeePenDlg,
  {$IFNDEF D6}
  TeeHTML,
  {$ENDIF}
  TeeSubdivideMesh;

{$R *.dfm}

function ShortStrToInt(Value: ShortString): Integer;
{$IFNDEF WIN64}
// Value = eax
// Result = eax
asm
  push ebx
  push esi
  mov esi,eax
  xor eax,eax
  movzx ecx,Byte([esi]) // read length byte
  cmp ecx,0
  je @exit
  movzx ebx,Byte([esi+1])
  xor edx,edx // edx = 0
  cmp ebx,45 // check for negative '-' = #45
  jne @loop
  dec edx // edx = -1
  inc esi // skip '-'
  dec ecx
  @loop:
  inc esi
  movzx ebx,Byte([esi])
  imul eax,10
  sub ebx,48 // '0' = #48
  add eax,ebx
  dec ecx
  jnz @loop
  mov ecx,eax
  and ecx,edx
  shl ecx,1
  sub eax,ecx
  @exit:
  pop esi
  pop ebx
end;
{$ELSE}
begin
  result:=StrToInt(Value);
end;
{$ENDIF}

procedure TObjBlockEditor.ShowStats;

  procedure AddPoint(const P:TPoint3DFloat; const S:String);
  begin
    with MemoInfo.Lines do
    begin
      Add(S);
      Add('X: '+FloatToStr(P.X));
      Add('Y: '+FloatToStr(P.Y));
      Add('Z: '+FloatToStr(P.Z));
    end;
  end;

var tmp : Integer;
    t   : Integer;
    Min, Max : TPoint3DFloat;
begin
  MemoInfo.Lines.Clear;
  MemoInfo.Lines.Add('Groups: '+IntToStr(ObjBlock.Items.Count));

  with ObjBlock.Geometry do
  begin
    MemoInfo.Lines.Add('Vertexes: '+IntToStr(VertexCount));
    MemoInfo.Lines.Add('Texture Coords: '+IntToStr(TextureCount));
    MemoInfo.Lines.Add('Normals: '+IntToStr(NormalCount));
    MemoInfo.Lines.Add('Smooth Groups: '+IntToStr(Length(SmoothGroups)));
  end;

  tmp:=0;

  with ObjBlock.Items do
  for t:=0 to Count-1 do
  if Block[t] is TFacesBlock then
      Inc(tmp,TFacesBlock(Block[t]).FacesCount);

  MemoInfo.Lines.Add('Faces: '+IntToStr(tmp));

  if ObjBlock.Geometry.VertexCount>0 then
  begin
    ObjBlock.VertexBounds(Min,Max);

    MemoInfo.Lines.Add('');
    AddPoint(Min,'Min:');
    AddPoint(Max,'Max:');
  end;
end;

{ TBaseObjBlock }

Constructor TBaseObjBlock.Create(AOwner:TComponent);
begin
  inherited;

  Geometry:=TGeometry.Create;
  TFacesBlock.InitPreview(Preview);
end;

Destructor TBaseObjBlock.Destroy;
begin
  FMaterials:=nil;
  Geometry.Free;
  inherited;
end;

procedure TBaseObjBlock.Clear;
begin
  inherited;
  Geometry.Clear;
  FMaterials:=nil;
end;

type
  TFacesBlockAccess=class(TFacesBlock);

procedure TBaseObjBlock.CalculateNormals;
var t : Integer;
    ICalcAverages  : Boolean;
begin
  ICalcAverages:=False;

  for t:=0 to Items.Count-1 do
      if TFacesBlockAccess(Items[t]).CalcNormals then
         ICalcAverages:=True;

  if ICalcAverages then
     Geometry.AverageNormals;

  Geometry.ClearSmoothGroups;
end;

procedure TBaseObjBlock.RecalcNormals;

  procedure ClearNormals;
  var t : Integer;
  begin
    Geometry.Normals:=nil;
    Geometry.NormalCount:=0;

    for t:=0 to Items.Count-1 do
        TFacesBlockAccess(Items[t]).ClearNormals;
  end;

begin
  ClearNormals;
  Geometry.ClearSmoothGroups;

  SetLength(Geometry.Normals,Geometry.VertexCount);
  CalculateNormals;

  DeleteLists;
  Repaint;
end;

procedure TBaseObjBlock.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data',ReadData,WriteData,
                             HasContents and (Geometry.VertexCount>0));
end;

type
  TGeometryAccess=class(TGeometry);

procedure TBaseObjBlock.ReadData(Stream: TStream);
begin
  TGeometryAccess(Geometry).ReadData(Stream);
end;

procedure TBaseObjBlock.WriteData(Stream: TStream);
begin
  TGeometryAccess(Geometry).WriteData(Stream);
end;

procedure TBaseObjBlock.ResetPreviewNormals;
var t : Integer;
begin
  for t:=0 to Items.Count-1 do
  if Items[t] is TFacesBlock then
     TFacesBlock(Items[t]).Preview:=Preview;

  Repaint;
end;

procedure TBaseObjBlock.VertexBounds(var Min,Max:TPoint3DFloat);
var t : Integer;
    First : Boolean;
begin
  First:=True;

  with Geometry do
  for t:=0 to Length(Vertex)-1 do
  begin
    with Vertex[t].Vertex do
    begin
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
        if X<Min.X then Min.X:=X else
        if X>Max.X then Max.X:=X;

        if Y<Min.Y then Min.Y:=Y else
        if Y>Max.Y then Max.Y:=Y;

        if Z<Min.Z then Min.Z:=Z else
        if Z>Max.Z then Max.Z:=Z;
      end;
    end;
  end;
end;

procedure TBaseObjBlock.NormalizeVertexes;
var Min,
    Max,
    tmpSize : TPoint3DFloat;
    t       : Integer;
begin
  VertexBounds(Min,Max);

  with tmpSize do
  begin
    if Max.X=Min.X then X:=1
                   else X:=1/(0.5*(Max.X-Min.X));

    if Max.Y=Min.Y then Y:=1
                   else Y:=1/(0.5*(Max.Y-Min.Y));

    if Max.Z=Min.Z then Z:=1
                   else Z:=1/(0.5*(Max.Z-Min.Z));
  end;

  with Geometry do
  for t:=0 to VertexCount-1 do
  with Vertex[t].Vertex do
  begin
    X:=-1+(X-Min.X)*tmpSize.X;
    Y:=-1+(Y-Min.Y)*tmpSize.Y;
    Z:=-1+(Z-Min.Z)*tmpSize.Z;
  end;

  if (tmpSize.X>tmpSize.Y) and (tmpSize.X>tmpSize.Z) then
  begin
    Scale.Point.X:=1;
    Scale.Point.Y:=tmpSize.X/tmpSize.Y;
    Scale.Point.Z:=tmpSize.X/tmpSize.Z;
  end
  else
  if (tmpSize.Y>tmpSize.X) and (tmpSize.Y>tmpSize.Z) then
  begin
    Scale.Point.Y:=1;
    Scale.Point.X:=tmpSize.Y/tmpSize.X;
    Scale.Point.Z:=tmpSize.Y/tmpSize.Z;
  end
  else
  begin
    Scale.Point.Z:=1;
    Scale.Point.X:=tmpSize.Z/tmpSize.X;
    Scale.Point.Y:=tmpSize.Z/tmpSize.Y;
  end;
end;

function TBaseObjBlock.FindMaterial(const AName: String): Integer;
var t : Integer;
    tmp : String;
begin
  result:=-1;

  tmp:=UpperCase(AName);

  for t:=0 to Length(FMaterials)-1 do
  if UpperCase(FMaterials[t].Name)=tmp then
  begin
    result:=t;
    exit;
  end;
end;

{ TObjBlock }

procedure TObjBlock.Clear;
begin
  inherited;
  FreeAndNil(IMaterial);
end;

function ShortTrimLeft(const S: ShortString): ShortString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);

  if I=1 then
     result:=S
  else
     result:=Copy(S, I, Maxint);
end;

function ShortTrim(const S: ShortString): ShortString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);

  if I > L then
     Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function ShortStringToDouble(const S:ShortString; var Code:Integer):Double;
var
  Digits,
  tmpL,
  ExpValue: Integer;
  Ch: AnsiChar;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;

  if s = '' then
  begin
    inc(Code);
    Exit;
  end;

  Neg    := False;
  NegExp := False;
  Valid  := False;

  while S[code+1] = ' ' do
        Inc(Code);

  Ch := S[code+1];

  if Ch in ['+', '-'] then
  begin
    inc(Code);
    Neg := (Ch = '-');
  end;

  tmpL:=Length(S);

  while code<tmpL do
  begin
      inc(Code);
      Ch := S[code];

      if not (Ch in ['0'..'9']) then
         break;
      Result := (Result * 10) + Ord(Ch) - Ord('0');
      Valid := True;
  end;

  Digits := 0;

  if Ch = '.' then
  begin
      while code<tmpL do
      begin
          inc(Code);
          Ch := S[code];

          if not (Ch in ['0'..'9']) then
          begin
              if not valid then {Starts with '.'}
              begin
                  if Ch = #0 then
                    dec(code); {s = '.'}
              end;

              break;
          end;

          Result := (Result * 10) + Ord(Ch) - Ord('0');
          Dec(Digits);
          Valid := true;
      end;
  end;

  ExpValue := 0;

  if (Ord(Ch) or $20) = ord('e') then
  begin {Ch in ['E','e']}
      Valid := false;
      Ch := S[code+1];

      if Ch in ['+', '-'] then
      begin
          inc(Code);
          NegExp := (Ch = '-');
          //Ch := S[code+1];
      end;

      while true do
      begin
          inc(Code);
          Ch := S[code];

          if not (Ch in ['0'..'9']) then
            break;

          ExpValue := (ExpValue * 10) + Ord(Ch) - Ord('0');
          Valid := true;
      end;

     if NegExp then
        ExpValue := -ExpValue;
  end;

  Digits := Digits + ExpValue;

  if Digits <> 0 then
     Result := Result * IntPower(10, Digits);

  if Neg then
     Result := -Result;

  if Valid then
     code := 0;
end;

procedure TObjBlock.Process(S:String);

  function TryStringToColor(S:ShortString):TColor;
  begin
    S:=UpperCase(ShortTrim(S));

    if not IdentToColor('cl'+S, Longint(result)) then
       if S='GLASS' then
          result:=(128 shl 24)+clWhite
       else
       if S='DKGREY' then
          result:=clDkGray
       else
          result:=clDefault;
  end;

  procedure SetGroupDefaults;
  begin
    Group.Geometry:=Geometry;
    Group.InitPreview(Group.Preview);

    with Group.Format do
    begin
      Color:=OldColor;
      Texture.PictureLink:=OldPicLink;
    end;
  end;

  procedure CheckGroup(AParent:TBlocks);
  begin
    if not Assigned(Group) then
    begin
      Group:=TFacesBlock.Create(Owner);

      SetGroupDefaults;

      AParent.Add(Group);
    end;
  end;

  procedure AddFaceItem(Index:Integer; S:ShortString);
  var tmp : Integer;
      i   : Integer;
  begin
    with Group.Faces[Group.FacesCount] do
    begin
      tmp:=Length(Data);

      if Index>=tmp then
         SetLength(Data,tmp+4);

      with Data[Index] do
      begin
        i:=Pos('/',S);
        if i>0 then
        begin
          Vertex:=ShortStrToInt(Copy(S,1,i-1));
          if Vertex<0 then
             Vertex:=Geometry.VertexCount+Vertex+1;

          Assert(Vertex<Geometry.VertexCount+1,'Vertex not found: '+IntToStr(Vertex)+' '+S);

          Delete(S,1,i);

          i:=Pos('/',S);
          if i>0 then
          begin
            if i>1 then
            begin
              Texture:=ShortStrToInt(Copy(S,1,i-1));
              if Texture<0 then
                 Texture:=Geometry.TextureCount+Texture+1;

              Assert(Texture<Geometry.TextureCount+1,'Texture not found: '+IntToStr(Texture)+' '+S);

              HasTextures:=True;
            end;

            Normal:=ShortStrToInt(Copy(S,i+1,Length(S)));
            if Normal<0 then
               Normal:=Geometry.NormalCount+Normal+1;

            Assert(Normal<Geometry.NormalCount+1,'Normal not found: '+IntToStr(Normal)+' '+S);
          end
          else
          begin
            Texture:=ShortStrToInt(S);
            if Texture<0 then
               Texture:=Geometry.TextureCount+Texture+1;

            Assert(Texture<Geometry.TextureCount+1,'Texture not found: '+IntToStr(Texture)+' '+S);

            HasTextures:=True;
            Normal:=-1;
          end;
        end
        else
        begin
          Vertex:=ShortStrToInt(S);
          if Vertex<0 then
             Vertex:=Geometry.VertexCount+Vertex+1;

          Assert(Vertex<Geometry.VertexCount+1,'Vertex not found: '+IntToStr(Vertex)+' '+S);

          Normal:=-1;
        end;
      end;
    end;
  end;

  procedure AddFace(S:String; IsLine:Boolean=False);
  var i : Integer;
      FaceItemIndex : Integer;
  begin
    CheckGroup(Items);

    with Group do
    begin
      if Length(Faces)<FacesCount+1 then
         SetLength(Faces,FacesCount+100);

      with Faces[FacesCount] do
      begin
        HasTextures:=False;
        Line:=IsLine;
        SmoothGroup:=OldSmoothGroup;
      end;
    end;

    Delete(s,1,2);

    FaceItemIndex:=0;
    SetLength(Group.Faces[Group.FacesCount].Data,3);

    repeat
      while s[1]=' ' do Delete(s,1,1);
      i:=Pos(' ',s);

      if i>0 then
      begin
        AddFaceItem(FaceItemIndex,Copy(s,1,i-1));
        Delete(s,1,i);
        Inc(FaceItemIndex);
      end
      else
      if s<>'' then
      begin
        AddFaceItem(FaceItemIndex,s);
        Inc(FaceItemIndex);
        break;
      end;

    until s='';

    with Group.Faces[Group.FacesCount] do
         if Length(Data)<>FaceItemIndex then
            SetLength(Data,FaceItemIndex);

    Inc(Group.FacesCount);
  end;

  procedure ParsePoint(S:ShortString; var Data:TPoint3DFloat);
  var i : Integer;
      Code : Integer;
  begin
    with Data do
    begin
      Delete(s,1,2);
      while s[1]=' ' do Delete(s,1,1);

      i:=Pos(' ',s);

      X:=ShortStringToDouble(Copy(s,1,i-1),Code);
      if Code<>0 then
         raise Exception.Create('Wrong X format: '+Copy(s,1,i-1));

      Delete(s,1,i);
      while s[1]=' ' do Delete(s,1,1);

      i:=Pos(' ',s);
      if i>0 then
      begin
        Y:=ShortStringToDouble(Copy(s,1,i-1),Code);
        if Code<>0 then
           raise Exception.Create('Wrong Y format: '+Copy(s,1,i-1));

        Delete(s,1,i);
        while s[1]=' ' do Delete(s,1,1);

        i:=Pos(' ',s);
        if i>0 then
        begin
          Z:=ShortStringToDouble(Copy(s,1,i-1),Code);
          if Code<>0 then
             raise Exception.Create('Wrong Z format: '+Copy(s,1,i-1));
        end
        else
        begin
          Z:=ShortStringToDouble(s,Code);
          if Code<>0 then
             raise Exception.Create('Wrong Z format: '+s);
        end;
      end
      else
      begin
        Y:=ShortStringToDouble(s,Code);
        if Code<>0 then
           raise Exception.Create('Wrong Y format: '+s);

        Z:=0;
      end;
    end;
  end;

  function LinkPath:String;
  begin
    result:=ExtractFilePath(LinkFile);
    result:=TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,result);
  end;

  procedure LoadMaterial(const AFile:String);
  var tmp : String;
      t   : Integer;
      tmpMat : TStringList;
  begin
    if not Assigned(IMaterial) then
       IMaterial:=TStringList.Create;

    tmp:=LinkPath;

    tmpMat:=TStringList.Create;
    try
      if FileExists(tmp+'\'+AFile) then
         tmpMat.LoadFromFile(tmp+'\'+AFile)
      else
      if FileExists(AFile) then
         tmpMat.LoadFromFile(AFile);

      with tmpMat do
      for t:=0 to Count-1 do
          Strings[t]:=Trim(Strings[t]);

      IMaterial.AddStrings(tmpMat);
    finally
      tmpMat.Free;
    end;
  end;

  procedure ProcessMaterial(Material:ShortString);

     function FindTag(Start:Integer; const Tag:ShortString; out TagData:ShortString):Boolean;
     const NewMtlTag='NEWMTL';
     var t : Integer;
     begin
       for t:=Start to IMaterial.Count-1 do
       if UpperCase(Copy(IMaterial[t],1,Length(Tag)))=UpperCase(Tag) then
       begin
         TagData:=Trim(Copy(IMaterial[t],Length(Tag)+1,Length(IMaterial[t])));
         result:=True;
         exit;
       end
       else
       if UpperCase(Copy(IMaterial[t],1,Length(NewMtlTag)))=NewMtlTag then
          break;

       result:=False;
       TagData:='';
     end;

     function ParseColor(S:ShortString):TColor;
     var r,g,b : Byte;
         i     : Integer;
     begin
       s:=ShortTrimLeft(s);
       
       i:=Pos(' ',s);
       r:=Round(255*StrToFloat(Copy(s,1,i-1)));
       Delete(s,1,i);

       i:=Pos(' ',s);
       g:=Round(255*StrToFloat(Copy(s,1,i-1)));
       Delete(s,1,i);

       b:=Round(255*StrToFloat(s));

       result:=RGB(r,g,b);
     end;

  var t : Integer;
      Found : Boolean;
      tmp,
      s     : ShortString;
  begin
    Found:=False;

    if Assigned(IMaterial) then
    for t:=0 to IMaterial.Count-1 do
    begin
      s:=IMaterial[t];

      if Copy(s,1,7)='newmtl ' then
      begin
        if UpperCase(Copy(s,8,Length(s)))=UpperCase(Material) then
        begin
          if FindTag(t+1,'map_Kd ',tmp) then
          begin
            OldPicLink:=LinkPath+'\'+tmp;
            Group.Format.Texture.PictureLink:=OldPicLink;

            OldColor:=clDefault;
            Group.Format.Color:=OldColor;

            Found:=True;
          end
          else
          if FindTag(t+1,'Kd ',tmp) then
          begin
            OldColor:=ParseColor(tmp);
            Group.Format.Color:=OldColor;

            OldPicLink:='';
            Group.Format.Texture.PictureLink:=OldPicLink;

            Found:=True;
          end;

          if FindTag(t+1,'d ',tmp) and (tmp<>'1.0') then
             Group.Format.Transparency:=255-Round(255*StrToFloat(tmp));

          break;
        end;
      end;
    end;

    if not Found then
    begin
      OldColor:=TryStringToColor(Material);
      OldPicLink:='';

      SetGroupDefaults;
    end;
  end;

  procedure NewGroup(Sibling:Boolean);
  var tmp : TBlocks;
  begin
    if Assigned(Group) then
    begin
      tmp:=Group.Parent;

      if Group.FacesCount=0 then
      begin
        Group.Parent.Remove(Group);
        Group.Free;
      end;

      Group:=nil;
    end
    else
      tmp:=Items;

    CheckGroup(tmp);
  end;

  procedure ProcessGroup(s:ShortString);

    function FindGroup(AGroup:ShortString):TBlocks;
    var t : Integer;
    begin
      AGroup:=UpperCase(AGroup);

      for t:=0 to Items.Count-1 do
      if (Items[t] is TCustomObjectBlock) and (UpperCase(Items[t].Title)=AGroup) then
      begin
        result:=TCustomObjectBlock(Items[t]).Items;
        exit;
      end;

      result:=nil;
    end;

  {var i : Integer;
      tmpG : TBlocks;
      tmpO : TCustomObjectBlock;
      tmp  : ShortString;}
  begin
    NewGroup(False);

    {
    i:=Pos(' ',s);

    if i>0 then
    begin
      tmp:=Copy(s,1,i-1);
      tmpG:=FindGroup(tmp);

      if Assigned(tmpG) then
      begin
        Delete(s,1,i);

        Group.Parent:=tmpG;
        Group.Title:=s;
      end
      else
      begin
        tmpO:=TObjectBlock.Create(Owner);
        tmpO.Parent:=Items;
        tmpO.Title:=tmp;

        Delete(s,1,i);
        Group.Parent:=tmpO.Items;
        Group.Title:=s;
      end;
    end
    else
    }
      Group.Title:=s;
  end;

var i : Integer;
    tmp : ShortString;
begin
  with Geometry do
  if Copy(s,1,2)='v ' then
  begin
    if Length(Vertex)<VertexCount+1 then
       SetLength(Vertex,VertexCount+100);

    ParsePoint(s,Vertex[VertexCount].Vertex);

    Inc(VertexCount);
  end
  else
  if Copy(s,1,3)='vt ' then
  begin
    if Length(Texture)<TextureCount+1 then
       SetLength(Texture,TextureCount+100);

    ParsePoint(s,Texture[TextureCount]);
    with Texture[TextureCount] do Y:=-Y;

    Inc(TextureCount);
  end
  else
  if Copy(s,1,3)='vn ' then
  begin
    if Length(Normals)<NormalCount+1 then
       SetLength(Normals,NormalCount+100);

    ParsePoint(s,Normals[NormalCount].Normal);
    Inc(NormalCount);
  end
  else
  if Copy(s,1,2)='f ' then
     AddFace(s)
  else
  if Copy(s,1,2)='l ' then
     AddFace(s,True)
  else
  if Copy(s,1,7)='usemtl ' then
  begin
    if Assigned(Group) and (Group.FacesCount>0) then
    begin
      NewGroup(True);
      Group.Title:=ShortTrim(Copy(s,8,Length(s)));
    end
    else
      CheckGroup(Items);

    ProcessMaterial(Copy(s,8,Length(s)));
  end
  else
  if Copy(s,1,7)='mtllib ' then
  begin
    s:=Copy(s,8,Length(s));

    repeat
      s:=ShortTrim(s);

      i:=Pos(' ',s);

      if i>0 then
      begin
        LoadMaterial(Copy(s,1,i-1));
        Delete(s,1,i);
      end
      else
      begin
        LoadMaterial(s);
        s:='';
      end;
    until s='';
  end
  else
  if Copy(s,1,2)='g ' then
     ProcessGroup(ShortTrim(Copy(s,3,Length(s))))
  else
  if Copy(s,1,2)='s ' then
  begin
    tmp:=ShortTrim(Copy(s,3,Length(s)));

    if (UpperCase(tmp)='OFF') or (tmp='0') then
       OldSmoothGroup:=-1
    else
       OldSmoothGroup:=NewSmoothGroup(StrToInt64(tmp));
  end;
end;

procedure TObjBlock.LoadItems(const ASource,AFile:String);
var List : Text;

  procedure DoLoad;
  var s : String;
      Old : Char;
      tmpMin,
      tmpMax : TPoint3DFloat;
  begin
    Old:={$IFDEF D15}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF D15}FormatSettings.{$ENDIF}DecimalSeparator:='.';

    try
      Clear;

      Group:=nil;
      OldColor:=clDefault;
      OldPicLink:='';

      OldSmoothGroup:=Geometry.NewSmoothGroup(1);

      while not eof(List) do
      begin
        Readln(List,s);
        Process(TrimLeft(s));
      end;
    finally
      {$IFDEF D15}FormatSettings.{$ENDIF}DecimalSeparator:=Old;
    end;

    NormalizeVertexes;

//    for t:=0 to Items.Count-1 do
//        TObjBlockItem(Items[t]).NormalizeVertex;

    CalculateNormals;

    BoundingBox(tmpMin,tmpMax);

    Repaint;
  end;

begin
  if AFile<>'' then
  begin
    AssignFile(List,TBlocks.ParseFileName('',AFile));
    Reset(List);

    try
      DoLoad;
      Title:=RemoveFileExtension(ExtractFileName(AFile));
    finally
      CloseFile(List);
    end;
  end;
end;

function TObjBlock.GetEditor: String;
begin
  result:='TObjBlockEditor';
end;

procedure TObjBlockEditor.FormShow(Sender: TObject);
begin
  ObjBlock:=TBaseObjBlock(Tag);

  if Assigned(ObjBlock) then
  begin
    ShowStats;

    CBPreviewNormals.Checked:=ObjBlock.Preview.Normals;
    BNormalColor.SymbolColor:=ObjBlock.Preview.Color;
    UDLength.Position:=ObjBlock.Preview.Length;

    if (ObjBlock is TObjBlock) and Assigned(TObjBlock(ObjBlock).IMaterial) then
       MemoMaterial.Lines.AddStrings(TObjBlock(ObjBlock).IMaterial);

    TBVertex.Max:=ObjBlock.Geometry.VertexCount-1;
    TBVertex.Frequency:=TBVertex.Max div 50;
  end;
end;

procedure TObjBlockEditor.Button2Click(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    ObjBlock.RecalcNormals;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TObjBlockEditor.CBPreviewNormalsClick(Sender: TObject);
begin
  ObjBlock.Preview.Normals:=CBPreviewNormals.Checked;
  ObjBlock.ResetPreviewNormals;
end;

procedure TObjBlockEditor.ENormalLengthChange(Sender: TObject);
begin
  if Showing then
  begin
    ObjBlock.Preview.Length:=UDLength.Position;
    ObjBlock.ResetPreviewNormals;
  end;
end;

procedure TObjBlockEditor.BNormalColorClick(Sender: TObject);
begin
  ObjBlock.Preview.Color:=BNormalColor.SymbolColor;
  ObjBlock.ResetPreviewNormals;
end;

procedure TObjBlockEditor.FillGroups;
var t : Integer;
begin
  with ObjBlock.Items do
    for t:=0 to Count-1 do
    if Block[t] is TFacesBlock then
       LBGroups.Items.AddObject(TFacesBlock(Block[t]).Title,Block[t]);
end;

procedure TObjBlockEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabGroups then
  if LBGroups.Items.Count=0 then
  begin
    LBGroups.Items.Clear;

    FillGroups;

    if LBGroups.Items.Count>0 then
    begin
      LBGroups.ItemIndex:=0;
      LBGroupsClick(Self);
    end;
  end;
end;

function TObjBlockEditor.CurrentFaceBlock:TFacesBlock;
begin
  result:=TFacesBlock(LBGroups.Items.Objects[LBGroups.ItemIndex]);
end;

procedure TObjBlockEditor.LocateVertex(Sender: TObject; Index:Integer);
begin
  TBVertex.Position:=Index;
  TBVertexChange(Self);
end;

procedure TObjBlockEditor.LBGroupsClick(Sender: TObject);

  procedure AddMaterials;
  var t : Integer;
  begin
    with IFaceEditor.CBMaterial.Items do
    begin
      Clear;
      Add('(default)');

      for t:=0 to Length(ObjBlock.FMaterials)-1 do
          Add(ObjBlock.FMaterials[t].Name);
    end;
  end;

var t : Integer;
    tmpCurrent : TFacesBlock;
begin
  if LBGroups.ItemIndex<>-1 then
  begin
    if not Assigned(IFaceEditor) then
    begin
      IFaceEditor:=TFacesBlockEditor.Create(Self);
      IFaceEditor.Align:=alClient;

      AddMaterials;

      TTeeVCL.AddFormTo(IFaceEditor,TabGroups);
      IFaceEditor.OnLocateVertex:=LocateVertex;
    end;

    CBFaceVisible.Checked:=CurrentFaceBlock.Visible;

    tmpCurrent:=CurrentFaceBlock;

    IFaceEditor.Tag:={$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(tmpCurrent);
    IFaceEditor.OnShow(IFaceEditor);

    if CBShowCurrent.Checked then
    with ObjBlock.Items do
       for t:=0 to Count-1 do
       if Block[t] is TFacesBlock then
       with TFacesBlock(Block[t]) do
       begin
         Visible:=(Block[t]=tmpCurrent);

         if Visible then
            Format.VisibleInterior:=True;
       end;
  end;
end;

procedure TObjBlockEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabNormals;
  XYZ:=TPointXYZFloat.Create(nil,0,XYZChanged);
end;

procedure TObjBlockEditor.CBFaceVisibleClick(Sender: TObject);
begin
  CurrentFaceBlock.Visible:=CBFaceVisible.Checked;
end;

procedure TObjBlockEditor.CBShowCurrentClick(Sender: TObject);
var t : Integer;
begin
  with ObjBlock.Items do
     for t:=0 to Count-1 do
     if Block[t] is TFacesBlock then
        if not CBShowCurrent.Checked then
        with TFacesBlock(Block[t]) do
        begin
          Visible:=True;
          Format.VisibleInterior:=False;
        end
        else
        begin
          Block[t].Visible:=(Block[t]=CurrentFaceBlock);

          if Block[t].Visible then
             Block[t].Format.VisibleInterior:=True;
        end;

  ObjBlock.Repaint;
end;

procedure TObjBlockEditor.SortbyTitle1Click(Sender: TObject);
begin
  SortbyTitle1.Checked:=True;
  ResortGroups;
end;

procedure TObjBlockEditor.ResortGroups;
var tmp : TFacesBlock;
begin
  if LBGroups.ItemIndex=-1 then
     tmp:=nil
  else
     tmp:=CurrentFaceBlock;

  LBGroups.Items.Clear;
  LBGroups.Sorted:=SortbyTitle1.Checked;
  FillGroups;

  if Assigned(tmp) then
     LBGroups.ItemIndex:=LBGroups.Items.IndexOfObject(tmp);
end;

procedure TObjBlockEditor.SortbyIndex1Click(Sender: TObject);
begin
  SortbyIndex1.Checked:=True;
  ResortGroups;
end;

procedure TObjBlockEditor.TBVertexChange(Sender: TObject);
var tmp,
    t,
    tt,
    ttt : Integer;
begin
  tmp:=TBVertex.Position;
  EVertex.Text:=IntToStr(tmp);

  LVertexFaces.Items.Clear;

  with ObjBlock.Items do
  for t:=0 to Count-1 do
  if Block[t] is TFacesBlock then
  with TFacesBlock(Block[t]) do
  begin
    for tt:=0 to FacesCount-1 do
        for ttt:=0 to Length(Faces[tt].Data)-1 do
        if Faces[tt].Data[ttt].Vertex-1=tmp then
        begin
          LVertexFaces.Items.AddObject(Title+' '+IntToStr(tt),Block[t]);
          break;
        end;
  end;
end;

procedure TObjBlockEditor.EVertexChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(EVertex.Text,tmp) then
     if (tmp>=TBVertex.Min) and (tmp<=TBVertex.Max) then
     begin
       TBVertex.Position:=tmp;
       TBVertexChange(Self);
     end;
end;

procedure TObjBlockEditor.PageControl2Change(Sender: TObject);
begin
  if not Assigned(IPoint) then
  begin
    IPoint:=TPointEditor.Create(Self);
    IPoint.Factor:=0.001;
    IPoint.Align:=alClient;
    TTeeVCL.AddFormTo(IPoint,TabVertexPoint);
  end;

  XYZ.Point:=ObjBlock.Geometry.Vertex[TBVertex.Position].Vertex;
  IPoint.SelectPoint(XYZ);
end;

procedure TObjBlockEditor.XYZChanged(Sender: TObject);
begin
  ObjBlock.Geometry.Vertex[TBVertex.Position].Vertex:=XYZ.Point;
  ObjBlock.Repaint;
end;

procedure TObjBlockEditor.FormDestroy(Sender: TObject);
begin
  XYZ.Free;
end;

Destructor TObjBlock.Destroy;
begin
  FreeAndNil(IMaterial);
  inherited;
end;

procedure TObjBlockEditor.Button1Click(Sender: TObject);
begin
  with TSubDivideMesh.Create(CurrentFaceBlock) do
  try
    SubDivide;

    with TFacesBlockAccess(CurrentFaceBlock) do
    begin
      ClearNormals;
      CalcNormals;
      DeleteLists;
    end;
  finally
    Free;
  end;
end;

initialization
  RegisterBlock(TObjBlock);
  RegisterClass(TObjBlockEditor);
end.
