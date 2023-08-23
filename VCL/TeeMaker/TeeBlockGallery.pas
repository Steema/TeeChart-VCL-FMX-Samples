unit TeeBlockGallery;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF D6}
  Types,
  {$ENDIF}
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QMenus, QButtons,
  QComCtrls, QImgList,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons,
  ComCtrls, ImgList,
  {$ENDIF}
  TeeProcs, TeeMakerControl, TeeBlocks, TeeComma, TeeMakerLibrary;

type
  TBlockGallery = class(TForm)
    Panel1: TPanel;
    TeeCommander1: TTeeCommander;
    BitBtn1: TBitBtn;
    PopupMenu1: TPopupMenu;
    Shadows1: TMenuItem;
    Reflection1: TMenuItem;
    Boundingbox1: TMenuItem;
    Axes1: TMenuItem;
    Texture1: TMenuItem;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    Borders1: TMenuItem;
    PageControl1: TPageControl;
    TabBasic: TTabSheet;
    ListBlocks: TListBox;
    TabLibrary: TTabSheet;
    Splitter1: TSplitter;
    Images: TImageList;
    Gradient1: TMenuItem;
    Floor1: TMenuItem;
    MakerGallery: TMaker;
    Antialias1: TMenuItem;
    TabBlocks: TTabSheet;
    TreeBlocks: TTreeView;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBlocksClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBlocksDblClick(Sender: TObject);
    procedure MakerGallery1DblClick(Sender: TObject);
    procedure Shadows1Click(Sender: TObject);
    procedure Reflection1Click(Sender: TObject);
    procedure Boundingbox1Click(Sender: TObject);
    procedure Axes1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Texture1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure Borders1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Floor1Click(Sender: TObject);
    procedure Gradient1Click(Sender: TObject);
    procedure Antialias1Click(Sender: TObject);
    procedure TreeBlocksDblClick(Sender: TObject);
    procedure TreeBlocksChange(Sender: TObject; Node: TTreeNode);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Current  : TBlockClass;
    CurrentBlocks : TBlocks;
    CurrentBlock  : TCustomBlock;

    ILibrary : TMakerLibrary;
    ITexture : String;
    LastIndex: Integer;

    procedure CheckLibrary;
    function CreateNewObjectBlock(ABlockOwner:TComponent):TCustomObjectBlock;
    procedure LibraryOpenObject(Sender:TObject);
    procedure LibrarySelectNode(Sender: TObject; Node: TTreeNode);
    function NewBlock:TCustomBlock;
    function SelectedBlock(ABlockOwner:TComponent):TCustomBlock;
  public
    { Public declarations }

    class function ModalShow(AOwner,ABlockOwner:TComponent;
                             ACurrent:TBlockClass=nil;
                             ForGallery:Boolean=False):TCustomBlock;

    class function ChooseBlock(AOwner:TComponent; ABlocks:TBlocks;
                               ABlock:TCustomBlock=nil):TCustomBlock;
    class function ChooseObject(AOwner:TComponent):String;

    class procedure ModalPreview(AOwner:TComponent; ABlock:TCustomBlock);

    procedure PrepareForGallery(ABlock:TCustomBlock);
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  TeeBlockEditor, TeeObjFormat, TeePenDlg, TeCanvas, TeeMakerConst,
  TeeBlockClasses;

const
  GalleryKey='\BlockGallery';

procedure TBlockGallery.FormCreate(Sender: TObject);
var tmpList : TStringList;
begin
  LastIndex:=-1;
  
  tmpList:=BlockClasses.Sorted;
  try
    ListBlocks.Items:=tmpList;
  finally
    tmpList.Free;
  end;

  MakerGallery.Gradient.EndColor:=clBlue;
  MakerGallery.Options.Floor.Reflection:=35;
  MakerGallery.Options.DrawShadows:=Shadows1.Checked;

  if TeeMakerReadRegistry(GalleryKey,'GalleryMax','')='' then
  begin
    Width:=StrToIntDef(TeeMakerReadRegistry(GalleryKey,'GalleryWidth',''),Width);
    Height:=StrToIntDef(TeeMakerReadRegistry(GalleryKey,'GalleryHeight',''),Height);
  end
  else
    WindowState:=wsMaximized;

  ITexture:=TeeMakerReadRegistry(GalleryKey,'GalleryTexture',TeeMakerLibraryTag+'Basic\Rocks.bmp');
end;

function TBlockGallery.SelectedBlock(ABlockOwner:TComponent):TCustomBlock;
var tmpClass : {$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF};
begin
  if PageControl1.ActivePage=TabBasic then
  begin
    tmpClass:={$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(ListBlocks.Items.Objects[ListBlocks.ItemIndex]);
    result:=BlockClasses[tmpClass].Create(ABlockOwner);
  end
  else
  if PageControl1.ActivePage=TabBlocks then
  begin
    result:=CurrentBlocks.CloneBlock(TCustomBlock(TreeBlocks.Selected.Data));
    result.Location.Value:=0;
  end
  else
     result:=CreateNewObjectBlock(ABlockOwner);
end;

function TBlockGallery.NewBlock:TCustomBlock;
var AMin, AMax : TPoint3DFloat;
begin
  result:=SelectedBlock(Self);

  result.Cursor:=crHandPoint;
  
  MakerGallery.Blocks.Clear;
  MakerGallery.Blocks.Add(result);

  if result is TCustomObjectBlock then
  begin
    Screen.Cursor:=crHourGlass;
    try
      TCustomObjectBlock(result).Items;
    finally
      Screen.Cursor:=crDefault;
    end;

    result.BoundingBox(AMin,AMax);
    result.Bounds.Bottom:=0;
  end;
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TBlockGallery.PrepareForGallery(ABlock:TCustomBlock);
begin
  ABlock.Format.Color:=RGB(64+Random(191),64+Random(191),64+Random(191));

  if Texture1.Checked then
     if ABlock.Format.Texture.PictureLink='' then
        ABlock.Format.Texture.PictureLink:=ITexture;

  TBlockAccess(ABlock).PrepareForGallery;
end;

procedure TBlockGallery.ListBlocksClick(Sender: TObject);
begin
  if PageControl1.Visible and (PageControl1.ActivePage=TabBasic) then
  if ListBlocks.ItemIndex<>LastIndex then
  begin
    PrepareForGallery(NewBlock);
    LastIndex:=ListBlocks.ItemIndex;
  end;
end;

class function TBlockGallery.ModalShow(AOwner,ABlockOwner:TComponent;
                                       ACurrent:TBlockClass=nil; ForGallery:Boolean=False):TCustomBlock;
begin
  with TBlockGallery.Create(AOwner) do
  try
    Current:=ACurrent;

    if ShowModal=mrOk then
    begin
      result:=SelectedBlock(ABlockOwner);

      if ForGallery then
         PrepareForGallery(result);
    end
    else
       result:=nil;
  finally
    Free;
  end;
end;

type
  TCustomObjectBlockAccess=class(TCustomObjectBlock);

class procedure TBlockGallery.ModalPreview(AOwner:TComponent; ABlock:TCustomBlock);
var tmp : TCustomBlock;
begin
  with TBlockGallery.Create(AOwner) do
  try
    BCancel.Hide;
    PageControl1.Hide;

    tmp:=ABlock.Clone;

    if tmp is TCustomObjectBlock then
       TCustomObjectBlock(tmp).LinkFile:=TCustomObjectBlockAccess(ABlock).CompleteLinkFile;

    tmp.Location.Point:=PointFloat(0,0,0);
    tmp.Scale.Point:=PointFloat(1,1,1);
    tmp.Rotation.Point:=PointFloat(0,0,0);

    MakerGallery.Blocks.Add(tmp);

    ShowModal;
  finally
    Free;
  end;
end;

procedure TBlockGallery.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    if Key=Ord('B') then
       Borders1Click(Self)
    else
    if Key=Ord('S') then
       Shadows1Click(Self)
    else
    if Key=Ord('R') then
       Reflection1Click(Self)
    else
    if Key=Ord('X') then
       Boundingbox1Click(Self)
    else
    if Key=Ord('A') then
       Axes1Click(Self)
    else
    if Key=Ord('T') then
       Texture1Click(Self);

    MakerGallery.Invalidate;
  end;
end;

procedure TBlockGallery.ListBlocksDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TBlockGallery.MakerGallery1DblClick(Sender: TObject);
begin
  TBlockEditor.ModalShow(Self,MakerGallery.Blocks[0]);
  MakerGallery.CancelMouse:=True;
end;

procedure TBlockGallery.Shadows1Click(Sender: TObject);
begin
  Shadows1.Checked:=not Shadows1.Checked;

  with MakerGallery.Options do
       DrawShadows:=not DrawShadows;
end;

procedure TBlockGallery.Reflection1Click(Sender: TObject);
begin
  Reflection1.Checked:=not Reflection1.Checked;

  with MakerGallery.Options.Floor do
  if Reflection1.Checked then Reflection:=35
                         else Reflection:=0;
end;

procedure TBlockGallery.Boundingbox1Click(Sender: TObject);
begin
  Boundingbox1.Checked:=not Boundingbox1.Checked;
  MakerGallery.Options.BoundingBox:=Boundingbox1.Checked;
end;

procedure TBlockGallery.Axes1Click(Sender: TObject);
begin
  Axes1.Checked:=not Axes1.Checked;
  MakerGallery.Options.View3DAxes:=Axes1.Checked;
end;

procedure TBlockGallery.BitBtn1Click(Sender: TObject);
begin
  with BitBtn1.ClientToScreen(BitBtn1.ClientRect.BottomRight) do
       PopupMenu1.Popup(X,Y);
end;

procedure TBlockGallery.Texture1Click(Sender: TObject);
begin
  Texture1.Checked:=not Texture1.Checked;

  if PageControl1.Visible then
  begin
    LastIndex:=-1;
    ListBlocksClick(Self);
  end
  else
     MakerGallery.Blocks.HideTextures:=not Texture1.Checked;
end;

procedure TBlockGallery.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if WindowState=wsMaximized then
     TeeMakerWriteRegistry(GalleryKey,'GalleryMax','TRUE')
  else
  begin
    TeeMakerWriteRegistry(GalleryKey,'GalleryMax','');
    TeeMakerWriteRegistry(GalleryKey,'GalleryWidth',TeeStr(Width));
    TeeMakerWriteRegistry(GalleryKey,'GalleryHeight',TeeStr(Height));
  end;
end;

procedure TBlockGallery.FormShow(Sender: TObject);
var tmp : TObject;
begin
  if PageControl1.Visible then
  begin
    if Assigned(Current) then
    begin
      tmp:=TObject(BlockClasses.IndexOf(Current));
      ListBlocks.ItemIndex:=ListBlocks.Items.IndexOfObject(tmp);
    end;

    if ListBlocks.ItemIndex=-1 then
       if ListBlocks.Items.Count>0 then
          ListBlocks.ItemIndex:=0;

    if ListBlocks.ItemIndex<>-1 then
       ListBlocksClick(Self);

    if ListBlocks.CanFocus then
       ListBlocks.SetFocus;
  end;
end;

procedure TBlockGallery.Borders1Click(Sender: TObject);
begin
  Borders1.Checked:=not Borders1.Checked;
  MakerGallery.Blocks.HideBorders:=not Borders1.Checked;
end;

procedure TBlockGallery.LibraryOpenObject(Sender:TObject);
begin
  LibrarySelectNode(Self,TTreeNode(Sender));
  ModalResult:=mrOk;
end;

function TBlockGallery.CreateNewObjectBlock(ABlockOwner:TComponent):TCustomObjectBlock;
begin
  if ILibrary.SelectedIsMaker then
     result:=TObjectBlock.Create(ABlockOwner)
  else
     result:=TObjBlock.Create(ABlockOwner);

  result.LinkFile:=ILibrary.SelectedLinkFile;
end;

procedure TBlockGallery.LibrarySelectNode(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and ILibrary.NodeIsFile(Node) and (not ILibrary.IsBasicNode(Node)) then
  begin
    PrepareForGallery(NewBlock);
    BOK.Enabled:=True;
  end;
end;

procedure TBlockGallery.CheckLibrary;
begin
   if not Assigned(ILibrary) then
   begin
     ILibrary:=TMakerLibrary.Create(Self);
     ILibrary.Align:=alClient;

     ILibrary.TreeObjects.Images:=Images;

     ILibrary.GroupTextures.Hide;
     ILibrary.GroupObjects.Caption:='';

     ILibrary.OnOpenObject:=LibraryOpenObject;

     TTeeVCL.AddFormTo(ILibrary,TabLibrary);

     ILibrary.LibraryPath:=MakerGallery.Blocks.LibraryPath;

     ILibrary.TreeObjects.OnChange:=LibrarySelectNode;

     ILibrary.TryFillTrees(False);
   end;
end;

procedure TBlockGallery.PageControl1Change(Sender: TObject);

  procedure AddBlock(ParentNode:TTreeNode; ABlock:TCustomBlock);
  var t : Integer;
  begin
    if ABlock<>CurrentBlock then
    begin
      ParentNode:=TreeBlocks.Items.AddChildObject(ParentNode,
                                     TBlockAccess(ABlock).TitleOrName,ABlock);

      TBlockAccess(ABlock).IData:=ParentNode;

      if ABlock is TCustomObjectBlock then
      with TCustomObjectBlockAccess(ABlock) do
           if LinkFile='' then
              for t:=0 to Items.Count-1 do
                  AddBlock(ParentNode,Item[t]);
    end;
  end;

  procedure FillTreeBlocks;
  var t : Integer;
  begin
    with CurrentBlocks do
    for t:=0 to Count-1 do
        AddBlock(nil,Block[t]);
  end;

begin
  if PageControl1.ActivePage=TabLibrary then
     CheckLibrary
  else
  if PageControl1.ActivePage=TabBlocks then
  begin
    if TreeBlocks.Items.Count=0 then
       FillTreeBlocks;
  end;

  BOk.Enabled:=(PageControl1.ActivePage=TabBasic) or
               ((PageControl1.ActivePage=TabLibrary) and Assigned(ILibrary.TreeObjects.Selected)) or
               ((PageControl1.ActivePage=TabBlocks) and Assigned(TreeBlocks.Selected));
end;

procedure TBlockGallery.Floor1Click(Sender: TObject);
begin
  Floor1.Checked:=not Floor1.Checked;
  MakerGallery.Options.Floor.Visible:=Floor1.Checked;
end;

procedure TBlockGallery.Gradient1Click(Sender: TObject);
begin
  Gradient1.Checked:=not Gradient1.Checked;
  MakerGallery.Gradient.Visible:=Gradient1.Checked;
end;

function ChangeBlock(AOwner,AVisualOwner:TComponent; AVisual:TVisualBlock):TVisualBlock;
var tmpClass : TBlockClass;
begin
  if Assigned(AVisual) then
     tmpClass:=TBlockClass(AVisual.ClassType)
  else
     tmpClass:=nil;

  result:=TBlockGallery.ModalShow(AOwner,AVisualOwner,tmpClass);
end;

procedure TBlockGallery.Antialias1Click(Sender: TObject);
begin
  Antialias1.Checked:=not Antialias1.Checked;
  MakerGallery.Render.Antialias:=Antialias1.Checked;
end;

class function TBlockGallery.ChooseBlock(AOwner:TComponent; ABlocks:TBlocks;
                                         ABlock:TCustomBlock=nil):TCustomBlock;
begin
  with TBlockGallery.Create(AOwner) do
  try
    TabBasic.TabVisible:=False;
    TabLibrary.TabVisible:=False;
    TabBlocks.TabVisible:=True;

    CurrentBlocks:=ABlocks;
    CurrentBlock:=ABlock;

    PageControl1.ActivePage:=TabBlocks;
    PageControl1Change(nil);

    if ShowModal=mrOk then
       result:=TCustomBlock(TreeBlocks.Selected.Data)
    else
       result:=nil;
  finally
    Free;
  end;
end;

class function TBlockGallery.ChooseObject(AOwner: TComponent): String;
begin
  with TBlockGallery.Create(AOwner) do
  try
    TabBasic.TabVisible:=False;
    PageControl1.ActivePage:=TabLibrary;
    PageControl1Change(nil);

    if ShowModal=mrOk then
       result:=ILibrary.SelectedLinkFile
    else
       result:='';
  finally
    Free;
  end;
end;

procedure TBlockGallery.TreeBlocksDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TBlockGallery.TreeBlocksChange(Sender: TObject; Node: TTreeNode);
begin
  BOK.Enabled:=Assigned(TreeBlocks.Selected);

  if BOK.Enabled then
     NewBlock;
end;

procedure TBlockGallery.Button1Click(Sender: TObject);
var tmp : TBlockClass;
begin
  tmp:=TBlockChooser.Choose;

  if Assigned(tmp) then
  begin
    Current:=tmp;
    FormShow(Self);
  end;
end;

initialization
  TeeOnChangeVisual:=ChangeBlock;
finalization
  TeeOnChangeVisual:=nil;
end.
