{**********************************************}
{  Texture Selector dialog                     }
{  Copyright (c) 2007-2025 by Steema Software  }
{**********************************************}
unit TeeTextureSelector;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  {$IFNDEF D15}
  FileCtrl,
  {$ENDIF}
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QComCtrls,
  QImgList,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ImgList,
  {$ENDIF}
  TeeMakerLibrary, TeePenDlg, TeCanvas, TeeBlocks, TeeProcs, TeeMakerControl;

type
  TTextureSelector = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelList: TPanel;
    ListTextures: TListBox;
    SplitterList: TSplitter;
    LabelSize: TLabel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    LabelFileName: TLabel;
    LabelFileSize: TLabel;
    Images: TImageList;
    CBEmbedd: TCheckBox;
    Panel6: TPanel;
    procedure ListTexturesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListTexturesDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListTexturesEnter(Sender: TObject);
    procedure CBStretchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Preview  : TMaker;

    IBlocks  : TBlocks;
    IFormat  : TBlockFormat;
    ILibrary : TMakerLibrary;
    IFolder  : String;

    procedure PreviewMouseUp(Sender: TObject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
                             
    function PreviewTexture:TBlockTexture;
    function SelectedPicture:TPicture;
    procedure ShowPicParams(const FileName:String);
    procedure TreeChanged(Sender: TObject; Node: TTreeNode);
    procedure TreeEnter(Sender: TObject);
  public
    { Public declarations }
    procedure Fill(APath:String); overload;
    procedure Fill(Blocks:TBlocks; AFormat:TBlockFormat; Components:Boolean=True); overload;

    class function ModalShow(AOwner:TComponent; AParent:TBlocks;
                             AFormat:TBlockFormat):Boolean; overload;

    class function ModalShow(AOwner:TComponent; APicFile:String):Boolean; overload;
    class function ModalShow(AOwner:TComponent; AGraphic:TGraphic):Boolean; overload;
  end;

function FileSize(const FileName:String):Int64;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeURL, TeeBackImage, TeeMakerConst, TeeMakerEditor;

class function TTextureSelector.ModalShow(AOwner:TComponent; APicFile:String):Boolean;
begin
  with TTextureSelector.Create(AOwner) do
  try
    if {$IFDEF D15}SysUtils.{$ENDIF}DirectoryExists(APicFile) then
    begin
      IFolder:=APicFile;
      Fill(IFolder);
      ListTexturesClick(nil);
    end
    else
    begin
      PanelList.Visible:=False;

      if APicFile<>'' then
         APicFile:=TBlocks.ParseFileName(TeeMsg_TexturesLibrary,APicFile);

      PreviewTexture.PictureLink:=APicFile;
      ShowPicParams(APicFile);
    end;

    SplitterList.Visible:=False;

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

class function TTextureSelector.ModalShow(AOwner:TComponent; AParent:TBlocks; AFormat:TBlockFormat):Boolean;
begin
  with TTextureSelector.Create(AOwner) do
  try
    Fill(AParent,AFormat);
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

function IsPictureFile(const AFilter,AFileName:String):Boolean;
begin
  result:=Pos('*'+UpperCase(ExtractFileExt(AFileName)),AFilter)>0;
end;

procedure TTextureSelector.Fill(APath:String);
var f : TSearchRec;
    tmpFilter : String;
begin
  ListTextures.Items.Clear;

  if FindFirst(APath+'\*.*',faAnyFile,f)=0 then
  begin
   tmpFilter:=UpperCase(GraphicFilter(TGraphic));

   Repeat
     if (f.Attr and faDirectory)=faDirectory then
     else
     if IsPictureFile(tmpFilter,f.Name) then
        ListTextures.Items.Add(f.Name);

    Until FindNext(f)<>0;

    FindClose(f);

    if ListTextures.Items.Count>0 then
       ListTextures.ItemIndex:=0;
  end;
end;

procedure TTextureSelector.Fill(Blocks:TBlocks; AFormat:TBlockFormat; Components:Boolean=True);

  procedure AddTextures(ABlocks:TBlocks);
  var t : Integer;
      tmp : TPicture;
      tmpItems : TBlocks;
      tmpTitle : String;
  begin
    with ABlocks do
    for t:=0 to Count-1 do
    begin
      if Block[t] is TCustomObjectBlock then
         tmpItems:=TCustomObjectBlock(Block[t]).Items
      else
         tmpItems:=nil;

      if Assigned(tmpItems) and (tmpItems.Count>0) then
         AddTextures(tmpItems)
      else
      begin
        tmp:=Block[t].Format.Texture.Picture;

        if Assigned(tmp) then
        begin
          if tmp.Graphic<>nil then
             if ListTextures.Items.IndexOfObject(tmp)=-1 then
             begin
               tmpTitle:=Block[t].Title;

               if tmpTitle='' then
                  tmpTitle:=Block[t].ClassName;

               ListTextures.Items.AddObject(tmpTitle,tmp);
             end;
        end;
      end;
    end;

    if Components and
       Assigned(Blocks.Owner) and Assigned(Blocks.Owner.Owner) and
       (not (csDesigning in Blocks.Owner.Owner.ComponentState)) then
    with Blocks.Owner.Owner do
    begin
      for t:=0 to ComponentCount-1 do
      begin
        if Components[t] is TImage then
        begin
          tmp:=TImage(Components[t]).Picture;

          if Assigned(tmp) then
          begin
            if tmp.Graphic<>nil then
               if ListTextures.Items.IndexOfObject(tmp)=-1 then
               begin
                 tmpTitle:=Components[t].Name;

                 if tmpTitle='' then
                    tmpTitle:=Components[t].ClassName;

                 ListTextures.Items.AddObject(tmpTitle,tmp);
               end;
          end;
        end;
      end;
    end;
  end;

begin
  IFormat:=AFormat;
  IBlocks:=Blocks;

  if Assigned(Blocks) then
  begin
    ListTextures.Sorted:=False;

    AddTextures(Blocks.DrawBlocks);

    ListTextures.Sorted:=True;

    with IFormat.Texture do
    if Picture<>nil then
       if Picture.Graphic<>nil then
          ListTextures.ItemIndex:=ListTextures.Items.IndexOfObject(Picture.Graphic);

    ListTexturesClick(Self);
  end
  else
  begin
    ListTextures.Hide;
    SplitterList.Hide;
  end;
end;

function TTextureSelector.SelectedPicture:TPicture;
begin
  if PanelList.Visible then
     result:=TPicture(ListTextures.Items.Objects[ListTextures.ItemIndex])
  else
     result:=PreviewTexture.Picture;
end;

type
  TBlockPictureAccess=class(TBlockPicture);

procedure TTextureSelector.ListTexturesClick(Sender: TObject);
var tmp     : TPicture;
    tmpFile : String;
begin
  if ListTextures.ItemIndex=-1 then
  begin
    PreviewTexture.Picture:=nil;

    CBEmbedd.Visible:=False;

    LabelSize.Caption:='';
    LabelFileName.Caption:='';
    LabelFileSize.Caption:='';
  end
  else
  begin
    if IFolder<>'' then
    begin
      tmpFile:=IFolder+'\'+ListTextures.Items[ListTextures.ItemIndex];

      PreviewTexture.PictureLink:=tmpFile;
    end
    else
    begin
      CBEmbedd.Visible:=True;

      tmp:=Self.SelectedPicture;

      if tmp is TBlockPicture then
         tmpFile:=TBlockPictureAccess(tmp).LoadedSource
      else
         tmpFile:='';

      if tmpFile='' then
      begin
        CBEmbedd.Checked:=True;
        CBEmbedd.Enabled:=False;
      end
      else
        CBEmbedd.Enabled:=True;

      PreviewTexture.AssignPicture(tmp);
    end;

    ShowPicParams(tmpFile);
  end;

  Preview.Invalidate;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

procedure TTextureSelector.Button1Click(Sender: TObject);
var tmp     : TBlockPicture;
    tmpFile : String;
begin
  if Assigned(IFormat) then
  begin
    if ListTextures.ItemIndex<>-1 then
    with IFormat.Texture do
    begin
      IBlocks.RemoveTexture(Picture);

      tmp:=TBlockPicture(ListTextures.Items.Objects[ListTextures.ItemIndex]);

      if CBEmbedd.Checked then
         Picture:=tmp
      else
      begin
        tmpFile:=TBlockPictureAccess(tmp).LoadedSource;
        TBlocks.CheckLibraryPath(TeeMsg_ObjectsLibrary,tmpFile);
        PictureLink:=tmpFile;
      end;
    end
    else
    if Assigned(ILibrary) and (ILibrary.TreeTextures.Selected<>nil) then
       IFormat.Texture.PictureLink:=ILibrary.SelectedTexture;
  end;

  ModalResult:=mrOk;
end;

procedure TTextureSelector.ListTexturesDblClick(Sender: TObject);
begin
  Button1Click(Self);
end;

function FileSize(const FileName:String):Int64;
var f: TFileStream;
begin
  f:=TFileStream.Create(FileName,fmOpenRead);
  try
    result:=f.Size;
  finally
    f.Free;
  end;
end;

procedure TTextureSelector.ShowPicParams(const FileName:String);

  function FormatFileSize(const ASize:Int64):String;
  begin
    if ASize>1024*1024 then
       result:=FormatFloat('0.##',ASize/(1024*1024))+' MB'
    else
    if ASize>1024 then
       result:=FormatFloat('0.##',ASize/1024)+' KB'
    else
       result:=IntToStr(ASize)+' Bytes';
  end;

  function GraphicSize(AGraphic:TGraphic):Int64;
  var tmp : TMemoryStream;
  begin
    tmp:=TMemoryStream.Create;
    try
      AGraphic.SaveToStream(tmp);
      result:=tmp.Size;
    finally
      tmp.Free;
    end;
  end;

begin
  with PreviewTexture.Picture.Graphic do
       LabelSize.Caption:=IntToStr(Width)+'x'+IntToStr(Height);

  if FileName='' then
  begin
    LabelFileName.Caption:='('+SelectedPicture.Graphic.ClassName+')';
    LabelFileSize.Caption:=FormatFileSize(GraphicSize(SelectedPicture.Graphic));
  end
  else
  begin
    if TeeIsURL(FileName) then
    begin
      LabelFileName.Caption:=FileName;
      LabelFileSize.Caption:=FormatFileSize(GraphicSize(SelectedPicture.Graphic));
    end
    else
    begin
      LabelFileName.Caption:=ExtractFileName(FileName);
      LabelFileSize.Caption:=FormatFileSize(FileSize(FileName));
    end;
  end;
end;

procedure TTextureSelector.TreeChanged(Sender: TObject; Node: TTreeNode);
var tmp : String;
begin
  if Assigned(Node) and ILibrary.NodeIsFile(Node) then
  begin
    tmp:=ILibrary.SelectedTexture;

    if tmp<>'' then
    begin
      PreviewTexture.PictureLink:=tmp;

      CBEmbedd.Visible:=True;

      ShowPicParams(tmp);
    end;
  end;
end;

procedure TTextureSelector.FormShow(Sender: TObject);
begin
  {$IFNDEF CLX}
  Panel1.DoubleBuffered:=True;
  {$ENDIF}

  if SplitterList.Visible then
  begin
    ILibrary:=TMakerLibrary.Create(Self);

    with ILibrary do
    begin
      TreeObjects.Images:=Self.Images;
      TreeTextures.Images:=Self.Images;

      GroupTextures.Caption:='Library';
      GroupTextures.Parent:=Self.PanelList;

      if Assigned(Self.IBlocks) then
         LibraryPath:=Self.IBlocks.LibraryPath;

      FillTextures;

      TreeTextures.OnChange:=Self.TreeChanged;
      TreeTextures.OnEnter:=Self.TreeEnter;

      if not ListTextures.Visible then
         GroupTextures.Align:=alClient;

      if Assigned(IFormat) then
         if IFormat.Texture.PictureLink<>'' then
            ILibrary.SelectTextureFile(IFormat.Texture.PictureLink);
    end;
  end;
end;

procedure TTextureSelector.TreeEnter(Sender: TObject);
begin
  ListTextures.ItemIndex:=-1;
  TreeChanged(Self,ILibrary.TreeTextures.Selected);
end;

procedure TTextureSelector.ListTexturesEnter(Sender: TObject);
begin
  ListTexturesClick(Self);
end;

procedure TTextureSelector.CBStretchClick(Sender: TObject);
begin
  Preview.Invalidate;
end;

procedure TTextureSelector.FormCreate(Sender: TObject);
begin
  Preview:=TMaker.Create(Self);
  Preview.Align:=alClient;
  Preview.Parent:=Panel1;
  Preview.Options.Floor.Visible:=False;

  Preview.Blocks.Add(TRectangleBlock.Create(Self));
  Preview.OnMouseUp:=PreviewMouseUp;
end;

function TTextureSelector.PreviewTexture:TBlockTexture;
begin
  result:=Preview.Blocks[0].Format.Texture;
end;

class function TTextureSelector.ModalShow(AOwner: TComponent;
  AGraphic: TGraphic): Boolean;
begin
  with TTextureSelector.Create(AOwner) do
  try
    PreviewTexture.Picture.Graphic:=AGraphic;
    SplitterList.Visible:=False;
    PanelList.Visible:=False;

    ShowPicParams('');
    
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TTextureSelector.PreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssAlt in Shift) and (Button=mbRight) then
     TMakerEditor.ModalShow(Self,Preview);
end;

end.
