unit TeeBlockFormat;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QButtons, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs, TeePenDlg, TeeBlocks;

type
  TBlockFormatEditor = class(TVisualEditor)
    PageControl1: TPageControl;
    TabFormat: TTabSheet;
    TabTexture: TTabSheet;
    Label10: TLabel;
    Label28: TLabel;
    BlockColor: TButtonColor;
    BlockTransp: TScrollBar;
    TBShininess: TTrackBar;
    BlockSolid: TCheckBox;
    PageTexture: TPageControl;
    TabSheet1: TTabSheet;
    SBTextureSelect: TSpeedButton;
    Label18: TLabel;
    SBLoadExternal: TSpeedButton;
    BLoadPic: TButton;
    BSavePicture: TButton;
    BlockPictureLink: TComboFlat;
    TabTextureFormat: TTabSheet;
    CBImageTransp: TCheckBox;
    RGTranspMode: TRadioGroup;
    BTranspColor: TButtonColor;
    PanelButtons: TPanel;
    BOK: TButton;
    BCancel: TButton;
    CBImageAlpha: TCheckBox;
    TabSheet3: TTabSheet;
    Label79: TLabel;
    BlockTextureRotate: TScrollBar;
    LTextureRotate: TLabel;
    GroupBox1: TGroupBox;
    Label30: TLabel;
    Label31: TLabel;
    BlockTextureRepeatX: TEdit;
    BlockTextureRepeatY: TEdit;
    TabSheet4: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BlockTextureTransX: TScrollBar;
    BlockTextureTransY: TScrollBar;
    BlockTextureTransZ: TScrollBar;
    TabSheet5: TTabSheet;
    BlockBorderVisible: TCheckBox;
    Label4: TLabel;
    BlockBorderColor: TButtonColor;
    BlockBorderStyle: TComboFlat;
    Label5: TLabel;
    BlockBorderTransp: TScrollBar;
    Label6: TLabel;
    BlockBorderWidth: TEdit;
    UDBorderWidth: TUpDown;
    BEmbeddPic: TButton;
    LTextureX: TLabel;
    LTextureZ: TLabel;
    LTextureY: TLabel;
    CBAlphaInvert: TCheckBox;
    BlockColorDefault: TCheckBox;
    Label7: TLabel;
    BlockTextureRepeatZ: TEdit;
    BlockVisibleInterior: TCheckBox;
    BlockBright: TCheckBox;
    LTransp: TLabel;
    CBParentTexture: TCheckBox;
    procedure BlockTranspChange(Sender: TObject);
    procedure TBShininessChange(Sender: TObject);
    procedure CBImageTranspClick(Sender: TObject);
    procedure RGTranspModeClick(Sender: TObject);
    procedure BTranspColorClick(Sender: TObject);
    procedure SBTextureSelectClick(Sender: TObject);
    procedure BlockPictureLinkChange(Sender: TObject);
    procedure BLoadPicClick(Sender: TObject);
    procedure BlockSolidClick(Sender: TObject);
    procedure SBLoadExternalClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BlockTextureRepeatXChange(Sender: TObject);
    procedure BlockTextureRepeatYChange(Sender: TObject);
    procedure BlockTextureRotateChange(Sender: TObject);
    procedure CBImageAlphaClick(Sender: TObject);
    procedure BlockTextureTransXChange(Sender: TObject);
    procedure BlockTextureTransYChange(Sender: TObject);
    procedure BlockTextureTransZChange(Sender: TObject);
    procedure BlockBorderVisibleClick(Sender: TObject);
    procedure BlockBorderStyleChange(Sender: TObject);
    procedure BlockBorderTranspChange(Sender: TObject);
    procedure BlockBorderWidthChange(Sender: TObject);
    procedure BlockBorderColorClick(Sender: TObject);
    procedure BEmbeddPicClick(Sender: TObject);
    procedure PageTextureChange(Sender: TObject);
    procedure CBAlphaInvertClick(Sender: TObject);
    procedure BlockColorClick(Sender: TObject);
    procedure BlockColorDefaultClick(Sender: TObject);
    procedure BlockTextureRepeatZChange(Sender: TObject);
    procedure BlockVisibleInteriorClick(Sender: TObject);
    procedure BlockBrightClick(Sender: TObject);
    procedure BSavePictureClick(Sender: TObject);
    procedure CBParentTextureClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    IFormat    : TBlockFormat;
    IModifying : Boolean;
    ITextureDone : Boolean;

    procedure CheckTransparentControls;
    function Current:TCustomBlock;
    procedure SetPictureLink;
  public
    { Public declarations }
    class function FromScaleValue(const Value:Double):Integer;
    procedure RefreshFormat(AFormat:TBlockFormat);
    class function ToScaleValue(const Value:Integer):Double;

    class function ModalShow(AOwner:TComponent; AFormat:TBlockFormat):Boolean;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeTextureSelector, TeeBrushDlg, TeeMakerConst;

type
  TBlockFormatAccess=class(TBlockFormat);
  TBlockTextureAccess=class(TBlockTexture);

function TBlockFormatEditor.Current:TCustomBlock;
begin
  result:=TBlockFormatAccess(IFormat).IOwner;
end;

procedure TBlockFormatEditor.RefreshFormat(AFormat:TBlockFormat);
begin
  IFormat:=AFormat;

  IModifying:=True;

  with IFormat do
  begin
    // General

    BlockSolid.Checked:=IFormat.Solid;
    BlockColor.LinkProperty(IFormat,'Color'); // Do not localize
    BlockColorDefault.Checked:=IFormat.Color=clDefault;
    BlockVisibleInterior.Checked:=IFormat.VisibleInterior;
    BlockBright.Checked:=IFormat.Bright;
    BlockTransp.Position:=IFormat.Transparency;

    // Border

    BlockBorderColor.LinkProperty(IFormat.Border,'Color'); // Do not localize
    UDBorderWidth.Position:=IFormat.Border.Width;
    BlockBorderVisible.Checked:=IFormat.Border.Visible;
    BlockBorderStyle.ItemIndex:=Ord(IFormat.Border.Style);
    BlockBorderTransp.Position:=IFormat.Border.Transparency;

    // Texture
    ITextureDone:=False;

    TBShininess.Position:=IFormat.Shininess;
  end;

  IModifying:=False;
end;

procedure TBlockFormatEditor.CheckTransparentControls;
var tmpFilt : TGraphic;
begin
  CBImageTransp.Enabled:=TBlockTextureAccess(IFormat.Texture).HasTexture;
  CBImageTransp.Checked:=CBImageTransp.Enabled and IFormat.Texture.PictureTransparent;

  if CBImageTransp.Enabled then
     tmpFilt:=IFormat.Texture.Picture.Graphic
  else
     tmpFilt:=nil;

  RGTranspMode.Enabled:=CBImageTransp.Checked and (tmpFilt is TBitmap);

  if tmpFilt is TBitmap then
  begin
    RGTranspMode.ItemIndex:=Ord(TBitmap(tmpFilt).TransparentMode);
    BTranspColor.Enabled:=RGTranspMode.ItemIndex=1;
    BTranspColor.LinkProperty(TBitmap(tmpFilt),'TransparentColor'); // Do not localize
  end
  else
  begin
    BTranspColor.Enabled:=False;
    BTranspColor.LinkProperty(nil,'');
  end;

  CBImageAlpha.Checked:=IFormat.Texture.PictureAlpha;
  CBAlphaInvert.Enabled:=CBImageAlpha.Checked;

  if CBAlphaInvert.Enabled then
     CBAlphaInvert.Checked:=IFormat.Texture.AlphaInvert;

  BSavePicture.Enabled:=TBlockTextureAccess(IFormat.Texture).HasTexture and (tmpFilt<>nil);
end;

procedure TBlockFormatEditor.BlockTranspChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Transparency:=BlockTransp.Position;
    LTransp.Caption:=IntToStr(IFormat.Transparency);
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.TBShininessChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Shininess:=TBShininess.Position;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.CBImageTranspClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Texture.PictureTransparent:=CBImageTransp.Checked;
    CheckTransparentControls;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.RGTranspModeClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if IFormat.Texture.Picture.Graphic is TBitmap then
    begin
      TBitmap(IFormat.Texture.Picture.Graphic).TransparentMode:=TTransparentMode(RGTranspMode.ItemIndex);
      CheckTransparentControls;

      Current.Parent.RemoveTexture(IFormat.Texture.Picture);

      MarkDirty;
    end;
  end;
end;

procedure TBlockFormatEditor.BTranspColorClick(Sender: TObject);
var tmp : TGraphic;
begin
  tmp:=IFormat.Texture.Picture.Graphic;

  if tmp is TBitmap then
  begin
    TBitmap(tmp).TransparentMode:=tmFixed;
    TBitmap(tmp).TransparentColor:=BTranspColor.SymbolColor;
  end;

  Current.Parent.RemoveTexture(IFormat.Texture.Picture);

  MarkDirty;
end;

procedure TBlockFormatEditor.SetPictureLink;
begin
  BlockPictureLink.Text:=IFormat.Texture.PictureLink;

  if TBlockTextureAccess(IFormat.Texture).HasTexture then
     BLoadPic.Caption:=TeeMsg_ClearCaption
  else
     BLoadPic.Caption:=TeeMsg_LoadCaption;

  BEmbeddPic.Enabled:=TBlockTextureAccess(IFormat.Texture).HasTexture and
                      (IFormat.Texture.PictureLink<>'');
end;

procedure TBlockFormatEditor.SBTextureSelectClick(Sender: TObject);
begin
  if TTextureSelector.ModalShow(Self,Current.Parent,IFormat) then
  begin
    {if BLoadPic.Caption=TeeMsg_ClearCaption then
       BLoadPicClick(Self);}

    SetPictureLink;
    MarkDirty;
//    BLoadPicClick(Self);
  end;
end;

procedure TBlockFormatEditor.BlockPictureLinkChange(Sender: TObject);
begin
  if not IModifying then
     BLoadPic.Caption:=TeeMsg_LoadCaption;
end;

procedure TBlockFormatEditor.BLoadPicClick(Sender: TObject);

  procedure AddToCombo;
  begin
    with IFormat.Texture do
    if PictureLink<>'' then
       if BlockPictureLink.Items.IndexOf(PictureLink)=-1 then
          BlockPictureLink.Items.Add(PictureLink);
  end;

begin
  AddToCombo;

  if BLoadPic.Caption=TeeMsg_ClearCaption then
  begin
    IFormat.Texture.PictureLink:='';
    IFormat.Texture.Picture:=nil;
    BLoadPic.Caption:=TeeMsg_LoadCaption;
  end
  else
  begin
    IFormat.Texture.PictureLink:=BlockPictureLink.Text;

    if IFormat.Texture.PictureLink<>'' then
    begin
      BLoadPic.Caption:=TeeMsg_ClearCaption;

      with IFormat.Texture do
      if Assigned(Picture.Graphic) then
      begin
        PictureTransparent:=Picture.Graphic.Transparent;
        CBImageTransp.Checked:=PictureTransparent;
      end;
    end;
  end;

  BEmbeddPic.Enabled:=Assigned(IFormat.Texture.Picture) and (IFormat.Texture.PictureLink<>'');

  MarkDirty;
end;

procedure TBlockFormatEditor.BlockSolidClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Solid:=BlockSolid.Checked;
    BlockColor.Enabled:=BlockSolid.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.SBLoadExternalClick(Sender: TObject);
var tmp : String;
begin
  tmp:=TeeGetPictureFileName(Self);

  if tmp<>'' then
  begin
    TBlocks.CheckLibraryPath(TeeMsg_TexturesLibrary,tmp);
    BlockPictureLink.Text:=tmp;

    // Twice:
    BLoadPicClick(Self);
    BLoadPicClick(Self);
  end;
end;

procedure TBlockFormatEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  PageTexture.ActivePageIndex:=0;
end;

procedure TBlockFormatEditor.BlockTextureRepeatXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with IFormat.Texture.Scale do
         X:=StrToFloatDef(BlockTextureRepeatX.Text,X);

    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockTextureRepeatYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with IFormat.Texture.Scale do
         Y:=StrToFloatDef(BlockTextureRepeatY.Text,Y);

    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockTextureRotateChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Texture.Rotation:=BlockTextureRotate.Position;
    LTextureRotate.Caption:=IntToStr(BlockTextureRotate.Position);
    MarkDirty;
  end;
end;

class function TBlockFormatEditor.ModalShow(AOwner:TComponent; AFormat:TBlockFormat):Boolean;
begin
  with TBlockFormatEditor.Create(AOwner) do
  try
    PanelButtons.Visible:=True;

    RefreshFormat(AFormat);

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

procedure TBlockFormatEditor.CBImageAlphaClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Texture.PictureAlpha:=CBImageAlpha.Checked;
    CBAlphaInvert.Enabled:=CBImageAlpha.Checked;
    MarkDirty;
  end;
end;

class function TBlockFormatEditor.FromScaleValue(
  const Value: Double): Integer;
begin
  if Value=1 then
     result:=0
  else
  if Value>1 then
     result:=Round(Value*100)
  else
     result:=-Round((1.001-Value)*1000);
end;

class function TBlockFormatEditor.ToScaleValue(
  const Value: Integer): Double;
begin
  if Value=0 then
     result:=1
  else
  if Value>0 then
     result:=1+(Value*0.01)
  else
     result:=1.001+(Value*0.001);
end;

procedure TBlockFormatEditor.BlockTextureTransXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Format.Texture.Translate.X:=BlockTextureTransX.Position*0.001;
    LTextureX.Caption:=FormatFloat(TeeMsg_DefaultFormat,Current.Format.Texture.Translate.X);
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockTextureTransYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Format.Texture.Translate.Y:=BlockTextureTransY.Position*0.001;
    LTextureY.Caption:=FormatFloat(TeeMsg_DefaultFormat,Current.Format.Texture.Translate.Y);
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockTextureTransZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Format.Texture.Translate.Z:=BlockTextureTransZ.Position*0.001;
    LTextureZ.Caption:=FormatFloat(TeeMsg_DefaultFormat,Current.Format.Texture.Translate.Z);
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBorderVisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Border.Visible:=BlockBorderVisible.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBorderStyleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Border.Style:=TPenStyle(BlockBorderStyle.ItemIndex);
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBorderTranspChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Border.Transparency:=BlockBorderTransp.Position;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBorderWidthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Border.Width:=UDBorderWidth.Position;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBorderColorClick(Sender: TObject);
begin
  MarkDirty;
end;

procedure TBlockFormatEditor.BEmbeddPicClick(Sender: TObject);
begin
  TBlockTextureAccess(IFormat.Texture).SetEmbeddedPicture;
  BEmbeddPic.Enabled:=False;
  BlockPictureLink.Text:='';

  MarkDirty;
end;

procedure TBlockFormatEditor.PageTextureChange(Sender: TObject);
begin
  if PageTexture.ActivePage=TabTextureFormat then
  begin
    IModifying:=True;
    CheckTransparentControls;
    IModifying:=False;
  end;
end;

procedure TBlockFormatEditor.CBAlphaInvertClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Texture.AlphaInvert:=CBAlphaInvert.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockColorClick(Sender: TObject);
begin
  BlockColorDefault.Checked:=False;
  MarkDirty;
end;

procedure TBlockFormatEditor.BlockColorDefaultClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if BlockColorDefault.Checked then
       IFormat.Color:=clDefault
    else
       IFormat.Color:=TBlockFormatAccess(IFormat).GetRealColor;

    BlockColor.Invalidate;

    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockTextureRepeatZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with IFormat.Texture.Scale do
         Z:=StrToFloatDef(BlockTextureRepeatZ.Text,Z);

    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockVisibleInteriorClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.VisibleInterior:=BlockVisibleInterior.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BlockBrightClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.Bright:=BlockBright.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.BSavePictureClick(Sender: TObject);
begin
  TTeeVCL.SavePictureDialog(Self,IFormat.Texture.Picture.Graphic);
end;

procedure TBlockFormatEditor.CBParentTextureClick(Sender: TObject);
begin
  if not IModifying then
  begin
    IFormat.ParentTexture:=CBParentTexture.Checked;
    MarkDirty;
  end;
end;

procedure TBlockFormatEditor.PageControl1Change(Sender: TObject);

  procedure AddExternalTextures(ACombo:TComboFlat);

    procedure AddTextures(const ABlocks:TBlocks);
    var t : Integer;
        tmp : String;
    begin
      with ABlocks do
      for t:=0 to Count-1 do
      begin
        tmp:=Block[t].Format.Texture.PictureLink;

        if tmp<>'' then
           if ACombo.Items.IndexOf(tmp)=-1 then
              ACombo.Add(tmp);

        if Block[t] is TCustomObjectBlock then
           AddTextures(TCustomObjectBlock(Block[t]).Items);
      end;
    end;

  begin
    ACombo.Items.BeginUpdate;

    try
      ACombo.Clear;

      if Assigned(Current.Parent) then
         AddTextures(Current.Parent);
    finally
      ACombo.Items.EndUpdate;
      ACombo.Sorted:=True;
    end;
  end;

  procedure RefreshTexture;
  begin
    IModifying:=True;

    AddExternalTextures(BlockPictureLink);
    SetPictureLink;

    if Assigned(TBlockTextureAccess(IFormat.Texture).FScale) then
    begin
      BlockTextureRepeatX.Text:=FloatToStr(IFormat.Texture.Scale.X);
      BlockTextureRepeatY.Text:=FloatToStr(IFormat.Texture.Scale.Y);
      BlockTextureRepeatZ.Text:=FloatToStr(IFormat.Texture.Scale.Z);
    end
    else
    begin
      BlockTextureRepeatX.Text:='1';
      BlockTextureRepeatY.Text:='1';
      BlockTextureRepeatZ.Text:='1';
    end;

    CBParentTexture.Checked:=IFormat.ParentTexture;

    BlockTextureRotate.Position:=Round(IFormat.Texture.Rotation);
    LTextureRotate.Caption:=IntToStr(BlockTextureRotate.Position);

    if Assigned(TBlockTextureAccess(IFormat.Texture).FTranslate) then
    with IFormat.Texture.Translate do
    begin
      BlockTextureTransX.Position:=Round(X*1000);
      BlockTextureTransY.Position:=Round(Y*1000);
      BlockTextureTransZ.Position:=Round(Z*1000);

      LTextureX.Caption:=FormatFloat(TeeMsg_DefaultFormat,X);
      LTextureY.Caption:=FormatFloat(TeeMsg_DefaultFormat,Y);
      LTextureZ.Caption:=FormatFloat(TeeMsg_DefaultFormat,Z);
    end;

    CheckTransparentControls;

    IModifying:=False;
  end;

begin
  if PageControl1.ActivePage=TabTexture then
     if not ITextureDone then
     begin
       RefreshTexture;
       ITextureDone:=True;
     end;
end;

end.
