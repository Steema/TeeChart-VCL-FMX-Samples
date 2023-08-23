unit TeeViewBlock;
{$I TeeDefs.inc}


interface

uses
  Windows, Messages, 
  SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, 

  {$IFDEF D17}
  System.Types,
  {$ENDIF}

  TeeBlocks, TeeMakerControl, TeeCamera, TeCanvas;

type
  TCameraViewBlock=class(TRectangleBlock)
  private
    FCamera : TMakerCamera;

    IBitmap     : TBitmap;
    IRefreshing : Boolean;

    procedure CameraChanged(Sender:TObject);
    procedure SetCamera(const Value: TMakerCamera);
  protected
    function GetEditor:String; override;
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure DrawBlock; override;
  published
    property Camera:TMakerCamera read FCamera write SetCamera;
  end;

  TView2DBlock=class(TRectangleBlock)
  private
    IRefreshing : Boolean;
  public
    procedure Draw; override;
  end;

  TCameraViewBlockEditor = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    View : TCameraViewBlock;
    ICameraEditor : TCameraEditor;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeProcs, TeeGLCanvas, TeePenDlg, OpenGL2;

procedure TCameraViewBlockEditor.FormShow(Sender: TObject);
begin
  View:=TCameraViewBlock(Tag);

  if Assigned(View) then
  begin
    if not Assigned(ICameraEditor) then
    begin
      ICameraEditor:=TCameraEditor.Create(Self);
      ICameraEditor.Align:=alClient;
      TTeeVCL.AddFormTo(ICameraEditor,Self,View.Camera);
    end
    else
      ICameraEditor.RefreshCamera(View.Camera);
  end;
end;

{ TCameraViewBlock }

Constructor TCameraViewBlock.Create(AOwner: TComponent);
begin
  inherited;

  FCamera:=TMakerCamera.Create(nil);
  FCamera.OnChange:=CameraChanged;
  FCamera.View:=TView3DOptions.Create(nil);

  IBitmap:=TBitmap.Create;
end;

Destructor TCameraViewBlock.Destroy;
begin
  IBitmap.Free;
  FCamera.View.Free;
  FCamera.Free;
  inherited;
end;

procedure TCameraViewBlock.CameraChanged(Sender:TObject);
begin
  Repaint;
end;

type
  TBlocksAccess=class(TBlocks);
  TGLCanvasAccess=class(TGLCanvas);

{$DEFINE TEEFRAMETOTEXTURE}

procedure TCameraViewBlock.DrawBlock;
{$IFDEF TEEFRAMETOTEXTURE}
var tmpTexture : Cardinal;
{$ENDIF}
begin
  if IPicking or TBlocksAccess(Parent.DrawBlocks).IDrawingReflection then
     inherited
  else
  if (not Parent.DrawBlocks.Shadows.Visible) and (not IRefreshing) then
  begin
    IRefreshing:=True;
    Parent.Parent.AutoRepaint:=False;

    {$IFDEF TEEFRAMETOTEXTURE}
    if Format.Texture.Picture.Graphic=nil then
    begin
      FCamera.DrawToBitmap(Parent.Parent,IBitmap);
      Format.Texture.Picture.Graphic:=IBitmap;
    end
    else
    begin
      if TGLCanvasAccess(ICanvas).FindTexture(Format.Texture.Picture.Graphic,tmpTexture) then
         FCamera.DrawToBitmap(Parent.Parent,nil,tmpTexture);
    end;

    {$ELSE}

    FCamera.DrawToBitmap(Parent.Parent,IBitmap)
    Parent.RemoveTexture(Format.Texture.Picture.Graphic);
    Format.Texture.Picture.Graphic:=IBitmap;

    {$ENDIF}

    Parent.Parent.AutoRepaint:=True;
    IRefreshing:=False;

    inherited;
  end;
end;

procedure TCameraViewBlock.SetCamera(const Value: TMakerCamera);
begin
  FCamera.Assign(Value);
end;

function TCameraViewBlock.GetEditor: String;
begin
  result:='TCameraViewBlockEditor';
end;

procedure TCameraViewBlock.PrepareForGallery;
begin
  inherited;
  Format.Texture.PictureLink:='';
  Format.Color:=clWhite;
end;

{ TView2DBlock }

type
  TTeePanelAccess=class(TCustomTeePanelExtended);
  TMakerOptionsAccess=class(TMakerOptions);

procedure TView2DBlock.Draw;
const
  tmpInv=1/255.0;

var AColor : TColor;
    tmpR : TRect;
begin
  if (not IPicking) and (not IRefreshing) then
  begin
    IRefreshing:=True;

    with Size.Point do
    begin
      tmpR.Left:=Round(Location.Point.X-X*0.5);
      tmpR.Top:=Parent.Parent.Height-Round(Location.Point.Y-Z*0.5);
      tmpR.Right:=tmpR.Left+Round(X);
      tmpR.Bottom:=tmpR.Top+Round(Z);
    end;

    with tmpR do
         glViewport(Left,Top,Right-Left,Bottom-Top);

    TGLCanvasAccess(ICanvas).DoProjection;

    with tmpR do
         glScissor(Left,Top,Right-Left,Bottom-Top);

    glEnable(GL_SCISSOR_TEST);

    AColor:=ColorToRGB(ICanvas.BackColor);
    glClearColor( Byte(AColor)*tmpInv,
                Byte(AColor shr 8)*tmpInv,
                Byte(AColor shr 16)*tmpInv,
                1);

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    with TTeePanelAccess(Parent.Parent) do
    begin
      FillPanelRect(ClientRect);

      if HasBackImage and
         (not BackImage.Inside) then
           DrawBitmap(ClientRect,0);
    end;

    TGLCanvasAccess(ICanvas).SetModelView;

    with TMakerOptionsAccess(TMaker(Parent.Parent).Options) do
         DrawReflection(Floor);

    Parent.DrawBlocks.Draw;

    glDisable(GL_SCISSOR_TEST);

    with ICanvas do
    begin
      FrontPlaneBegin;
      Brush.Style:=bsClear;
      Rectangle(Parent.Parent.ClientRect);
      FrontPlaneEnd;
    end;

    with Parent.Parent do
         glViewport(0,0,Width,Height);

    IRefreshing:=False;
  end;
end;

initialization
  RegisterBlock(TView2DBlock);
  RegisterBlock(TCameraViewBlock);
  RegisterClass(TCameraViewBlockEditor);
end.
