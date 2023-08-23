unit TeeCamera;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OpenGL2, TeCanvas, TeeProcs;

type
  TFrameBuffer=class
  private
    fb,
    color_rb : GLuint;
    depth_rb : GLuint;
    status   : GLenum;
    W,H      : Integer;
    HasDepth : Boolean;

    FEnabled : Boolean;

    procedure SetEnabled(const Value:Boolean);
  public
    Constructor Create(const Width,Height:Integer; Depth:Boolean);
    Destructor Destroy; override;

    property Enabled:Boolean read FEnabled write SetEnabled;
  end;

  TMakerCamera=class(TCollectionItem)
  private
    FLocation : TPointXYZFloat;
    FOnChange : TNotifyEvent;
    FPerspective : Double;
    FRotation : TPointXYZFloat;
    FTitle    : String;
    FZoom     : Double;

    IBuffer     : TFrameBuffer;
    IViewBackup : TView3DOptions;

    procedure Changed(Sender:TObject);
    procedure SetLocation(const Value:TPointXYZFloat);
    procedure SetPerspective(const Value:Double);
    procedure SetRotation(const Value:TPointXYZFloat);
    procedure SetTitle(const Value:String);
    procedure SetZoom(const Value:Double);
  public
    View : TView3DOptions;

    Constructor Create(AOwner:TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure DrawToBitmap(const APanel:TCustomTeePanel; const ABitmap:TBitmap; ATexture:Cardinal=0);
    procedure SetFromView(const AView:TView3DOptions);
    procedure SetToView(const View:TView3DOptions);
  published
    property Location:TPointXYZFloat read FLocation write SetLocation;
    property Perspective:Double read FPerspective write SetPerspective;
    property Rotation:TPointXYZFloat read FRotation write SetRotation;
    property Title:String read FTitle write SetTitle;
    property Zoom:Double read FZoom write SetZoom;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TMakerCameras=class(TOwnedCollection)
  private
    FSelected : TMakerCamera;

    function Get(Index:Integer):TMakerCamera;
    function GetCurrent:Integer;
    procedure Put(Index:Integer; const Value:TMakerCamera);
    procedure SetCurrent(Index:Integer);
    procedure SetSelected(Value:TMakerCamera);
  public
    property Camera[Index:Integer]:TMakerCamera read Get write Put; default;
    property Selected:TMakerCamera read FSelected write SetSelected;
  published
    property Current:Integer read GetCurrent write SetCurrent default -1;
  end;

  TCameraEditor = class(TForm)
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    LRotation: TLabel;
    LElevation: TLabel;
    LTilt: TLabel;
    LXOffset: TLabel;
    LYOffset: TLabel;
    LZOffset: TLabel;
    Label123: TLabel;
    LZoom: TLabel;
    Label24: TLabel;
    LCameraPerspective: TLabel;
    CameraTilt: TScrollBar;
    CameraElevation: TScrollBar;
    CameraRotation: TScrollBar;
    CameraZoom: TScrollBar;
    CameraPerspective: TScrollBar;
    Label9: TLabel;
    procedure CameraTiltChange(Sender: TObject);
    procedure CameraElevationChange(Sender: TObject);
    procedure CameraRotationChange(Sender: TObject);
    procedure CameraZoomChange(Sender: TObject);
    procedure CameraPerspectiveChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Camera     : TMakerCamera;
    IModifying : Boolean;
  public
    { Public declarations }
    procedure RefreshCamera(ACamera:TMakerCamera);
  end;

implementation

{$R *.dfm}

uses
  TeeGLCanvas;

{ TMakerCamera }

Constructor TMakerCamera.Create(AOwner:TCollection);
begin
  inherited;

  FLocation:=TPointXYZFloat.Create(nil,0,Changed);
  FRotation:=TPointXYZFloat.Create(nil,0,Changed);
  IViewBackup:=TView3DOptions.Create(nil);
end;

Destructor TMakerCamera.Destroy;
begin
  IBuffer.Free;
  IViewBackup.Free;
  FLocation.Free;
  FRotation.Free;

  inherited;
end;

procedure TMakerCamera.Assign(Source:TPersistent);
begin
  if Source is TMakerCamera then
  with TMakerCamera(Source) do
  begin
    Self.FLocation.Point:=FLocation.Point;
    Self.FRotation.Point:=FRotation.Point;
    Self.FPerspective:=FPerspective;
    Self.FZoom:=FZoom;
  end
  else
    inherited;
end;

procedure TMakerCamera.SetFromView(const AView:TView3DOptions);
var tmp : TView3DOptions;
begin
  tmp:=AView;

  with FRotation.Point do
  begin
    X:=tmp.RotationFloat;
    Y:=tmp.ElevationFloat;
    Z:=tmp.TiltFloat;
  end;

  with FLocation.Point do
  begin
    X:=tmp.HorizOffsetFloat;
    Y:=tmp.VertOffsetFloat;
    Z:=tmp.ZOffset;
  end;

  FZoom:=tmp.ZoomFloat;
  FPerspective:=tmp.Perspective;
end;

procedure TMakerCamera.Changed(Sender:TObject);
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

procedure TMakerCamera.SetToView(const View:TView3DOptions);
begin
  with View do
  begin
    HorizOffsetFloat:=FLocation.Point.X;
    VertOffsetFloat:=FLocation.Point.Y;
    ZOffset:=FLocation.Point.Z;

    RotationFloat:=FRotation.Point.X;
    ElevationFloat:=FRotation.Point.Y;
    TiltFloat:=FRotation.Point.Z;

    ZoomFloat:=FZoom;
    Perspective:=Round(FPerspective);
  end;
end;

procedure TMakerCamera.SetLocation(const Value:TPointXYZFloat);
begin
  FLocation.Assign(Value);
end;

procedure TMakerCamera.SetPerspective(const Value:Double);
begin
  if FPerspective<>Value then
  begin
    FPerspective:=Value;
    Changed(Self);
  end;
end;

procedure TMakerCamera.SetRotation(const Value:TPointXYZFloat);
begin
  FRotation.Assign(Value);
end;

procedure TMakerCamera.SetTitle(const Value:String);
begin
  FTitle:=Value;
end;

procedure TMakerCamera.SetZoom(const Value:Double);
begin
  if FZoom<>Value then
  begin
    FZoom:=Value;
    Changed(Self);
  end;
end;

type
  TPanelAccess=class(TCustomTeePanelExtended);
  TGLCanvasAccess=class(TGLCanvas);

procedure TMakerCamera.DrawToBitmap(const APanel:TCustomTeePanel; const ABitmap: TBitmap; ATexture:Cardinal=0);

  procedure InternalDrawToBitmap;
  begin
    with TPanelAccess(APanel) do
    begin
      if Assigned(IBuffer) then
         IBuffer.Enabled:=True
      else
         IBuffer:=TFrameBuffer.Create(Width,Height,True);

      if ATexture<>0 then
      begin
        glFramebufferTexture2D(GL_FRAMEBUFFER_EXT,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D,ATexture,0);

        Assert(TGLCanvas(Canvas).CheckGLError,'FrameBufferTexture2D: '+IntToStr(TGLCanvasAccess(Canvas).ISavedError));
      end;

      Canvas.BackColor:=Color;
      InternalDraw(ChartBounds);

      if Assigned(ABitmap) then
         TGLCanvas(Canvas).BufferToBitmap(ABitmap,ClientRect);

      IBuffer.Enabled:=False;
    end;
  end;

begin
  APanel.AutoRepaint:=False;

  IViewBackup.Assign(APanel.View3DOptions);

  if Zoom=0 then
     SetFromView(APanel.View3DOptions)
  else
     SetToView(APanel.View3DOptions);

  InternalDrawToBitmap;

  APanel.View3DOptions:=IViewBackup;

  with APanel do
  begin
    TGLCanvasAccess(Canvas).{$IFDEF BLOCKS}iDoProjection{$ELSE}DoProjection{$ENDIF};
    TGLCanvasAccess(Canvas).{$IFDEF BLOCKS}iSetModelView{$ELSE}SetModelView{$ENDIF};
  end;
end;

{ TMakerCameras }

function TMakerCameras.Get(Index:Integer):TMakerCamera;
begin
  result:=TMakerCamera(inherited Items[Index]);
end;

function TMakerCameras.GetCurrent: Integer;
begin
  if Assigned(Selected) then
     result:=Selected.Index
  else
     result:=-1;
end;

procedure TMakerCameras.Put(Index:Integer; const Value:TMakerCamera);
begin
  inherited Items[Index]:=Value;
end;

procedure TMakerCameras.SetCurrent(Index: Integer);
begin
  if Index=-1 then
     Selected:=nil
  else
     Selected:=Camera[Index];
end;

procedure TMakerCameras.SetSelected(Value:TMakerCamera);
begin
  if FSelected<>Value then
  begin
    FSelected:=Value;
    FSelected.Changed(Self);
  end;
end;

function FormatData(const Value:Double):String;
begin
  result:=FormatFloat('0.##',Value);
end;

procedure TCameraEditor.RefreshCamera(ACamera:TMakerCamera);
begin
  IModifying:=True;

  Camera:=ACamera;

  with Camera do
  begin
    with Rotation do
    begin
      CameraRotation.Position:=Round(X) mod 360;
      CameraElevation.Position:=Round(Y) mod 360;
      CameraTilt.Position:=Round(Z) mod 360;

      LRotation.Caption:=FormatData(X);
      LElevation.Caption:=FormatData(Y);
      LTilt.Caption:=FormatData(Z);
    end;

    with Location do
    begin
      LXOffset.Caption:=FormatData(X);
      LYOffset.Caption:=FormatData(Y);
      LZOffset.Caption:=FormatData(Z);
    end;

    LZoom.Caption:=FormatData(Zoom);
    CameraPerspective.Position:=Round(Perspective);
    LCameraPerspective.Caption:=FormatData(Perspective);
  end;

  IModifying:=False;
end;

procedure TCameraEditor.CameraTiltChange(Sender: TObject);
begin
  if not IModifying then
     Camera.Rotation.Z:=CameraTilt.Position;
end;

procedure TCameraEditor.CameraElevationChange(Sender: TObject);
begin
  if not IModifying then
     Camera.Rotation.Y:=CameraElevation.Position;
end;

procedure TCameraEditor.CameraRotationChange(Sender: TObject);
begin
  if not IModifying then
     Camera.Rotation.X:=CameraRotation.Position;
end;

procedure TCameraEditor.CameraZoomChange(Sender: TObject);
begin
  if not IModifying then
     Camera.Zoom:=CameraZoom.Position;
end;

procedure TCameraEditor.CameraPerspectiveChange(Sender: TObject);
begin
  if not IModifying then
     Camera.Perspective:=CameraPerspective.Position;
end;

procedure TCameraEditor.FormShow(Sender: TObject);
begin
  Camera:=TMakerCamera(Tag);

  if Assigned(Camera) then
     RefreshCamera(Camera);
end;

{ TFrameBuffer }

Constructor TFrameBuffer.Create(const Width,Height:Integer; Depth:Boolean);
begin
  inherited Create;

  W:=Width;
  H:=Height;
  HasDepth:=Depth;

  glGenFramebuffersEXT(1, @fb);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fb);

  glGenRenderbuffersEXT(1, @color_rb);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, color_rb);

  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_RGBA8, W, H);
  // glRenderBufferStorageMultisample

  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_RENDERBUFFER_EXT, color_rb);

  if Depth then
  begin
    glGenRenderbuffersEXT(1, @depth_rb);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, depth_rb);
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, W, H);
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depth_rb);
  end;

  status:=glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);

  if status=GL_FRAMEBUFFER_COMPLETE_EXT then
  begin
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fb);
    FEnabled:=True;
  end;
end;

Destructor TFrameBuffer.Destroy;
begin
  Enabled:=False;

  glDeleteRenderbuffersEXT(1, @color_rb);

  if HasDepth then
     glDeleteRenderbuffersEXT(1, @depth_rb);

  glDeleteFramebuffersEXT(1, @fb);

  inherited;
end;

procedure TFrameBuffer.SetEnabled(const Value:Boolean);
begin
  FEnabled:=Value;

  if FEnabled then
     glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fb)
  else
     glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

end.
