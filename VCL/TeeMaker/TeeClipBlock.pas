unit TeeClipBlock;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF D6}
  Types,
  {$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  TeeBlocks, TeCanvas, TeePointEditor;

type
  TClipBlock=class;

  TClipPlane=class(TPersistent)
  private
    FActive   : Boolean;
    FPlane    : TPointXYZFloat;
    FPosition : Double;

    IOwner : TClipBlock;

    procedure Changed(Sender:TObject);
    procedure DrawPreview(const RotX,RotY,RotZ:Double);
    procedure SetActive(const Value: Boolean);
    procedure SetPlane(const Value: TPointXYZFloat);
    procedure SetPosition(const Value: Double);
  public
    Constructor Create;
    Destructor Destroy; override;
  published
    property Active:Boolean read FActive write SetActive default False;
    property Plane:TPointXYZFloat read FPlane write SetPlane;
    property Position:Double read FPosition write SetPosition;
  end;

  TClipPlanes=class(TPersistent)
  private
    FBottom: TClipPlane;
    FFront: TClipPlane;
    FBack: TClipPlane;
    FRight: TClipPlane;
    FLeft: TClipPlane;
    FTop: TClipPlane;

    procedure DisableClip;
    procedure EnableClip;

    procedure SetBack(const Value: TClipPlane);
    procedure SetBottom(const Value: TClipPlane);
    procedure SetFront(const Value: TClipPlane);
    procedure SetLeft(const Value: TClipPlane);
    procedure SetRight(const Value: TClipPlane);
    procedure SetTop(const Value: TClipPlane);
  public
    Preview : Boolean;

    Constructor Create;
    Destructor Destroy; override;

    class procedure ClipPlane(APlane:TClipPlane; Index:Integer);
  published
    property Left:TClipPlane read FLeft write SetLeft;
    property Right:TClipPlane read FRight write SetRight;
    property Top:TClipPlane read FTop write SetTop;
    property Bottom:TClipPlane read FBottom write SetBottom;
    property Front:TClipPlane read FFront write SetFront;
    property Back:TClipPlane read FBack write SetBack;
  end;

  TClipBlock=class(TObjectBlock)
  private
    FPlanes: TClipPlanes;

    procedure SetPlanes(const Value: TClipPlanes);
  protected
    procedure DoDrawItems; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    function GetEditor:String; override;
  published
    property Planes:TClipPlanes read FPlanes write SetPlanes;
  end;

  TClipBlockEditor = class(TVisualEditor)
    ListBox1: TListBox;
    Panel1: TPanel;
    PanelPoint: TPanel;
    Panel2: TPanel;
    CBPreview: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    CBActive: TCheckBox;
    EPosition: TEdit;
    ScrollBar1: TScrollBar;
    procedure ListBox1Click(Sender: TObject);
    procedure EPositionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBActiveClick(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure CBPreviewClick(Sender: TObject);
  private
    { Private declarations }
    IPlane : TClipPlane;
    IPoint : TPointEditor;

    Clip : TClipBlock;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  OpenGL2, TeeProcs, TeePenDlg;

{ TClipBlock }

(*
function TClipBlock.BoundingBox(var AMin,AMax:TPoint3DFloat):Boolean;
var tmp : Double;
begin
  with Size.Point do
  begin
    tmp:=X*0.5;
    AMin.x:=-tmp;
    AMax.x:=tmp;

    tmp:=Z*0.5;
    AMin.z:=-tmp;
    AMax.z:=tmp;

    tmp:=Y*0.5;
    AMin.y:=-tmp;
    AMax.y:=tmp;
  end;

  result:=True;
end;
*)

Constructor TClipBlock.Create(AOwner: TComponent);
begin
  inherited;
  FPlanes:=TClipPlanes.Create;

  with FPlanes do
  begin
    FLeft.IOwner:=Self;
    FRight.IOwner:=Self;
    FTop.IOwner:=Self;
    FBottom.IOwner:=Self;
    FFront.IOwner:=Self;
    FBack.IOwner:=Self;
  end;
end;

Destructor TClipBlock.Destroy;
begin
  FPlanes.Free;
  inherited;
end;

procedure TClipBlock.DoDrawItems;
var tmp : Boolean;
begin
  tmp:=not IBlocks.DrawBlocks.Shadows.Visible;

  if tmp then
     FPlanes.EnableClip;

  inherited;

  if tmp then
     FPlanes.DisableClip;

  if FPlanes.Preview then
  with FPlanes do
  begin
    FLeft.DrawPreview(90,0,0);
    FRight.DrawPreview(90,0,0);

    FTop.DrawPreview(0,90,0);
    FBottom.DrawPreview(0,90,0);
    {
    FFront.DrawPreview;
    FBack.DrawPreview;}
  end;
end;

function TClipBlock.GetEditor: String;
begin
  result:='TClipBlockEditor';
end;

procedure TClipBlock.SetPlanes(const Value: TClipPlanes);
begin
  FPlanes.Assign(Value);
end;

{ TClipPlanes }

procedure TClipPlanes.SetBack(const Value: TClipPlane);
begin
  FBack.Assign(Value);
end;

procedure TClipPlanes.SetBottom(const Value: TClipPlane);
begin
  FBottom.Assign(Value);
end;

procedure TClipPlanes.SetFront(const Value: TClipPlane);
begin
  FFront.Assign(Value);
end;

procedure TClipPlanes.SetLeft(const Value: TClipPlane);
begin
  FLeft.Assign(Value);
end;

procedure TClipPlanes.SetRight(const Value: TClipPlane);
begin
  FRight.Assign(Value);
end;

procedure TClipPlanes.SetTop(const Value: TClipPlane);
begin
  FTop.Assign(Value);
end;

procedure TClipPlanes.DisableClip;
begin
  if FLeft.FActive then
     glDisable(GL_CLIP_PLANE0);

  if FRight.FActive then
     glDisable(GL_CLIP_PLANE1);

  if FTop.FActive then
     glDisable(GL_CLIP_PLANE2);

  if FBottom.FActive then
     glDisable(GL_CLIP_PLANE3);

  if FFront.FActive then
     glDisable(GL_CLIP_PLANE4);

  if FBack.FActive then
     glDisable(GL_CLIP_PLANE5);
end;

class procedure TClipPlanes.ClipPlane(APlane:TClipPlane; Index:Integer);
type
  TPlane=packed Array[0..3] of Double;

var tmp : TPlane;
begin
  with APlane.Plane.Point do
  begin
    tmp[0]:=X;
    tmp[1]:=Y;
    tmp[2]:=Z;
  end;

  tmp[3]:=APlane.Position;

  glClipPlane(Index,@tmp);
  glEnable(Index);
end;

procedure TClipPlanes.EnableClip;
begin
  if FLeft.FActive then
     ClipPlane(FLeft,GL_CLIP_PLANE0);

  if FRight.FActive then
     ClipPlane(FRight,GL_CLIP_PLANE1);

  if FTop.FActive then
     ClipPlane(FTop,GL_CLIP_PLANE2);

  if FBottom.FActive then
     ClipPlane(FBottom,GL_CLIP_PLANE3);

  if FFront.FActive then
     ClipPlane(FFront,GL_CLIP_PLANE4);

  if FBack.FActive then
     ClipPlane(FBack,GL_CLIP_PLANE5);
end;

Constructor TClipPlanes.Create;
begin
  inherited;

  FLeft:=TClipPlane.Create;
  FLeft.Plane.Point.X:=1;

  FRight:=TClipPlane.Create;
  FRight.Plane.Point.X:=-1;

  FTop:=TClipPlane.Create;
  FTop.Plane.Point.Y:=-1;

  FBottom:=TClipPlane.Create;
  FBottom.Plane.Point.Y:=1;

  FFront:=TClipPlane.Create;
  FFront.Plane.Point.Z:=1;

  FBack:=TClipPlane.Create;
  FBack.Plane.Point.Z:=-1;
end;

Destructor TClipPlanes.Destroy;
begin
  FBack.Free;
  FFront.Free;
  FBottom.Free;
  FTop.Free;
  FRight.Free;
  FLeft.Free;

  inherited;
end;

procedure TClipPlane.DrawPreview(const RotX,RotY,RotZ:Double);
var tmp : TRectangleBlock;
begin
  if Active then
  begin
    tmp:=TRectangleBlock.Create(nil);

    with tmp do
    try
      with Self.Plane.Point do
        Location.Point:=PointFloat(-X*Position,-Z*Position,-Y*Position);

      Size.Point:=PointFloat(200,0,200);

      with Rotation.Point do
      begin
        X:=RotX;
        Y:=RotY;
        Z:=RotZ;
      end;

      Format.ParentTexture:=False;
      Format.Transparency:=100;

      tmp.DrawBlock(IOwner.Items);

      {
      TBlockAccess(tmp).IBlocks:=IOwner.Items;
      TBlockAccess(tmp).ICanvas:=IOwner.ICanvas;
      DrawBlock;
      TBlockAccess(tmp).IBlocks:=nil;
      }

    finally
      Free;
    end;
  end;
end;

{ TClipPlane }

Constructor TClipPlane.Create;
begin
  inherited;
  FPlane:=TPointXYZFloat.Create(nil,0,Changed);
end;

Destructor TClipPlane.Destroy;
begin
  FPlane.Free;
  inherited;
end;

procedure TClipPlane.Changed(Sender:TObject);
begin
  IOwner.Repaint;
end;

procedure TClipPlane.SetActive(const Value: Boolean);
begin
  if FActive<>Value then
  begin
    FActive:=Value;
    IOwner.Repaint;
  end;
end;

procedure TClipPlane.SetPlane(const Value: TPointXYZFloat);
begin
  FPlane.Assign(Value);
end;

procedure TClipPlane.SetPosition(const Value: Double);
begin
  if FPosition<>Value then
  begin
    FPosition:=Value;
    IOwner.Repaint;
  end;
end;

procedure TClipBlockEditor.ListBox1Click(Sender: TObject);

  procedure ShowEditor(Plane:TClipPlane);
  begin
    IPlane:=Plane;
    
    CBActive.Checked:=Plane.Active;

    if not Assigned(IPoint) then
    begin
      IPoint:=TPointEditor.Create(Self);
      IPoint.Factor:=0.001;
      IPoint.Align:=alClient;
      TTeeVCL.AddFormTo(IPoint,PanelPoint,Plane.FPlane);
    end
    else
      IPoint.SelectPoint(Plane.FPlane);

    EPosition.Text:=FloatToStr(Plane.Position);
  end;

begin
  case ListBox1.ItemIndex of
    0: ShowEditor(Clip.Planes.Left);
    1: ShowEditor(Clip.Planes.Right);
    2: ShowEditor(Clip.Planes.Top);
    3: ShowEditor(Clip.Planes.Bottom);
    4: ShowEditor(Clip.Planes.Front);
  else
    ShowEditor(Clip.Planes.Back);
  end;
end;

procedure TClipBlockEditor.EPositionChange(Sender: TObject);
var tmp : Double;
begin
  if TryStrToFloat(EPosition.Text,tmp) then
     IPlane.Position:=tmp;
end;

procedure TClipBlockEditor.FormShow(Sender: TObject);
begin
  if Tag<>0 then
  begin
    Clip:=TClipBlock(Tag);

    CBPreview.Checked:=Clip.Planes.Preview;

    ListBox1.ItemIndex:=0;
    ListBox1Click(Self);
  end;
end;

procedure TClipBlockEditor.CBActiveClick(Sender: TObject);
begin
  IPlane.Active:=CBActive.Checked;
  MarkDirty;
end;

procedure TClipBlockEditor.ScrollBar1Change(Sender: TObject);
begin
  EPosition.Text:=IntToStr(ScrollBar1.Position);
end;

procedure TClipBlockEditor.CBPreviewClick(Sender: TObject);
begin
  Clip.Planes.Preview:=CBPreview.Checked;
  Clip.Repaint;
end;

initialization
  RegisterBlocks([TClipBlock]);
  RegisterClass(TClipBlockEditor);
end.
