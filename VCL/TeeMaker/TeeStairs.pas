unit TeeStairs;
{$I TeeDefs.inc}

interface

uses
  Windows,
  Classes, SysUtils, Forms, StdCtrls, Controls,

  {$IFDEF D6}
  Types,
  {$ENDIF}

  TeCanvas, TeeBlocks;

type
  TStairsBlock=class(TCustomBlock)
  private
    FBack       : Boolean;
    FSides      : Boolean;
    FStepHeight : Integer;
    FStepDepth  : Integer;
    FStepRoundY : Integer;
    FStepRoundX : Integer;

    IListBack   : Integer;
    IListSide1  : Integer;
    IListSide2  : Integer;
    IStepBlock  : TCustomBlock;

    procedure FreeStepBlock;
    procedure SetBack(const Value: Boolean);
    procedure SetSides(const Value: Boolean);
    procedure SetStepDepth(const Value: Integer);
    procedure SetStepHeight(const Value: Integer);
    procedure SetStepRoundX(const Value: Integer);
    procedure SetStepRoundY(const Value: Integer);
  protected
    procedure DeleteLists; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Back:Boolean read FBack write SetBack default False;
    property Sides:Boolean read FSides write SetSides default False;
    property StepDepth:Integer read FStepDepth write SetStepDepth default 20;
    property StepHeight:Integer read FStepHeight write SetStepHeight default 10;
    property StepRoundX:Integer read FStepRoundX write SetStepRoundX default 10;
    property StepRoundY:Integer read FStepRoundY write SetStepRoundY default 10;
  end;

  TStairsEditor = class(TVisualEditor)
    Label63: TLabel;
    Label64: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    BlockStairsSides: TCheckBox;
    BlockStepDepth: TScrollBar;
    BlockStepHeight: TScrollBar;
    BlockStairsBack: TCheckBox;
    BlockStairsRoundX: TScrollBar;
    BlockStairsRoundY: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure BlockStairsSidesClick(Sender: TObject);
    procedure BlockStepDepthChange(Sender: TObject);
    procedure BlockStepHeightChange(Sender: TObject);
    procedure BlockStairsBackClick(Sender: TObject);
    procedure BlockStairsRoundXChange(Sender: TObject);
    procedure BlockStairsRoundYChange(Sender: TObject);
  private
    { Private declarations }
    Stairs : TStairsBlock;
    IModifying : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  OpenGL2, TeeRoundRect;

{ TStairs }
Constructor TStairsBlock.Create(AOwner:TComponent);
begin
  inherited;

  FStepHeight:=10;
  FStepDepth:=20;
  FStepRoundX:=10;
  FStepRoundY:=10;
end;

Destructor TStairsBlock.Destroy;
begin
  FreeStepBlock;
  inherited;
end;

procedure TStairsBlock.Assign(Source: TPersistent);
begin
  if Source is TStairsBlock then
  with TStairsBlock(Source) do
  begin
    Self.FSides:=FSides;
    Self.FStepDepth:=FStepDepth;
    Self.FStepHeight:=FStepHeight;
    Self.FStepRoundX:=FStepRoundX;
    Self.FStepRoundY:=FStepRoundY;
  end;

  inherited;
end;

type
  TBlockAccess=class(TCustomBlock);
  TBlockFormatAccess=class(TBlockFormat);

procedure TStairsBlock.Draw;
var tmp      : TPoint3DArray;
    NumSteps : Integer;
    tmpX     : Double;
    tmpZ     : Double;
    tmpX0    : Double;
    tmpZ0    : Double;
    t        : Integer;
    Old      : Boolean;
begin
  // Internal Block
  if (FStepRoundX<>0) or (FStepRoundY<>0) then
  begin
    if not Assigned(IStepBlock) then
       IStepBlock:=TRoundRectBlock.Create(nil);

    with TRoundRectBlock(IStepBlock).Corners do
    begin
      LeftTop.X:=Self.FStepRoundX;
      LeftTop.Y:=Self.FStepRoundY;

      RightTop.Point:=PointFloat(0,0);
      LeftBottom.Point:=PointFloat(0,0);
      RightBottom.Point:=PointFloat(0,0);
    end;
  end
  else
  if not Assigned(IStepBlock) then
     IStepBlock:=TCubeBlock.Create(nil);

  TBlockAccess(IStepBlock).IBlocks:=IBlocks;
  TBlockAccess(IStepBlock).ICanvas:=ICanvas;
  TBlockAccess(IStepBlock).IPicking:=IPicking;

  Old:=Parent.Parent.AutoRepaint;
  Parent.Parent.AutoRepaint:=False;

  IStepBlock.Size.SetPoint(StepDepth/Size.Point.X,2,StepHeight/Size.Point.Z);

  IStepBlock.Format:=Format;

  Parent.Parent.AutoRepaint:=Old;

  // Coordinates
  NumSteps:=Round(2/IStepBlock.Size.X);

  tmpX:=-1;
  tmpZ:=-1;

  tmpX0:=tmpX;
  tmpZ0:=tmpZ;

  // Draw steps:
  for t:=0 to NumSteps-1 do
  begin
    with IStepBlock.Location.Point do
    begin
      X:=tmpX+(IStepBlock.Size.X*0.5);
      Z:=tmpZ+(IStepBlock.Size.Z*0.5);
    end;

    IStepBlock.DrawBlock;

    tmpX:=tmpX+IStepBlock.Size.X;
    tmpZ:=tmpZ+IStepBlock.Size.Z;

    if tmpZ>1 then
       break;
  end;

  // Sides:
  if FSides then
     if IListSide1=0 then
     begin
       SetLength(tmp,3);

       tmp[0]:=PointFloat(tmpX,-tmpZ,1);
       tmp[1]:=PointFloat(tmpX0,-tmpZ0,1);
       tmp[2]:=PointFloat(tmpX,tmpZ0+2,1);

       TBlockFormatAccess(Format).ConvexPolygon(IListSide1,tmp);

       tmp[0].Z:=-1;
       tmp[1].Z:=-1;
       tmp[2].Z:=-1;

       TBlockFormatAccess(Format).ConvexPolygon(IListSide2,tmp);

       tmp:=nil;
     end
     else
     begin
       glCallList(IListSide1);
       glCallList(IListSide2);
     end;

  // Back side:
  if FBack then
     if IListBack=0 then
     begin
       SetLength(tmp,4);

       tmp[0]:=PointFloat(tmpX,-tmpZ,1);
       tmp[1]:=PointFloat(tmpX,-tmpZ,-1);
       tmp[2]:=PointFloat(tmpX,tmpZ0+2,-1);
       tmp[3]:=PointFloat(tmpX,tmpZ0+2,1);

       TBlockFormatAccess(Format).ConvexPolygon(IListBack,tmp);

       tmp:=nil;
     end
     else
       glCallList(IListBack);
end;

procedure TStairsBlock.SetStepDepth(const Value: Integer);
begin
  FStepDepth := Value;
  Repaint;
end;

procedure TStairsBlock.SetStepHeight(const Value: Integer);
begin
  FStepHeight := Value;
  Repaint;
end;

procedure TStairsBlock.SetSides(const Value: Boolean);
begin
  FSides := Value;
  Repaint;
end;

procedure TStairsBlock.SetBack(const Value: Boolean);
begin
  FBack := Value;
  Repaint;
end;

procedure TStairsBlock.FreeStepBlock;
begin
  if Assigned(IStepBlock) then
  begin
    TBlockAccess(IStepBlock).IBlocks:=nil;
    FreeAndNil(IStepBlock);
  end;
end;

procedure TStairsBlock.SetStepRoundX(const Value: Integer);
begin
  FStepRoundX := Value;
  FreeStepBlock;
  Repaint;
end;

procedure TStairsBlock.SetStepRoundY(const Value: Integer);
begin
  FStepRoundY := Value;
  FreeStepBlock;
  Repaint;
end;

procedure TStairsBlock.DeleteLists;
begin
  inherited;

  DeleteList(IListBack);
  DeleteList(IListSide1);
  DeleteList(IListSide2);
end;

procedure TStairsEditor.FormShow(Sender: TObject);
begin
  Stairs:=TStairsBlock(Tag);

  if Assigned(Stairs) then
  with Stairs do
  begin
    IModifying:=True;

    BlockStairsBack.Checked:=Back;
    BlockStairsSides.Checked:=Sides;
    BlockStepDepth.Position:=StepDepth;
    BlockStepHeight.Position:=StepHeight;

    BlockStairsRoundX.Position:=StepRoundX;
    BlockStairsRoundY.Position:=StepRoundY;

    IModifying:=False;
  end;
end;

procedure TStairsEditor.BlockStairsSidesClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.Sides:=BlockStairsSides.Checked;
    MarkDirty;
  end;
end;

procedure TStairsEditor.BlockStepDepthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.StepDepth:=BlockStepDepth.Position;
    MarkDirty;
  end;
end;

procedure TStairsEditor.BlockStepHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.StepHeight:=BlockStepHeight.Position;
    MarkDirty;
  end;
end;

procedure TStairsEditor.BlockStairsBackClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.Back:=BlockStairsBack.Checked;
    MarkDirty;
  end;
end;

procedure TStairsEditor.BlockStairsRoundXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.StepRoundX:=BlockStairsRoundX.Position;
    MarkDirty;
  end;
end;

procedure TStairsEditor.BlockStairsRoundYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Stairs.StepRoundY:=BlockStairsRoundY.Position;
    MarkDirty;
  end;
end;

initialization
  RegisterBlock(TStairsBlock);
  RegisterClass(TStairsEditor);
end.
