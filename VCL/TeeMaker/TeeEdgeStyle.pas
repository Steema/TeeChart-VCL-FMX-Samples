unit TeeEdgeStyle;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs, TeeBlocks;

type
  TEdgeEditor = class(TVisualEditor)
    GroupBox11: TGroupBox;
    Label18: TLabel;
    Label28: TLabel;
    Label30: TLabel;
    BlockPieRoundX: TScrollBar;
    BlockPieRoundY: TScrollBar;
    BlockRoundStyle: TComboFlat;
    EBlockRoundSlices: TEdit;
    BlockRoundSlices: TUpDown;
    LRoundX: TLabel;
    LRoundY: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BlockPieRoundXChange(Sender: TObject);
    procedure BlockPieRoundYChange(Sender: TObject);
    procedure BlockRoundStyleChange(Sender: TObject);
    procedure EBlockRoundSlicesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IModifying : Boolean;
  public
    { Public declarations }
    Edge : TBlockEdge;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

procedure TEdgeEditor.FormShow(Sender: TObject);
begin
  IModifying:=True;

  if Assigned(Edge) then
  with Edge do
  begin
    BlockPieRoundX.Position:=Round(X);
    BlockPieRoundY.Position:=Round(Y);

    LRoundX.Caption:=TeeStr(BlockPieRoundX.Position)+'%';
    LRoundY.Caption:=TeeStr(BlockPieRoundY.Position)+'%';

    BlockRoundSlices.Position:=Slices;
    BlockRoundStyle.ItemIndex:=Ord(Style);

  end;

  IModifying:=False;
end;

procedure TEdgeEditor.BlockPieRoundXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Edge.X:=BlockPieRoundX.Position;
    LRoundX.Caption:=TeeStr(BlockPieRoundX.Position)+'%';
    MarkDirty;
  end;
end;

procedure TEdgeEditor.BlockPieRoundYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Edge.Y:=BlockPieRoundY.Position;
    LRoundY.Caption:=TeeStr(BlockPieRoundY.Position)+'%';
    MarkDirty;
  end;
end;

procedure TEdgeEditor.BlockRoundStyleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Edge.Style:=TBlockEdgeStyle(BlockRoundStyle.ItemIndex);
    MarkDirty;
  end;
end;

procedure TEdgeEditor.EBlockRoundSlicesChange(Sender: TObject);
begin
  if not IModifying then
  if Assigned(Edge) then
  begin
    Edge.Slices:=BlockRoundSlices.Position;
    MarkDirty;
  end;
end;

procedure TEdgeEditor.FormCreate(Sender: TObject);
begin
  IModifying:=True;
end;

end.
