unit TeeTapeEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, TeeBlocks, TeeExtruded, TeeRoundRect, TeCanvas, StdCtrls, ComCtrls;

type
  TTapeBlockEditor = class(TVisualEditor)
    PageControl12: TPageControl;
    TabSheet9: TTabSheet;
    Label41: TLabel;
    Label43: TLabel;
    Label34: TLabel;
    LabelBlockSlices: TLabel;
    LabelTapeRadius: TLabel;
    LabelTapeRound: TLabel;
    BlockTapeRadius: TScrollBar;
    BlockTapeRound: TScrollBar;
    BlockTapeClosed: TCheckBox;
    BlockTapeSlices: TScrollBar;
    BlockTapeColorEach: TCheckBox;
    BlockTapeAdjust: TCheckBox;
    TabSheet12: TTabSheet;
    Label101: TLabel;
    LabelBlockTape3D: TLabel;
    Label111: TLabel;
    LabelBlockSlices3D: TLabel;
    BlockTape3D: TScrollBar;
    BlockTapeSlices3D: TScrollBar;
    BlockTapeCorners: TCheckBox;
    TabTapeCorners: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure BlockTapeRoundChange(Sender: TObject);
    procedure BlockTapeRadiusChange(Sender: TObject);
    procedure BlockTapeClosedClick(Sender: TObject);
    procedure BlockTapeSlicesChange(Sender: TObject);
    procedure BlockTape3DChange(Sender: TObject);
    procedure BlockTapeColorEachClick(Sender: TObject);
    procedure BlockTapeAdjustClick(Sender: TObject);
    procedure BlockTapeSlices3DChange(Sender: TObject);
    procedure BlockTapeCornersClick(Sender: TObject);
  private
    { Private declarations }
    Tape : TTapeBlock;

    ITapeCornersEditor : TRoundRectEditor;
    IModifying : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeProcs, TeePenDlg;

{ TTapeBlockEditor }

type
  TTapeBlockAccess=class(TTapeBlock);

procedure TTapeBlockEditor.FormShow(Sender: TObject);
begin
  Tape:=TTapeBlock(Tag);

  if Assigned(Tape) then
  with Tape do
  begin
    IModifying:=True;

    BlockTapeRadius.Position:=Round(Radius*2);
    LabelTapeRadius.Caption:=FormatFloat('0.##',Radius);

    BlockTapeRound.Position:=Roundness;
    LabelTapeRound.Caption:=FormatFloat('0.##',Roundness);

    BlockTapeClosed.Visible:=not TTapeBlockAccess(Tape).ForceClosed;

    BlockTapeClosed.Checked:=Closed;

    BlockTapeSlices.Position:=Slices;
    LabelBlockSlices.Caption:=TeeStr(BlockTapeSlices.Position);

    BlockTape3D.Position:=Round(Tape3D);
    LabelBlockTape3D.Caption:=FormatFloat('0.###',Tape3D);

    BlockTapeSlices3D.Position:=Slices3D;
    LabelBlockSlices3D.Caption:=TeeStr(BlockTapeSlices3D.Position);

    BlockTapeColorEach.Checked:=TapeColorEach;
    BlockTapeAdjust.Checked:=AdjustTexture;

    BlockTapeCorners.Checked:=Corners.Enabled;

    if not Assigned(ITapeCornersEditor) then
    begin
      ITapeCornersEditor:=TRoundRectEditor.Create(Self);
      ITapeCornersEditor.Corners:=Corners;
      ITapeCornersEditor.OnDirty:=OnDirty;

      TTeeVCL.AddFormTo(ITapeCornersEditor,TabTapeCorners);
    end;

    IModifying:=False;
  end;
end;

procedure TTapeBlockEditor.BlockTapeRoundChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Roundness:=BlockTapeRound.Position;
    LabelTapeRound.Caption:=FormatFloat('0.##',Tape.Roundness);
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Radius:=BlockTapeRadius.Position*0.5;
    LabelTapeRadius.Caption:=FormatFloat('0.##',Tape.Radius);
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeClosedClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Closed:=BlockTapeClosed.Checked;
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Slices:=BlockTapeSlices.Position;
    LabelBlockSlices.Caption:=TeeStr(BlockTapeSlices.Position);
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTape3DChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Tape3D:=BlockTape3D.Position;
    LabelBlockTape3D.Caption:=FormatFloat('0.###',Tape.Tape3D);
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeColorEachClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.TapeColorEach:=BlockTapeColorEach.Checked;
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeAdjustClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.AdjustTexture:=BlockTapeAdjust.Checked;
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeSlices3DChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Slices3D:=BlockTapeSlices3D.Position;
    LabelBlockSlices3D.Caption:=TeeStr(BlockTapeSlices3D.Position);
    MarkDirty;
  end;
end;

procedure TTapeBlockEditor.BlockTapeCornersClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Tape.Corners.Enabled:=BlockTapeCorners.Checked;
    MarkDirty;
  end;
end;

initialization
  RegisterClass(TTapeBlockEditor);
end.
