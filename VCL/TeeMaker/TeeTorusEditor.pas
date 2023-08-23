unit TeeTorusEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, TeCanvas, TeeBlocks;

type
  TTorusEditor = class(TVisualEditor)
    Label20: TLabel;
    Label21: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    LabelTorusStart: TLabel;
    LabelTorusTotal: TLabel;
    Label62: TLabel;
    BlockTorusXRad: TLabel;
    BlockTorusYRad: TLabel;
    BlockTorusStart: TScrollBar;
    BlockTorusTotal: TScrollBar;
    Edit1: TEdit;
    Edit2: TEdit;
    BlockTorusSides: TUpDown;
    BlockTorusRings: TUpDown;
    BlockTorusXRadius: TScrollBar;
    BlockTorusYRadius: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure BlockTorusStartChange(Sender: TObject);
    procedure BlockTorusTotalChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BlockTorusXRadiusChange(Sender: TObject);
    procedure BlockTorusYRadiusChange(Sender: TObject);
  private
    { Private declarations }

    Torus      : TTorusBlock;
    IModifying : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeProcs;

procedure TTorusEditor.FormShow(Sender: TObject);
begin
  Torus:=TTorusBlock(Tag);

  if Assigned(Torus) then
  with Torus do
  begin
    IModifying:=True;

    BlockTorusStart.Position:=Round(StartAngle);
    LabelTorusStart.Caption:=TeeStr(BlockTorusStart.Position);

    BlockTorusXRadius.Position:=Round(Radius.X);
    BlockTorusXRad.Caption:=TeeStr(BlockTorusXRadius.Position)+'%';

    BlockTorusYRadius.Position:=Round(Radius.Y);
    BlockTorusYRad.Caption:=TeeStr(BlockTorusYRadius.Position)+'%';

    BlockTorusTotal.Position:=Round(TotalAngle);
    LabelTorusTotal.Caption:=TeeStr(BlockTorusTotal.Position);

    BlockTorusSides.Position:=Sides;
    BlockTorusRings.Position:=Rings;

    IModifying:=False;
  end;
end;

procedure TTorusEditor.BlockTorusStartChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.StartAngle:=BlockTorusStart.Position;
    LabelTorusStart.Caption:=TeeStr(BlockTorusStart.Position);
    MarkDirty;
  end;
end;

procedure TTorusEditor.BlockTorusTotalChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.TotalAngle:=BlockTorusTotal.Position;
    LabelTorusTotal.Caption:=TeeStr(BlockTorusTotal.Position);
    MarkDirty;
  end;
end;

procedure TTorusEditor.Edit1Change(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.Sides:=BlockTorusSides.Position;
    MarkDirty;
  end;
end;

procedure TTorusEditor.Edit2Change(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.Rings:=BlockTorusRings.Position;
    MarkDirty;
  end;
end;

procedure TTorusEditor.BlockTorusXRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.Radius.X:=BlockTorusXRadius.Position;
    BlockTorusXRad.Caption:=TeeStr(BlockTorusXRadius.Position)+'%';
    MarkDirty;
  end;
end;

procedure TTorusEditor.BlockTorusYRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Torus.Radius.Y:=BlockTorusYRadius.Position;
    BlockTorusYRad.Caption:=TeeStr(BlockTorusYRadius.Position)+'%';
    MarkDirty;
  end;
end;

initialization
  RegisterClass(TTorusEditor);
end.
