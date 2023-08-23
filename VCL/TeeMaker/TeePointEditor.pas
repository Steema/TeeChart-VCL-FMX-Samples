unit TeePointEditor;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs, TeeBlocks;

type
  TPointEditor = class(TVisualEditor)
    Label58: TLabel;
    LabelPathY: TLabel;
    Label60: TLabel;
    PathX: TLabel;
    PathY: TLabel;
    PathZ: TLabel;
    BlockPathX: TScrollBar;
    BlockPathY: TScrollBar;
    BlockPathZ: TScrollBar;
    BlockPathColor: TButtonColor;
    BlockPathDefault: TCheckBox;
    procedure BlockPathXChange(Sender: TObject);
    procedure BlockPathYChange(Sender: TObject);
    procedure BlockPathZChange(Sender: TObject);
    procedure BlockPathColorClick(Sender: TObject);
    procedure BlockPathDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    IModifying : Boolean;
    Current    : TPointXYZFloat;
  public
    { Public declarations }

    Factor : Double;

    procedure SelectPoint(const APoint:TPointXYZFloat);
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

procedure TPointEditor.SelectPoint(const APoint:TPointXYZFloat);
var tmpOk : Boolean;
begin
  IModifying:=True;

  Current:=APoint;

  tmpOk:=Assigned(Current);

  if tmpOk then
  begin
    with Current.Point do
    begin
      BlockPathX.Position:=Round(X/Factor);
      BlockPathY.Position:=Round(Y/Factor);
      BlockPathZ.Position:=Round(Z/Factor);

      PathX.Caption:=FormatFloat('0.###',X);
      PathY.Caption:=FormatFloat('0.###',Y);
      PathZ.Caption:=FormatFloat('0.###',Z);
    end;

    if Current is TPointXYZColor then
    begin
      BlockPathColor.LinkProperty(Current,'Color');
      BlockPathDefault.Checked:=TPointXYZColor(Current).Color=clDefault;
    end
    else
    begin
      BlockPathColor.Hide;
      BlockPathDefault.Hide;
    end;
  end
  else
  begin
    BlockPathX.Position:=0;
    BlockPathY.Position:=0;
    BlockPathZ.Position:=0;

    PathX.Caption:='0';
    PathY.Caption:='0';
    PathZ.Caption:='0';
  end;

  EnableControls(tmpOk,[BlockPathX,BlockPathY,BlockPathZ]);

  IModifying:=False;
end;

procedure TPointEditor.BlockPathXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.X:=BlockPathX.Position*Factor;
    PathX.Caption:=FormatFloat('0.###',Current.Point.X);
    MarkDirty;
  end;
end;

procedure TPointEditor.BlockPathYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Y:=BlockPathY.Position*Factor;
    PathY.Caption:=FormatFloat('0.###',Current.Point.Y);
    MarkDirty;
  end;
end;

procedure TPointEditor.BlockPathZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Z:=BlockPathZ.Position*Factor;
    PathZ.Caption:=FormatFloat('0.###',Current.Point.Z);
    MarkDirty;
  end;
end;

procedure TPointEditor.BlockPathColorClick(Sender: TObject);
begin
  BlockPathDefault.Checked:=False;
  MarkDirty;
end;

procedure TPointEditor.BlockPathDefaultClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TPointXYZColor(Current) do
    if BlockPathDefault.Checked then
       Color:=clDefault
    else
       Color:=BlockPathColor.SymbolColor;

    BlockPathColor.Invalidate;

    MarkDirty;
  end;
end;

procedure TPointEditor.FormCreate(Sender: TObject);
begin
  Factor:=1;
end;

procedure TPointEditor.FormShow(Sender: TObject);
begin
  if Tag<>0 then
     SelectPoint(TPointXYZFloat(Tag));
end;

end.
