{********************************************}
{ TeeMaker 2.0                               }
{ Copyright (c) 2002-2026 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeColorPalette;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,

  {$IFDEF D17}
  System.Types, System.UITypes,
  {$ENDIF}

  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs, TeeDraw3D, TeeGDIPlus;

type
  TColorPalette = class(TForm)
    TrackBar1: TTrackBar;
    ColorPalette: TDraw3D;
    Panel2: TPanel;
    LabelRGB: TLabel;
    LabelHLS: TLabel;
    ShapeColor: TButtonColor;
    PanelOK: TPanel;
    Button1: TButton;
    Button2: TButton;
    SBTransp: TScrollBar;
    procedure ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ColorPaletteAfterDraw(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ShapeColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ColorPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorPaletteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBTranspChange(Sender: TObject);
  private
    { Private declarations }
    FOnCurrentChanged : TNotifyEvent;

    ICurrent       : TColor;
    IDragging      : Boolean;

    ChangingCurrent: Boolean;
    ColorPaletteOk : Boolean;
    CrossPen       : TTeePen;

    function ApplyAlpha(const AColor:TColor):TColor;
    function ColorPosition(const AColor:TColor):TPoint;
    procedure DrawCross;
    function MouseUnderCurrent:Boolean;
    procedure SetCurrent(AColor:TColor);
    procedure SetShapeColor(const AColor:TColor; SetTrackLum:Boolean=False);
  public
    { Public declarations }

    MouseOver:Boolean;

    class function Edit(AOwner:TComponent; const AColor:TColor):TColor;

    function Selected:TColor;
    property CurrentColor:TColor read ICurrent write SetCurrent;
    property OnCurrentChanged:TNotifyEvent read FOnCurrentChanged
                                           write FOnCurrentChanged;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeConst, TeeFilters;

procedure TColorPalette.ColorPaletteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var tmp : TColor;
begin
  if IDragging then
  begin
    DrawCross;

    ICurrent:=ApplyAlpha(HLSToColor(X,TrackBar1.Position,255-Y));

    SetShapeColor(ICurrent);

    DrawCross;

    if Assigned(FOnCurrentChanged) then
       FOnCurrentChanged(Self);
  end
  else
  begin
    with TTeeCanvas3D(ColorPalette.Canvas) do
    if Assigned(Bitmap) then
    begin
      tmp:=Bitmap.Canvas.Pixels[X,Y];
      tmp:=ApplyAlpha(tmp);
      SetShapeColor(tmp);
    end;

    if MouseUnderCurrent then
    begin
      ColorPalette.Cursor:=crHandPoint;
      ColorPalette.DragMode:=dmManual;
    end
    else
    if ColorPalette.Cursor=crHandPoint then
    begin
      ColorPalette.Cursor:=crDefault;
      ColorPalette.DragMode:=dmAutomatic;
    end;
  end;
end;

function TColorPalette.MouseUnderCurrent:Boolean;
begin
  if CurrentColor<>clNone then
  with ColorPosition(CurrentColor) do
    result:=PointInRect(TeeRect(x-4,y-4,x+4,y+4),ColorPalette.GetCursorPos)
  else
    result:=False;
end;

function TColorPalette.ColorPosition(const AColor:TColor):TPoint;
var h,l,s : Word;
begin
  with RGBValue(AColor) do
  if (Blue=Green) and (Blue=Red) then
  begin
    result.X:=Blue div 15;
    result.Y:=256+7;
  end
  else
  begin
    ColorToHLS(AColor,h,l,s);
    result.X:=h;
    result.Y:=255-s;
  end;
end;

procedure TColorPalette.DrawCross;
var tmp : TPoint;
begin
  Exit;

  with ColorPalette.Canvas do
  begin
    AssignVisiblePen(CrossPen);

    tmp:=ColorPosition(CurrentColor);
    Line(tmp.X-4,tmp.Y,tmp.X+4,tmp.Y);
    Line(tmp.X,tmp.Y-4,tmp.X,tmp.Y+4);
  end;
end;

procedure TColorPalette.ColorPaletteAfterDraw(Sender: TObject);

  procedure DrawColorPalette;
  var h,l,s : Integer;
      tmp : {$IFDEF CLX}TRGBAArray{$ELSE}TRGBArray{$ENDIF};
      c   : TRGB;
  begin
    l:=TrackBar1.Position;

    with TTeeCanvas3D(ColorPalette.Canvas) do
    begin
      TeeCalcLines(tmp,Bitmap);

      {$IFOPT R+}
      {$DEFINE WASRANGE}
      {$R-}
      {$ENDIF}

      for h:=0 to 255 do
          for s:=0 to 255 do
              tmp[255-s,h]:=RGBValue(HLSToColor(h,l,s));

      for s:=0 to 255 do
      begin
        c:=RGBValue(RGB(s,s,s));

        for h:=0 to 15 do
            tmp[256+h,s]:=c;
      end;

      {$IFDEF WASRANGE}
      {$R+}
      {$ENDIF}
      
      tmp:=nil;
    end;
  end;

begin
  if not ColorPaletteOk then
  begin
    DrawColorPalette;
    ColorPaletteOk:=True;
  end;

  if CurrentColor<>clNone then
     DrawCross;
end;

procedure TColorPalette.TrackBar1Change(Sender: TObject);
begin
  ColorPaletteOk:=False;

  if (not ChangingCurrent) and (CurrentColor<>clNone) then
  begin
    with ColorPosition(ICurrent) do
      ICurrent:=ApplyAlpha(HLSToColor(x,TrackBar1.Position,255-y));

    if Assigned(FOnCurrentChanged) then
       FOnCurrentChanged(Self);
  end;

  ColorPalette.Invalidate;
end;

procedure TColorPalette.SetShapeColor(const AColor:TColor; SetTrackLum:Boolean=False);
var tmpHue,
    tmpLum,
    tmpSat : Word;
begin
  ShapeColor.SymbolColor:=AColor;
  ShapeColor.Invalidate;

  with RGBValue(AColor) do
       LabelRGB.Caption:=Format(TeeMsg_RGB,[TeeStr(Red),TeeStr(Green),TeeStr(Blue)])+
                   ' '+IntToStr(ColorPalette.Canvas.ColorAlpha(AColor));

  ColorToHLS(AColor,tmpHue,tmpLum,tmpSat);

  if SetTrackLum and (AColor<>clWhite) and (AColor<>clBlack) then
     TrackBar1.Position:=tmpLum;

  LabelHLS.Caption:=Format(TeeMsg_HLS,[TeeStr(tmpHue),TeeStr(tmpLum),TeeStr(tmpSat)]);
end;

procedure TColorPalette.ShapeColorClick(Sender: TObject);
begin
  SetShapeColor(ShapeColor.SymbolColor);
end;

procedure TColorPalette.FormCreate(Sender: TObject);
begin
  ICurrent:=clNone;

  MouseOver:=True;

  ColorPalette.Width:=256;
  ColorPalette.Height:=256+16;

  CrossPen:=TTeePen.Create(nil);

  with CrossPen do
  begin
    Mode:=pmXor;
    Color:=clWhite;
    Width:=2;
    Style:=psSolid;
    EndStyle:=esFlat;
  end;
end;

function TColorPalette.Selected: TColor;
begin
  result:=ShapeColor.SymbolColor;
end;

procedure TColorPalette.SetCurrent(AColor: TColor);
begin
  ChangingCurrent:=True;

  AColor:=ColorToRGB(AColor);
  SetShapeColor(AColor,True);
  ICurrent:=AColor;

  SBTransp.Position:=Round(ColorPalette.Canvas.ColorAlpha(ICurrent)/2.55);

  ChangingCurrent:=False;

  ColorPaletteOk:=False;
  ColorPalette.Invalidate;
end;

procedure TColorPalette.FormDestroy(Sender: TObject);
begin
  CrossPen.Free;
end;

procedure TColorPalette.ColorPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if MouseOver and MouseUnderCurrent then
     IDragging:=True;
end;

procedure TColorPalette.ColorPaletteMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  IDragging:=False;
end;

class function TColorPalette.Edit(AOwner: TComponent;
  const AColor: TColor): TColor;
begin
  with TColorPalette.Create(AOwner) do
  try
    //PositionToCenter

    PanelOK.Visible:=True;
    MouseOver:=False;

    CurrentColor:=AColor;

    if ShowModal=mrOk then
       result:=CurrentColor
    else
       result:=AColor;
  finally
    Free;
  end;
end;

function TColorPalette.ApplyAlpha(const AColor:TColor):TColor;
begin
  result:=ColorPalette.Canvas.ColorFrom(AColor,Round(SBTransp.Position*2.55));
end;

procedure TColorPalette.SBTranspChange(Sender: TObject);
begin
  ICurrent:=ApplyAlpha(ICurrent);
  SetShapeColor(ICurrent);
end;

end.
