unit Series_Squarified;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, TeeProcs, TeEngine, Chart, StdCtrls, ExtCtrls,
  Base, TeeSquarifiedMap;

type
  TSquarifiedBasic = class(TBaseForm)
    PanelLeft: TPanel;
    MemoData: TMemo;
    CBMarks: TCheckBox;
    CBBorders: TCheckBox;
    CBHorizInverted: TCheckBox;
    CBUsePalette: TCheckBox;
    Panel2: TPanel;
    procedure MemoDataChange(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
    procedure CBBordersClick(Sender: TObject);
    procedure CBHorizInvertedClick(Sender: TObject);
    procedure CBUsePaletteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Series1 : TSquarifiedMapSeries;

    procedure AddData;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSquarifiedBasic.MemoDataChange(Sender: TObject);
begin
  AddData;
end;

procedure TSquarifiedBasic.CBMarksClick(Sender: TObject);
begin
  Series1.Marks.Visible:=CBMarks.Checked;
end;

procedure TSquarifiedBasic.CBBordersClick(Sender: TObject);
begin
  Series1.Pen.Visible:=CBBorders.Checked;
end;

procedure TSquarifiedBasic.CBHorizInvertedClick(Sender: TObject);
begin
  if CBHorizInverted.Checked then
     Series1.Inverted:=siHorizontal
  else
     Series1.Inverted:=siNone;
end;

procedure TSquarifiedBasic.CBUsePaletteClick(Sender: TObject);
begin
  Series1.Palette.UsePalette:=CBUsePalette.Checked;

  Series1.Palette.UseColorRange:=not CBUsePalette.Checked;
  Series1.ColorEachPoint:=not CBUsePalette.Checked;
end;

procedure TSquarifiedBasic.FormCreate(Sender: TObject);

  procedure CreateChartAndSeries;
  begin
    Chart1.View3D := False;

    Series1 := TSquarifiedMapSeries.Create(Self);
    Series1.ParentChart := Chart1;
  end;

begin
  CreateChartAndSeries;

  AddData;
end;

// Returns the two parts of a string (text and value), eg:  abc 123
function GetTextAndValue(const S:String; out AText:String; out AValue:Double):Boolean;
var i : Integer;
    tmp : String;
begin
  result:=False;

  i:=Pos(' ',S);

  if i>0 then
  begin
    AText:= Trim(Copy(S,1,i-1));
    tmp:= Trim(Copy(S,i+1,Length(s)));

    result:=TryStrToFloat(tmp,AValue);
  end;
end;

procedure TSquarifiedBasic.AddData;

  procedure AddText(const ALines:TStrings);
  var t : Integer;
      tmpText : String;
      tmpValue : Double;
  begin
    for t:=0 to ALines.Count-1 do
        if GetTextAndValue(ALines[t],tmpText,tmpValue) then
           Series1.AddItem(0,tmpValue,tmpText);
  end;

begin
  Series1.BeginUpdate;
  try
    Series1.Clear;

    // Add the top-most item:
    Series1.AddItem(-1,0,'Demo');

    // Add children:
    AddText(MemoData.Lines);

    // Or manually add items:

    // Series1.AddItem(0, 6, 'a');
    // Series1.AddItem(0, 2, 'd');
    // Series1.AddItem(0, 1, 'e');
    // Series1.AddItem(0, 6, 'b');
    // Series1.AddItem(0, 3, 'f');
    // Series1.AddItem(0, 2, 'g');
    // Series1.AddItem(0, 4, 'c');

    Series1.FinishData;
  finally
    Series1.EndUpdate;
  end;
end;

initialization
  RegisterClass(TSquarifiedBasic);
end.
