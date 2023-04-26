unit All_Series;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TeeProcs, TeEngine, Chart;

type
  TAllSeriesForm = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }

    procedure AddAllStyles;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeTools;

procedure TAllSeriesForm.FormCreate(Sender: TObject);
begin
  ListBox1.ItemHeight:=24;
  AddAllStyles;

  ListBox1.Sorted:=True;

  ListBox1.ItemIndex:=8; // Bar style
  ListBox1Click(Self);
end;

procedure TAllSeriesForm.AddAllStyles;
var t : Integer;
begin
  ListBox1.Items.BeginUpdate;

  for t:=0 to TeeSeriesTypes.Count-1 do
      if TeeSeriesTypes[t].FunctionClass=nil then
         ListBox1.Items.AddObject(TeeSeriesTypes[t].Caption,TObject(t));

  ListBox1.Items.EndUpdate;
end;

type
  TSeriesAccess=class(TChartSeries);

procedure TAllSeriesForm.ListBox1Click(Sender: TObject);
var tmpClass : TChartSeriesClass;
    tmpSeries : TChartSeries;
    tmp,
    tmpSub : Integer;
begin
  tmp:=ListBox1.ItemIndex;

  if tmp=-1 then Exit;

  tmp:=Integer(ListBox1.Items.Objects[tmp]);

  Chart1.ClearChart;
  Chart1.View3D:=CheckBox1.Checked;

  tmpClass:=TeeSeriesTypes[tmp].SeriesClass;

  tmpSeries:=Chart1.AddSeries(tmpClass);

  tmpSub:=TeeSeriesTypes[tmp].SubIndex;

  if tmpSub<>0 then
     TSeriesAccess(tmpSeries).SetSubGallery(tmpSeries,tmpSub);

  tmpSeries.FillSampleValues;

  tmpSeries.ColorEachPoint:=True;

  Chart1.Title.Caption:=ListBox1.Items[ListBox1.ItemIndex];

  Chart1.Tools.Add(TMarksTipTool);
end;

procedure TAllSeriesForm.CheckBox1Click(Sender: TObject);
begin
  Chart1.View3D:=CheckBox1.Checked;
end;

procedure TAllSeriesForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var tmp : Integer;
    tmpBitmap : TBitmap;
    tmpClass : TClass;
begin
  ListBox1.Canvas.FillRect(Rect);

  tmp:=Integer(ListBox1.Items.Objects[Index]);

  TeeSeriesTypes[tmp].SeriesClass.ClassName;

  tmpBitmap:=TBitmap.Create;
  try
    tmpClass:=TeeSeriesTypes[tmp].SeriesClass;

    TeeLoadBitmap(tmpBitmap,tmpClass.ClassName,tmpClass.ClassParent.ClassName);
    ListBox1.Canvas.Draw(Rect.Left,Rect.Top,tmpBitmap);

    ListBox1.Canvas.TextOut(Rect.Left+26,Rect.Top+4,ListBox1.Items[Index]);
  finally
    tmpBitmap.Free;
  end;
end;

initialization
  RegisterClass(TAllSeriesForm);
end.
