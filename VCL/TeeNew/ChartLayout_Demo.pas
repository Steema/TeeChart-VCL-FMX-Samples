unit ChartLayout_Demo;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  TeeChartLayout, ComCtrls;

type
  TChartLayoutDemo = class(TForm)
    ChartLayout1: TChartLayout;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    CBAutomatic: TCheckBox;
    Label2: TLabel;
    Edit2: TEdit;
    UDWidth: TUpDown;
    Label3: TLabel;
    Edit3: TEdit;
    UDHeight: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBAutomaticClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Creating : Boolean;
    
    procedure AddCharts;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeProcs, TeEngine, Series, Clipbrd;

procedure TChartLayoutDemo.AddCharts;
var t : Integer;
    tmp : TChartSeries;
begin
  for t:=0 to 15 do
  begin
    // Create a series
    tmp:=TBarSeries.Create(Self);
    tmp.FillSampleValues;

    // Set a different color for each series
    tmp.Color:=GetDefaultColor(t);

    // Add series to layout
    ChartLayout1.Add('Chart '+IntToStr(t)).AddSeries(tmp);
  end;
end;

procedure TChartLayoutDemo.FormShow(Sender: TObject);
begin
  UDWidth.Position:=ChartLayout1.ChartWidth;
  UDHeight.Position:=ChartLayout1.ChartHeight;

  AddCharts;

  Creating:=False;
end;

procedure TChartLayoutDemo.Button1Click(Sender: TObject);
var tmp : TBitmap;
begin
  tmp:=ChartLayout1.Bitmap;  // <-- all Charts into a single bitmap picture
  try
    Clipboard.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TChartLayoutDemo.CBAutomaticClick(Sender: TObject);
begin
  if CBAutomatic.Checked then
     ChartLayout1.Columns:=0; // 0 = Automatic

  Edit1.Enabled:=not CBAutomatic.Checked;
  UpDown1.Enabled:=Edit1.Enabled;
end;

procedure TChartLayoutDemo.Edit1Change(Sender: TObject);
begin
  ChartLayout1.Columns:=UpDown1.Position;
end;

procedure TChartLayoutDemo.Edit2Change(Sender: TObject);
begin
  if not Creating then
     ChartLayout1.ChartWidth:=UDWidth.Position;
end;

procedure TChartLayoutDemo.Edit3Change(Sender: TObject);
begin
  if not Creating then
     ChartLayout1.ChartHeight:=UDHeight.Position;
end;

procedure TChartLayoutDemo.FormCreate(Sender: TObject);
begin
  Creating:=True;
end;

initialization
  RegisterClass(TChartLayoutDemo);
end.
