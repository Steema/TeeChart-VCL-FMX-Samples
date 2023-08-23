unit Unit_Test_Chart3D;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeCanvas, TeeBlocks, TeeChartBlock, ExtCtrls,
  TeeProcs, TeeMakerControl, TeeChart3D, Chart, EditChar, Series, TeEngine,
  TeeExtruded, TeeMakerEditor, TeeGLUT, BubbleCh, TeeChartGrid,
  TeeLinePointEditor, TeeEditCha, CandleCh, TeeCandlEdi, TeeEdiWall,
  TeeMakerConst, TeeConst, Menus;

type
  TForm1 = class(TForm)
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ScrollBar1: TScrollBar;
    Object1: TObjectBlock;
    PopupMenu1: TPopupMenu;
    procedure Chart3D1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Chart3D1Click(Sender: TObject);
begin
  Chart3D1.SetFocus;
end;

procedure TForm1.FormCreate(Sender: TObject);
var tmp : String;
begin
  tmp:=ExtractFilePath(Application.ExeName);
  ChartBlock1.TemplateFile:=tmp+'\default.hou';

  //Chart3D1.Options.Floor.Visible:=False;

  //Chart3D1.Blocks.LibraryPath:='c:\root\teechartvcl\demos9\teeMaker\library';

//  ChartBlock1.Chart.AddSeries(TBubbleSeries).FillSampleValues(6);
  ChartBlock1.Chart.AddSeries(TCandleSeries).FillSampleValues(6);

  TCandleSeries(ChartBlock1.Chart[0]).Pointer.Pen.Visible:=False;
  TCandleSeries(ChartBlock1.Chart[0]).CandleDepth:=20;
  TCandleSeries(ChartBlock1.Chart[0]).CandleWidth:=40;

//  (ChartBlock1.Chart[0] as TPointSeries).Pointer.Style:=psVisual;

// (ChartBlock1.Chart[0] as TPointSeries).Pointer.Size:=20;
// (ChartBlock1.Chart[0] as TPointSeries).Pointer.Depth:=20;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart3D1.Chart);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,ChartBlock1.TemplateFile);
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
(ChartBlock1.Chart[0] as TPointSeries).Pointer.Size:=ScrollBar1.Position;
(ChartBlock1.Chart[0] as TPointSeries).Pointer.Depth:=ScrollBar1.Position;
end;

end.
