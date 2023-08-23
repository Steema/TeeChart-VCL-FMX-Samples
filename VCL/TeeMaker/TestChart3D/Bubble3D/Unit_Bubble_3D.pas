unit Unit_Bubble_3D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeeEdit, TeEngine, Series, BubbleCh, TeCanvas, TeeBlocks,
  TeeChartBlock, TeeBlockEditor, TeeBlockGallery, TeePoEdi, TeeProcs, TeeMakerControl,
  TeeChart3D, ExtCtrls, TeeComma, TeeBarEdit, StdCtrls, TeeMakerEditor,
  Chart, TeeAnimateEditor, TeeBlockCanvas, TeeEdiWall;

type
  TForm1 = class(TForm)
    TeeCommander1: TTeeCommander;
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Series1: TBubbleSeries;
    ChartEditorPanel1: TChartEditorPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeeObjFormat;

procedure TForm1.FormCreate(Sender: TObject);
var tmp : TObjBlock;
    tmpS : TChartSeries;
begin
  ChartEditorPanel1.Chart:=ChartBlock1.Chart;

  tmp:=TObjBlock.Create(Self);
  tmp.LinkFile:='$(TEEMAKER)\Obj\General\ducky.obj';

  tmpS:=Series1;
  ChangeSeriesType(tmpS,THorizLineSeries);

  with (tmpS as THorizLineSeries) do
  begin
    Pointer.Size:=4;
    FillSampleValues(4);
  end;

//  Series1.Visuals.Template:=tmp;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

end.
