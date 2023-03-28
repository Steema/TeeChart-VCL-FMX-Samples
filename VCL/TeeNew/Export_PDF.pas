unit Export_PDF;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  jpeg,
  Base, TeEngine, Series, GanttCh, TeeProcs, Chart, ArrowCha, BubbleCh,
  OHLChart, CandleCh,
  TeePDFOptions;  // <-- To show the PDF Export editor dialog

type
  TPDFExportForm = class(TBaseForm)
    Button1: TButton;
    Button2: TButton;
    Series1: TLineSeries;
    Series2: TCandleSeries;
    Series3: TBubbleSeries;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeePDFCanvas, TeExport;

procedure TPDFExportForm.Button1Click(Sender: TObject);
begin
  TeeSavePanel(TPDFExportFormat,Chart1);
end;

procedure TPDFExportForm.Button2Click(Sender: TObject);
begin
  TeeExport(Self,Chart1);
end;

procedure TPDFExportForm.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.Color := clWhite;
  Chart1.BevelOuter := bvNone;
  Series1.FillSampleValues(7);
  Series2.FillSampleValues(7);
  Series3.FillSampleValues(7);
end;

procedure TPDFExportForm.Chart1AfterDraw(Sender: TObject);
begin
  Chart1.Canvas.Pie(100,100,150,300,150,100,200,200);
end;

initialization
  RegisterClass(TPDFExportForm);
end.
