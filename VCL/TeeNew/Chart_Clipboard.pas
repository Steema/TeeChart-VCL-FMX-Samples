unit Chart_Clipboard;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series;

type
  TChartClipboard = class(TBaseForm)
    Button1: TButton;
    Series1: TPointSeries;
    Series2: TLineSeries;
    Series3: TFastLineSeries;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartClipboard.Button1Click(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;

  // custom size:
  // Chart1.CopyToClipboardBitmap( Rect( 0,0, 400, 200 ) );
end;

procedure TChartClipboard.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.SeriesList.FillSampleValues(10);
end;

initialization
  RegisterClass(TChartClipboard);
end.
