unit Export_Charts;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TExportChartsForm = class(TBaseForm)
    Series1: TLineSeries;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses 
  TeeJPEG,
  TeeGIF,
  TeePNG,

  {$IFNDEF CPUX64}
  TeePCX,
  {$ENDIF}

  TeePDFCanvas,
  TeeSVGCanvas,
  TeePSCanvas,
  TeeVMLCanvas,
  TeeHTML5Canvas,
  TeExport;

procedure TExportChartsForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(20);
end;

procedure TExportChartsForm.Button1Click(Sender: TObject);
begin
  TeeExport(Self,Chart1);
end;

initialization
  RegisterClass(TExportChartsForm);
end.
