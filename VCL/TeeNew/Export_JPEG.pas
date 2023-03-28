unit Export_JPEG;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, GanttCh, TeeProcs, Chart, ArrowCha;

type
  TJPEGExportForm = class(TBaseForm)
    Button1: TButton;
    Button2: TButton;
    Series1: TArrowSeries;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeJPEG, TeExport;

procedure TJPEGExportForm.Button1Click(Sender: TObject);
begin
  TeeSavePanel(TJPEGExportFormat,Chart1);
end;

procedure TJPEGExportForm.Button2Click(Sender: TObject);
begin
  TeeExport(Self,Chart1);
end;

procedure TJPEGExportForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

initialization
  RegisterClass(TJPEGExportForm);
end.
