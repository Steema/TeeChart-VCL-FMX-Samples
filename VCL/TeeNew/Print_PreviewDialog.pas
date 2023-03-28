unit Print_PreviewDialog;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeEdit;

type
  TPrintPreviewDialog = class(TBaseForm)
    Button1: TButton;
    Series1: TBarSeries;
    ChartPreviewer1: TChartPreviewer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPrintPreviewDialog.Button1Click(Sender: TObject);
begin
  ChartPreviewer1.Execute;
end;

procedure TPrintPreviewDialog.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);
end;

initialization
  RegisterClass(TPrintPreviewDialog);
end.
