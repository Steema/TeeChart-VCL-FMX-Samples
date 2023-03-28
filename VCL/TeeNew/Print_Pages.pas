unit Print_Pages;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ExtDlgs,
  Base, Chart, TeEngine, Series, TeeProcs, TeeEdiGene, TeePreviewPanel,
  TeeNavigator;

type
  TPrintPagesForm = class(TBaseForm)
    Button1: TButton;
    Series1: TBarSeries;
    ChartPageNavigator1: TChartPageNavigator;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Chart1PageChange(Sender: TObject);
  private
    { Private declarations }
    PrintDialog1: TPrintDialog;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPrintPagesForm.Button1Click(Sender: TObject);
begin
  With PrintDialog1 do
  begin
    FromPage:=1;
    ToPage:=Chart1.NumPages;
    MinPage:=FromPage;
    MaxPage:=ToPage;
    if Execute then Chart1.PrintPages(FromPage,ToPage);
  end;
end;

procedure TPrintPagesForm.FormCreate(Sender: TObject);
begin
  inherited;

  PrintDialog1:=TPrintDialog.Create(Self);
  PrintDialog1.Options:=[poPageNums];

  Series1.FillSampleValues(20);
  ChartPageNavigator1.EnableButtons;
  Chart1PageChange(Self);
end;

procedure TPrintPagesForm.Chart1PageChange(Sender: TObject);
begin
  Label2.Caption:=IntToStr(Chart1.Page)+' of '+IntToStr(Chart1.NumPages);
end;

initialization
  RegisterClass(TPrintPagesForm);
end.
