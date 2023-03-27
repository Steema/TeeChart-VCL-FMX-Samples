unit Export_Chart_as_String;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls,
  Series, ComCtrls;

type
  TExportChartString = class(TBaseForm)
    Splitter1: TSplitter;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Series1: TBarSeries;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportChartString: TExportChartString;

implementation

{$R *.dfm}

uses
  TeeStore;

procedure TExportChartString.Button1Click(Sender: TObject);
begin
  Memo2.Lines.Text:=SaveChartToString(Chart1);

  Memo3.Lines.Text:=TSeriesDataText.From(Chart1);
end;

procedure TExportChartString.Button2Click(Sender: TObject);
begin
  LoadChartFromString(Chart1,Memo2.Lines.Text);
end;

initialization
  RegisterClass(TExportChartString);
end.
