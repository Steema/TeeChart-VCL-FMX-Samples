unit Export_XAML;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeXAMLCanvas;

type
  TExportXAML = class(TBaseForm)
    Button1: TButton;
    Button2: TButton;
    Series1: TBarSeries;
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

uses
  TeExport;

procedure TExportXAML.Button1Click(Sender: TObject);
begin
  TeeSavePanel(TXAMLExportFormat,Chart1);
end;

procedure TExportXAML.Button2Click(Sender: TObject);
begin
  TeeExport(Self,Chart1);
end;

procedure TExportXAML.FormCreate(Sender: TObject);
begin
  inherited;

  with Chart1 do
  begin
    Gradient.Visible:=True;
    Gradient.MidColor:=clGreen;
    Title.Font.Style:=[fsBold];
  end;
end;

initialization
  RegisterClass(TExportXAML);
end.
