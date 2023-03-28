unit Chart_Transparent;
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
  TChartTransparent = class(TBaseForm)
    Button1: TButton;
    Series1: TPieSeries;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartTransparent.Button1Click(Sender: TObject);
begin
  { no border }
  Chart1.BevelOuter:=bvNone;

  { set transparent }
  Chart1.Color:=clNone;

  { copy to clipboard in metafile mode }
  Chart1.CopyToClipboardMetafile(True);

  { restore previous color }
  Chart1.Color:=clBtnFace;

  { previous border }
  Chart1.BevelOuter:=bvRaised;
end;

procedure TChartTransparent.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(9);
end;

initialization
  RegisterClass(TChartTransparent);
end.
