unit Chart_Gallery;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeePenDlg, TeeProcs, TeEngine, Chart, Series, TeeGally;

type
  TChartGallery = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Gallery: TTeeGallery;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartGallery.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.Visible:=False;
  Panel1.Visible:=False;

  Gallery:=TTeeGallery.Create(Self);
  Gallery.P1.Visible:=False;

  With Gallery do
  begin
    Align:=alClient;
    BorderStyle:=bsNone;
    Parent:=Self;
  end;
end;

procedure TChartGallery.FormShow(Sender: TObject);
begin
  inherited;
  Gallery.Show;
end;

initialization
  RegisterClass(TChartGallery);
end.
