unit Legend_VertSpa;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, Bar3D, TeCanvas;

type
  TLegendVertSpa = class(TBaseForm)
    Edit1: TEdit;
    UpDown1: TUpDown;
    Label1: TLabel;
    Series1: TBar3DSeries;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLegendVertSpa.Edit1Change(Sender: TObject);
begin
  Chart1.Legend.VertSpacing:=UpDown1.Position;
end;

procedure TLegendVertSpa.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(5);
end;

initialization
  RegisterClass(TLegendVertSpa);
end.
