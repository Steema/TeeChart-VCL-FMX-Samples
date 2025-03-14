unit Histogram_Transparency;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, StatChar, TeeProcs, Chart;

type
  THistogramTransp = class(TBaseForm)
    Series1: THistogramSeries;
    Series2: THistogramSeries;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure THistogramTransp.Edit1Change(Sender: TObject);
begin
  Series2.Transparency:=UpDown1.Position;
end;

procedure THistogramTransp.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.SeriesList.FillSampleValues(25);

  UpDown1.Position:=Series2.Transparency;
end;

initialization
  RegisterClass(THistogramTransp);
end.
