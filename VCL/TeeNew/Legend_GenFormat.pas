unit Legend_GenFormat;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages, 
  {$ENDIF}
  SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeeDonut;

type
  TLegendGenFormat = class(TBaseForm)
    PageControl1: TPageControl;
    Series1: TDonutSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}


Uses TeeCustomShapeEditor;

procedure TLegendGenFormat.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

procedure TLegendGenFormat.FormShow(Sender: TObject);
begin
  inherited;
  TFormTeeShape.InsertObjectForm(PageControl1,Chart1.Legend).RefreshControls(Chart1.Legend);
end;

initialization
  RegisterClass(TLegendGenFormat);
end.
