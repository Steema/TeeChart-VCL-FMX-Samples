unit Series_LCDGauges;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Base, TeeLinearGauge, TeEngine, TeeNumericGauge, TeeProcs, Chart,
  EditChar, TeeTools, TeeLinkTool;

type
  TLEDGauges = class(TBaseForm)
    Series3: TLinearGauge;
    Series4: TLinearGauge;
    Button1: TButton;
    Button2: TButton;
    ChartTool1: TLinkTool;
    Series1: TNumericGauge;
    Series2: TNumericGauge;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLEDGauges.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

procedure TLEDGauges.Button2Click(Sender: TObject);
begin
  Chart1.SeriesList.FillSampleValues;
end;

initialization
  RegisterClass(TLEDGauges);
end.
