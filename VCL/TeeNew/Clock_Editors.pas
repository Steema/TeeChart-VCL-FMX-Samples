unit Clock_Editors;
{$I TeeDefs.inc}

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, Series, MyPoint, TeEngine, TeePolar, TeeRose, TeeProcs, Chart;

type
  TClockEdi = class(TBaseForm)
    Chart2: TChart;
    Series1: TClockSeries;
    Series2: TMyPointSeries;
    Button1: TButton;
    Button2: TButton;
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

Uses EditChar;

procedure TClockEdi.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TClockEdi.Button2Click(Sender: TObject);
begin
  EditSeries(Self,Series2);
end;

procedure TClockEdi.FormCreate(Sender: TObject);
begin
  inherited;
  Series2.FillSampleValues(6);
end;

initialization
  RegisterClass(TClockEdi);
end.
