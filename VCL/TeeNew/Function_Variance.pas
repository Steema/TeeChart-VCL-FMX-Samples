unit Function_Variance;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, StatChar, Series;

type
  TVarianceDemo = class(TBaseForm)
    Series1: TPointSeries;
    Series2: TLineSeries;
    TeeFunction1: TVarianceFunction;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TVarianceDemo.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
end;

procedure TVarianceDemo.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues;
end;

initialization
  RegisterClass(TVarianceDemo);
end.
