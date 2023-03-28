unit Function_Custom;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeEngine, TeeFunci, Series, TeeProcs, Chart;

type
  TCustomFunctionDemo = class(TBaseForm)
    Series1: TLineSeries;
    TeeFunction1: TCustomTeeFunction;
    Button1: TButton;
    procedure TeeFunction1Calculate(Sender: TCustomTeeFunction;
      const x: Double; var y: Double);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses EditChar;

procedure TCustomFunctionDemo.TeeFunction1Calculate(Sender: TCustomTeeFunction;
  const x: Double; var y: Double);
begin
  y:=Sin(x/10);
end;

procedure TCustomFunctionDemo.Button1Click(Sender: TObject);
begin
  EditSeriesDataSource(Self,Series1);
end;

initialization
  RegisterClass(TCustomFunctionDemo);
end.
