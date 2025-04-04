unit Function_Correlation;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, CurvFitt, TeEngine, Series, TeeProcs, Chart;

type
  TCorrelationFunctionDemo = class(TBaseForm)
    Series1: TPointSeries;
    Series2: TLineSeries;
    TeeFunction1: TCorrelationFunction;
    CheckBox1: TCheckBox;
    Series3: TLineSeries;
    TeeFunction2: TTrendFunction;
    Button1: TButton;
    Label1: TLabel;
    Coeficient: TLabel;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCorrelationFunctionDemo.CheckBox1Click(Sender: TObject);
begin
  Series3.Visible:=CheckBox1.Checked;
end;

procedure TCorrelationFunctionDemo.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues;

  Coeficient.Caption:=FloatToStr(TeeFunction1.Coefficient(Series1));
end;

procedure TCorrelationFunctionDemo.FormCreate(Sender: TObject);
begin
  inherited;
  Button1Click(Self);
end;

initialization
  RegisterClass(TCorrelationFunctionDemo);
end.
