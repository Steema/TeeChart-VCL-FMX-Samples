unit Function_Lagrange;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, ExtCtrls, StdCtrls, ComCtrls,
  Graphics, Controls, Forms, Dialogs, Base,

  TeeGDIPlus, TeeProcs, TeEngine, Chart, Series,

  CurvFitt;  // <-- CurvFitt is the unit with TLagrandeFunction

type
  TLagrange_Function = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Lagrange : TLagrangeFunction;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLagrange_Function.FormShow(Sender: TObject);
begin
  inherited;

  // Add some sample data
  Series1.Clear;
  Series1.AddXY ( -9, 5);
  Series1.AddXY ( -4, 2);
  Series1.AddXY ( -1,-2);
  Series1.AddXY (  7, 9);

  // Create Lagrange function (can be also done at design-time, editor->functions)
  Lagrange := TLagrangeFunction.Create(Self);
  Lagrange.AutoRange  := False;
  Lagrange.StartRange := -10;
  Lagrange.EndRange   :=  10;

  // Set Lagrange function to Series2
  Series2.FunctionType := Lagrange;

  Series2.DataSource := Series1;
  Series2.CheckDataSource;
end;

procedure TLagrange_Function.TrackBar1Change(Sender: TObject);
begin
  Lagrange.StartRange := TrackBar1.Position;
  Lagrange.ReCalculate;
end;

procedure TLagrange_Function.TrackBar2Change(Sender: TObject);
begin
  Lagrange.EndRange := TrackBar2.Position;
  Lagrange.ReCalculate;
end;

procedure TLagrange_Function.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues(4);
end;

initialization
  RegisterClass(TLagrange_Function);
end.
