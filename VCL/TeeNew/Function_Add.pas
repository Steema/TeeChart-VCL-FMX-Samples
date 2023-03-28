unit Function_Add;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeeFunci, Series, TeeProcs, Chart;

type
  TAddForm = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TLineSeries;
    CheckBox1: TCheckBox;
    TeeFunction1: TAddTeeFunction;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAddForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then TeeFunction1.Period:=2
                       else TeeFunction1.Period:=0; { all points }
end;

procedure TAddForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);
end;

initialization
  RegisterClass(TAddForm);
end.
