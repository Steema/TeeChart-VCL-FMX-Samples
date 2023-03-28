unit Function_Subtract;
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
  TSubtractForm = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TLineSeries;
    Series3: TBarSeries;
    TeeFunction1: TSubtractTeeFunction;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSubtractForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.AddArray([2,3,5,7,1,4]);
  Series3.AddArray([1,5,9,3,8,2]);
end;

initialization
  RegisterClass(TSubtractForm);
end.
