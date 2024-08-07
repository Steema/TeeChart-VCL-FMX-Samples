unit Function_Cumulative;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeeCumu, Series, TeeProcs, Chart;

type
  TCumulativeForm = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TLineSeries;
    TeeFunction1: TCumulative;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCumulativeForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.AddArray([7,5,6,8,2,1,9,3,4,1]);
end;

initialization
  RegisterClass(TCumulativeForm);
end.
