unit Function_Trend;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, CurvFitt, Series, TeeProcs, Chart, TeeTools;

type
  TTrendForm = class(TBaseForm)
    Series1: TAreaSeries;
    Series2: TLineSeries;
    TeeFunction1: TTrendFunction;
    Button1: TButton;
    ChartTool1: TMarksTipTool;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTrendForm.FormCreate(Sender: TObject);
begin
  inherited;
  Button1Click(Self);
end;

procedure TTrendForm.Button1Click(Sender: TObject);
begin
  Series1.Clear;
  Series1.FillSampleValues(20);
end;

initialization
  RegisterClass(TTrendForm);
end.
