unit Function_Kurtosis;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, CurvFitt, Series, TeeProcs, Chart, TeeTools, StatChar;

type
  TKurtosisForm = class(TBaseForm)
    Series1: TAreaSeries;
    Button1: TButton;
    ChartTool1: TMarksTipTool;
    Series2: TLineSeries;
    TeeFunction1: TKurtosisFunction;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KurtosisForm: TKurtosisForm;  

implementation

{$R *.dfm}

procedure TKurtosisForm.FormCreate(Sender: TObject);
begin
  inherited;
  Button1Click(Self);
end;

procedure TKurtosisForm.Button1Click(Sender: TObject);
begin
  Series1.Clear;
  Series1.FillSampleValues(20);
end;

initialization
  RegisterClass(TKurtosisForm);
end.
