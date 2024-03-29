unit Function_PVO;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeEngine, Series, CandleCh, TeeProcs, Chart, TeePVOFunction, StatChar;

type
  TPVOFuncdemo = class(TBaseForm)
    Button1: TButton;
    Series1: TVolumeSeries;
    Series2: TFastLineSeries;
    TeeFunction1: TPVOFunction;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses EditChar;

procedure TPVOFuncdemo.Button1Click(Sender: TObject);
begin
  EditSeriesDataSource(Self,Series2);
end;

procedure TPVOFuncdemo.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(100);
end;

initialization
  RegisterClass(TPVOFuncdemo);
end.
