unit Tool_TransposeSeries;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools, TeeTransposeSeries;

type
  TTransposeSeriesToolForm = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TBarSeries;
    Series3: TBarSeries;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    TransposeSeries : TTransposeSeriesTool;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTransposeSeriesToolForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.MultiBar:=mbNone;

  Series3.SetNull(5);
  Series3.Delete(5);

  TransposeSeries:=TTransposeSeriesTool.Create(Self);
  TransposeSeries.ParentChart:=Chart1;
end;

procedure TTransposeSeriesToolForm.Button1Click(Sender: TObject);
begin
  TransposeSeries.Transpose;
end;

initialization
  RegisterClass(TTransposeSeriesToolForm);
end.
