unit Function_Gator;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  {$ENDIF}
  Base, TeEngine, TeCanvas, Series, OHLChart, CandleCh, TeeProcs, Chart,
  StatChar;

type
  TGatorFunctionForm = class(TBaseForm)
    Series1: TCandleSeries;
    Button1: TButton;
    Series2: TLineSeries;
    TeeFunction1: TAlligatorFunction;
    Series3: TVolumeSeries;
    TeeFunction2: TGatorFunction;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GatorFunctionForm: TGatorFunctionForm;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

procedure TGatorFunctionForm.FormCreate(Sender: TObject);
begin
  inherited;
  Button1Click(Self);
end;

procedure TGatorFunctionForm.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues(200);
end;

initialization
  RegisterClass(TGatorFunctionForm);
end.
