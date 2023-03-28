unit Function_RVI;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeCanvas, Series, OHLChart, CandleCh, TeeProcs, Chart,
  StatChar;

type
  TRVIFunctionForm = class(TBaseForm)
    Series1: TCandleSeries;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Series2: TLineSeries;
    TeeFunction1: TRVIFunction;
    CheckBox1: TCheckBox;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RVIFunctionForm: TRVIFunctionForm;

implementation

{$R *.dfm}

procedure TRVIFunctionForm.Edit1Change(Sender: TObject);
begin
  TeeFunction1.Period:=UpDown1.Position;
  Chart1.Title.Text[0]:='RVI(' + FloatToStr(TeeFunction1.Period) + ')';
end;

procedure TRVIFunctionForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(300);
end;

procedure TRVIFunctionForm.CheckBox1Click(Sender: TObject);
begin
  TeeFunction1.Signal.Visible:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TRVIFunctionForm);
end.
