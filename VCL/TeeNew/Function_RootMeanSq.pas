unit Function_RootMeanSq;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, StatChar, Series, TeeProcs, Chart;

type
  TRootMeanSqForm = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    TeeFunction1: TRMSFunction;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    Procedure DisplayCalc;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TRootMeanSqForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(20);
  DisplayCalc;
end;

procedure TRootMeanSqForm.CheckBox1Click(Sender: TObject);
begin
  TeeFunction1.Complete:=CheckBox1.Checked;
  DisplayCalc;
end;

Procedure TRootMeanSqForm.DisplayCalc;
begin
  Label2.Caption:=FormatFloat('#.##',Series2.YValues[0]);
end;

initialization
  RegisterClass(TRootMeanSqForm);
end.
