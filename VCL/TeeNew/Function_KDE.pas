unit Function_KDE;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeEngine, Series, TeeProcs, Chart, ExtCtrls,
  StdCtrls, StatChar;

type
  TKDEFunctionForm = class(TBaseForm)
    Series1: TLineSeries;
    Chart2: TChart;
    Series2: TLineSeries;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    KDE : TKDEFunction;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeFuncEdit;

procedure TKDEFunctionForm.FormShow(Sender: TObject);
begin
  inherited;

  // Create KDE function
  KDE:=TKDEFunction.Create(Self);
  KDE.ParentSeries:=Series2;

  Series2.DataSource:=Series1;

  // Add random data
  Button1Click(Self);
end;

procedure TKDEFunctionForm.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues;
end;

procedure TKDEFunctionForm.Button2Click(Sender: TObject);
begin
  TTeeFuncEditor.EditFunction(Self,KDE);
end;

initialization
  RegisterClass(TKDEFunctionForm);
end.
