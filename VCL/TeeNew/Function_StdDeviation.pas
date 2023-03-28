unit Function_StdDeviation;
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
  TStdDeviationForm = class(TBaseForm)
    Series1: TLineSeries;
    Series2: TLineSeries;
    TeeFunction1: TStdDeviationFunction;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TStdDeviationForm.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex=0 then TeeFunction1.Complete:=False
                           else TeeFunction1.Complete:=True;
end;

procedure TStdDeviationForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(30);
end;

procedure TStdDeviationForm.FormShow(Sender: TObject);
begin
  inherited;
  ComboBox1.ItemIndex:=0;
end;

procedure TStdDeviationForm.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues(30);
end;

initialization
  RegisterClass(TStdDeviationForm);
end.
