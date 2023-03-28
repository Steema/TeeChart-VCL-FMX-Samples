unit Circled_Gradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeEngine, Series, TeePolar, TeeProcs, Chart;

type
  TCircledGradient = class(TBaseForm)
    Series1: TPolarSeries;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses TeeEdiGrad;

procedure TCircledGradient.Button1Click(Sender: TObject);
begin
  TTeeGradientEditor.Edit(Self,Series1.CircleGradient);
end;

procedure TCircledGradient.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
  Series1.Shadow.Visible := false;
end;

initialization
  RegisterClass(TCircledGradient);
end.
