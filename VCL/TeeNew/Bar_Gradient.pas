unit Bar_Gradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series {};

type
  TBarGradient = class(TBaseForm)
    Series1: TBarSeries;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

Uses TeeEdiGrad;

procedure TBarGradient.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);
end;

procedure TBarGradient.Button1Click(Sender: TObject);
begin
  TTeeGradientEditor.Edit(Self,Series1.Gradient,True,True);
end;

initialization
  RegisterClass(TBarGradient);
end.
