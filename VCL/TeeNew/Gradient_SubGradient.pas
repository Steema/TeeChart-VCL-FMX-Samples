unit Gradient_SubGradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas,
  Gradient_RotationAngle, TeeEdiGrad;

type
  TGradientSubGradient = class(TGradientRotationAngle)
    bSubGradient: TButtonGradient;
    sbTransparency: TScrollBar;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure sbTransparencyChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TGradientSubGradient.FormCreate(Sender: TObject);
begin
  inherited;

  bSubGradient.LinkGradient(Chart1.Gradient.SubGradient);

  with Chart1.Gradient.SubGradient do
  begin
    EndColor := clRed;
    StartColor := clYellow;
    Visible:=True;
    Direction := gdFromCenter;
    Transparency := 50;
  end;
end;

procedure TGradientSubGradient.sbTransparencyChange(Sender: TObject);
begin
  Chart1.Gradient.SubGradient.Transparency:=sbTransparency.Position;
  Label3.Caption:=intToStr(sbTransparency.Position);
end;

initialization
  RegisterClass(TGradientSubGradient);
end.
