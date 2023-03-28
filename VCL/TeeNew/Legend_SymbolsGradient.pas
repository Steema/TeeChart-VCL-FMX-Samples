unit Legend_SymbolsGradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeeEdiGrad, TeeSurfa;

type
  TLegendSymbolsGradientForm = class(TBaseForm)
    cbVisible: TCheckBox;
    bGradient: TButtonGradient;
    Series1: TSurfaceSeries;
    procedure FormCreate(Sender: TObject);
    procedure cbVisibleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLegendSymbolsGradientForm.FormCreate(Sender: TObject);
begin
  inherited;
  bGradient.LinkGradient(Chart1.Legend.Symbol.Gradient);
end;

procedure TLegendSymbolsGradientForm.cbVisibleClick(Sender: TObject);
begin
  Chart1.Legend.Symbol.Gradient.Visible:=cbVisible.Checked;
end;

initialization
  RegisterClass(TLegendSymbolsGradientForm);
end.
 
