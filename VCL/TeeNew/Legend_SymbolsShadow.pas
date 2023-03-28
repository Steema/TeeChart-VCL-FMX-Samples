unit Legend_SymbolsShadow;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeeEdiGrad;

type
  TLegendSymbolsShadow = class(TBaseForm)
    Series1: THorizBarSeries;
    cbVisible: TCheckBox;
    bShadow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbVisibleClick(Sender: TObject);
    procedure bShadowClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeShadowEditor;

procedure TLegendSymbolsShadow.FormCreate(Sender: TObject);
begin
  inherited;

  with Chart1.Legend.Symbol.Shadow do
  begin
    HorizSize:=2;
    VertSize:=2;
    Color:=clGray;
    Smooth:=true;
    Visible:=true;
  end;
end;

procedure TLegendSymbolsShadow.cbVisibleClick(Sender: TObject);
begin
  Chart1.Legend.Symbol.Shadow.Visible:=cbVisible.Checked;
end;

procedure TLegendSymbolsShadow.bShadowClick(Sender: TObject);
begin
  EditTeeShadow(Self, Chart1.Legend.Symbol.Shadow);
end;

initialization
  RegisterClass(TLegendSymbolsShadow);
end.
