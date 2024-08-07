unit Filter_Light;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas,
  TeeAntiAlias, TeePolar, TeeRose, TeeLighting,
  TeeNumericGauge, TeeLinearGauge, TeeCircularGauge, TeePenDlg;

type
  TFilterLight = class(TBaseForm)
    ChartTool1: TAntiAliasTool;
    Label4: TLabel;
    CBStyle: TComboFlat;
    Label5: TLabel;
    TBLeft: TTrackBar;
    Label6: TLabel;
    TBTop: TTrackBar;
    Label7: TLabel;
    TBFactor: TTrackBar;
    Series1: TClockGauge;
    procedure FormCreate(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure TBLeftChange(Sender: TObject);
    procedure TBTopChange(Sender: TObject);
    procedure TBFactorChange(Sender: TObject);
  private
    { Private declarations }
    LightFilter : TLightFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterLight.FormCreate(Sender: TObject);
begin
  inherited;

  LightFilter:=TLightFilter.Create(ChartTool1.Filters);

  with LightFilter do
  begin
    Style:=lsLinear;
    Left:=TBLeft.Position;
    Top:=TBTop.Position;
    Factor:=TBFactor.Position;
  end;

  CBStyle.ItemIndex:=0;
end;

procedure TFilterLight.CBStyleChange(Sender: TObject);
begin
  if CBStyle.ItemIndex=0 then LightFilter.Style:=lsLinear
                         else LightFilter.Style:=lsSpotLight;
end;

procedure TFilterLight.TBLeftChange(Sender: TObject);
begin
  LightFilter.Left:=TBLeft.Position;
end;

procedure TFilterLight.TBTopChange(Sender: TObject);
begin
  LightFilter.Top:=TBTop.Position;
end;

procedure TFilterLight.TBFactorChange(Sender: TObject);
begin
  LightFilter.Factor:=TBFactor.Position;
end;

initialization
  RegisterClass(TFilterLight);
end.

