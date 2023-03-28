unit Filter_HueLumSat;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeAntiAlias,
  TeeSurfa, 

  jpeg,
  
  TeeFilters;

type
  TFilterHueLumSat = class(TBaseForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    sbHue: TScrollBar;
    sbLum: TScrollBar;
    sbSat: TScrollBar;
    Series1: TColorGridSeries;
    ChartTool1: TAntiAliasTool;
    procedure FormCreate(Sender: TObject);
    procedure sbHueChange(Sender: TObject);
    procedure sbLumChange(Sender: TObject);
    procedure sbSatChange(Sender: TObject);
  private
    { Private declarations }
    HueLumSatFilter: THueLumSatFilter;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFilterHueLumSat.FormCreate(Sender: TObject);
begin
  inherited;
  HueLumSatFilter := THueLumSatFilter.Create(ChartTool1.Filters);

  with HueLumSatFilter do
  begin
    Hue:=25;
    Luminance:=25;
    Saturation:=25;
  end;
end;

procedure TFilterHueLumSat.sbHueChange(Sender: TObject);
begin
  HueLumSatFilter.Hue:=sbHue.Position;
  Invalidate;  
end;

procedure TFilterHueLumSat.sbLumChange(Sender: TObject);
begin
  HueLumSatFilter.Luminance:=sbLum.Position;
  Invalidate;
end;

procedure TFilterHueLumSat.sbSatChange(Sender: TObject);
begin
  HueLumSatFilter.Saturation:=sbSat.Position;
  Invalidate;
end;

initialization
  RegisterClass(TFilterHueLumSat);
end.
