unit Series_Equalizer;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeGDIPlus, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls,
  Series, TeeEqualizerSeries, ComCtrls;

type
  TEqualizerSeriesForm = class(TBaseForm)
    Series1: TEqualizerSeries;
    lblDegradeInterval: TLabel;
    LblDownStep: TLabel;
    UDDegradeInt: TUpDown;
    edDegradeInt: TEdit;
    EdDownStep: TEdit;
    cbxDegradeHigh: TCheckBox;
    Timer1: TTimer;
    procedure cbxDegradeHighClick(Sender: TObject);
    procedure EdDownStepChange(Sender: TObject);
    procedure edDegradeIntChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }

    procedure Populate;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TEqualizerSeriesForm.cbxDegradeHighClick(Sender: TObject);
begin
  Series1.DegradeHigh:=cbxDegradeHigh.Checked;
end;

procedure TEqualizerSeriesForm.EdDownStepChange(Sender: TObject);
begin
  Series1.DownStep:=StrToFloat(EdDownStep.Text);
end;

procedure TEqualizerSeriesForm.edDegradeIntChange(Sender: TObject);
begin
  Series1.DegradeInterval:=UDDegradeInt.Position;
end;

procedure TEqualizerSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;

  UDDegradeInt.Position:=Series1.DegradeInterval;
  EdDownStep.Text:=FloatToStr(Series1.DownStep);
  cbxDegradeHigh.Checked:=Series1.DegradeHigh;
end;

procedure TEqualizerSeriesForm.Populate;
var i : Integer;
begin
  for i := 0 to Series1.Count-1 do
      Series1.YValues[i] := Random(100);
end;

procedure TEqualizerSeriesForm.Timer1Timer(Sender: TObject);
begin
  Populate;
end;

initialization
  RegisterClass(TEqualizerSeriesForm);
end.
