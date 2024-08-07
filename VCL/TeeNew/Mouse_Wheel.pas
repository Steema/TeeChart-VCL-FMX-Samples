unit Mouse_Wheel;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeePenDlg;

type
  TMouseWheelForm = class(TBaseForm)
    Series1: THorizLineSeries;
    LabelMouse: TLabel;
    ComboFlat1: TComboFlat;
    procedure FormCreate(Sender: TObject);
    procedure Chart1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboFlat1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMouseWheelForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(100);
  Chart1.TabStop:=True;
end;

procedure TMouseWheelForm.Chart1Click(Sender: TObject);
begin
  if not Chart1.Focused then Chart1.SetFocus;
end;

procedure TMouseWheelForm.FormShow(Sender: TObject);
begin
  inherited;

  if Mouse.WheelPresent then
     LabelMouse.Caption:='Mouse Wheel present.'
  else
     LabelMouse.Caption:='VCL tells your Mouse has no Wheel.';
end;

procedure TMouseWheelForm.ComboFlat1Change(Sender: TObject);
begin
  // Changed: global variable TeeUseMouseWheel is now: Chart1.Panning.MouseWheel

  case ComboFlat1.ItemIndex of
    0: Chart1.Panning.MouseWheel:=pmwNormal;
    1: Chart1.Panning.MouseWheel:=pmwInverted;
  else
    Chart1.Panning.MouseWheel:=pmwNone;
  end;

  Chart1.SetFocus;
end;

initialization
  RegisterClass(TMouseWheelForm);
end.
