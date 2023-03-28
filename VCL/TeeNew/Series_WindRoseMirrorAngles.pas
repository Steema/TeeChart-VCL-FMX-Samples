unit Series_WindRoseMirrorAngles;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePolar, TeeRose,
  TeePenDlg;

type
  TSeriesWindRoseMirrorAngles = class(TBaseForm)
    Series1: TWindRoseSeries;
    cbMirror: TCheckBox;
    cbMirrorLabels: TCheckBox;
    ComboFlat1: TComboFlat;
    procedure cbMirrorClick(Sender: TObject);
    procedure cbMirrorLabelsClick(Sender: TObject);
    procedure ComboFlat1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesWindRoseMirrorAngles.cbMirrorClick(Sender: TObject);
begin
  Series1.MirrorAngles:=cbMirror.Checked;
end;

procedure TSeriesWindRoseMirrorAngles.cbMirrorLabelsClick(Sender: TObject);
begin
  Series1.MirrorLabels:=cbMirrorLabels.Checked;
end;

procedure TSeriesWindRoseMirrorAngles.ComboFlat1Change(Sender: TObject);
begin
  Series1.AngleIncrement:=StrToFloat(ComboFlat1.CurrentItem);
end;

initialization
  RegisterClass(TSeriesWindRoseMirrorAngles);
end.
