unit Surface_Sides;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeeSurfa {};

type
  TSurfaceSides = class(TBaseForm)
    Series1: TSurfaceSeries;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeBrushDlg;

procedure TSurfaceSides.CheckBox1Click(Sender: TObject);
begin
  inherited;
  if CheckBox1.Checked then
  begin
    Series1.SideBrush.Style:=bsDiagCross;
    Series1.SideBrush.Color:=clBlue;
  end
  else Series1.SideBrush.Style:=bsClear;
end;

procedure TSurfaceSides.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.FillSampleValues(12);
end;

procedure TSurfaceSides.Button1Click(Sender: TObject);
begin
  TBrushDialog.Edit(Self,Series1.SideBrush);
end;

initialization
  RegisterClass(TSurfaceSides);
end.
