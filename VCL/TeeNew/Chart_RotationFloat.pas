unit Chart_RotationFloat;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, GanttCh;

type
  TChartRotationFloat = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    Series1: TGanttSeries;
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

procedure TChartRotationFloat.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.View3DOptions.RotationFloat:=310.20;
end;

procedure TChartRotationFloat.Button1Click(Sender: TObject);
begin
  Chart1.View3DOptions.RotationFloat:=StrToFloat(Edit1.Text);
end;

initialization
  RegisterClass(TChartRotationFloat);
end.
