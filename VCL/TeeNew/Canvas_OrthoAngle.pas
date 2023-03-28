unit Canvas_OrthoAngle;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TOrthoAngle = class(TBaseForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    Series1: TBarSeries;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TOrthoAngle.TrackBar1Change(Sender: TObject);
begin
  Chart1.View3DOptions.OrthoAngle:=TrackBar1.Position;
  Label2.Caption:=IntToStr(TrackBar1.Position);
end;

procedure TOrthoAngle.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(6);
end;

initialization
  RegisterClass(TOrthoAngle);
end.
  
