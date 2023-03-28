unit Surface_Smooth;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeeSurfa, TeeProcs, Chart {};

type
  TSurfaceSmooth = class(TBaseForm)
    Series1: TSurfaceSeries;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSurfaceSmooth.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.FillSampleValues(10);
end;

procedure TSurfaceSmooth.CheckBox1Click(Sender: TObject);
begin
  inherited;
  Series1.SmoothPalette:=CheckBox1.Checked
end;

initialization
  RegisterClass(TSurfaceSmooth);
end.
