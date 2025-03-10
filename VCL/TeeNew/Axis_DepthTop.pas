unit Axis_DepthTop;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages, 
  {$ENDIF}
  SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, TeeSurfa;

type
  TDepthTopAxisForm = class(TBaseForm)
    CheckBox1: TCheckBox;
    Series1: TTowerSeries;
    CheckBox2: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TDepthTopAxisForm.CheckBox1Click(Sender: TObject);
begin
  Chart1.Axes.DepthTop.Visible := CheckBox1.Checked;
end;

procedure TDepthTopAxisForm.CheckBox2Click(Sender: TObject);
begin
  Chart1.Axes.Depth.Visible := CheckBox2.Checked;
end;

procedure TDepthTopAxisForm.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.View3D := True;
  Series1.FillSampleValues;
end;

initialization
  RegisterClass(TDepthTopAxisForm);
end.
