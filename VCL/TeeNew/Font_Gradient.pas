unit Font_Gradient;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series;

type
  TFontGradient = class(TBaseForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeEdiGrad, TeCanvas;

procedure TFontGradient.Button1Click(Sender: TObject);
begin
  TTeeGradientEditor.Edit(Self,Chart1.Title.Font.Gradient);
end;

procedure TFontGradient.CheckBox1Click(Sender: TObject);
begin
  Chart1.Title.Font.Gradient.Visible:=CheckBox1.Checked;

  CheckBox2.Enabled:=CheckBox1.Checked;
end;

procedure TFontGradient.FormCreate(Sender: TObject);
begin
  inherited;

  // Activate Font gradient:

  with Chart1.Title.Font.Gradient do
  begin
    Visible:=True;
    StartColor:=clNavy;
    MidColor:=clRed;
    EndColor:=clYellow;
    Direction:=gdLeftRight; // Note: Requires using "TeCanvas.pas" unit
  end;
end;

procedure TFontGradient.CheckBox2Click(Sender: TObject);
begin
  Chart1.Title.Font.Outline.Fill.Gradient.Visible:=CheckBox2.Checked;
end;

initialization
  RegisterClass(TFontGradient);
end.
