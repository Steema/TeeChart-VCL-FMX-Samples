unit Chart_Shadow;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart;

type
  TChartShadow = class(TBaseForm)
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses TeeShadowEditor;

procedure TChartShadow.Button1Click(Sender: TObject);
begin
  EditTeeShadow(Self, Chart1.Shadow);
end;

procedure TChartShadow.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.Shadow.HorizSize:=6;
  Chart1.Shadow.VertSize:=6;
  Chart1.Shadow.Color:=clDkGray;
end;

procedure TChartShadow.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then Chart1.Shadow.Size:=6
                       else Chart1.Shadow.Size:=0;
end;

initialization
  RegisterClass(TChartShadow);
end.
