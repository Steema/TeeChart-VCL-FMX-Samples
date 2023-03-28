unit Margins_Units;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, TeCanvas;

type
  TChartMarginUnits = class(TBaseForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Label3: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartMarginUnits.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex=0 then
  begin
    Chart1.MarginUnits:=muPercent;
    Label3.Caption:='%';
  end
  else
  begin
    Chart1.MarginUnits:=muPixels;
    Label3.Caption:='pixels';
  end;
end;

procedure TChartMarginUnits.FormCreate(Sender: TObject);
begin
  inherited;
  ComboBox1.ItemIndex:=1;
  ComboBox1Change(Self);
end;

procedure TChartMarginUnits.Edit1Change(Sender: TObject);
begin
  Chart1.MarginLeft  :=UpDown1.Position;
  Chart1.MarginTop   :=UpDown1.Position;
  Chart1.MarginRight :=UpDown1.Position;
  Chart1.MarginBottom:=UpDown1.Position;
end;

initialization
  RegisterClass(TChartMarginUnits);
end.
