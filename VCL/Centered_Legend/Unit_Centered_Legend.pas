unit Unit_Centered_Legend;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    Chart1: TChart;
    Series1: TBarSeries;
    Panel1: TPanel;
    CBHoriz: TCheckBox;
    Label1: TLabel;
    CBCentered: TComboBox;
    TBMargin: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure CBHorizClick(Sender: TObject);
    procedure CBCenteredChange(Sender: TObject);
    procedure TBMarginChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.CBCenteredChange(Sender: TObject);
begin
  case CBCentered.ItemIndex of
    0: Chart1.Legend.Centered:=lcAutomatic;
    1: Chart1.Legend.Centered:=lcChart;
    2: Chart1.Legend.Centered:=lcPanel;
  else
    Chart1.Legend.Centered:=lcNo;
  end;
end;

procedure TMainForm.CBHorizClick(Sender: TObject);
begin
  if CBHoriz.Checked then
     Chart1.Legend.Alignment:=laBottom
  else
     Chart1.Legend.Alignment:=laRight;

  if CBHoriz.Checked then
     TBMargin.Position:=Chart1.MarginLeft
  else
     TBMargin.Position:=Chart1.MarginTop;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Chart1.Legend.Alignment:=laBottom;

  Chart1.Legend.Centered:=lcChart;

  CBCentered.ItemIndex:=1;

  Chart1.MarginLeft:=20;
end;

procedure TMainForm.TBMarginChange(Sender: TObject);
begin
  if CBHoriz.Checked then
     Chart1.MarginLeft:=TBMargin.Position
  else
     Chart1.MarginTop:=TBMargin.Position;
end;

end.
