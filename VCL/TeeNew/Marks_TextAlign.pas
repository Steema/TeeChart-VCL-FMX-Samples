unit Marks_TextAlign;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TMarksTextAlign = class(TBaseForm)
    Label1: TLabel;
    cbAlign: TComboFlat;
    Series1: TPointSeries;
    procedure cbAlignChange(Sender: TObject);
    procedure Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer;
      var MarkText: String);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksTextAlign.cbAlignChange(Sender: TObject);
begin
  Series1.Marks.TextAlign:=TAlignment(cbAlign.ItemIndex);
end;

procedure TMarksTextAlign.Series1GetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
begin
  MarkText := 'Point Info.' + TeeLineSeparator +
    'x : ' + FloatToStr(Series1.XValue[ValueIndex]) + TeeLineSeparator +
    'y : ' + FloatToStr(Series1.YValue[ValueIndex]);
end;

procedure TMarksTextAlign.FormCreate(Sender: TObject);
begin
  inherited;

  cbAlign.ItemIndex:=2;
  cbAlignChange(self);
end;

initialization
  RegisterClass(TMarksTextAlign);
end.
