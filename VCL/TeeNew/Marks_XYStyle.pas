unit Marks_XYStyle;
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
  TMarksXYStyle = class(TBaseForm)
    Series1: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer;
      var MarkText: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksXYStyle.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);

  Series1.Marks.Style:=smsXY;
end;

procedure TMarksXYStyle.Series1GetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
var i:Integer;
begin
  inherited;
  i:=Pos(' ',MarkText);
  if i>0 then MarkText[i]:='-';
end;

initialization
  RegisterClass(TMarksXYStyle);
end.
