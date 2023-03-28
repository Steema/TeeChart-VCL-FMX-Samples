unit Series_ContourLevelMarksOverlap;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas,
  Series_ContourLevelMarks, TeeSurfa;

type
  TSeriesContourLevelMarksOverlap = class(TSeriesContourLevelMarks)
    cbAntiOverlap: TCheckBox;
    procedure cbAntiOverlapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesContourLevelMarksOverlap.cbAntiOverlapClick(
  Sender: TObject);
begin
  Series1.ContourMarks.AntiOverlap:=cbAntiOverlap.Checked;
end;

procedure TSeriesContourLevelMarksOverlap.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.ContourMarks.AntiOverlap:=true;
end;

initialization
  RegisterClass(TSeriesContourLevelMarksOverlap);
end.

