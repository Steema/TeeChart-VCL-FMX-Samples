unit Series_CalendarClicked;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  Base, TeEngine, Series, TeeProcs, Chart, TeeCalendar;

type
  TSeriesCalendarClicked = class(TBaseForm)
    Series1: TCalendarSeries;
    Label1: TLabel;
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesCalendarClicked.Chart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var ClickedDate : TDateTime;
begin
  if Series1.ClickedDate(x,y,ClickedDate) then
     Label1.Caption:='Date : ' + DateTimeToStr(ClickedDate)
  else
     Label1.Caption:='Move mouse over dates...';
end;

initialization
  RegisterClass(TSeriesCalendarClicked);
end.
