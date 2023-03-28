unit Canvas_UnicodeTextOut;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart;

type
  TCanvasUnicodeTextOut = class(TBaseForm)
    procedure Chart1AfterDraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCanvasUnicodeTextOut.Chart1AfterDraw(Sender: TObject);
var UnicodeString : WideString;
begin
  inherited;

  SetLength(UnicodeString, 19);

  UnicodeString[ 1] := WideChar($0152);
  UnicodeString[ 2] := WideChar($03E0);
  UnicodeString[ 3] := WideChar($0416);
  UnicodeString[ 4] := Widechar($0539);
  UnicodeString[ 5] := WideChar($0634);
  UnicodeString[ 6] := WideChar($0950);
  UnicodeString[ 7] := WideChar($0B10);
  UnicodeString[ 8] := WideChar($0B86);
  UnicodeString[ 9] := WideChar($0C0B);
  UnicodeString[10] := WideChar($0D60);
  UnicodeString[11] := WideChar($0E12);
  UnicodeString[12] := WideChar($0EDD);
  UnicodeString[13] := WideChar($0F00);
  UnicodeString[14] := WideChar($10C5);
  UnicodeString[15] := WideChar($1124);
  UnicodeString[16] := WideChar($20A9);
  UnicodeString[17] := WideChar($2103);
  UnicodeString[18] := WideChar($3020);
  UnicodeString[19] := WideChar($FFFD);

  with Chart1.Canvas.Font do
  begin
    Size:=15;
    Name:='Arial Unicode MS';
  end;

  TextOutW(Chart1.Canvas.Handle, 10, 10, PWideChar(UnicodeString),
                                         Length(UnicodeString));
end;

initialization
  RegisterClass(TCanvasUnicodeTextOut);
end.
