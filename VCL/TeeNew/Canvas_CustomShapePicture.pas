unit Canvas_CustomShapePicture;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ImgList,
  Base, TeCanvas, TeEngine, Series, TeeProcs, Chart, TeeDonut;

type
  TCanvasCustonShapePicture = class(TBaseForm)
    Series1: TDonutSeries;
    ImageList1: TImageList;
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

procedure TCanvasCustonShapePicture.FormCreate(Sender: TObject);
var i: integer;
begin
  inherited;

  for i:=0 to Series1.Count-1 do
    ImageList1.GetBitmap(i,Series1.Marks[i].Picture.Bitmap);
end;

procedure TCanvasCustonShapePicture.Series1GetMarkText(
  Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
begin
  MarkText:='      ' + TeeLineSeparator +
            '      ';
end;

initialization
  RegisterClass(TCanvasCustonShapePicture);
end.
