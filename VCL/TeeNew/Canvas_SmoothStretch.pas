unit Canvas_SmoothStretch;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas;

type
  TCanvasSmoothStretch = class(TBaseForm)
    RadioGroup1: TRadioGroup;
    Series1: TPieSeries;
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Image2: TImage;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCanvasSmoothStretch.RadioGroup1Click(Sender: TObject);
var B : TBitmap;
begin
  B:=TBitmap.Create;
  try
    B.Width:=Image2.Width;
    B.Height:=Image2.Height;

    if RadioGroup1.ItemIndex=0 then
       SmoothStretch(Image1.Picture.Bitmap, B)  // smooth
    else
       // normal
       B.Canvas.StretchDraw(Rect(0,0,B.Width,B.Height),Image1.Picture.Bitmap);

    Image2.Picture.Assign(B);
  finally
    B.Free;
  end;
end;

procedure TCanvasSmoothStretch.FormCreate(Sender: TObject);
begin
  inherited;

  // Draw a chart onto Image1 for this example:
  Chart1.Legend.Visible:=False;

  Chart1.Width:=Image1.Width;
  Chart1.Height:=Image1.Height;

  Image1.Picture.Assign(Chart1);

  RadioGroup1Click(Self);
end;

initialization
  RegisterClass(TCanvasSmoothStretch);
end.
