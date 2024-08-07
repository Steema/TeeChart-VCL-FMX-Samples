unit Legend_Image;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  {$IFNDEF LINUX}
  Jpeg,
  {$ENDIF}
  Base, TeeProcs, TeEngine, Chart, Series, TeePolar, TeeRose;

type
  TLegendImage = class(TBaseForm)
    CheckBox1: TCheckBox;
    Series1: TWindRoseSeries;
    Image1: TImage;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLegendImage.CheckBox1Click(Sender: TObject);
begin
  inherited;
  if CheckBox1.Checked then
     Chart1.Legend.Brush.Image.Assign(Image1.Picture)
  else
     Chart1.Legend.Brush.Image.Assign(nil)
end;

procedure TLegendImage.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
  Chart1.Legend.Brush.Image.Assign(Image1.Picture);
end;

initialization
  RegisterClass(TLegendImage);
end.
