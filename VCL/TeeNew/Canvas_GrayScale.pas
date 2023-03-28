unit Canvas_GrayScale;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TGrayScaleForm = class(TBaseForm)
    Series1: TPieSeries;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    CBMethod: TComboBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBMethodChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses 
  TeCanvas, TeeFilters;  // <--- TeeGrayScale proc

procedure TGrayScaleForm.CheckBox1Click(Sender: TObject);
begin
  CheckBox2.Enabled:=CheckBox1.Checked;
  Chart1.Repaint;
end;

procedure TGrayScaleForm.Chart1AfterDraw(Sender: TObject);
var tmpBitmap : TBitmap;
begin
  if CheckBox1.Checked then { do gray scale... }
  begin
    tmpBitmap:=(Chart1.Canvas as TTeeCanvas3D).Bitmap;

    // If the current Canvas uses the Bitmap (double-buffered canvas) then:

    if tmpBitmap<>nil then
       TeeGrayScale(tmpBitmap,CheckBox2.Checked,CBMethod.ItemIndex);
  end;
end;

procedure TGrayScaleForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

procedure TGrayScaleForm.CBMethodChange(Sender: TObject);
begin
  Chart1.Repaint;
end;

procedure TGrayScaleForm.FormShow(Sender: TObject);
begin
  inherited;
  CBMethod.ItemIndex:=0;
end;

initialization
  RegisterClass(TGrayScaleForm);
end.
