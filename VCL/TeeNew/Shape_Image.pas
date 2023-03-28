unit Shape_Image;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, TeeShape, TeeProcs, Chart;

type
  TShapeImage = class(TBaseForm)
    Series1: TChartShape;
    Series2: TChartShape;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TShapeImage.CheckBox1Click(Sender: TObject);
begin
  Series1.Transparent:=CheckBox1.Checked;
  Series2.Transparent:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TShapeImage);
end.
