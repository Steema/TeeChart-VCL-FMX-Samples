unit Unit_Quadrant;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeEngine, Vcl.ExtCtrls,
  VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeTools, VCLTee.Series;

type
  TFormQuadrant = class(TForm)
    Chart1: TChart;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Quadrant : TQuadrantTool;

    procedure Cosmetics;
    function SampleData:TPointSeries;
  public
    { Public declarations }
  end;

var
  FormQuadrant: TFormQuadrant;

implementation

{$R *.dfm}

procedure TFormQuadrant.FormCreate(Sender: TObject);
begin
  Chart1.View3D:=False;

  Chart1.Title.Caption:='TeeChart Quadrant';

  // Quadrant created by code.
  // It can also be done totally codeless at design-time using the
  // Chart editor (double click the chart at design-time, go to Tools)

  Quadrant:=TQuadrantTool.Create(Self);

  Quadrant.Quadrant0.Text:='Zero';
  Quadrant.Quadrant1.Text:='One';
  Quadrant.Quadrant2.Text:='Two';
  Quadrant.Quadrant3.Text:='Three';

  Quadrant.Quadrant0.Margins.Left:=20;
  Quadrant.Quadrant1.Font.Size:=14;
  Quadrant.Quadrant2.Font.Size:=16;
  Quadrant.Quadrant2.Font.Color:=clRed;

  Quadrant.Quadrant0.Color:=clWebNavajoWhite;
  Quadrant.Quadrant1.Color:=clWebLavender;
  Quadrant.Quadrant2.Color:=clWebAzure;
  Quadrant.Quadrant3.Color:=clWebDarkKhaki;

  Chart1.Tools.Add(Quadrant);

  Chart1.AddSeries(SampleData);

  Cosmetics;
end;

procedure TFormQuadrant.Cosmetics;
begin
  Chart1.Axes.Left.SetMinMax(0,100);
  Chart1.Axes.Bottom.SetMinMax(0,100);

  Chart1.Axes.Left.Grid.Hide;
  Chart1.Axes.Bottom.Grid.Hide;
  Chart1.Axes.Right.Grid.Hide;
  Chart1.Axes.Top.Grid.Hide;

  Chart1.Axes.Top.Texts.Style:=talValue;
  Chart1.Axes.Bottom.Texts.Style:=talValue;

  Chart1.Legend.Hide;
end;

function TFormQuadrant.SampleData:TPointSeries;
begin
  result:=TPointSeries.Create(Self);

  result.AddXY(5,20,'USA');
  result.AddXY(80,60,'Peru');
  result.AddXY(35,40,'Catalonia');
  result.AddXY(65,90,'Gambia');
  result.AddXY(25,30,'Malasia');

  result.Pointer.Style:=psCircle;
  result.Pointer.Size:=12;
  result.Marks.Transparent:=True;
  result.Marks.ArrowLength:=14;
  result.Marks.Arrow.Hide;
  result.Marks.Show;
  result.ColorEachPoint:=True;

  result.HorizAxis:=aBothHorizAxis;
  result.VertAxis:=aBothVertAxis;
end;

end.
