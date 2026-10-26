unit Tool_Quadrant;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Classes, Graphics, Controls, Forms, StdCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools;

type
  TQuadrantToolForm = class(TBaseForm)
    CBActive: TCheckBox;
    CBCrossHair: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBActiveClick(Sender: TObject);
    procedure CBCrossHairClick(Sender: TObject);
  private
    Quadrant: TQuadrantTool;
  end;

implementation

{$R *.dfm}

procedure TQuadrantToolForm.FormCreate(Sender: TObject);
var Points: TPointSeries;
begin
  inherited;

  Chart1.View3D:=False;
  Chart1.Title.Caption:='TeeChart Quadrant';
  Chart1.Legend.Visible:=False;

  // The tool can also be added at design-time using the chart editor.
  Quadrant:=TQuadrantTool.Create(Self);
  Chart1.Tools.Add(Quadrant);

  Quadrant.Quadrant0.Text:='Zero';
  Quadrant.Quadrant1.Text:='One';
  Quadrant.Quadrant2.Text:='Two';
  Quadrant.Quadrant3.Text:='Three';

  Quadrant.Quadrant1.Font.Size:=14;
  Quadrant.Quadrant2.Font.Size:=16;
  Quadrant.Quadrant2.Font.Color:=clRed;

  Quadrant.Quadrant0.Color:=clWebNavajoWhite;
  Quadrant.Quadrant1.Color:=clWebLavender;
  Quadrant.Quadrant2.Color:=clWebAzure;
  Quadrant.Quadrant3.Color:=clWebDarkKhaki;

  Quadrant.CrossHair.Visible:=CBCrossHair.Checked;
  Quadrant.CrossHair.Pen.Visible:=True;
  Quadrant.CrossHair.Pen.Color:=clRed;

  Points:=TPointSeries.Create(Self);
  Chart1.AddSeries(Points);

  Points.AddXY(5,20,'USA');
  Points.AddXY(80,60,'Peru');
  Points.AddXY(35,40,'Catalonia');
  Points.AddXY(65,90,'Gambia');
  Points.AddXY(25,30,'Malasia');
  Points.AddXY(15,10,'France');
  Points.AddXY(85,10,'UK');
  Points.AddXY(20,75,'Portugal');

  Points.Pointer.Style:=psCircle;
  Points.Pointer.HorizSize:=12;
  Points.Pointer.VertSize:=12;
  Points.Marks.Transparent:=True;
  Points.Marks.ArrowLength:=14;
  Points.Marks.Arrow.Visible:=False;
  Points.Marks.Visible:=True;
  Points.ColorEachPoint:=True;
  Points.HorizAxis:=aBothHorizAxis;
  Points.VertAxis:=aBothVertAxis;

  Chart1.Axes.Left.SetMinMax(0,100);
  Chart1.Axes.Bottom.SetMinMax(0,100);
  Chart1.Axes.Left.Grid.Visible:=False;
  Chart1.Axes.Bottom.Grid.Visible:=False;
  Chart1.Axes.Right.Grid.Visible:=False;
  Chart1.Axes.Top.Grid.Visible:=False;
  Chart1.Axes.Top.Texts.Style:=talValue;
  Chart1.Axes.Bottom.Texts.Style:=talValue;

  Memo1.Clear;
  Memo1.Lines.Add('The Quadrant Tool divides the chart into four colored regions with independent text and formatting.');
  Memo1.Lines.Add('Use the checkboxes to toggle the tool and its central crosshair.');
  Memo1.Lines.Add('Double-click this text to edit the chart and customize the quadrants on the Tools tab.');
end;

procedure TQuadrantToolForm.CBActiveClick(Sender: TObject);
begin
  Quadrant.Active:=CBActive.Checked;
end;

procedure TQuadrantToolForm.CBCrossHairClick(Sender: TObject);
begin
  Quadrant.CrossHair.Visible:=CBCrossHair.Checked;
end;

initialization
  RegisterClass(TQuadrantToolForm);
end.
