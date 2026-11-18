unit Boundary_Tool_Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.TeEngine, VCLTee.TeeProcs,
  VCLTee.Chart, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  VCLTee.TeeTools, VCLTee.Series, VCLTee.TeeDragPoint;

type
  TFormBoundary = class(TForm)
    Panel1: TPanel;
    Chart1: TChart;
    TBPadding: TTrackBar;
    Label1: TLabel;
    CBSmooth: TCheckBox;
    CBStyle: TComboBox;
    TBSmoothSteps: TTrackBar;
    Series1: TPointSeries;
    Series2: TPointSeries;
    Series3: TPointSeries;
    procedure CBSmoothClick(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TBPaddingChange(Sender: TObject);
    procedure TBSmoothStepsChange(Sender: TObject);
  private
    { Private declarations }

    Tool1,
    Tool2,
    Tool3 : TBoundaryTool;
  public
    { Public declarations }
  end;

var
  FormBoundary: TFormBoundary;

implementation

{$R *.dfm}

procedure TFormBoundary.CBSmoothClick(Sender: TObject);
begin
  TBSmoothSteps.Enabled := CBSmooth.Checked;

  Tool1.Smooth := CBSmooth.Checked;
  Tool2.Smooth := CBSmooth.Checked;
  Tool3.Smooth := CBSmooth.Checked;
end;

procedure TFormBoundary.CBStyleChange(Sender: TObject);
begin
  Tool1.Style := TBoundaryToolStyle(CBStyle.ItemIndex);
  Tool2.Style := TBoundaryToolStyle(CBStyle.ItemIndex);
  Tool3.Style := TBoundaryToolStyle(CBStyle.ItemIndex);
end;

procedure TFormBoundary.FormCreate(Sender: TObject);

  // Setup one series and return a new Boundary tool for it
  function InitSeries(const ASeries:TPointSeries; X0,X1,Y0,Y1:Integer):TBoundaryTool;

    procedure AddRandomPoints;
    var t : Integer;
    begin
      ASeries.BeginUpdate;
      ASeries.Clear;

      for t:=0 to 19 do
          ASeries.AddXY(X0+Random(X1-X0),Y0+Random(Y1-Y0));

      ASeries.EndUpdate;
    end;

  var Drag : TDragPointTool;
  begin
    ASeries.XValues.Order:=loNone;
    ASeries.Pointer.Style:=psCircle;
    ASeries.Pointer.Size:=8;

    Drag:=TDragPointTool.Create(Self);
    Drag.Series:=ASeries;
    //Drag.Cursor:=crHandPoint;

    AddRandomPoints;

    result:=TBoundaryTool.Create(Self);
    result.Series:=ASeries;

    result.Pen.Color:=ASeries.Color;
    result.Brush.Transparency:=50;
  end;

begin
  Tool1:=InitSeries(Series1,10,50,0,50);
  Tool2:=InitSeries(Series2,80,150,60,150);
  Tool3:=InitSeries(Series3,20,60,90,130);

  Chart1.Axes.Left.Minimum:=0;

  // Boundary can also work without a series :
  // Tool1.Points:=MyArrayOfPoints
end;

procedure TFormBoundary.TBPaddingChange(Sender: TObject);
begin
  Tool1.Padding := TBPadding.Position;
  Tool2.Padding := TBPadding.Position;
  Tool3.Padding := TBPadding.Position;
end;

procedure TFormBoundary.TBSmoothStepsChange(Sender: TObject);
begin
  Tool1.SmoothSteps := TBSmoothSteps.Position;
  Tool2.SmoothSteps := TBSmoothSteps.Position;
  Tool3.SmoothSteps := TBSmoothSteps.Position;
end;

end.
