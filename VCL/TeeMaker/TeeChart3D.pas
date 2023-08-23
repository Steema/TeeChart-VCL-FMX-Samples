unit TeeChart3D;
{$I TeeDefs.inc}

interface

uses
  Classes, SysUtils,
  TeeProcs, TeEngine, Chart, TeeBlocks, TeeChartBlock, TeeMakerControl;

type
  TChart3D=class(TMaker)
  private
    FChart : TChartBlock;

    procedure CreateChart;
    function GetAxes: TChartAxes;
    function GetBlock: TChartBlock;
    function GetChart: TChart;
    procedure SetBlock(const Value: TChartBlock);
    procedure SetChart(const Value: TChart);
    procedure SetupChart;
  protected
    function GetEditablePanel:TCustomTeePanel; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
  public
    Constructor Create(AOwner:TComponent); override;

    property Axes:TChartAxes read GetAxes;
    property ChartBlock:TChartBlock read GetBlock write SetBlock;
  published
    property Chart:TChart read GetChart write SetChart;
  end;

implementation

{ TChart3D }

Constructor TChart3D.Create(AOwner: TComponent);
begin
  inherited;

  AutoRepaint:=False;

  Gradient.EndColor:=8388608; // clNavy;

  with View3DOptions do
  begin
    Zoom:=75;
    Rotation:=0;
    Elevation:=0;
  end;

  if csDesigning in ComponentState then
     if (not Assigned(Owner)) or (not (csLoading in Owner.ComponentState)) then
        CreateChart;

  AutoRepaint:=True;
end;

procedure TChart3D.CreateChart;
begin
  if not Assigned(FChart) then
  begin
    FChart:=TChartBlock.Create(Owner);
    FChart.Title:='Chart';

    Blocks.Add(FChart);

    if Assigned(Owner) then
       FChart.Name:=TeeGetUniqueName(Owner,'ChartBlock');
  end;
end;

function TChart3D.GetChart: TChart;
begin
  result:=ChartBlock.Chart;
end;

procedure TChart3D.SetChart(const Value: TChart);
begin
  ChartBlock.Chart:=Value;
end;

function TChart3D.GetEditablePanel:TCustomTeePanel;
begin
  result:=Chart;
end;

procedure TChart3D.Loaded;
begin
  inherited;
  SetupChart;
end;

procedure TChart3D.SetupChart;
begin
(* Problem: This cannot be done !
   We should not set the FChart.Chart.Parent property, otherwise
   the VCL Chart control will be a child item in Controls[] and will force
   the VCL to try to repaint it (as a normal VCL Chart) over our 3D OpenGL
   canvas:

// Design-time bug ("Control has no parent Window")
//  if Assigned(FChart) then
//     FChart.Chart.Parent:=Self;
*)
end;

procedure TChart3D.ReadState(Reader: TReader);
var t : Integer;
begin
  if Assigned(FChart) and
     (not (csLoading in FChart.ComponentState))
     and (not (csAncestor in FChart.ComponentState)) then
          FreeAndNil(FChart);

  inherited;

  for t:=0 to Blocks.Count-1 do
      if Blocks[t] is TChartBlock then
      begin
        if Assigned(FChart) then
           FChart.RemoveFreeNotification(Self);

        FreeAndNil(FChart);

        ChartBlock:=TChartBlock(Blocks[t]);
        break;
      end;

  SetupChart;
end;

function TChart3D.GetBlock: TChartBlock;
begin
  if not Assigned(FChart) then
     CreateChart;

  result:=FChart;
end;

type
  TTeePanelAccess=class(TCustomTeePanel);

procedure TChart3D.SetBlock(const Value: TChartBlock);
begin
  if FChart<>Value then
  begin
    if Assigned(FChart) then
       FChart.RemoveFreeNotification(Self);

    FChart:=Value;

    if Assigned(FChart) then
    begin
      FChart.FreeNotification(Self);

      TTeePanelAccess(FChart.Chart).GLComponent:=Render;
    end;
  end;
end;

procedure TChart3D.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FChart) and (AComponent=FChart) then
     ChartBlock:=nil;
end;

function TChart3D.GetAxes: TChartAxes;
begin
  result:=Chart.Axes;
end;

end.
