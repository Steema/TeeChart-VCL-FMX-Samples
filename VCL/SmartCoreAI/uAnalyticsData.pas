unit uAnalyticsData;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TMetricRow = record
    Agent: string;
    Metric: string;
    Value: Double;
    UnitName: string;
    Notes: string;
  end;

  TAnalyticsData = class(TComponent)
  private
    FRows: TList<TMetricRow>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AAgent, AMetric: string; AValue: Double;
      const AUnitName, ANotes: string);
    function Count: Integer;
    function Row(AIndex: Integer): TMetricRow;
  end;

implementation

constructor TAnalyticsData.Create(AOwner: TComponent);
begin
  inherited;
  FRows := TList<TMetricRow>.Create;
end;

destructor TAnalyticsData.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TAnalyticsData.Clear;
begin
  FRows.Clear;
end;

procedure TAnalyticsData.Add(const AAgent, AMetric: string; AValue: Double;
  const AUnitName, ANotes: string);
var
  LRow: TMetricRow;
begin
  LRow.Agent := AAgent;
  LRow.Metric := AMetric;
  LRow.Value := AValue;
  LRow.UnitName := AUnitName;
  LRow.Notes := ANotes;
  FRows.Add(LRow);
end;

function TAnalyticsData.Count: Integer;
begin
  Result := FRows.Count;
end;

function TAnalyticsData.Row(AIndex: Integer): TMetricRow;
begin
  Result := FRows[AIndex];
end;

end.
