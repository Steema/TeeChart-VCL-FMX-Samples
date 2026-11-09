unit uAnalyticsSample;

interface

uses
  System.SysUtils, System.JSON, uAnalyticsData;

procedure LoadSampleData(AData: TAnalyticsData; const AAgent: string;
  const AJSON: string);

implementation

procedure LoadSampleData(AData: TAnalyticsData; const AAgent: string;
  const AJSON: string);
var
  LRoot: TJSONValue;
  LValue, LItem: TJSONValue;
  LArray: TJSONArray;
  LObject: TJSONObject;
  LMetric, LUnitName, LNotes: string;
  LNumber: Double;
begin
  LRoot := TJSONObject.ParseJSONValue(AJSON);
  try
    if not (LRoot is TJSONArray) then
      raise EConvertError.Create('The agent response must be a JSON array.');
    LArray := TJSONArray(LRoot);
    for LItem in LArray do
    begin
      if not (LItem is TJSONObject) then
        Continue;
      LObject := TJSONObject(LItem);
      LMetric := LObject.GetValue<string>('metric', '');
      LUnitName := LObject.GetValue<string>('unit', '');
      LNotes := LObject.GetValue<string>('notes', '');
      if LMetric.IsEmpty then
        Continue;
      LValue := LObject.GetValue('value');
      if (LValue = nil) or not TryStrToFloat(LValue.Value, LNumber,
        TFormatSettings.Invariant) then
        Continue;
      AData.Add(AAgent, LMetric, LNumber, LUnitName, LNotes);
    end;
  finally
    LRoot.Free;
  end;
end;

end.
