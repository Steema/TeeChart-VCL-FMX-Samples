{*********************************************}
{  TeeChart Charting Library                  }
{  Copyright (c) 2026 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit fAnalytics;

interface

{
  Example using Embarcadero SmartCore AI components and TeeChart.

  https://docwiki.embarcadero.com/RADStudio/Florence/en/SmartCore_AI_Component_Pack

}

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.JSON, System.Threading,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Grids, Data.DB,

  // SmartCore AI
  SmartCoreAI.Types, SmartCoreAI.Comp,
  SmartCoreAI.Driver.Gemini, SmartCoreAI.Driver.OpenAI,
  SmartCoreAI.Driver.Claude, SmartCoreAI.Driver.Ollama,

  // TeeChart
  VCLTee.Chart, VCLTee.Series, VCLTee.TeeProcs, VCLTee.TeEngine,

  // Demo
  uAnalyticsData, uAnalyticsSample, System.JSON.Types, SmartCoreAI.Exceptions,
  SmartCoreAI.Mapping, SmartCoreAI.Driver.Models;

type
  TfrmAnalytics = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Chart1: TChart;
    StringGrid1: TStringGrid;
    BSample: TButton;
    BAsk: TButton;
    BCancel: TButton;
    AIConnection1: TAIConnection;
    Series1: TBarSeries;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AIChatAgent1: TAIChatAgent;
    Provider: TComboBox;
    Prompt: TEdit;
    Model: TEdit;
    APIKey: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BSampleClick(Sender: TObject);
    procedure BAskClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure AIChatAgent1Started(Sender: TObject);
    procedure AIChatAgent1Partial(Sender: TObject; const AText: string;
      AKind: TAIProgressKind);
    procedure AIChatAgent1Complete(Sender: TObject; const AText: string;
      AKind: TAIProgressKind);
    procedure AIChatAgent1Error(Sender: TObject; AException: Exception);
    procedure AIChatAgent1Cancelled(Sender: TObject);
    procedure ProviderChange(Sender: TObject);
  private
    FData: TAnalyticsData;

    procedure AddSample;
    procedure BuildUI;
    procedure ConfigureConnection;
    procedure RefreshViews;
    procedure SetBusy(ABusy: Boolean);
  end;

var
  frmAnalytics: TfrmAnalytics;

implementation

{$R *.dfm}

procedure TfrmAnalytics.FormCreate(Sender: TObject);
begin
  FData := TAnalyticsData.Create(Self);

  AIChatAgent1.Stream := True;
  AIChatAgent1.AutoRegisterInvokables := False;
  AIChatAgent1.Options.MaxOutputTokens := 1400;
  AIChatAgent1.Instructions.Text :=
    'You are a careful business data analyst. Reply first with one valid JSON array of metric objects, no markdown fences. Each object has metric (short name), value (number), unit (short unit), notes (short explanation). Example: [{"metric":"Revenue","value":1250.5,"unit":"USD","notes":"Q1 estimate"}]. Then add a short plain-English explanation after the array. Use numerical values grounded in the user request; when data is missing, clearly state assumptions and use an illustrative estimate.';

  BuildUI;
  ConfigureConnection;
  AddSample;
end;

procedure TfrmAnalytics.BAskClick(Sender: TObject);
var P: string;
begin
  ConfigureConnection;

  AIChatAgent1.NewSession;

  P := Trim(Prompt.Text);
  if P.IsEmpty then Exit;

  AIChatAgent1.Prompt := P + sLineBreak + sLineBreak +
    'Return your metric array first, then a short explanation. Keep numeric values JSON-compatible.';

  Memo1.Lines.Add('User: ' + P);
  Memo1.Lines.Add('Agent: ');

  TTask.Run(procedure begin AIChatAgent1.Execute; end);
end;

procedure TfrmAnalytics.BCancelClick(Sender: TObject);
begin
  AIChatAgent1.Cancel;
end;

procedure TfrmAnalytics.BSampleClick(Sender: TObject);
begin
  AddSample;
  Memo1.Lines.Add('Loaded four local sample metrics. No provider call was made.');
end;

procedure TfrmAnalytics.BuildUI;
begin
  Prompt.Text := 'Compare revenue, cost and margin for a fictional Q1 business. Return 4 illustrative metrics.';

  Chart1.Title.Text.Text := 'Agent Metrics';
  Chart1.Legend.Visible := False;

  StringGrid1.FixedRows := 1;
  StringGrid1.ColCount := 5;
  StringGrid1.RowCount := 2;

  StringGrid1.Cells[0,0] := 'Agent';
  StringGrid1.Cells[1,0] := 'Metric';
  StringGrid1.Cells[2,0] := 'Value';
  StringGrid1.Cells[3,0] := 'Unit';
  StringGrid1.Cells[4,0] := 'Notes';

  StringGrid1.ColWidths[0] := 115;
  StringGrid1.ColWidths[1] := 140;
  StringGrid1.ColWidths[2] := 105;
  StringGrid1.ColWidths[3] := 80;
  StringGrid1.ColWidths[4] := 420;

  Memo1.Lines.Add('Ready. Sample values are illustrative; AI responses are appended as new chart/grid rows.');
end;

procedure TfrmAnalytics.ProviderChange(Sender: TObject);
begin
  case Provider.ItemIndex of
    0: Model.Text := 'gemini-3.1-flash-lite';
    1: Model.Text := 'gpt-5.6-luna';
    2: Model.Text := 'claude-haiku-4-5';
    3: Model.Text := 'qwen3-vl:2b';
  end;

  ConfigureConnection;
end;

procedure TfrmAnalytics.ConfigureConnection;
begin
  case Provider.ItemIndex of
    0: AIConnection1.DriverName := 'Gemini';
    1: AIConnection1.DriverName := 'OpenAI';
    2: AIConnection1.DriverName := 'Claude';
    3: AIConnection1.DriverName := 'Ollama';
  end;

  AIConnection1.Params.APIKey := ApiKey.Text;
  AIConnection1.Params.Model := Model.Text;

  AIChatAgent1.Model := Model.Text;
end;

procedure TfrmAnalytics.SetBusy(ABusy: Boolean);
begin
  BAsk.Enabled := not ABusy;
  BSample.Enabled := not ABusy;
  BCancel.Enabled := ABusy;
end;

procedure TfrmAnalytics.AIChatAgent1Cancelled(Sender: TObject);
begin
  TThread.Queue(nil, procedure begin Memo1.Lines.Add('[Cancelled]'); SetBusy(False); end);
end;

procedure TfrmAnalytics.AIChatAgent1Complete(Sender: TObject;
  const AText: string; AKind: TAIProgressKind);
var
  S: string;
  N: Integer;
begin
  S := AText;
  N := S.IndexOf('[');
  if N > 0 then S := S.Substring(N);
  N := S.LastIndexOf(']');
  if N >= 0 then S := S.Substring(0, N+1);

  TThread.Queue(nil, procedure
  begin
    try
      FData.Clear;
      LoadSampleData(FData, Provider.Text + ' agent', S);

      RefreshViews;
      Memo1.Lines.Add('Structured metrics added from response.');
    except
      on E: Exception do Memo1.Lines.Add('Could not parse metric JSON: ' + E.Message);
    end;

    SetBusy(False);
  end);
end;

procedure TfrmAnalytics.AIChatAgent1Error(Sender: TObject;
  AException: Exception);
var M: string;
begin
  M := AException.Message;
  TThread.Queue(nil, procedure begin Memo1.Lines.Add('[Error] ' + M); SetBusy(False); end);
end;

procedure TfrmAnalytics.AIChatAgent1Partial(Sender: TObject;
  const AText: string; AKind: TAIProgressKind);
begin
  if AKind in [TAIProgressKind.ContentStarted, TAIProgressKind.Content] then
    TThread.Queue(nil,
       procedure
       begin
         Memo1.Lines[Memo1.Lines.Count-1] :=
         Memo1.Lines[Memo1.Lines.Count-1] + AText;
       end);
end;

procedure TfrmAnalytics.AIChatAgent1Started(Sender: TObject);
begin
  TThread.Queue(nil, procedure begin SetBusy(True); end);
end;

procedure TfrmAnalytics.AddSample;
begin
  FData.Clear;
  FData.Add('Sample', 'Revenue', 128500, 'USD', 'Illustrative quarterly revenue');
  FData.Add('Sample', 'Cost', 77300, 'USD', 'Illustrative operating cost');
  FData.Add('Sample', 'Gross margin', 39.8, '%', 'Illustrative margin percent');
  FData.Add('Sample', 'Growth', 12.4, '%', 'Illustrative year-over-year growth');

  RefreshViews;
end;

procedure TfrmAnalytics.RefreshViews;
var I: Integer; R: TMetricRow;
begin
  Series1.Clear;

  StringGrid1.RowCount := FData.Count + 1;

  for I := 0 to FData.Count - 1 do
  begin
    R := FData.Row(I);
    StringGrid1.Cells[0,I+1] := R.Agent;
    StringGrid1.Cells[1,I+1] := R.Metric;
    StringGrid1.Cells[2,I+1] := FormatFloat('0.##', R.Value, TFormatSettings.Invariant);
    StringGrid1.Cells[3,I+1] := R.UnitName;
    StringGrid1.Cells[4,I+1] := R.Notes;

    Series1.Add(R.Value, R.Metric);
  end;
end;

end.


