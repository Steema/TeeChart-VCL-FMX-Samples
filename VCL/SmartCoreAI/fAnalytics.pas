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
  uAnalyticsData, uAnalyticsSample;

type
  TfrmAnalytics = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Chart1: TChart;
    StringGrid1: TStringGrid;
    BSample: TButton;
    BAsk: TButton;
    BCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BSampleClick(Sender: TObject);
    procedure BAskClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
  private
    FConnection: TAIConnection;
    FAgent: TAIChatAgent;
    FData: TAnalyticsData;
    FSeries: TBarSeries;
    FProvider: TComboBox;
    FModel, FApiKey, FPrompt: TEdit;
    procedure BuildUI;
    procedure ProviderChange(Sender: TObject);
    procedure AgentStarted(Sender: TObject);
    procedure AgentPartial(Sender: TObject; const AText: string; AKind: TAIProgressKind);
    procedure AgentComplete(Sender: TObject; const AText: string; AKind: TAIProgressKind);
    procedure AgentError(Sender: TObject; AException: Exception);
    procedure AgentCancelled(Sender: TObject);
    procedure ConfigureConnection;
    procedure RefreshViews;
    procedure SetBusy(ABusy: Boolean);
    procedure AddSample;
  end;

var
  frmAnalytics: TfrmAnalytics;

implementation

{$R *.dfm}

procedure TfrmAnalytics.FormCreate(Sender: TObject);
begin
  FData := TAnalyticsData.Create(Self);
  FConnection := TAIConnection.Create(Self);

  FAgent := TAIChatAgent.Create(Self);
  FAgent.Connection := FConnection;
  FAgent.Stream := True;
  FAgent.AutoRegisterInvokables := False;
  FAgent.Options.MaxOutputTokens := 1400;
  FAgent.Instructions.Text :=
    'You are a careful business data analyst. Reply first with one valid JSON array of metric objects, no markdown fences. Each object has metric (short name), value (number), unit (short unit), notes (short explanation). Example: [{"metric":"Revenue","value":1250.5,"unit":"USD","notes":"Q1 estimate"}]. Then add a short plain-English explanation after the array. Use numerical values grounded in the user request; when data is missing, clearly state assumptions and use an illustrative estimate.';
  FAgent.OnStarted := AgentStarted;
  FAgent.OnPartial := AgentPartial;
  FAgent.OnComplete := AgentComplete;
  FAgent.OnError := AgentError;
  FAgent.OnCancelled := AgentCancelled;

  BuildUI;
  ConfigureConnection;
  AddSample;
end;

procedure TfrmAnalytics.BAskClick(Sender: TObject);
var P: string;
begin
  ConfigureConnection;

  FAgent.NewSession;

  P := Trim(FPrompt.Text);
  if P.IsEmpty then Exit;
  FAgent.Prompt := P + sLineBreak + sLineBreak +
    'Return your metric array first, then a short explanation. Keep numeric values JSON-compatible.';

  Memo1.Lines.Add('User: ' + P);
  Memo1.Lines.Add('Agent: ');

  TTask.Run(procedure begin FAgent.Execute; end);
end;

procedure TfrmAnalytics.BCancelClick(Sender: TObject);
begin
  FAgent.Cancel;
end;

procedure TfrmAnalytics.BSampleClick(Sender: TObject);
begin
  AddSample;
  Memo1.Lines.Add('Loaded four local sample metrics. No provider call was made.');
end;

procedure TfrmAnalytics.BuildUI;
var
  L: TLabel;
  P : TPanel;
begin
  P:=Panel1;

  L := TLabel.Create(Self); L.Parent := P; L.SetBounds(12, 10, 75, 20); L.Caption := 'Provider';
  FProvider := TComboBox.Create(Self); FProvider.Parent := P; FProvider.SetBounds(12, 32, 150, 27);
  FProvider.Style := csDropDownList; FProvider.Items.AddStrings(['Gemini','OpenAI','Claude','Ollama']); FProvider.ItemIndex := 0; FProvider.OnChange := ProviderChange;
  L := TLabel.Create(Self); L.Parent := P; L.SetBounds(174, 10, 70, 20); L.Caption := 'Model';
  FModel := TEdit.Create(Self); FModel.Parent := P; FModel.SetBounds(174, 32, 205, 27); FModel.Text := 'gemini-3.1-flash-lite';
  L := TLabel.Create(Self); L.Parent := P; L.SetBounds(391, 10, 95, 20); L.Caption := 'API key (optional)';
  FApiKey := TEdit.Create(Self); FApiKey.Parent := P; FApiKey.SetBounds(391, 32, 240, 27); FApiKey.PasswordChar := '*';

  FPrompt := TEdit.Create(Self); FPrompt.Parent := P; FPrompt.SetBounds(12, 75, 980, 27); FPrompt.Text := 'Compare revenue, cost and margin for a fictional Q1 business. Return 4 illustrative metrics.';

  Chart1.Title.Text.Text := 'Agent Metrics';
  Chart1.Legend.Visible := False;
  FSeries := TBarSeries.Create(Self);
  Chart1.AddSeries(FSeries);

  Chart1.Parent:=Self;

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
  case FProvider.ItemIndex of
    0: FModel.Text := 'gemini-3.1-flash-lite';
    1: FModel.Text := 'gpt-5.6-luna';
    2: FModel.Text := 'claude-haiku-4-5';
    3: FModel.Text := 'qwen3-vl:2b';
  end;

  ConfigureConnection;
end;

procedure TfrmAnalytics.ConfigureConnection;
begin
  case FProvider.ItemIndex of
    0: FConnection.DriverName := 'Gemini';
    1: FConnection.DriverName := 'OpenAI';
    2: FConnection.DriverName := 'Claude';
    3: FConnection.DriverName := 'Ollama';
  end;

  FConnection.Params.APIKey := FApiKey.Text;
  FConnection.Params.Model := FModel.Text;

  FAgent.Model := FModel.Text;
end;

procedure TfrmAnalytics.SetBusy(ABusy: Boolean);
begin
  BAsk.Enabled := not ABusy;
  BSample.Enabled := not ABusy;
  BCancel.Enabled := ABusy;
end;

procedure TfrmAnalytics.AgentStarted(Sender: TObject);
begin
  TThread.Queue(nil, procedure begin SetBusy(True); end);
end;

procedure TfrmAnalytics.AgentPartial(Sender: TObject; const AText: string; AKind: TAIProgressKind);
begin
  if AKind in [TAIProgressKind.ContentStarted, TAIProgressKind.Content] then
    TThread.Queue(nil,
       procedure
       begin
         Memo1.Lines[Memo1.Lines.Count-1] :=
         Memo1.Lines[Memo1.Lines.Count-1] + AText;
       end);
end;

procedure TfrmAnalytics.AgentComplete(Sender: TObject; const AText: string; AKind: TAIProgressKind);
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
      LoadSampleData(FData, FProvider.Text + ' agent', S);

      RefreshViews;
      Memo1.Lines.Add('Structured metrics added from response.');
    except
      on E: Exception do Memo1.Lines.Add('Could not parse metric JSON: ' + E.Message);
    end;

    SetBusy(False);
  end);
end;

procedure TfrmAnalytics.AgentError(Sender: TObject; AException: Exception);
var M: string;
begin
  M := AException.Message;
  TThread.Queue(nil, procedure begin Memo1.Lines.Add('[Error] ' + M); SetBusy(False); end);
end;

procedure TfrmAnalytics.AgentCancelled(Sender: TObject);
begin
  TThread.Queue(nil, procedure begin Memo1.Lines.Add('[Cancelled]'); SetBusy(False); end);
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
  FSeries.Clear;

  StringGrid1.RowCount := FData.Count + 1;

  for I := 0 to FData.Count - 1 do
  begin
    R := FData.Row(I);
    StringGrid1.Cells[0,I+1] := R.Agent;
    StringGrid1.Cells[1,I+1] := R.Metric;
    StringGrid1.Cells[2,I+1] := FormatFloat('0.##', R.Value, TFormatSettings.Invariant);
    StringGrid1.Cells[3,I+1] := R.UnitName;
    StringGrid1.Cells[4,I+1] := R.Notes;

    FSeries.Add(R.Value, R.Metric);
  end;
end;

end.


