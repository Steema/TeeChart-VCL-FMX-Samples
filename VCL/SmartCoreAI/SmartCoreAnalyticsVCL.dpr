program SmartCoreAnalyticsVCL;

uses
  Vcl.Forms,
  fAnalytics in 'fAnalytics.pas' {frmAnalytics},
  uAnalyticsData in 'uAnalyticsData.pas',
  uAnalyticsSample in 'uAnalyticsSample.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnalytics, frmAnalytics);
  Application.Run;
end.
