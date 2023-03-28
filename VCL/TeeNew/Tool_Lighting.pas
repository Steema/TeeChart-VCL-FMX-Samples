unit Tool_Lighting;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, TeeLighting, TeeTools, EditChar, TeeSurfa;

type
  TLightToolForm = class(TBaseForm)
    Button1: TButton;
    ChartTool1: TRotateTool;
    ChartTool2: TLightTool;
    Label1: TLabel;
    Series1: TTowerSeries;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TLightToolForm.Button1Click(Sender: TObject);
begin
  EditChartTool(Self, ChartTool2);
end;

procedure TLightToolForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
end;

initialization
  RegisterClass(TLightToolForm)
end.
