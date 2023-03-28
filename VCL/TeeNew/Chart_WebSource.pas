unit Chart_WebSource;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeURL, TeeProcs, TeEngine, Chart, TeeComma;

type
  TChartWebSourceForm = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    ChartWebSource1: TChartWebSource;
    TeeCommander1: TTeeCommander;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartWebSourceForm.Button1Click(Sender: TObject);
begin
  ChartWebSource1.URL:=Edit1.Text;

  Screen.Cursor:=crHourGlass;
  try
    ChartWebSource1.Execute;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

initialization
  RegisterClass(TChartWebSourceForm);
end.
