unit Series_PolarZoom;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools, TeeComma, TeePolar;

type
  TPolarZoom = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    Series1: TPolarSeries;
    Series2: TPolarSeries;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PolarZoom: TPolarZoom;

implementation

{$R *.dfm}

procedure TPolarZoom .FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues;
  Series2.FillSampleValues;
end;

initialization
  RegisterClass(TPolarZoom );
end.
