unit Commander_FullRotation;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeComma;

type
  TCommanderFullRotation = class(TBaseForm)
    cbFullRotation: TCheckBox;
    TeeCommander1: TTeeCommander;
    Series1: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure cbFullRotationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCommanderFullRotation.FormCreate(Sender: TObject);
begin
  inherited;

  with TeeCommander1 do
  begin
    FullRotation:=true;
    DefaultButton:=tcbRotate;
  end;
end;

procedure TCommanderFullRotation.cbFullRotationClick(Sender: TObject);
begin
  TeeCommander1.FullRotation:=cbFullRotation.Checked;
end;

initialization
  RegisterClass(TCommanderFullRotation);
end.
