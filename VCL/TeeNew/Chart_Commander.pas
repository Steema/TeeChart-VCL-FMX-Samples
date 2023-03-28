unit Chart_Commander;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeeComma;

type
  TTeeCommanderForm = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    Series1: TPieSeries;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTeeCommanderForm.CheckBox1Click(Sender: TObject);
begin
  TeeCommander1.Vertical:=CheckBox1.Checked;
  if TeeCommander1.Vertical then TeeCommander1.Align:=alLeft
                            else TeeCommander1.Align:=alTop;
end;

procedure TTeeCommanderForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

initialization
  RegisterClass(TTeeCommanderForm);
end.
