unit Pie_AutoMarkPosition;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeEngine, Series, TeeProcs, Chart;

type
  TPieAutoMarkPos = class(TBaseForm)
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

procedure TPieAutoMarkPos.CheckBox1Click(Sender: TObject);
begin
  Series1.AutoMarkPosition:=CheckBox1.Checked;
end;

procedure TPieAutoMarkPos.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(16);
end;

initialization
  RegisterClass(TPieAutoMarkPos);
end.
