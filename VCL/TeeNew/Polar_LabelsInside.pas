unit Polar_LabelsInside;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeePolar, TeeProcs, Chart;

type
  TPolarLabelsInside = class(TBaseForm)
    Series1: TPolarSeries;
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

procedure TPolarLabelsInside.CheckBox1Click(Sender: TObject);
begin
  Series1.CircleLabelsInside:=CheckBox1.Checked
end;

procedure TPolarLabelsInside.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);
  Series1.CircleLabelsInside:=True;
end;

initialization
  RegisterClass(TPolarLabelsInside);
end.
