unit Volume_Origin;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeEngine, Series, CandleCh, TeeProcs, Chart;

type
  TVolumeOrigin = class(TBaseForm)
    Series1: TVolumeSeries;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TVolumeOrigin.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(50);
end;

procedure TVolumeOrigin.Edit1Change(Sender: TObject);
begin
  Series1.YOrigin:=UpDown1.Position;
end;

procedure TVolumeOrigin.CheckBox1Click(Sender: TObject);
begin
  Series1.UseYOrigin:=CheckBox1.Checked;
  Edit1.Enabled:=CheckBox1.Checked;
  UpDown1.Enabled:=CheckBox1.Checked;
end;

initialization
  RegisterClass(TVolumeOrigin);
end.
