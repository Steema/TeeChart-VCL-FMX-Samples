unit Marks_DrawEvery;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeCanvas, TeEngine, Chart, Series;

type
  TMarksEvery = class(TBaseForm)
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Series1: TLineSeries;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksEvery.Edit1Change(Sender: TObject);
begin
  Series1.Marks.DrawEvery:=UpDown1.Position
end;

procedure TMarksEvery.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(50);
end;

initialization
  RegisterClass(TMarksEvery);
end.
