unit Canvas_Transparency;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  {$IFNDEF LINUX}
  Jpeg,
  {$ENDIF}
  Base, TeEngine, Series, BubbleCh, TeeProcs, Chart;

type
  TTransparencyForm = class(TBaseForm)
    Series1: TBubbleSeries;
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    Label2: TLabel;
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TTransparencyForm.ScrollBar1Change(Sender: TObject);
begin
  Chart1.Title.Transparency:=ScrollBar1.Position;
  Chart1.Legend.Transparency:=ScrollBar1.Position;
  Chart1.BackWall.Transparency:=ScrollBar1.Position;

  Label2.Caption:=IntToStr(ScrollBar1.Position)+'%';
end;

procedure TTransparencyForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);

  ScrollBar1Change(Self);
end;

initialization
  RegisterClass(TTransparencyForm);
end.
