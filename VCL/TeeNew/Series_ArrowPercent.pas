unit Series_ArrowPercent;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, ArrowCha;

type
  TSeriesArrowPercent = class(TBaseForm)
    Series1: TArrowSeries;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    CBFilled: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CBFilledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSeriesArrowPercent.FormCreate(Sender: TObject);
begin
  inherited;

  // Set arrow head size in pixels
  Series1.ArrowHeight:=50;
  Series1.ArrowWidth:=50;

  // Arrow Percent applies to filled arrows
  Series1.Filled:=True;

  Edit1.Text:=IntToStr(Series1.ArrowPercent);
end;

procedure TSeriesArrowPercent.Edit1Change(Sender: TObject);
begin
  Series1.ArrowPercent:=StrToInt(Edit1.Text);
end;

procedure TSeriesArrowPercent.CBFilledClick(Sender: TObject);
begin
  Series1.Filled:=CBFilled.Checked;
end;

initialization
  RegisterClass(TSeriesArrowPercent);
end.
