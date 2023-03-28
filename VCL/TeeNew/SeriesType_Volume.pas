unit SeriesType_Volume;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF D16}
  System.UITypes,
  {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeePenDlg, TeEngine, Series, CandleCh, TeeProcs, Chart;

type
  TVolumeForm = class(TBaseForm)
    Series1: TVolumeSeries;
    ButtonPen1: TButtonPen;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonPen1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses EditChar;

procedure TVolumeForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(100);

  ButtonPen1.LinkPen(Series1.Pen);
end;

procedure TVolumeForm.Button1Click(Sender: TObject);
begin
  EditSeries(Self,Series1);
end;

procedure TVolumeForm.ButtonPen1Click(Sender: TObject);
begin
  inherited;
  Series1.SeriesColor:=Series1.Pen.Color;
end;

initialization
  RegisterClass(TVolumeForm);
end.
