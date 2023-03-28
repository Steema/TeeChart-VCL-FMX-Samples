unit Marks_Styles;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TMarksStyles = class(TBaseForm)
    Label1: TLabel;
    cbStyle: TComboFlat;
    Series1: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksStyles.FormCreate(Sender: TObject);
begin
  inherited;
  cbStyle.ItemIndex:=10;
  cbStyleChange(Self);
end;

procedure TMarksStyles.cbStyleChange(Sender: TObject);
begin
  Series1.Marks.Style:=TSeriesMarksStyle(cbStyle.ItemIndex);
end;

initialization
  RegisterClass(TMarksStyles);
end.
