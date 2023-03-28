unit ComboFlat_Demo;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeePenDlg;

type
  TComboFlatForm = class(TBaseForm)
    ComboFlat1: TComboFlat;
    Label1: TLabel;
    Series1: TBarSeries;
    procedure ComboFlat1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TComboFlatForm.ComboFlat1Change(Sender: TObject);
begin
  Series1.Clear;
  Series1.Add(Random(1000),ComboFlat1.Items[ComboFlat1.ItemIndex]);

  Series1.Marks.Style:=smsLabelValue;
end;

procedure TComboFlatForm.FormCreate(Sender: TObject);
begin
  inherited;
  ComboFlat1.ItemIndex:=0;
end;

procedure TComboFlatForm.FormShow(Sender: TObject);
begin
  inherited;
  ComboFlat1Change(Self); 
end;

initialization
  RegisterClass(TComboFlatForm);
end.
