unit ComboFlat_DropDownWidth;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, ComboFlat_SelectedObject;

type
  TComboFlatDropDownWidth = class(TComboFlatSelectedObject)
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math;

procedure TComboFlatDropDownWidth.FormCreate(Sender: TObject);
begin
  inherited;

  cbSeries.ItemIndex:=1;
  cbSeriesChange(self);

  UpDown1.Position:=cbSeries.DropDownWidth;
end;

procedure TComboFlatDropDownWidth.Edit1Change(Sender: TObject);
begin
  if UpDown1.Position < cbSeries.Width then
  begin
    ShowMessage('Width cannot be less than ComboFlat width');
    UpDown1.Position:=cbSeries.DropDownWidth;
  end
  else
    cbSeries.DropDownWidth:=UpDown1.Position;
end;

initialization
  RegisterClass(TComboFlatDropDownWidth);
end.
