unit Marks_Shadow;
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
  Base, TeEngine, Series, TeeProcs, Chart, TeCanvas, TeePenDlg;

type
  TMarksShadow = class(TBaseForm)
    Series1: TBarSeries;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Shape1: TShape;
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksShadow.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then Series1.Marks.Shadow.Size:=UpDown1.Position
                       else Series1.Marks.Shadow.Size:=0
end;

procedure TMarksShadow.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(6);
  Shape1.Brush.Color:=Series1.Marks.Shadow.Color;
end;

procedure TMarksShadow.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  With Shape1.Brush do
  begin
    Color:=TButtonColor.Edit(Self,Color);
    Series1.Marks.Shadow.Color:=Color;
  end;
end;

procedure TMarksShadow.Edit1Change(Sender: TObject);
begin
  Series1.Marks.Shadow.Size:=UpDown1.Position;
  CheckBox1.Checked:=Series1.Marks.Shadow.Size>0;
end;

initialization
  RegisterClass(TMarksShadow);
end.
