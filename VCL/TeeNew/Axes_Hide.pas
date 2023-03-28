unit Axes_Hide;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeSurfa;

type
  TAxesHide = class(TBaseForm)
    CheckBox1: TCheckBox;
    Series1: TColorGridSeries;
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math;

procedure TAxesHide.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
     Chart1.Axes.Hide
  else
     Chart1.Axes.Visible:=True;
end;

initialization
  RegisterClass(TAxesHide);
end.
