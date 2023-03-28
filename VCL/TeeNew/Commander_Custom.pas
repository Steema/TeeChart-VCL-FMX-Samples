unit Commander_Custom;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,  Buttons,
  Base, TeeComma, TeeProcs, TeEngine, Chart;

type
  TCommanderCustom = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Button1Click(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeCommanderEditor;

procedure TCommanderCustom.FormCreate(Sender: TObject);
var tmpButton : TSpeedButton;
begin
  inherited;
  TeeCommander1.CreateControls([tcbNormal, tcbRotate, tcbSeparator, tcb3D ]);
  { add a new custom button to TeeCommader }
  tmpButton:=TeeCommander1.CreateButton(100, Button1Click, 'Customize','',0);

  { set the custom button bitmap }
  tmpButton.Glyph.Assign(SpeedButton1.Glyph);
  tmpButton.NumGlyphs:=SpeedButton1.NumGlyphs; { 2 images in button }
end;

procedure TCommanderCustom.Button1Click(Sender: TObject);
begin
  TeeCommanderEdit(Self,TeeCommander1);
end;

procedure TCommanderCustom.SpeedButton1Click(Sender: TObject);
begin
  ShowMessage('(This is an example)');
end;

initialization
  RegisterClass(TCommanderCustom);
end.
