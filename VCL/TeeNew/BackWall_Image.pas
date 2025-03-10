unit BackWall_Image;
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
  Base, TeeProcs, TeEngine, Chart;

type
  TBackWallImage = class(TBaseForm)
    CheckBox1: TCheckBox;
    Image1: TImage;
    Button1: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeBrushDlg;

procedure TBackWallImage.CheckBox1Click(Sender: TObject);
begin
  inherited;
  Chart1.BackWall.Transparent:=not CheckBox1.Checked
end;

procedure TBackWallImage.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.BackWall.Transparent:=False;
  Chart1.BackWall.Brush.Image.Assign(Image1.Picture);

  Chart1.BackWall.Brush.Color:=clGreen;
end;

procedure TBackWallImage.Button1Click(Sender: TObject);
begin
  TBrushDialog.Edit(Self,Chart1.BackWall.Brush)
end;

initialization
  RegisterClass(TBackWallImage);
end.
