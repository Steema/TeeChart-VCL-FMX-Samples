unit Series_ImagePoint;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ExtDlgs,
  Base, TeCanvas, TeEngine, Series, ImaPoint, TeeProcs, Chart;

type
  TImagePointForm = class(TBaseForm)
    Series1: TImagePointSeries;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Label3: TLabel;
    Edit2: TEdit;
    UpDown2: TUpDown;
    Label4: TLabel;
    ScrollBar1: TScrollBar;
    procedure Image1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses TeeBrushDlg;

procedure TImagePointForm.Image1Click(Sender: TObject);
var tmpSt : String;
begin
  tmpSt:=TeeGetPictureFileName(Self);
  if tmpSt<>'' then
  begin
    Series1.ImagePoint.LoadFromFile(tmpSt);
    Image1.Picture.Assign(Series1.ImagePoint);
  end;
end;

procedure TImagePointForm.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(10);
  Image1.Picture.Assign(Series1.ImagePoint);
  UpDown1.Position:=Series1.Pointer.HorizSize;
  UpDown2.Position:=Series1.Pointer.VertSize;
end;

procedure TImagePointForm.Edit1Change(Sender: TObject);
begin
  if Showing then Series1.Pointer.HorizSize:=UpDown1.Position
end;

procedure TImagePointForm.Edit2Change(Sender: TObject);
begin
  if Showing then Series1.Pointer.VertSize:=UpDown2.Position
end;

procedure TImagePointForm.ScrollBar1Change(Sender: TObject);
begin
  Series1.Pointer.Picture.Angle:=ScrollBar1.Position;
end;

initialization
  RegisterClass(TImagePointForm);
end.
