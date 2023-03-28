unit Assign_Picture;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeePolar, TeeProcs, Chart;

type
  TAssignPicture = class(TBaseForm)
    Button1: TButton;
    Series1: TRadarSeries;
    Image1: TImage;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAssignPicture.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(8);
end;

procedure TAssignPicture.Button1Click(Sender: TObject);
begin
  Image1.Picture.Assign(Chart1);
end;

procedure TAssignPicture.FormShow(Sender: TObject);
begin
  inherited;
  Image1.Width:=Width div 2;
end;

initialization
  RegisterClass(TAssignPicture);
end.
