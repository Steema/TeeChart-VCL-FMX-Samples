unit TeePointItemEditor;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  TeCanvas, TeeBlocks, TeeExtruded;

type
  TPointItemEditor = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EX: TEdit;
    EY: TEdit;
    EZ: TEdit;
    Button1: TButton;
    LabelLink: TLabel;
    BClearLink: TButton;
    procedure Button1Click(Sender: TObject);
    procedure BClearLinkClick(Sender: TObject);
    procedure EXChange(Sender: TObject);
    procedure EYChange(Sender: TObject);
    procedure EZChange(Sender: TObject);
  private
    { Private declarations }

    Blocks : TBlocks;
    Point  : TPointItem;

    procedure RefreshLink;
  public
    { Public declarations }
    procedure RefreshPoint(APoint:TPointItem; ABlocks:TBlocks);
  end;

implementation

{$R *.dfm}

uses
  TeeProcs, TeeActionGallery, TeeBlockEditor;

{ TPointItemEditor }

procedure TPointItemEditor.RefreshPoint(APoint: TPointItem; ABlocks:TBlocks);
begin
  Point:=APoint;
  Blocks:=ABlocks;

  if Assigned(Point) then
  with Point do
  begin
    EX.Text:=FloatToStr(Point.X);
    EY.Text:=FloatToStr(Point.X);
    EZ.Text:=FloatToStr(Point.X);

    RefreshLink;
  end;
end;

procedure TPointItemEditor.RefreshLink;
begin
  LabelLink.Caption:=TActionGallery.PropertyText(Point.Link);

  with Point.Link do
       BClearLink.Enabled:=(Instance<>nil) or (PropertyName<>'');

  EnableControls(not BClearLink.Enabled,[EX,EY,EZ]);
end;

procedure TPointItemEditor.Button1Click(Sender: TObject);
begin
  if TMakerPropertySelector.ModalShow(Self,Blocks,Point.Link) then
     RefreshLink;
end;

procedure TPointItemEditor.BClearLinkClick(Sender: TObject);
begin
  Point.Link:=nil;
  BClearLink.Enabled:=False;
  LabelLink.Caption:='';

  EnableControls(True,[EX,EY,EZ]);
end;

procedure TPointItemEditor.EXChange(Sender: TObject);
begin
  Point.Point.X:=StrToFloat(EX.Text);
end;

procedure TPointItemEditor.EYChange(Sender: TObject);
begin
  Point.Point.Y:=StrToFloat(EY.Text);
end;

procedure TPointItemEditor.EZChange(Sender: TObject);
begin
  Point.Point.Z:=StrToFloat(EZ.Text);
end;

end.
