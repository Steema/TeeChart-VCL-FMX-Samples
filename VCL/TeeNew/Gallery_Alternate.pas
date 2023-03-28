unit Gallery_Alternate;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  TeeGalleryAlternate, TeePenDlg;

type
  TGalleryAlternate = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Gallery : TTeeGalleryForm;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TGalleryAlternate.FormShow(Sender: TObject);
begin
  Gallery:=TTeeGalleryForm.Create(Self);
  Gallery.Align:=alClient;
  TTeeVCL.AddFormTo(Gallery,Self);
end;

procedure TGalleryAlternate.FormDestroy(Sender: TObject);
begin
  Gallery.Free;
end;

initialization
  RegisterClass(TGalleryAlternate);
end.
