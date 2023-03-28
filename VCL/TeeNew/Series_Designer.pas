unit Series_Designer;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Menus,
  SysUtils, Classes;

type
  TSeriesDesigner = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Image2: TImage;
    Image1: TImage;
    Image3: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  RegisterClass(TSeriesDesigner);
end.
