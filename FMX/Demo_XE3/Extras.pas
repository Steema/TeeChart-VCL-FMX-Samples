unit Extras;

interface

uses
  {$IFDEF D17}
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo;

type
  TFmxExtras = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization
  RegisterClass(TFmxExtras);
end.
