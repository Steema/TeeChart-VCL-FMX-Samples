unit DBChart_Agg;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  SysUtils, Classes;

type
  TDBChartAgg = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  RegisterClass(TDBChartAgg);
end.
