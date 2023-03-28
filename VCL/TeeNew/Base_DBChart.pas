unit Base_DBChart;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, DBTables,
  Base, TeeProcs, TeEngine, Chart, DBChart, Series, DB;

type
  TBaseDBChart = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    DBChart1: TDBChart;
    BaseSplitter1: TSplitter;
    procedure Memo1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure CheckTable(ATable:TTable);

implementation

{$R *.dfm}

Uses EditChar, DBEditCh;  { to show Database DBChart editor dialogs }

procedure CheckTable(ATable:TTable);
var tmp : TStringList;
begin
  tmp:=TStringList.Create;
  try
    Session.GetAliasNames(tmp);

    if tmp.IndexOf(ATable.DatabaseName)=-1 then
    begin
      if tmp.IndexOf('BCDEMOS')=-1 then
         Raise Exception.Create('Database BDE Alias "BCDEMOS" cannot be found.');
      ATable.DatabaseName:='BCDEMOS';
    end;
  finally
    tmp.Free;
  end;
end;

procedure TBaseDBChart.Memo1DblClick(Sender: TObject);
begin
  EditChart(Self,DBChart1);
end;

end.
