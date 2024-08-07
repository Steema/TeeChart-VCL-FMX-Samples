unit ChartAsDataset;

interface

uses
  {$IFDEF D17}
  System.Rtti, FMX.Grid.Style, Data.Bind.Controls,
  System.Bindings.Outputs, FMX.ScrollBox, FMX.StdCtrls,
  FMX.Controls.Presentation,
  {$ENDIF}
  
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Base, FMXTee.Engine,
  FMXTee.Series, Data.DB, FMXTee.Dataset, FMX.Layouts, FMX.Grid, FMXTee.Procs,
  FMXTee.Chart, FMX.Bind.Navigator, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  FMX.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope,
  Data.Bind.DBLinks, FMX.Bind.DBLinks
  
  {$IFNDEF MACOS}
  {$IFNDEF CPUX64}
  , MidasLib
  {$ENDIF}
  {$ENDIF}
  ;

type
  TChartDataSetDemo = class(TBaseForm)
    Chart1: TChart;
    ChartDataSet1: TChartDataSet;
    Series1: THorizBarSeries;
    Label1: TLabel;
    Layout1: TLayout;
    StringGrid1: TStringGrid;
    BindNavigator1: TBindNavigator;
    DataSource1: TDataSource;
    BindingsList1: TBindingsList;
    BindScopeDB1: TBindScopeDB;
    DBLinkStringGrid11: TBindDBGridLink;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure DBLinkStringGrid11Activated(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TChartDataSetDemo.DBLinkStringGrid11Activated(Sender: TObject);
begin
  inherited;
//  DBLinkStringGrid11.Columns.Items[0].StyleLookup:='ColorColumn';
end;

procedure TChartDataSetDemo.FormCreate(Sender: TObject);
begin
  inherited;

  ChartDataSet1.Close;

  Series1.Add(50,'Apples');
  Series1.Add(75,'Oranges');
  Series1.Add(34,'Bananas');
  Series1.Add(91,'Kiwis');
  Series1.Add(19,'Pears');

  ChartDataSet1.Open;
end;

initialization
  RegisterClass(TChartDataSetDemo);
end.
