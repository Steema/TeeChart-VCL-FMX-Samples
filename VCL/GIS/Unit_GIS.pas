unit Unit_GIS;

interface

{
  www.steema.com

  Example using TeeChart GIS layers (raster maps) from web servers
  (Google, Carto, OSM etc)

  Graphic tiles can be optionally stored in a disk cache, see below.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeGIS;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    CBMapServer: TComboBox;
    Chart1: TChart;
    Button1: TButton;
    procedure CBMapServerChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    GIS : TGISRaster;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  IOUtils;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TeeGotoURL(Handle,GIS.Path);
end;

procedure TMainForm.CBMapServerChange(Sender: TObject);
begin
  GIS.MapServer:=TMapServer(CBMapServer.ItemIndex);

  Chart1.Title.Caption:=CBMapServer.Text+' '+GIS.Attribution;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var tmpFolder : String;
begin
  Chart1.View3D:=False;

  GIS:=TGISRaster.Create(Self);
  GIS.ParentChart:=Chart1;

  // Cosmetic
  GIS.HorizAxis:=aBothHorizAxis;
  GIS.VertAxis:=aBothVertAxis;

  // Latitude and Longitude
  GIS.SetBounds(41,42, 1.4,2.8);

  // Enable zoooming in-out with the mouse wheel
  Chart1.Panning.MouseWheel:=pmwNone;
  Chart1.Zoom.MouseWheel:=pmwNormal;

  // Optional: Disk cache to store map graphic tiles (to avoid http requests)

  tmpFolder:=TPath.Combine(TPath.GetTempPath,'TeeGISCache');
  ForceDirectories(tmpFolder);

  GIS.Path:=tmpFolder;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CBMapServer.ItemIndex:=2;
  CBMapServerChange(Self);
end;

end.