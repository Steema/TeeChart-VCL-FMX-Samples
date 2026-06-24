unit Unit_GIS;

interface

{
  www.steema.com

  Example using TeeChart GIS layers (raster maps) from web servers
  (Carto, OSM, Esri etc)

  Important note:
    Google servers (Satellit, Hybrid, Terrain) are disabled here due to
    license restrictions (desktop requests and offline caching are prohibited).

  Graphic tiles can be optionally stored in a disk cache, see below.
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeGIS,
  VCLTee.TeCanvas, VCLTee.EditChar;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    CBMapServer: TComboBox;
    Chart1: TChart;
    Button1: TButton;
    Button2: TButton;
    procedure CBMapServerChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TMainForm.Button2Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

procedure TMainForm.CBMapServerChange(Sender: TObject);
begin
  // These servers arent available, see the TeeGIS.pas unit notes on top.
  // Google Satellit
  // Google Hibrid
  // Google Terrain

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
  GIS.SetBounds(41,42, 1.4, 3.55);

  // Enable zoooming in-out with the mouse wheel
  Chart1.Panning.MouseWheel:=pmwNone;
  Chart1.Zoom.MouseWheel:=pmwNormal;

  // Optional: Disk cache to store map graphic tiles (to avoid http requests)

  tmpFolder:=TPath.Combine(TPath.GetTempPath,'TeeGISCache');
  ForceDirectories(tmpFolder);

  GIS.Path:=tmpFolder;

  // GDI instead of GDI+
  // Chart1.Canvas:=TTeeCanvas3D.Create;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CBMapServer.ItemIndex:=0;
  CBMapServerChange(Self);
end;

end.