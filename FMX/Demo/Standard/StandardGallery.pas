unit StandardGallery;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, Base,
  FMXTee.Chart.GalleryPanel, FMXTee.Editor.Chart, FMX.Controls.Presentation;

type
  TStandardDemo = class(TBaseForm)
    View3D: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure View3DChange(Sender: TObject);
  private
    { Private declarations }
    Gallery : TChartGalleryPanel;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  FMXTee.Constants;

procedure TStandardDemo.BEditClick(Sender: TObject);
begin
  TChartEditForm.Edit(Self,Gallery.SelectedChart);
end;

procedure TStandardDemo.FormCreate(Sender: TObject);
begin
  inherited;
  Gallery:=TChartGalleryPanel.Create(Self);
  Gallery.Parent:=Self;
  Gallery.Align:=TAlignLayout.Client;

  Gallery.CreateGalleryPage(TeeMsg_GalleryStandard);
end;

procedure TStandardDemo.View3DChange(Sender: TObject);
begin
  Gallery.View3D:=View3D.IsChecked;
end;

initialization
  RegisterClass(TStandardDemo);
end.
