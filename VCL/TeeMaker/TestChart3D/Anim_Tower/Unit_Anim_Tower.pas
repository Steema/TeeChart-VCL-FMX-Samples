unit Unit_Anim_Tower;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, TeeSurfa, TeeMapSeries, TeeWorldSeries, TeCanvas,
  TeeBlocks, TeeChartBlock, ExtCtrls, TeeProcs, TeeMakerControl, TeeChart3D,
  StdCtrls, TeeGLCanvas, TeeanimateEditor, TeeNumberAnimation,
  TeeMakerEditor, TeeEdiSeri, TeeTowerEdit, TeeBlockCanvas, TeeBlockAnimations,
  Chart, EditChar;

type
  TForm1 = class(TForm)
    ChartBlock1: TChartBlock;
    Chart3D1: TChart3D;
    Series1: TTowerSeries;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure CreateAnimations;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses TeeAnimate;

{$R *.dfm}

type
  TMakerAccess=class(TMaker);
  TPanelAccess=class(TCustomTeePanelExtended);
  
procedure TForm1.FormCreate(Sender: TObject);
begin
//  Chart3D1.Options.Floor.Reflection:=0;
  Series1.FillSampleValues();
  ChartBlock1.CreateItems;

  TMakerAccess(Chart3D1).CheckMouseRuntime:=False;

  TPanelAccess(Chart3D1).DebugPaint:=TTeeDebugPaint.Create;

  CreateAnimations;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Chart3D1.Animates[0].Speed:=60;
  Chart3D1.Animates[0].Animate.Play;
end;

procedure TForm1.CreateAnimations;
begin
  Chart3D1.Animates[0].Animations.Clear;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Chart3D1.Animates[0].Animate;

    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Overlap:=5;
    Number.UseEndValue:=False;

    with Number do
    begin
      Duration:=40;
      PropertyName:='Bounds.Top';
      StartValue:=34;
      EndValue:=0;
    end;

    Prepare;
  end;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Chart3D1.Animates[0].Animate;

    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Overlap:=5;

    with Number do
    begin
      Duration:=40;
      PropertyName:='Format.Transparency';
      StartValue:=200;
      EndValue:=0;
    end;

    Prepare;
  end;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Chart3D1.Animates[0].Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';

    Overlap:=5;

    with Number do
    begin
      Duration:=80;
      PropertyName:='Rotation.X';
      StartValue:=0;
      EndValue:=360;
    end;

    Prepare;
  end;

  with TBlocksAnimation.Create(Self) do
  begin
    Animate:=Chart3D1.Animates[0].Animate;
    Instance:=ChartBlock1;
    PropertyName:='Series.Series1';
    Number.UseEndValue:=False;
    
    Overlap:=5;

    with Number do
    begin
      Duration:=100;
      PropertyName:='Format.Color';
      StartValue:=clWhite;
      EndValue:=clBlack;
    end;

    Prepare;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  EditChart(Self,ChartBlock1.Chart);
  CreateAnimations;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Chart3D1)
end;

end.
