{********************************************}
{ TeeMaker ChartBlock (TChart-->3D object)   }
{ Copyright (c) 2007-2023 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeChartBlock;
{$I TeeDefs.inc}

// Embedded Chart blocks

interface

uses
  {$IFNDEF D6}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  {$IFDEF D6}
  Types,
  {$ENDIF}

  {$IFDEF CLX}
  QForms, QControls, QExtCtrls, QStdCtrls, QComCtrls, QButtons, QDialogs,
  {$ELSE}
  Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Buttons, Dialogs,
  {$ENDIF}

  TeCanvas, TeeBlocks, TeeProcs, TeEngine, Chart;

type
  TCustomBlockChart=class(TChart)
  protected
    IBlock : TCustomBlock;

    procedure DoInternalDraw(const R:TRect);
    function Form3DClass:TClass; override;
    function GetDesignOwner:TComponent; override;
    procedure PanelPaint(const UserRect:TRect); override;

    {$IFNDEF D12}
    procedure SetParentComponent(Value: TComponent); override;
    {$ENDIF}
  public
    {$IFDEF D15}
    Constructor Create(AOwner:TComponent); override;
    {$ENDIF}

    Function GetParentComponent: TComponent; override;
    Function HasParent:Boolean; override;

    procedure Invalidate; override;

    {$IFDEF D12}
    procedure SetParentComponent(Value: TComponent); override;
    {$ENDIF}
  end;

  TChartBlock=class(TCustomObjectBlock, ITeeEventListener)
  private
    FChart        : TChart;
    FTemplateFile : String;

    IItemsOk    : Boolean;
    IItemsDone  : Boolean;
    IOwnedChart : Boolean;
    ITemplate   : TBlocks;

    procedure DrawChartDirect;
    function GetChart:TChart;
    function GetTemplate:TBlocks;
    procedure SetChart(const Value: TChart);
    procedure SetDirty;
    procedure SetTemplateFile(const Value: String);
  protected
    procedure ApplyTemplate;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    function GetEditor:String; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareForGallery; override;
    function SaveChildren:Boolean; override;
    procedure SetSeriesTemplate(Series:TChartSeries);
    procedure TeeEvent(Event:TTeeEvent);
  public
    DirectDraw : Boolean;
    KeepLeft   : Boolean;
    TextAsPictures : Boolean;
    TextDepth      : Double;

    Destructor Destroy; override;

    function BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean; override;
    procedure CreateItems;
    procedure DoDrawItems; override;
    procedure Draw; override;
    function Editor(const AOwner:TComponent; Embeddable:Boolean=False):TControl; override;
    function HasContents:Boolean; override;
    procedure LoadChart(const FileName:String);

    property Template:TBlocks read GetTemplate;
  published
    property Chart:TChart read GetChart write SetChart;
    property TemplateFile:String read FTemplateFile write SetTemplateFile;
  end;

  TChartBlockEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CBDirect: TCheckBox;
    Label1: TLabel;
    LabelItems: TLabel;
    Label2: TLabel;
    ETemplate: TEdit;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    CBTextAsPictures: TCheckBox;
    Label3: TLabel;
    TBTextDepth: TTrackBar;
    LTextDepth: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CBDirectClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBTextAsPicturesClick(Sender: TObject);
    procedure TBTextDepthChange(Sender: TObject);
  private
    { Private declarations }

    Chart : TChartBlock;

    procedure CountItems;
  public
    { Public declarations }

    class function CountAll(ABlocks:TBlocks):Integer;
  end;

implementation

uses
  {$IFDEF CLX}
  QGraphics,
  {$ELSE}
  Graphics,
  {$ENDIF}

  EditChar, OpenGL2, Series, TeeBlockCanvas, TeeLoadBlock, TeeMakerControl,
  TeeConst, TeeStore, TeePenDlg, TeeChartBlock3DEditor, TeeURL, TeeThemes,
  TeeThemeEditor;

{$R *.dfm}

{$IFDEF D15}
Constructor TCustomBlockChart.Create(AOwner: TComponent);
begin
  inherited;
  SetDesignVisible(False);
  ControlStyle:=ControlStyle + [csNoDesignVisible];
end;
{$ENDIF}

procedure TCustomBlockChart.DoInternalDraw(const R:TRect);
begin
  InternalDraw(R);
end;

function TCustomBlockChart.Form3DClass:TClass;
begin
  result:=TChartBlock3DEditor;
end;

function TCustomBlockChart.GetDesignOwner:TComponent;
begin
  if Assigned(IBlock) and
     Assigned(IBlock.Parent) and
     Assigned(IBlock.Parent.Parent) then
     result:=IBlock.Parent.Parent.Owner
  else
     result:=Owner;
end;

procedure TCustomBlockChart.PanelPaint(Const UserRect:TRect);
begin // No background behind Chart
end;

type
  TBlocksAccess=class(TBlocks);

procedure TCustomBlockChart.SetParentComponent(Value: TComponent);
begin
  if Value is TChartBlock then
  begin
    IBlock:=TCustomBlock(Value);

    TChartBlock(IBlock).FChart:=Self;
    Listeners.Add(TChartBlock(IBlock));
  end;
end;

Function TCustomBlockChart.GetParentComponent: TComponent;
begin
  result:=IBlock;
end;

Function TCustomBlockChart.HasParent:Boolean;
begin
  result:=True;
end;

procedure TCustomBlockChart.Invalidate;
begin
  if (not (csDestroying in ComponentState)) and Assigned(IBlock) then
     TChartBlock(IBlock).SetDirty;
end;

{ TChartBlock }

Destructor TChartBlock.Destroy;
begin
  FreeAndNil(ITemplate);
  Chart:=nil;
  inherited;
end;

procedure TChartBlock.DoDrawItems;
begin
  if DirectDraw and Assigned(Chart) then
     DrawChartDirect
  else
     inherited;
end;

type
  TTeePanelAccess=class(TCustomAxisPanel);

function TChartBlock.GetChart:TChart;
begin
  if not Assigned(FChart) then
  begin
    FChart:=TCustomBlockChart.Create(nil);
    TCustomBlockChart(FChart).IBlock:=Self;

    if Assigned(Owner) then
       FChart.Name:=TeeGetUniqueName(Owner,'ChartBlockChart');

    with FChart do
    begin
      Gradient.Visible:=False;
      Chart3DPercent:=100;

      with View3DOptions do
      begin
        Orthogonal:=False;
        VertOffset:=0;
      end;

      BevelOuter:=bvNone;
      BackWall.Transparent:=False;
      Walls.Size:=5;
      Title.Caption:='TeeChart';
    end;

    IOwnedChart:=True;

    TTeePanelAccess(FChart).Listeners.Add(Self);
  end;

  result:=FChart;
end;

procedure TChartBlock.Draw;
begin
  if not DirectDraw then
     if not IItemsDone then
        CreateItems;

  inherited;
end;

type
  TCustomTeePanelAccess=class(TCustomTeePanel);

procedure TChartBlock.CreateItems;

  function Maker:TMaker;
  begin
    result:=Parent.DrawBlocks.Parent as TMaker;
  end;

  procedure RemoveCustomBlocks(ABlocks:TBlocks);
  var t : Integer;
  begin
    t:=0;

    while t<ABlocks.Count do
    begin
      if Assigned(ABlocks[t].Owner) and (ABlocks[t].Owner<>Parent) then
         ABlocks.Remove(ABlocks[t])
      else
      begin
        if ABlocks[t] is TCustomObjectBlock then
           RemoveCustomBlocks(TCustomObjectBlock(ABlocks[t]).Items);

        Inc(t);
      end;
    end;
  end;

var tmp : TBlockCanvas;
    Old : TCanvas3D;
    OldParent : TWinControl;

    tmpMin,
    tmpMax : TPoint3DFloat;
begin
  if (not Assigned(Parent)) or
     (not Assigned(Parent.DrawBlocks.Parent)) then
        Exit;

  // if TeeYesNo('WARNING: Blocks will be replaced !!!)... then

  Chart.AutoRepaint:=False;

  Old:=TCustomTeePanelAccess(FChart).InternalCanvas;
  TCustomTeePanelAccess(FChart).InternalCanvas:=nil;

  RemoveCustomBlocks(Items);

  Items.Clear;

  tmp:=TBlockCanvas.CreateBlocks(Items);
  try
    tmp.LinesAsPipes:=True;
    tmp.PieTorus:=False;
    tmp.BeveledCubes:=True;
    tmp.TextAsPictures:=TextAsPictures;
    tmp.TextDepth:=TextDepth;

    FChart.Visible:=False;

    OldParent:=FChart.Parent;

    if not Assigned(OldParent) then
       FChart.Parent:=Parent.DrawBlocks.Parent;

    try
      FChart.Canvas:=tmp;

      FChart.Canvas.View3DOptions:=FChart.View3DOptions; // <-- fix: nil

      FChart.Draw(FChart.DelphiCanvas,Rect(0,0,FChart.Width,FChart.Height));

      TCustomTeePanelAccess(FChart).InternalCanvas:=Old;

    finally
      if not Assigned(OldParent) then
         FChart.Parent:=nil;
    end;

    FChart.AutoRepaint:=True;
  finally
    tmp.Free;
  end;

  if not KeepLeft then
  begin
    Location.Point.X:= -FChart.Width*0.5;
    Location.Point.Y:= 0;
    Location.Point.Z:= Maker.Options.Floor.Block.Location.Point.Z;
  end;

  IItemsDone:=True;
  IItemsOk:=True;

  BoundingBox(tmpMin,tmpMax);

  // Notify change:
  TBlocksAccess(Parent.DrawBlocks).ItemsChanged(Self)
end;

function TChartBlock.GetEditor:String;
begin
  result:='TChartBlockEditor';
end;

procedure TChartBlock.LoadChart(const FileName:String);
begin
  if Chart is TCustomBlockChart then
     TCustomBlockChart(Chart).IBlock:=Self;

  try
    if TeeIsURL(FileName) then
       LoadChartFromURL(TCustomChart(FChart),FileName)
    else
       LoadChartFromFile(TCustomChart(FChart),FileName);

    SetDirty;
  except
    on Exception do ;
  end;

  if Assigned(TBlocksAccess(Parent).OnLoaded) then
     TBlocksAccess(Parent).OnLoaded(Parent);
end;

type
  TChartAccess=class(TCustomTeePanel);

procedure TChartBlock.DrawChartDirect;
var OldR : TRect;
    Old  : TCanvas;
begin
  if FChart.Canvas<>TBlocksAccess(Parent).Parent.Canvas then
  begin
    FreeAndNil(TChartAccess(Chart).InternalCanvas);

    Old:=TBlocksAccess(Parent).Parent.Canvas.ReferenceCanvas;
    TChartAccess(Chart).Canvas:=TBlocksAccess(Parent).Parent.Canvas;
    TChartAccess(Chart).Canvas.ReferenceCanvas:=Old;
    TChartAccess(Chart).IsEmbedded:=True;
  end;

  glDisable(GL_TEXTURE_2D);

  with Size.Point do
       glTranslatef(-X*0.5, Z*0.5, Y*0.5);

  OldR:=TeeRect(0,0,Round(Size.Point.X),Round(Size.Point.Z));
  Chart.CustomChartRect:=True;
  Chart.ChartRect:=OldR;
  Chart.CustomWidth3D:=True;
  Chart.Width3D:=Round(Size.Point.Y);

  TCustomBlockChart(Chart).DoInternalDraw(OldR);
end;

function TChartBlock.Editor(const AOwner:TComponent; Embeddable:Boolean=False):TControl;
begin
  if Assigned(Chart) then
  begin
    result:=TChartBlockEditor.Create(AOwner);
    TChartBlockEditor(result).Chart:=Self;

    if Embeddable then
       result.Align:=alClient;
  end
  else
    result:=nil;
end;

function TChartBlock.BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean;
var tmp : Double;
begin
  if DirectDraw then
  begin
    with Size.Point do
    begin
      tmp:=X*0.5;
      AMin.x:=-tmp;
      AMax.x:=tmp;

      tmp:=Z*0.5;
      AMin.z:=-tmp;
      AMax.z:=tmp;

      tmp:=Y*0.5;
      AMin.y:=-tmp;
      AMax.y:=tmp;
    end;

    result:=True;
  end
  else
    result:=inherited BoundingBox(AMin,AMax);
end;

function TChartBlock.HasContents: Boolean;
begin
  if DirectDraw then
     result:=Assigned(Chart)
  else
     result:=inherited HasContents;
end;

type
  TSeriesAccess=class(TChartSeries);

procedure TChartBlock.PrepareForGallery;
var tmp : TChartSeries;
begin
  inherited;

  tmp:=Chart.AddSeries(TBarSeries.Create(Self));
  tmp.FillSampleValues;
  tmp.ColorEachPoint:=True;
  TSeriesAccess(tmp).ManualData:=True;

  Format.Texture.PictureLink:='';

  SetDirty;
end;

function TChartBlock.GetTemplate:TBlocks;
begin
  if not Assigned(ITemplate) then
     ITemplate:=TBlocks.Create(nil);

  result:=ITemplate;
end;

procedure TChartBlock.SetChart(const Value: TChart);
begin
  if FChart<>Value then
  begin
    if Assigned(FChart) then
    begin
      TTeePanelAccess(FChart).RemoveListener(Self);

      if IOwnedChart then
         FChart.Free
      else
         FChart.RemoveFreeNotification(Self);
    end;

    FChart:=Value;

    IOwnedChart:=False;

    if Assigned(FChart) then
    begin
      FChart.FreeNotification(Self);

      TTeePanelAccess(FChart).Listeners.Add(Self);

      SetDirty;
    end;
  end;
end;

type
  TWallAccess=class(TCustomChartWall);

procedure TChartBlock.ApplyTemplate;
var //tmp : TCustomBlock;
    t   : Integer;
begin
//  tmp:=Template.FindByName('Walls.Left');

  {
  if Assigned(tmp) then
     TWallAccess(Chart.Walls.Left).Visual:=tmp.Clone;
  }

  for t:=0 to Chart.SeriesCount-1 do
      SetSeriesTemplate(Chart[t]);
end;

procedure TChartBlock.SetDirty;
begin
  if Chart.AutoRepaint then
  begin
    IItemsDone:=False;
    Repaint;
  end;
end;

procedure TChartBlock.SetTemplateFile(const Value: String);
begin
  FTemplateFile:=Value;

  if FTemplateFile<>'' then
  begin
    Template.LoadFromFile(FTemplateFile);

    ApplyTemplate;
  end;
end;

procedure TChartBlockEditor.Button1Click(Sender: TObject);
begin
  EditChart(nil,Chart.Chart);
  Chart.SetDirty;
  CountItems;
end;

procedure TChartBlockEditor.CBDirectClick(Sender: TObject);
begin
  Chart.DirectDraw:=CBDirect.Checked;

  if Chart.DirectDraw then
  begin
    with Chart.Size.Point do
    if (X=0) or (Y=0) or (Z=0) then
    begin
      X:=100;
      Y:=100;
      Z:=100;
    end;

    Chart.Repaint;
  end
  else
    Chart.SetDirty;

  CountItems;
end;

procedure TChartBlockEditor.Button2Click(Sender: TObject);
var tmpFile   : String;
    tmpFilter : String;
begin
  tmpFilter:=TeeMsg_NativeFilter+' (*'+TeeMsg_TeeExtension+')|*'+TeeMsg_TeeExtension;

  tmpFile:='';

  if TLoadBlockDialog.ModalShow(Self,tmpFilter,TeeMsg_TeeExtension,tmpFile) then
     if tmpFile<>'' then
        Chart.LoadChart(tmpFile);

  CountItems;
end;

procedure TChartBlockEditor.FormShow(Sender: TObject);
begin
  if not Assigned(Chart) then
     Chart:=TChartBlock(Tag);

  if Assigned(Chart) then
  begin
    CountItems;

    CBDirect.Checked:=Chart.DirectDraw;
    ETemplate.Text:=Chart.TemplateFile;
  end;
end;

procedure TChartBlockEditor.CountItems;
begin
  LabelItems.Caption:=IntToStr(CountAll(Chart.Items));
end;

class function TChartBlockEditor.CountAll(ABlocks: TBlocks): Integer;
var t : Integer;
begin
  result:=0;

  with ABlocks do
  for t:=0 to Count-1 do
  begin
    Inc(result);

    if Block[t] is TCustomObjectBlock then
       Inc(result,CountAll(TCustomObjectBlock(Block[t]).Items));
  end;
end;

function TChartBlock.SaveChildren:Boolean;
begin
  result:=False;
end;

procedure TChartBlock.SetSeriesTemplate(Series:TChartSeries);
//var tmpBlock : TCustomBlock;
begin
{
  if not Assigned(Series.Visuals.Template) then
  if Assigned(ITemplate) then
  begin
    tmpBlock:=ITemplate.FindByName(Series.ClassName);

    if Assigned(tmpBlock) then
       Series.Visuals.Template:=tmpBlock.Clone;
  end;
  }
end;

procedure TChartBlock.TeeEvent(Event:TTeeEvent);
begin
  if Assigned(ITemplate) then
     if Event is TTeeSeriesEvent then
     with TTeeSeriesEvent(Event) do
     if Event=seAdd then
        if Series is TChartSeries then
           SetSeriesTemplate(TChartSeries(Series));
end;

procedure TChartBlock.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  Proc(Chart);
end;

procedure TChartBlock.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FChart) and (AComponent=FChart) then
     Chart:=nil;
end;

procedure TChartBlockEditor.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
     Chart.TemplateFile:=OpenDialog1.FileName;
end;

procedure TChartBlockEditor.CBTextAsPicturesClick(Sender: TObject);
begin
  Chart.TextAsPictures:=CBTextAsPictures.Checked;
  Chart.SetDirty;
end;

procedure TChartBlockEditor.TBTextDepthChange(Sender: TObject);
begin
  Chart.TextDepth:=TBTextDepth.Position;
  LTextDepth.Caption:=FloatToStr(Chart.TextDepth);

  Chart.SetDirty;
end;

initialization
  TeeActivateGroup;

  RegisterClass(TChartBlockEditor);
  RegisterClass(TCustomBlockChart);
  RegisterBlock(TChartBlock);

finalization
  TeeActivateGroup;

  UnRegisterClass(TChartBlockEditor);
  UnRegisterClass(TCustomBlockChart);
  UnRegisterClass(TChartBlock);
end.
