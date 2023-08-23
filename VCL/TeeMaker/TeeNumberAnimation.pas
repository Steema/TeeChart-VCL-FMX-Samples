unit TeeNumberAnimation;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,

  {$IFDEF D9}
  Types,
  {$ENDIF}

  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  TeEngine, TeeTools, TeeDragPoint, Series, TeeProcs, Chart, TeeAnimate,
  TeCanvas, TeeActionGallery, TeeSelectProperty, TeePenDlg, TeeGDIPlus,
  TeeAnimationEditor;

type
  TNumberAnimationEditor = class(TForm)
    PageControl1: TPageControl;
    TabCurve: TTabSheet;
    Panel1: TPanel;
    LBCurveType: TListBox;
    Chart1: TChart;
    Series1: TLineSeries;
    ChartTool1: TDragPointTool;
    ChartTool2: TColorLineTool;
    TabGeneral: TTabSheet;
    BDelete: TButton;
    ComboMode: TComboFlat;
    Label5: TLabel;
    TabSheet1: TTabSheet;
    Button4: TButton;
    GroupBox1: TGroupBox;
    Button3: TButton;
    BClearStartLink: TButton;
    Label1: TLabel;
    EStart: TEdit;
    StartColor: TSpeedButton;
    CBUseStart: TCheckBox;
    LStartLink: TLabel;
    GroupBox2: TGroupBox;
    Button2: TButton;
    BClearEndLink: TButton;
    Label2: TLabel;
    EndColor: TSpeedButton;
    EEnd: TEdit;
    CBKeepEnd: TCheckBox;
    TabSheet3: TTabSheet;
    Button1: TButton;
    LabelProperty: TLabel;
    LEndLink: TLabel;
    CBUseEnd: TCheckBox;
    procedure ChartTool1DragPoint(Sender: TDragPointTool; Index: Integer);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ChartTool1StartDrag(Sender: TDragPointTool; Index: Integer;
      var Start: Boolean);
    procedure LBCurveTypeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBUseStartClick(Sender: TObject);
    procedure EStartChange(Sender: TObject);
    procedure EEndChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure CBKeepEndClick(Sender: TObject);
    procedure ComboModeChange(Sender: TObject);
    procedure StartColorClick(Sender: TObject);
    procedure EndColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BClearStartLinkClick(Sender: TObject);
    procedure BClearEndLinkClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CBUseEndClick(Sender: TObject);
  private
    { Private declarations }
    Animation : TNumberAnimation;
    Selected  : Integer;
    IBasic    : TAnimationEditor;

    procedure AddCurve;
    function AskLink(ALink:TPropertyLink):Boolean;
    function ClickedPointer:Integer;
    procedure RefreshValues;
    procedure SetLinkLabel(ALabel:TLabel; ALink:TPropertyLink);
  public
    { Public declarations }
    class function EditProperty(AOwner:TComponent; Animation:TPropertyAnimation):Boolean;
    procedure RefreshAnimation(AAnimation:TNumberAnimation);
  end;

implementation

{$R *.dfm}

uses
  TeeMakerControl, TeeBlockEditor;

procedure TNumberAnimationEditor.LBCurveTypeClick(Sender: TObject);
begin
  with Animation.Curve do
  begin
    if LBCurveType.ItemIndex<3 then
       Reset;

    case LBCurveType.ItemIndex of
      0: begin
           Y[0]:=0; Y[1]:=0;
         end;
      1: begin
           Y[0]:=1; Y[1]:=-1;
         end;
      2: begin
           Y[0]:=-1; Y[1]:=1;
         end;
    else
    end;

    Calculate(Animation.Duration);
  end;

  AddCurve;
end;

type
  TNumberAnimationAccess=class(TNumberAnimation);

procedure TNumberAnimationEditor.AddCurve;
var t : Integer;
begin
  Series1.Clear;

  if ComboMode.ItemIndex=0 then
  begin
    with Animation.Curve do
    for t:=0 to Length(X)-1 do
        Series1.AddXY(X[t],Y[t]);
  end
  else
  begin
    with TNumberAnimationAccess(Animation) do
    for t:=0 to DurationFrames-1 do // Arbitrary value
        Series1.AddXY(t,CurveFrameValue(t));
  end;
end;

procedure TNumberAnimationEditor.ChartTool1DragPoint(Sender: TDragPointTool;
  Index: Integer);
begin
  LBCurveType.ItemIndex:=LBCurveType.Items.Count-1;

  with Sender.Series do
  begin
    XValue[0]:=0;

    if ComboMode.ItemIndex=0 then
       XValue[Count-1]:=1
    else
       XValue[Count-1]:=Animation.Duration-1;

    if (Index>0) and (XValue[Index]<XValue[Pred(Index)]) then
       XValue[Index]:=XValue[Pred(Index)]
    else
    if (Index<Count-1) and (XValue[Index]>XValue[Succ(Index)]) then
       XValue[Index]:=XValue[Succ(Index)];

    if ComboMode.ItemIndex=0 then
    begin
      if YValue[Index]<-1 then
         YValue[Index]:=-1
      else
      if YValue[Index]>1 then
         YValue[Index]:=1;
    end
    else
    begin
      if YValue[Index]<0 then
         YValue[Index]:=0
      else
      if YValue[Index]>1 then
         YValue[Index]:=1;
    end;
  end;

  with Animation.Curve do
  begin
    X:=TDoubleArray(Series1.XValues.Value);
    Y:=TDoubleArray(Series1.YValues.Value);
    Calculate(Animation.Duration);

    {
    SetLength(X,Series1.Count);
    SetLength(Y,Series1.Count);

    for t:=0 to Series1.Count-1 do
    begin
      X:=Series
    }
  end;
end;

procedure TNumberAnimationEditor.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Selected:=ClickedPointer;

  if Selected=-1 then
  begin
    Selected:=Series1.AddXY(Series1.GetHorizAxis.CalcPosPoint(X),
                            Series1.GetVertAxis.CalcPosPoint(Y));

    LBCurveType.ItemIndex:=LBCurveType.Items.Count-1;
  end;

  BDelete.Enabled:=(Selected>0) and (Selected<Series1.Count-1);
end;

procedure TNumberAnimationEditor.ChartTool1StartDrag(Sender: TDragPointTool;
  Index: Integer; var Start: Boolean);
begin
  Selected:=ClickedPointer;
  Start:=Selected<>-1;

  if Start then
  begin
    BDelete.Enabled:=(Selected>0) and (Selected<Series1.Count-1);
    Chart1.Invalidate;
  end;
end;

type
  TSeriesAccess=class(TCustomSeries);

function TNumberAnimationEditor.ClickedPointer:Integer;
var P : TPoint;
    t : Integer;
begin
  with TSeriesAccess(Series1) do
  begin
    P:=ParentChart.GetCursorPos;

    for t:=0 to Count-1 do
        if ClickedPointer(t,CalcXPos(t),CalcYPos(t),P.X,P.Y) then
        begin
          result:=t;
          Exit;
        end;
  end;

  result:=-1;
end;

procedure TNumberAnimationEditor.RefreshAnimation(AAnimation:TNumberAnimation);

  procedure GuessCurve;
  begin
    with Animation.Curve do
    begin
      if Length(X)=2 then
      begin
        if (Y[0]=0) and (Y[1]=0) then
           LBCurveType.ItemIndex:=0
        else
        if (Y[0]=1) and (Y[1]=-1) then
           LBCurveType.ItemIndex:=1
        else
        if (Y[0]=-1) and (Y[1]=1) then
           LBCurveType.ItemIndex:=2
        else
           LBCurveType.ItemIndex:=4;
      end
      else
      if Length(X)=0 then
        LBCurveType.ItemIndex:=0;
    end;
  end;

begin
  Animation:=AAnimation;

  if Assigned(Animation) then
  with Animation do
  begin
    if not Assigned(IBasic) then
    begin
      IBasic:=TAnimationEditor.Create(Self);
      IBasic.Align:=alClient;
      TeeTranslateControl(IBasic);
      TTeeVCL.AddFormTo(IBasic,TabGeneral,Animation);
    end;

    IBasic.RefreshAnimation(Animation);

    with Curve do
    if Length(X)=0 then
    begin
      LBCurveType.ItemIndex:=0;
      LBCurveTypeClick(Self);
    end
    else
      AddCurve;

    LabelProperty.Caption:=EditorName;

    RefreshValues;

    GuessCurve;
  end;

  StartColor.Visible:=Animation is TColorsAnimation;
  EndColor.Visible:=StartColor.Visible;
end;

procedure TNumberAnimationEditor.FormShow(Sender: TObject);
begin
  Selected:=-1;

  if Tag<>0 then
     RefreshAnimation(TNumberAnimation(Tag));
end;

procedure TNumberAnimationEditor.CBUseStartClick(Sender: TObject);
begin
  Animation.UseStartValue:=CBUseStart.Checked;
end;

procedure TNumberAnimationEditor.EStartChange(Sender: TObject);
begin
  Animation.StartValue:=StrToFloat(EStart.Text);
end;

procedure TNumberAnimationEditor.EEndChange(Sender: TObject);
begin
  Animation.EndValue:=StrToFloat(EEnd.Text);
end;

procedure TNumberAnimationEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabCurve then
  begin
    if Length(Animation.Curve.X)=0 then
    begin
      LBCurveType.ItemIndex:=0;
      LBCurveTypeClick(Self);
    end
    else
      AddCurve;
  end;
end;

procedure TNumberAnimationEditor.BDeleteClick(Sender: TObject);
begin
  Series1.Delete(Selected);
  Selected:=-1;
  BDelete.Enabled:=False;
end;

procedure TNumberAnimationEditor.Chart1AfterDraw(Sender: TObject);
var x,y,tmp : Integer;
begin
  if Selected<>-1 then
  with Chart1.Canvas do
  begin
    Brush.Style:=bsClear;
    Pen.Color:=clRed;
    Pen.Width:=1;

    x:=Series1.CalcXPos(Selected);
    y:=Series1.CalcYPos(Selected);

    tmp:=Series1.Pointer.Size;

    Rectangle(x-tmp,y-tmp,x+tmp,y+tmp);
  end;
end;

procedure TNumberAnimationEditor.CBKeepEndClick(Sender: TObject);
begin
  Animation.KeepEndValue:=CBKeepEnd.Checked;
end;

procedure TNumberAnimationEditor.ComboModeChange(Sender: TObject);
begin
  Chart1.Title.Visible:=ComboMode.ItemIndex=0;
  Chart1.SubTitle.Visible:=ComboMode.ItemIndex=1;

  if ComboMode.ItemIndex=0 then
  begin
    with Chart1.Axes.Left do
    begin
      Minimum:=-1;
      Maximum:=1;
      Automatic:=False;
    end;

    Chart1.Axes.Bottom.SetMinMax(0,1);
  end
  else
  begin
    with Chart1.Axes.Left do
    begin
      Automatic:=True;
    end;

    Chart1.Axes.Bottom.SetMinMax(0,Animation.Duration-1);
  end;

  AddCurve;
end;

procedure TNumberAnimationEditor.StartColorClick(Sender: TObject);
var tmp : TColor;
begin
  tmp:=Round(Animation.StartValue);

  if TButtonColor.EditColor(Self,tmp) then
     EStart.Text:=IntToStr(tmp);
end;

procedure TNumberAnimationEditor.EndColorClick(Sender: TObject);
var tmp : TColor;
begin
  tmp:=Round(Animation.EndValue);

  if TButtonColor.EditColor(Self,tmp) then
     EEnd.Text:=IntToStr(tmp);
end;

procedure TNumberAnimationEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabGeneral;
end;

class function TNumberAnimationEditor.EditProperty(AOwner:TComponent; Animation:TPropertyAnimation):Boolean;
var AObject : TObject;
    AName   : String;
begin
  AObject:=Animation.Instance;
  AName:=Animation.PropertyName;

  result:=TMakerPropertySelector.ModalShow(AOwner,
        (Animation.Animate.Panel as TMaker).Blocks,AObject,AName);

  if result then
  begin
    if AObject is TComponent then
       Animation.Instance:=AObject as TComponent;

    Animation.PropertyName:=AName;
  end;
end;

procedure TNumberAnimationEditor.Button1Click(Sender: TObject);
begin
  if EditProperty(Self,Animation) then
     LabelProperty.Caption:=Animation.EditorName;
end;

function TNumberAnimationEditor.AskLink(ALink:TPropertyLink):Boolean;
var tmpInst : TObject;
    tmpProp : String;
begin
  tmpInst:=ALink.Instance;
  tmpProp:=ALink.PropertyName;

  result:=TMakerPropertySelector.ModalShow(Self,(Animation.Animate.Panel as TMaker).Blocks,
                                           tmpInst,tmpProp);

  if result then
  with ALink do
  begin
    Instance:=tmpInst as TComponent;
    PropertyName:=tmpProp;
  end;
end;

procedure TNumberAnimationEditor.SetLinkLabel(ALabel:TLabel; ALink:TPropertyLink);
begin
  if Animation.HasLink(ALink) then
     ALabel.Caption:=TActionGallery.PropertyText(ALink)
  else
     ALabel.Caption:='';
end;

procedure TNumberAnimationEditor.Button2Click(Sender: TObject);
begin
  BClearEndLink.Enabled:=AskLink(Animation.EndLink);
  SetLinkLabel(LEndLink,Animation.EndLink);
end;

procedure TNumberAnimationEditor.BClearStartLinkClick(Sender: TObject);
begin
  Animation.StartLink:=nil;
  BClearStartLink.Enabled:=False;
  LStartLink.Caption:='';
end;

procedure TNumberAnimationEditor.BClearEndLinkClick(Sender: TObject);
begin
  Animation.EndLink:=nil;
  BClearEndLink.Enabled:=False;
  LEndLink.Caption:='';
end;

procedure TNumberAnimationEditor.Button3Click(Sender: TObject);
begin
  BClearStartLink.Enabled:=AskLink(Animation.StartLink);
  SetLinkLabel(LStartLink,Animation.StartLink);
end;

procedure TNumberAnimationEditor.Button4Click(Sender: TObject);
var tmpInst : TObject;
    tmpProp : String;
    tmpValue : Double;
begin
  with Animation do
  begin
    tmpInst:=StartLink.Instance;
    tmpProp:=StartLink.PropertyName;
    tmpValue:=StartValue;

    StartLink.Instance:=EndLink.Instance;
    StartLink.PropertyName:=EndLink.PropertyName;
    StartValue:=EndValue;

    EndLink.Instance:=tmpInst;
    EndLink.PropertyName:=tmpProp;
    EndValue:=tmpValue;
  end;

  RefreshValues;
end;

procedure TNumberAnimationEditor.RefreshValues;
begin
  with Animation do
  begin
    EStart.Text:=FloatToStr(StartValue);
    EEnd.Text:=FloatToStr(EndValue);

    CBUseStart.Checked:=UseStartValue;
    CBUseEnd.Checked:=UseEndValue;
    
    CBKeepEnd.Checked:=KeepEndValue;

    BClearStartLink.Enabled:=HasLink(StartLink);
    SetLinkLabel(LStartLink,StartLink);

    BClearEndLink.Enabled:=HasLink(EndLink);
    SetLinkLabel(LEndLink,EndLink);
  end;
end;

procedure TNumberAnimationEditor.CBUseEndClick(Sender: TObject);
begin
  Animation.UseEndValue:=CBUseEnd.Checked;
end;

initialization
  RegisterClass(TNumberAnimationEditor);
end.
