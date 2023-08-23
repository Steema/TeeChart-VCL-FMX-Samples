unit TeeMoveAnimation;

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  TeCanvas, TeeAnimate, TeeExtruded, TeePointItemEditor, TeeAnimationEditor,
  TeeExtrudedEditor;

type
  TMoveAnimation=class(TRangePropertyAnimation)
  private
    FPoints : TPointCollection;
    FPropY  : String;
    FPropZ  : String;
    FTwoWay : Boolean;

    Old    : TPoint3DFloat;

    IRealYPropertyName : String;
    IRealZPropertyName : String;

    function GetEnd:TPointXYZFloat;
    function GetStart:TPointXYZFloat;
    procedure SetEnd(const Value:TPointXYZFloat);
    procedure SetPoints(const Value:TPointCollection);
    procedure SetPropY(const Value:String);
    procedure SetPropZ(const Value:String);
    procedure SetStart(const Value:TPointXYZFloat);
  protected
    function EndAnimation:Boolean; override;
    procedure FixupReferences(const AParentSource:String); override;
    procedure FindRealNames; override;
    Procedure NextFrame(const Fraction:Single); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    Function IsEnabled:Boolean; override;
    function EditorClass:String; override;
    function EditorName:String; override;
    class function IsValidSource(const ASource:TObject; IsObject:Boolean):Boolean; override;
    procedure StoreValue; override;
  published
    property EndPoint:TPointXYZFloat read GetEnd write SetEnd stored False;
    property Points:TPointCollection read FPoints write SetPoints;
    property StartPoint:TPointXYZFloat read GetStart write SetStart stored False;
    property TwoWay:Boolean read FTwoWay write FTwoWay default False;
    property XProperty:String read FProp write SetProp;
    property YProperty:String read FPropY write SetPropY;
    property ZProperty:String read FPropZ write SetPropZ;
  end;

  TMoveAnimationEditor = class(TForm)
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    TabMove: TTabSheet;
    TabPoints: TTabSheet;
    PageControl2: TPageControl;
    TabStart: TTabSheet;
    TabEnd: TTabSheet;
    Panel1: TPanel;
    CBTwoWay: TCheckBox;
    TabSheet1: TTabSheet;
    Button1: TButton;
    LabelProperty: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    IBasic    : TAnimationEditor;
    IEnd      : TPointItemEditor;
    IPoints   : TExtrudedEditor;
    IStart    : TPointItemEditor;

    Animation : TMoveAnimation;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math, TypInfo, TeeBlocks, TeePenDlg, TeeMakerControl, TeeNumberAnimation, TeeProcs;

{ TMoveAnimation }

Constructor TMoveAnimation.Create(AOwner:TComponent);
begin
  inherited;
  FPoints:=TPointCollection.Create(Self,TPointItem);
  FPoints.Add;
  FPoints.Add;
end;

Destructor TMoveAnimation.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TMoveAnimation.Assign(Source:TPersistent);
begin
  if Source is TMoveAnimation then
  with TMoveAnimation(Source) do
  begin
    Self.Points:=FPoints;
    Self.FPropY:=FPropY;
    Self.FPropZ:=FPropZ;
    Self.FTwoWay:=FTwoWay;
  end;

  inherited;
end;

procedure TMoveAnimation.FindRealNames;
begin
  inherited;

  IRealInstance:=Instance;
  IRealYPropertyName:=YProperty;
  Fixup(IRealInstance,IRealYPropertyName);

  if ZProperty<>'' then
  begin
    IRealInstance:=Instance;
    IRealZPropertyName:=ZProperty;
    Fixup(IRealInstance,IRealZPropertyName);
  end;
end;

procedure TMoveAnimation.FixupReferences(const AParentSource:String);
var tmp   : TObject;
    tmpSt : String;
begin
  inherited;

  if HasProperty then
  begin
    tmp:=TBlocks.DoGetObjectProp(IRealInstance,IRealProperty);

    if tmp is TPointXYZFloat then
    begin
      tmpSt:=FProp;

      FProp:=tmpSt+'.X';
      FPropY:=tmpSt+'.Y';
      FPropZ:=tmpSt+'.Z';

      IRealProperty:=FProp;
      IRealYPropertyName:=FPropY;
      IRealZPropertyName:=FPropZ;
    end
    else
    if tmp is TPointXYFloat then
    begin
      tmpSt:=FProp;

      FProp:=tmpSt+'.X';
      FPropY:=tmpSt+'.Y';
      FPropZ:='';

      IRealProperty:=FProp;
      IRealYPropertyName:=FPropY;
      IRealZPropertyName:='';
    end
  end;
end;

function TMoveAnimation.EditorName:String;
begin
  result:='Move '+inherited EditorName;
end;

function TMoveAnimation.GetEnd:TPointXYZFloat;
begin
  result:=FPoints[FPoints.Count-1].Point;
end;

function TMoveAnimation.GetStart:TPointXYZFloat;
begin
  result:=FPoints[0].Point;
end;

procedure TMoveAnimation.SetEnd(const Value:TPointXYZFloat);
begin
  FPoints[FPoints.Count-1].Assign(Value);
end;

procedure TMoveAnimation.SetStart(const Value:TPointXYZFloat);
begin
  FPoints[0].Assign(Value);
end;

procedure TMoveAnimation.SetPoints(const Value:TPointCollection);
begin
  FPoints.Assign(Value);
end;

procedure TMoveAnimation.SetPropY(const Value:String);
begin
  if FPropY<>Value then
  begin
    FPropY:=Value;
    FindRealNames;
  end;
end;

procedure TMoveAnimation.SetPropZ(const Value:String);
begin
  if FPropZ<>Value then
  begin
    FPropZ:=Value;
    FindRealNames;
  end;
end;

function TMoveAnimation.IsEnabled: Boolean;
begin
  result:=inherited IsEnabled and (YProperty<>'');
end;

class function TMoveAnimation.IsValidSource(const ASource:TObject; IsObject:Boolean):Boolean;
begin
  result:=Assigned(ASource) and IsObject and
    ( (ASource is TPointXYZFloat) or (ASource is TPointXYFloat) );
end;

procedure TMoveAnimation.NextFrame(const Fraction:Single);
var tmpFrame : Double;
    tmp      : Double;
begin
  if TwoWay then tmpFrame:=Pi
            else tmpFrame:=HalfPi;

  tmpFrame:=tmpFrame*Fraction;
  tmpFrame:=Power(Sin(tmpFrame),1.3);

  tmp:=EndPoint.X-StartPoint.X;

  if tmp<>0 then
     SetPropValue(IRealInstance,IRealProperty,StartPoint.X+tmpFrame*tmp);

  tmp:=EndPoint.Y-StartPoint.Y;

  if tmp<>0 then
     SetPropValue(IRealInstance,IRealYPropertyName,StartPoint.Y+tmpFrame*tmp);

  if IRealZPropertyName<>'' then
  begin
    tmp:=EndPoint.Z-StartPoint.Z;

    if tmp<>0 then
       SetPropValue(IRealInstance,IRealZPropertyName,StartPoint.Z+tmpFrame*tmp);
  end;

  inherited;
end;

procedure TMoveAnimation.StoreValue;
begin
  inherited;

  if UseStartValue then
     Old:=StartPoint.Point
  else
  begin
    Old.X:=GetPropValue(IRealInstance,IRealProperty);
    Old.Y:=GetPropValue(IRealInstance,IRealYPropertyName);

    if IRealZPropertyName<>'' then
       Old.Z:=GetPropValue(IRealInstance,IRealZPropertyName);
  end;
end;

function TMoveAnimation.EndAnimation:Boolean;
begin
  if not (csDestroying in ComponentState) then
  begin
    if not KeepEndValue then
    begin
      SetPropValue(IRealInstance,IRealProperty,Old.X);
      SetPropValue(IRealInstance,IRealYPropertyName,Old.Y);

      if IRealZPropertyName<>'' then
         SetPropValue(IRealInstance,IRealZPropertyName,Old.Z);
    end;
  end;

  result:=inherited EndAnimation;
end;

function TMoveAnimation.EditorClass: String;
begin
  result:='TMoveAnimationEditor';
end;

procedure TMoveAnimationEditor.FormShow(Sender: TObject);
var tmp : TBlocks;
begin
  Animation:=TMoveAnimation(Tag);

  if Assigned(Animation) then
  begin
    if not Assigned(IBasic) then
    begin
      IBasic:=TAnimationEditor.Create(Self);
      IBasic.Align:=alClient;
      TeeTranslateControl(IBasic);
      TTeeVCL.AddFormTo(IBasic,TabGeneral,Animation);
    end;

    if not Assigned(IStart) then
    begin
      IStart:=TPointItemEditor.Create(Self);
      IStart.Align:=alClient;
      TeeTranslateControl(IStart);
      TTeeVCL.AddFormTo(IStart,TabStart);

      IEnd:=TPointItemEditor.Create(Self);
      IEnd.Align:=alClient;
      TeeTranslateControl(IEnd);
      TTeeVCL.AddFormTo(IEnd,TabEnd);
    end;

    if not Assigned(IPoints) then
    begin
      IPoints:=TExtrudedEditor.Create(Self);
      IPoints.Align:=alClient;
      TeeTranslateControl(IPoints);
      TTeeVCL.AddFormTo(IPoints,TabPoints);
    end;

    tmp:=(Animation.Animate.Panel as TMaker).Blocks;

    IStart.RefreshPoint(Animation.Points[0],tmp);
    IEnd.RefreshPoint(Animation.Points[Animation.Points.Count-1],tmp);
    IBasic.RefreshAnimation(Animation);
    IPoints.RefreshPoints(Animation.Points);

    CBTwoWay.Checked:=Animation.TwoWay;
    LabelProperty.Caption:=Animation.EditorName;
  end;
end;

procedure TMoveAnimationEditor.Button1Click(Sender: TObject);
begin
  if TNumberAnimationEditor.EditProperty(Self,Animation) then
     LabelProperty.Caption:=Animation.EditorName;
end;

initialization
  TeeRegisterAnimation(TMoveAnimation);
  RegisterClass(TMoveAnimationEditor);
end.
