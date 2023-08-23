unit TeeBlockAnimations;
{$I TeeDefs.inc}

interface

uses
  Windows, Classes, Forms, ComCtrls, Controls, StdCtrls,
  TeeAnimate, TeeAnimationEditor, TeCanvas, TeeBlocks, TeeNumberAnimation;

type
  TBlocksAnimation=class(TPropertyAnimation)
  private
    FNumber  : TNumberAnimation;
    FOverlap : Integer;

    procedure SetNumber(const Value:TNumberAnimation);
    procedure SetOverlap(const Value:Integer);
  protected
    function IsFolder:Boolean; override;
    procedure ResetItems;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function EditorClass: String; override;
    function EditorName: String; override;
    function IsEnabled:Boolean; override;
    class function IsValidSource(const ASource:TObject; IsObject:Boolean):Boolean; override;
    procedure Prepare; virtual;
  published
    property Number:TNumberAnimation read FNumber write SetNumber;
    property Overlap:Integer read FOverlap write SetOverlap default 0;
    property PropertyName;
  end;

  TFadeType=(ftIn,ftOut);

  TFadeBlocksAnimation=class(TBlocksAnimation)
  private
    FFadeType: TFadeType;

    procedure SetFadeType(const Value: TFadeType);
  protected
    procedure SetDuration(const Value:Integer); override;
  public
    Constructor Create(AOwner:TComponent); override;

    function EditorName:String; override;
    procedure Prepare; override;
  published
    property FadeType:TFadeType read FFadeType write SetFadeType default ftIn;
  end;

  TSequenceAnimation=class(TTeeAnimation)
  private
    procedure RefreshStarts;
  protected
    procedure Added(const AAnimation:TTeeAnimation); override;
    procedure ChildDurationChanged(const Child:TTeeAnimation); override;
    function IsFolder:Boolean; override;
    function IsSequence:Boolean; override;
    procedure Removed(const AAnimation:TTeeAnimation); override;
    procedure SetAnimate(const Value:TTeeAnimate); override;
  public
    Destructor Destroy; override;
    function EditorName:String; override;
  end;

  TBounceAnimation=class(TNumberAnimation)
  public
    procedure NextFrame(const Fraction:Single); override;
  end;

  TBlocksAnimationEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Edit1: TEdit;
    UDOverlap: TUpDown;
    TabNumber: TTabSheet;
    Label2: TLabel;
    EBlocksProperty: TEdit;
    Button1: TButton;
    LabelBlocks: TLabel;
    BChange: TButton;
    TabGeneral: TTabSheet;
    procedure Edit1Change(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EBlocksPropertyChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BChangeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Animation : TBlocksAnimation;
    INumber   : TNumberAnimationEditor;
    IBasic    : TAnimationEditor;

    procedure RefreshAnimation(AAnimation:TBlocksAnimation);
  public
    { Public declarations }
  end;

function BlockTitlePath(ABlock:TCustomBlock): String;

implementation

{$R *.dfm}

uses
  TypInfo, TeePenDlg, TeeProcs;

Constructor TBlocksAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FNumber:=TNumberAnimation.Create(nil);
end;

destructor TBlocksAnimation.Destroy;
begin
  FNumber.Free;
  inherited;
end;

function TBlocksAnimation.EditorClass: String;
begin
  result:='TBlocksAnimationEditor';
end;

function TBlocksAnimation.IsEnabled: Boolean;
begin
  result:=Enabled and ((not Assigned(Parent)) or Parent.IsEnabled);
end;

procedure TBlocksAnimation.SetNumber(const Value: TNumberAnimation);
begin
  FNumber.Assign(Value);
end;

procedure TBlocksAnimation.SetOverlap(const Value: Integer);
begin
  if FOverlap<>Value then
  begin
    FOverlap:=Value;
    ResetItems;
  end;
end;

type
  TNumberAnimAccess=class(TNumberAnimation);

procedure TBlocksAnimation.ResetItems;
var t : Integer;
    tmp : Integer;
begin
  tmp:=0;

  if Assigned(Animate) then
  with Animate.Animations do
  for t:=0 to Count-1 do
  with Animation[t] do
       if Parent=Self then
       begin
         Duration:=Self.Duration;
         StartFrame:=tmp*Self.Overlap;

         if Animation[t] is TNumberAnimation then
         with TNumberAnimAccess(Animation[t]) do
         begin
           StartValue:=Self.Number.StartValue;
           StartLink:=Self.Number.StartLink;

           Curve:=Self.Number.Curve;

           if not Number.UseEndValue then
              EndValue:=GetPropValue(IRealInstance,IRealProperty)
           else
           begin
             EndValue:=Self.Number.EndValue;
             EndLink:=Self.Number.EndLink;
           end;
         end;

         Inc(tmp);
       end;
end;

procedure TBlocksAnimation.Assign(Source: TPersistent);
begin
  if Source is TBlocksAnimation then
  with TBlocksAnimation(Source) do
  begin
    Self.Number:=Number;
    Self.FOverlap:=FOverlap;
  end;

  inherited;
end;

class function TBlocksAnimation.IsValidSource(const ASource:TObject; IsObject:Boolean):Boolean;
begin
  result:=Assigned(ASource) and IsObject and
    ( (ASource is TCustomObjectBlock) or (ASource is TBlocks) );
end;

type
  TBlocksAccess=class(TBlocks);

function TBlocksAnimation.EditorName: String;
var tmp : TCustomObjectBlock;
begin
  if Assigned(Instance) then
     if Instance is TCustomBlock then
        result:=BlockTitlePath(Instance as TCustomBlock)+'.'+PropertyName
     else
     if Instance is TBlocks then
     begin
       tmp:=TBlocksAccess(Instance as TBlocks).IObject;

       if Assigned(tmp) then
          result:=BlockTitlePath(tmp)+'.'+PropertyName
       else
          result:='Blocks '+PropertyName;
     end

  else
     result:=inherited EditorName;
end;

function TBlocksAnimation.IsFolder:Boolean;
begin
  result:=True;
end;

type
  TCustomBlockAccess=class(TCustomBlock);
  TPropAnimAccess=class(TPropertyAnimation);

procedure TBlocksAnimation.Prepare;

  procedure AddAnimation(AInstance:TComponent; AStart:Integer);
  var tmp : TNumberAnimation;
      tmpInstance : TObject;
      tmpProperty : String;
  begin
    tmpInstance:=AInstance;
    tmpProperty:=Number.PropertyName;

    Fixup(tmpInstance,tmpProperty);

    if IsColorProperty(tmpInstance,tmpProperty) then
       tmp:=TColorsAnimation.Create(Self)
    else
       tmp:=TNumberAnimation.Create(Self);

    if AInstance is TCustomBlock then
       tmp.Title:=TCustomBlockAccess(AInstance).TitleOrName;

    tmp.PropertyName:=Number.PropertyName;

    tmp.StartValue:=Number.StartValue;
    tmp.StartLink:=Number.StartLink;

    tmp.UseStartValue:=True;
    tmp.InitStart:=True;

    tmp.StartFrame:=AStart;
    tmp.Duration:=Number.Duration;
    tmp.Enabled:=True;
    tmp.Instance:=AInstance;

    tmp.Curve:=Number.Curve;

    if not Number.UseEndValue then
       tmp.EndValue:=GetPropValue(TPropAnimAccess(tmp).IRealInstance,TPropAnimAccess(tmp).IRealProperty)
    else
    begin
      tmp.EndValue:=Number.EndValue;
      tmp.EndLink:=Number.EndLink;
    end;

    tmp.Parent:=Self;

    Animate.Animations.Add(tmp);
  end;

  procedure RemoveAnimations;
  var t : Integer;
  begin
    with Animate.Animations do
    begin
      t:=0;

      while t<Count do
      begin
        if Animation[t].Parent=Self then
           Animation[t].Free
        else
           Inc(t);
      end;
    end;
  end;

var t   : Integer;
    Target : TCustomBlock;
    tmpTitle : String;
begin
  RemoveAnimations;

  tmpTitle:=PropertyName;

  if Instance is TCustomObjectBlock then
  begin
    if tmpTitle='' then
       Target:=Instance as TCustomBlock
    else
       Target:=(Instance as TCustomObjectBlock).Items.Find(tmpTitle,True);
  end
  else
  if Instance is TBlocks then
     Target:=(Instance as TBlocks).Find(tmpTitle,True)
  else
     Target:=nil;

  if Assigned(Target) then
     if Target is TCustomObjectBlock then
     begin
       for t:=0 to TCustomObjectBlock(Target).Items.Count-1 do
           AddAnimation(TCustomObjectBlock(Target)[t],t*Overlap)
     end
     else
        AddAnimation(Target,Overlap);
end;

function BlockTitlePath(ABlock:TCustomBlock):String;
begin
  result:=TCustomBlockAccess(ABlock).TitleOrName;

  while Assigned(TBlocksAccess(ABlock.Parent).IObject) do
  begin
    ABlock:=TBlocksAccess(ABlock.Parent).IObject;
    result:=TCustomBlockAccess(ABlock).TitleOrName+'.'+result;
  end;
end;

{ TSequenceAnimation }

procedure TSequenceAnimation.Added(const AAnimation: TTeeAnimation);
begin
  inherited;
  RefreshStarts;
end;

procedure TSequenceAnimation.RefreshStarts;
var t : Integer;
    tmp : Integer;
begin
  if Assigned(Animate) then
  begin
    tmp:=-1;

    with Animate.Animations do
    for t:=0 to Count-1 do
        with Animation[t] do
        if Parent=Self then
        begin
          if tmp=-1 then
             tmp:=StartFrame+Duration
          else
          begin
            StartFrame:=tmp;
            Inc(tmp,Duration);
          end;
        end;
  end;
end;

function TSequenceAnimation.EditorName: String;
begin
  result:='Sequence';
end;

function TSequenceAnimation.IsFolder:Boolean;
begin
  result:=True;
end;

procedure TSequenceAnimation.SetAnimate(const Value: TTeeAnimate);
begin
  inherited;
  RefreshStarts;
end;

procedure TSequenceAnimation.ChildDurationChanged(const Child:TTeeAnimation);
begin
  inherited;
  RefreshStarts;
end;

function TSequenceAnimation.IsSequence:Boolean;
begin
  result:=True;
end;

procedure TSequenceAnimation.Removed(const AAnimation:TTeeAnimation);
begin
  inherited;
  RefreshStarts;
end;

Destructor TSequenceAnimation.Destroy;
var t : Integer;
begin
  if Assigned(Animate) then
    with Animate.Animations do
    for t:=0 to Count-1 do
        with Animation[t] do
        if Parent=Self then
           Parent:=nil;

  inherited;
end;

{ TBounceAnimation }

procedure TBounceAnimation.NextFrame(const Fraction:Single);
var tmpStart : Double;
begin
  tmpStart:=StartValue;

  if Fraction<0.5 then
     Value:=tmpStart+(2*Fraction)*(EndValue-tmpStart)
  else
     Value:=EndValue-(2*(Fraction-0.5))*(EndValue-tmpStart);
end;

{ TBlocksAnimationEditor }

procedure TBlocksAnimationEditor.Edit1Change(Sender: TObject);
begin
  if Showing then
     Animation.Overlap:=UDOverlap.Position;
end;

procedure TBlocksAnimationEditor.RefreshAnimation(AAnimation:TBlocksAnimation);
begin
  Animation:=AAnimation;

  with Animation do
  begin
    UDOverlap.Position:=Overlap;
    EBlocksProperty.Text:=Number.PropertyName;
    LabelBlocks.Caption:=EditorName;
    BChange.Enabled:=False;

    if Assigned(INumber) then
       INumber.RefreshAnimation(Animation.Number);

    if Assigned(IBasic) then
       IBasic.RefreshAnimation(Animation);
  end;
end;

procedure TBlocksAnimationEditor.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=TabNumber then
  begin
    if not Assigned(INumber) then
    begin
      INumber:=TNumberAnimationEditor.Create(Self);
      INumber.Align:=alClient;
      TeeTranslateControl(INumber);
      TTeeVCL.AddFormTo(INumber,TabNumber,Animation.Number);
    end
    else
      INumber.RefreshAnimation(Animation.Number);
  end
  else
  if PageControl1.ActivePage=TabGeneral then
  begin
    if not Assigned(IBasic) then
    begin
      IBasic:=TAnimationEditor.Create(Self);
      IBasic.Align:=alClient;
      TeeTranslateControl(IBasic);
      TTeeVCL.AddFormTo(IBasic,TabGeneral,Animation);
    end
    else
      IBasic.RefreshAnimation(Animation);
  end;
end;

procedure TBlocksAnimationEditor.FormShow(Sender: TObject);
begin
  Animation:=TBlocksAnimation(Tag);

  if Assigned(Animation) then
     RefreshAnimation(Animation);
end;

procedure TBlocksAnimationEditor.EBlocksPropertyChange(Sender: TObject);
begin
  Animation.Number.PropertyName:=EBlocksProperty.Text;
  BChange.Enabled:=True;
end;

procedure TBlocksAnimationEditor.Button1Click(Sender: TObject);
begin
  if TNumberAnimationEditor.EditProperty(Self,Animation) then
     LabelBlocks.Caption:=Animation.EditorName;
end;

procedure TBlocksAnimationEditor.BChangeClick(Sender: TObject);
begin
  Animation.Prepare;
  BChange.Enabled:=False;
end;

procedure TBlocksAnimationEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage:=TabSheet1;
end;

{ TFadeBlocksAnimation }

Constructor TFadeBlocksAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Number.PropertyName:='Format.Transparency';
end;

function TFadeBlocksAnimation.EditorName: String;
begin
  result:='Fade '+inherited EditorName;
end;

procedure TFadeBlocksAnimation.Prepare;
begin
  with Number do
  begin
    Duration:=Self.Duration;

    if FFadeType=ftIn then
    begin
      StartValue:=255;
      EndValue:=0;
    end
    else
    begin
      StartValue:=0;
      EndValue:=255;
    end;
  end;

  inherited;
end;

procedure TFadeBlocksAnimation.SetDuration(const Value: Integer);
var tmp : Boolean;
begin
  tmp:=Duration<>Value;

  inherited;

  if tmp then
     Prepare;
end;

procedure TFadeBlocksAnimation.SetFadeType(const Value: TFadeType);
begin
  if FFadeType<>Value then
  begin
    FFadeType:=Value;
    Prepare;
  end;
end;

initialization
  TeeRegisterAnimation(TBlocksAnimation);
  TeeRegisterAnimation(TFadeBlocksAnimation);
  TeeRegisterAnimation(TSequenceAnimation);
  TeeRegisterAnimation(TBounceAnimation);

  RegisterClass(TBlocksAnimationEditor);
end.
