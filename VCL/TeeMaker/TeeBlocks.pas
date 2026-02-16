{********************************************}
{ TeeMaker - Basic 3D Blocks                 }
{ Copyright (c) 2002-2026 by Steema Software }
{       All Rights Reserved                  }
{********************************************}
unit TeeBlocks;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  {$IFDEF CLX}
  QGraphics, QCheckLst, QForms, QControls, QDialogs,
  {$ELSE}
  Graphics, CheckLst, Forms, Controls, Dialogs,
  {$ENDIF}

  {$IFDEF D7}
  Types,  // <-- TeePoint, TeeRect inline
  {$ENDIF}

  TypInfo,

  {$IFDEF D6}
  Variants,
  {$ENDIF}

  {$IFDEF D17}
  System.UITypes,
  {$ENDIF}

  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  //TeeGeometry,
  TeCanvas, TeeProcs, TeeGLCanvas, TeeAnimate, TeeGLSLShaders,
  OpenGL2;

type
  TCustomBlock=class;

  TBlockPicture=class(TPicture)
  private
    IBadFile    : Boolean; // Flag for inexistent files...
    ILoadThread : TThread;

    procedure TryLoad(const ParentSource,Source:String);
  protected
    LoadedSource : String;
  public
    class function FileGraphicClass(const FileName:String):TGraphicClass;
    class function LoadGraphicResource(const FileName:String):TGraphic;
    procedure LoadFromURL(const URL:String);
  end;

  TTile=class(TPointXYZFloat)
  private
    FOffset : TPointXYZFloat;

    function GetOffset:TPointXYZFloat;
    procedure SetOffset(const Value: TPointXYZFloat);
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Offset:TPointXYZFloat read GetOffset write SetOffset;
  end;

  TOnGetPointXYZ=function:TPoint3DFloat of object;

  TRotationXYZ=class(TPointXYZFloat)
  private
    FCenter : TPointXYZFloat;
    FFaceToViewer : Boolean;

    function GetCenter:TPointXYZFloat;
    function GetPoint:TPoint3DFloat;
    procedure SetCenter(const Value: TPointXYZFloat);
    procedure SetFaceToViewer(const Value: Boolean);
  protected
    FOnGet : TOnGetPointXYZ;
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Center:TPointXYZFloat read GetCenter write SetCenter;
    property FaceToViewer:Boolean read FFaceToViewer write SetFaceToViewer default False;
  end;

  TBlockFormat=class;

  TBlockTexture=class(TPersistent)
  private
    FAlpha       : Boolean;
    FAlphaInvert : Boolean;
    FPicture     : TBlockPicture;
    FPictureLink : String;
    FRotation    : Double;

    IAlphaBitmap   : TGraphic;
    IChanged       : TNotifyEvent;
    IFormat        : TBlockFormat;
    ISharedPicture : Boolean;
    ITextureSize   : TPoint;

    procedure Changed(Sender:TObject);
    procedure CheckPicTransp;
    function GetPicture: TBlockPicture;
    function GetScale:TPointXYZFloat;
    function GetTranslate:TPointXYZFloat;
    function IsPictureStored:Boolean;
    function IsRotationStored:Boolean;
    function OtherHasSamePictureLink(SharedOnly:Boolean):TBlockFormat;
    procedure SetAlpha(const Value: Boolean);
    procedure SetAlphaInvert(const Value: Boolean);
    procedure SetPicture(const Value: TBlockPicture);
    procedure SetPictureLink(const Value: String);
    procedure SetRotation(const Value: Double);
    procedure SetScale(const Value: TPointXYZFloat);
    procedure SetTranslate(const Value: TPointXYZFloat);
    procedure SetTransp(const Value: Boolean);
    procedure TryFreePicture;
  protected
    FScale     : TPointXYZFloat;
    FTranslate : TPointXYZFloat;
    FTransp    : Boolean;

    function HasTexture:Boolean;
    procedure SetAutomatic(Value:Boolean);
    procedure SetEmbeddedPicture;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure AssignPicture(const Value: TPicture);

    procedure Coord(const X,Y:Single); {$IFDEF D9}inline;{$ENDIF}
  published
    property AlphaInvert:Boolean read FAlphaInvert write SetAlphaInvert default False;
    property Picture:TBlockPicture read GetPicture write SetPicture stored IsPictureStored;
    property PictureAlpha:Boolean read FAlpha write SetAlpha default False;
    property PictureLink:String read FPictureLink write SetPictureLink;
    property PictureTransparent:Boolean read FTransp write SetTransp default False;
    property Rotation:Double read FRotation write SetRotation stored IsRotationStored;
    property Scale:TPointXYZFloat read GetScale write SetScale;
    property Translate:TPointXYZFloat read GetTranslate write SetTranslate;
  end;

  TBlockBorder=class(TPersistent)
  private
    FColor   : TColor;
    FStyle   : TPenStyle;
    FTransp  : Byte;
    FVisible : Boolean;
    FWidth   : Integer;

    IDefaultVisible : Boolean;
    IOwner   : TBlockFormat;

    procedure Changed;
    function IsVisibleStored:Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetTransp(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    IChanged : TNotifyEvent;

    procedure InitVisible(const Value:Boolean);
  public
    Constructor Create;

    procedure Assign(Source: TPersistent); override;
  published
    property Color:TColor read FColor write SetColor default clBlack;
    property Style:TPenStyle read FStyle write SetStyle default psSolid;
    property Transparency:Byte read FTransp write SetTransp default 0;
    property Visible:Boolean read FVisible write SetVisible stored IsVisibleStored;
    property Width:Integer read FWidth write SetWidth default 1;
  end;

  TRGBAlpha=record
    Red   : Byte;
    Green : Byte;
    Blue  : Byte;
    Alpha : Byte;
  end;

  TBlockFormat=class(TPersistent)
  private
    FBorder       : TBlockBorder;
    FBright       : Boolean;
    FColor        : TColor;
    FDrawInside   : Boolean;
    FParentTexture: Boolean;
    FShininess    : Integer;
    FSolid        : Boolean;
    FTexture      : TBlockTexture;
    FTransparency : Byte;

    IChanged        : TNotifyEvent;
    IOldShin        : Single;
    IRotatedTexture : Boolean;
    ITextureEnabled : Boolean;
    IWasMultiSample : Boolean;

    procedure CheckBlend;
    function GetBorder: TBlockBorder;
    procedure SetBorder(const Value: TBlockBorder);
    procedure SetBright(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetDrawInside(const Value:Boolean);
    procedure SetParentTexture(const Value:Boolean);
    procedure SetShininess(const Value: Integer);
    procedure SetSolid(const Value: Boolean);
    procedure SetTexture(const Value: TBlockTexture);
    procedure SetTransparency(const Value: Byte);
  protected
    ICurrentColor  : TColor;
    IOwner         : TCustomBlock;

    OnColorChanged : TNotifyEvent;

    procedure ConcavePolygon(var AList:Integer; {$IFDEF D6}const{$ENDIF} P:Array of TPoint3DFloat;
                             Invert:Boolean=False);
    procedure ConvexPolygon(var AList:Integer; const Points: Array of TPoint3DFloat;
                            Invert:Boolean=False);

    procedure DoSetBrush;
    procedure FinishPen;
    function GetRealColor:TColor;
    procedure FinishTransparency(const Value:Integer);
    procedure InitTransparency(const Value:Byte);
    function InternalPreparePen:Boolean;
    procedure PolylineList(out AList:Integer; const P:TPoint3DArray);
    procedure PrepareColor(const AColor:TColor); overload;
    procedure PrepareColor; overload; {$IFDEF D9}inline;{$ENDIF}
    procedure Start;
    procedure Finish;
    function PreparePen:Boolean;
    procedure SetDirectColor(const AColor:TColor);
  public
    Constructor Create(AOwner: TCustomBlock);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    class function ColorToGL(const AColor:TColor):TRGBAlpha;
    property Block:TCustomBlock read IOwner;
  published
    property Border:TBlockBorder read GetBorder write SetBorder;
    property Bright:Boolean read FBright write SetBright default False;
    property Color:TColor read FColor write SetColor default clDefault;
    property ParentTexture:Boolean read FParentTexture write SetParentTexture default True;
    property Shininess:Integer read FShininess write SetShininess default 0;
    property Solid:Boolean read FSolid write SetSolid default True;
    property Texture:TBlockTexture read FTexture write SetTexture;
    property Transparency:Byte read FTransparency write SetTransparency default 0;
    property VisibleInterior:Boolean read FDrawInside write SetDrawInside default False;
  end;

  TBlockActionItem=class(TCollectionItem)
  private
    FActions : TStrings;
    FTrigger : String;

    procedure SetActions(const Value: TStrings);
    procedure SetTrigger(const Value: String);
  public
    Constructor Create(Collection:TCollection); override;
    Destructor Destroy; override;

    procedure Add(const Action:String);
    procedure Assign(Source:TPersistent); override;
  published
    property Actions:TStrings read FActions write SetActions;
    property Trigger:String read FTrigger write SetTrigger;
  end;

  TBlockActions=class(TOwnedCollection)
  private
    function Get(Index: Integer): TBlockActionItem;
    procedure Put(Index: Integer; const Value: TBlockActionItem);
  public
    class function ActionToIndex(const Action:String):Integer;{$IFDEF D9}static;{$ENDIF}
    function Add(Trigger:String):TBlockActionItem;
    function OfEvent(AEvent:String):TBlockActionItem;

    property Action[Index:Integer]:TBlockActionItem read Get write Put; default;
  end;

  TBlockBounds=class(TPersistent)
  private
    IBlock : TCustomBlock;

    function GetLeft:Double;
    function GetRight:Double;
    function GetTop:Double;
    function GetBottom:Double;
    function GetFront:Double;
    function GetBack:Double;

    procedure SetLeft(const Value:Double);
    procedure SetRight(const Value:Double);
    procedure SetTop(const Value:Double);
    procedure SetBottom(const Value:Double);
    procedure SetFront(const Value:Double);
    procedure SetBack(const Value:Double);
  published
    property Left:Double read GetLeft write SetLeft;
    property Right:Double read GetRight write SetRight;
    property Top:Double read GetTop write SetTop;
    property Bottom:Double read GetBottom write SetBottom;
    property Front:Double read GetFront write SetFront;
    property Back:Double read GetBack write SetBack;
  end;

  TBlocks=class;

  TNotifyBlockEvent=procedure(const Sender:TCustomBlock) of object;

  TVisualBlock=class(TComponent)
  private
    FLocation : TPointXYZFloat;
    FSize     : TPointXYZFloat;

    procedure SetLocation(const Value:TPointXYZFloat);
    procedure SetSize(const Value: TPointXYZFloat);
  protected
    {$IFNDEF BLOCKS}
    IsCloneable : Boolean; // Experimental
    {$ENDIF}

    procedure AddPathPoint(const X,Y:Single; const AColor:TColor=clDefault); virtual;
    procedure ClearPath; virtual;
    procedure SetBounds(const R:TRect; const Z0,Z1:Single;
                        const Height:TCoordinate);
    procedure SetFormatting(const APen:TTeePen; const ABrush:TTeeBrush;
                            SetBrushImage:Boolean;
                            AColor:TColor; ATransparency:Byte); virtual; abstract;
    procedure SizeChanged(Sender:TObject); virtual;
  public
    {$IFDEF BLOCKS}
    IsCloneable : Boolean; // Experimental
    {$ENDIF}

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Repaint; virtual; abstract;
    function Editor(const AOwner:TComponent; Embeddable:Boolean=False):TControl; virtual; abstract;
  published
    property Location:TPointXYZFloat read FLocation write SetLocation;
    property Size:TPointXYZFloat read FSize write SetSize;
  end;

  TVisualEditor=class(TForm)
  private
    FOnDirty : TNotifyEvent;
  public
    procedure MarkDirty;
    procedure RefreshBlock(const AVisual:TVisualBlock); virtual;

    property OnDirty:TNotifyEvent read FOnDirty write FOnDirty;
  end;

  // BCB6 C++ problem, due to QC88686 bug, this type should be moved "up"
  // to the start of its "type" declaration.
  // Error was fixed in XE2:
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=88686
  // "The symbol ... has zero size, or excessive offset in module ..."
  TVisualEditorClass=class of TVisualEditor;

  TChangeVisual=function(AOwner,AVisualOwner:TComponent; AVisual:TVisualBlock):TVisualBlock;
  TEditVisual=function(AOwner:TComponent; AVisual:TVisualBlock):Boolean;

  TCustomBlock=class(TVisualBlock)
  private
    FActions  : TBlockActions;
    FBounds   : TBlockBounds;
    FCursor   : TCursor;
    FFormat   : TBlockFormat;
    FRotation : TRotationXYZ;
    FScale    : TPointXYZFloat;
    FTile     : TTile;
    FTitle    : String;
    FVisible  : Boolean;

    IShown      : Boolean;
    IsObjectBlock : Boolean;

    FOnClick : TMouseEvent;
    FOnDragging : TNotifyBlockEvent;
    FOnShow  : TNotifyBlockEvent;

    procedure CheckOnShow;

    function GetActions:TBlockActions;
    function GetBlockIndex:Integer;
    function GetBounds:TBlockBounds;
    function GetCursor:TCursor;
    function GetRotation:TRotationXYZ;
    function GetScale:TPointXYZFloat;
    function GetTile:TTile;

    function IsActionsStored: Boolean;

    procedure SetActions(const Value:TBlockActions);
    procedure SetCursor(const Value:TCursor);
    procedure SetBlockIndex(AIndex:Integer);
    procedure SetBlockBounds(const Value:TBlockBounds);
    procedure SetFormat(const Value:TBlockFormat);
    procedure SetRotation(const Value: TRotationXYZ);
    procedure SetScale(const Value: TPointXYZFloat);
    procedure SetTile(const Value: TTile);
    procedure SetTitle(const Value: String);
  protected
    IBlocks : TBlocks;
    ICanvas : {$IFDEF BLOCKS}IGraphicsGL{$ELSE}TGLCanvas{$ENDIF};
    IData   : TObject;
    IPicking: Boolean;

    procedure DefaultTransform;
    function DesignHandles(AOwner:TComponent):TCustomBlock; dynamic;
    procedure Dragged;
    procedure DrawSelected; dynamic;
    procedure GetRotationCenter(out AX,AY,AZ:Single; AddParents:Boolean);
    function InternalProjectPoint(const Matrix:TMatrix4d; const X,Y,Z:Single):TPoint3DFloat;
    procedure InternalTransform;
    procedure ModelMatrix(out AMatrix:TMatrix4d); // <-- BCB error D2011, cannot be a function
    function TitleOrName:String;
    procedure StartTransform; virtual;
    procedure EndTransform; {$IFDEF D10}inline;{$ENDIF}

    function CreateNewList:Integer;
    procedure DeleteList(var AList:Integer);
    procedure DeleteLists; virtual;
    function GetEditor:String; dynamic;
    Function GetOwner:TPersistent; override;

    function HasActions(AEvent:String=''):Boolean;
    function HasRotation:Boolean;
    function HasScale:Boolean;
    function HasTile:Boolean;

    procedure ReadState(Reader: TReader); override;

    procedure SetBlocks(const Value: TBlocks); virtual;

    procedure SetFormatting(const APen:TTeePen; const ABrush:TTeeBrush;
                            SetBrushImage:Boolean;
                            AColor:TColor; ATransparency:Byte); override;

    procedure PrepareForGallery; dynamic;
    procedure SetParentComponent(AParent: TComponent); override;
    function ShouldDraw(After:Boolean=False):Boolean; virtual;
    function ShouldDrawInterior:Boolean;

    procedure SetVisible(const Value: Boolean); virtual;
    function UsesDepth:Boolean; dynamic;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean; virtual;
    procedure CalcBounds(var AMin,AMax:TPoint3DFloat);
    procedure Clear; virtual;
    function Clicked(X,Y:Integer):TCustomBlock;
    function Clone:TCustomBlock;
    procedure Draw; virtual; abstract;
    procedure DrawBlock(AItems:TBlocks); overload;
    procedure DrawBlock; overload; virtual;
    function Editor(const AOwner:TComponent; Embeddable:Boolean=False):TControl; override;
    procedure InitTitle(const Prefix:String);
    procedure Move(const AX,AY,AZ:Single);
    function ProjectPoint(const X,Y,Z:Single):TPoint3DFloat;
    procedure Repaint; override;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    property Index:Integer read GetBlockIndex write SetBlockIndex;
    property Parent:TBlocks read IBlocks write SetBlocks;
  published
    property Actions:TBlockActions read GetActions write SetActions stored IsActionsStored;
    property Bounds:TBlockBounds read GetBounds write SetBlockBounds stored False;
    property Cursor:TCursor read GetCursor write SetCursor default crDefault;
    property Format:TBlockFormat read FFormat write SetFormat;
    property Rotation:TRotationXYZ read GetRotation write SetRotation;
    property Scale:TPointXYZFloat read GetScale write SetScale;
    property Tile:TTile read GetTile write SetTile;
    property Title:String read FTitle write SetTitle;
    property Visible:Boolean read FVisible write SetVisible default True;

    // Events:
    property OnClick:TMouseEvent read FOnClick write FOnClick;
    property OnDragging:TNotifyBlockEvent read FOnDragging write FOnDragging;
    property OnShow:TNotifyBlockEvent read FOnShow write FOnShow;
  end;

  TCubeSides=class(TPersistent)
  private
    IAnyUsed : Boolean;
    IOwner   : TCustomBlock;

    function AllVisible:Boolean;
    function CreateFormat:TBlockFormat;
    procedure FormatChanged(Sender:TObject);
    procedure FreeAndCheck(var ASide:TBlockFormat);

    function GetLeft:TBlockFormat;
    function GetTop:TBlockFormat;
    function GetRight:TBlockFormat;
    function GetBottom:TBlockFormat;
    function GetBack:TBlockFormat;
    function GetFront:TBlockFormat;

    function IsLeftStored:Boolean;
    function IsTopStored:Boolean;
    function IsRightStored:Boolean;
    function IsBottomStored:Boolean;
    function IsBackStored:Boolean;
    function IsFrontStored:Boolean;

    procedure SetLeft(const Value:TBlockFormat);
    procedure SetTop(const Value:TBlockFormat);
    procedure SetRight(const Value:TBlockFormat);
    procedure SetBottom(const Value:TBlockFormat);
    procedure SetBack(const Value:TBlockFormat);
    procedure SetFront(const Value:TBlockFormat);
  protected
    FLeft   : TBlockFormat;
    FTop    : TBlockFormat;
    FRight  : TBlockFormat;
    FBottom : TBlockFormat;
    FBack   : TBlockFormat;
    FFront  : TBlockFormat;
  public
    Constructor Create(AOwner: TCustomBlock);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function SideOf(Index:Integer):TBlockFormat;
  published
    property Left:TBlockFormat read GetLeft write SetLeft stored IsLeftStored;
    property Top:TBlockFormat read GetTop write SetTop stored IsTopStored;
    property Right:TBlockFormat read GetRight write SetRight stored IsRightStored;
    property Bottom:TBlockFormat read GetBottom write SetBottom stored IsBottomStored;
    property Back:TBlockFormat read GetBack write SetBack stored IsBackStored;
    property Front:TBlockFormat read GetFront write SetFront stored IsFrontStored;
  end;

  TCubeBlock=class(TCustomBlock)
  private
    FSides : TCubeSides;

    IListPen : Integer;

    procedure SetSides(const Value:TCubeSides);
  protected
    procedure DeleteLists; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Sides:TCubeSides read FSides write SetSides;
  end;

  TCustomCoverBlock=class(TCustomBlock)
  private
    function Get1:TBlockFormat;
    function Get2:TBlockFormat;
    procedure Set1(const Value: TBlockFormat);
    procedure Set2(const Value: TBlockFormat);
  protected
    FBrush1 : TBlockFormat;
    FBrush2 : TBlockFormat;
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function HasBrush1:Boolean;
    function HasBrush2:Boolean;
  published
    property Brush1:TBlockFormat read Get1 write Set1 stored HasBrush1;
    property Brush2:TBlockFormat read Get2 write Set2 stored HasBrush2;
  end;

  TBlockEdgeStyle=(resRound,resBevel);

  TBlockEdge=class(TPointXYFloat)
  private
    FSlices : Integer;
    FStyle  : TBlockEdgeStyle;

    function Active:Boolean;
    procedure SetSlices(const Value: Integer);
    procedure SetStyle(const Value:TBlockEdgeStyle);
  public
    Constructor Create(const AOwner:TPersistent; const AValue:Single=0;
                       const CanvasChanged:TNotifyEvent=nil); override;

    procedure Assign(Source: TPersistent); override;
  published
    property Slices:Integer read FSlices write SetSlices default 16;
    property Style:TBlockEdgeStyle read FStyle write SetStyle default resRound;
  end;

  TCylinderBlock=class(TCustomCoverBlock)
  private
    FAngle      : Integer;
    FBottomEdge : TBlockEdge;
    FSlices     : Integer;
    FStacks     : Integer;
    FStartAngle : Integer;
    FTopEdge    : TBlockEdge;

    ICoverList1    : Integer;
    ICoverList2    : Integer;
    ICurrentFormat : TBlockFormat;
    IList          : Integer;
    IListCover     : Integer;
    IStackData     : TPoint3DArray;

    procedure Changed(Sender:TObject);
    function GetCover:TBlockFormat;
    procedure SetAngle(const Value: Integer);
    procedure SetBottomEdge(const Value: TBlockEdge);
    procedure SetCover(const Value: TBlockFormat);
    procedure SetSlices(const Value: Integer);
    procedure SetStacks(const Value: Integer);
    procedure SetStartAngle(const Value: Integer);
    procedure SetTopEdge(const Value: TBlockEdge);
  protected
    FBrushCover  : TBlockFormat;

    IConeX       : Single;
    IConeY       : Single;
    ICoverHole   : TFloatPoint;
    IHideCovers  : Boolean;

    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function HasBrushCover:Boolean;
  published
    property Angle:Integer read FAngle write SetAngle default 360;
    property BottomEdge:TBlockEdge read FBottomEdge write SetBottomEdge;
    property BrushCover:TBlockFormat read GetCover write SetCover;
    property Slices:Integer read FSlices write SetSlices default 32;
    property Stacks:Integer read FStacks write SetStacks default 1;
    property StartAngle:Integer read FStartAngle write SetStartAngle default 0;
    property TopEdge:TBlockEdge read FTopEdge write SetTopEdge;
  end;

  TEllipseBlock=class(TCustomBlock)
  private
    FSlices : Integer;

    IListSolid  : Integer;
    IListBorder : Integer;

    procedure SetSlices(const Value:Integer);
  protected
    procedure DeleteLists; override;
    procedure PrepareForGallery; override;
    procedure ReadState(Reader: TReader); override;
  public
    Constructor Create(AOwner: TComponent); override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Slices:Integer read FSlices write SetSlices default 90;
  end;

 TTeeTextBlock=class(TCustomBlock)
  private
    FAlign     : TAlignment;
    FFont      : TTeeFont;
    FFontStyle : TTeeFontStyle;
    FLines     : TStrings;
    FLink      : TPropertyLink;
    FLinkFormat: String;

    IChanged    : Boolean;
    IExtentOk   : Boolean;
    IFont       : Integer;
    ILinkText   : String;
    ITextExtent : TPoint;

    procedure ColorChanged(Sender:TObject);
    function DoGetPropValue(AInstance:TObject; const AProp:String):Variant;
    function GetText:String;
    procedure SetAlign(const Value: TAlignment);
    procedure SetFont(const Value: TTeeFont);
    procedure SetFontStyle(const Value: TTeeFontStyle);
    procedure SetLines(const Value: TStrings);
    procedure SetLink(const Value: TPropertyLink);
    procedure SetLinkFormat(const Value: String);
    procedure SetText(const Value: String);
    procedure TextChanged(Sender:TObject);
  protected
    procedure DeleteLists; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation); override;
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;

    property Text:String read GetText write SetText;
  published
    property Alignment:TAlignment read FAlign write SetAlign default taLeftJustify;
    property Font:TTeeFont read FFont write SetFont;
    property FontStyle:TTeeFontStyle read FFontStyle write SetFontStyle default fsNormal;
    property Lines:TStrings read FLines write SetLines;
    property LinkFormat:String read FLinkFormat write SetLinkFormat;
    property LinkText:TPropertyLink read FLink write SetLink;
  end;

  TConeBlock=class(TCylinderBlock)
  private
    FConeSize : TPointXYFloat;

    procedure ConeSizeChanged(Sender:TObject);
    procedure SetConeSize(const Value: TPointXYFloat);
  protected
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property ConeSize:TPointXYFloat read FConeSize write SetConeSize;
    property Stacks default 32;
  end;

  // Pending Assign from here to bottom...

  TLightBlock=class(TCustomBlock)
  private
    FDiffuse      : TColor;
    FFixed        : Boolean;
    FSpecular     : TColor;
    FSpot         : Integer;
    FSpotExp      : Integer;
    FUseDirection : Boolean;

    ILamp : TConeBlock;

    function GetShowLamp:Boolean;
    Procedure SetDiffuse(const Value:TColor);
    procedure SetFixed(const Value: Boolean);
    procedure SetShowLamp(const Value: Boolean);
    Procedure SetSpecular(const Value:TColor);
    procedure SetSpot(const Value: Integer);
    procedure SetSpotExp(const Value: Integer);
    procedure SetUseDirection(const Value: Boolean);
  protected
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean; override;
    procedure Draw; override;
    class procedure GLColor(const AColor:TColor; var Value:GLMat);
    procedure InitLight;
  published
    property Diffuse:TColor read FDiffuse write SetDiffuse default clWhite;
    property Fixed:Boolean read FFixed write SetFixed default False;
    property ShowLamp:Boolean read GetShowLamp write SetShowLamp default True;
    property Specular:TColor read FSpecular write SetSpecular default clWhite;
    property Spot:Integer read FSpot write SetSpot default 45;
    property SpotExponent:Integer read FSpotExp write SetSpotExp default 14;
    property UseDirection:Boolean read FUseDirection write SetUseDirection default True;
  end;

  TTetrahedronBlock=class(TCustomBlock)
  public
    procedure Draw; override;
  end;

  TTorusBlock=class(TCustomCoverBlock)
  private
    FRadius     : TPointXYFloat;
    FRings      : Integer;
    FSides      : Integer;
    FStartAngle : Double;
    FTotalAngle : Double;

    IList       : Integer;
    IListSide1  : Integer;
    IListSide2  : Integer;

    procedure RadiusChanged(Sender:TObject);
    procedure SetRings(const Value: Integer);
    procedure SetSides(const Value: Integer);
    procedure SetStartAngle(const Value: Double);
    procedure SetTotalAngle(const Value: Double);
    procedure SetRadius(const Value: TPointXYFloat);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function GetEditor:String; override;
  published
    property Radius:TPointXYFloat read FRadius write SetRadius;
    property Rings:Integer read FRings write SetRings default 30;
    property Sides:Integer read FSides write SetSides default 20;
    property StartAngle:Double read FStartAngle write SetStartAngle;
    property TotalAngle:Double read FTotalAngle write SetTotalAngle;
  end;

  TEllipsoidBlock=class(TCustomBlock)
  private
    FEccentricity : Double;
    FSides      : Integer;
    FStacks     : Integer;
    FTotal      : Double;
    FTotalAngle : Double;

    IListSphere : Integer;
    IListCover  : Integer;

    function GetCover:TBlockFormat;
    function IsTotalStored:Boolean;
    procedure SetCover(const Value: TBlockFormat);
    procedure SetEccentricity(const Value: Double);
    procedure SetSides(const Value: Integer);
    procedure SetStacks(const Value: Integer);
    procedure SetTotal(const Value: Double);
    procedure SetTotalAngle(const Value: Double);
  protected
    FCover : TBlockFormat;

    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
    function HasCover:Boolean;
  published
    property Cover:TBlockFormat read GetCover write SetCover;
    property Eccentricity:Double read FEccentricity write SetEccentricity;
    property Sides:Integer read FSides write SetSides default 32;
    property Stacks:Integer read FStacks write SetStacks default 32;
    property Total:Double read FTotal write SetTotal stored IsTotalStored;
    property TotalAngle:Double read FTotalAngle write SetTotalAngle;
  end;

  TSphereBlock=class(TEllipsoidBlock)
  private
    function GetRadius:Double;
    procedure SetRadius(const Value: Double);
  public
    Constructor Create(AOwner: TComponent); override;
  published
    property Radius:Double read GetRadius write SetRadius;
  end;

  TTubeBlock=class(TConeBlock)
  private
    FTube : TBlockFormat;

    procedure SetTube(const Value:TBlockFormat);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Stacks default 1;
    property Tube:TBlockFormat read FTube write SetTube;
  end;

  TBevelCubeStyle=(bsRound, bsBevel, bsCut);

  TBevelSizeStyle=(bsPercentMin, bsPercent);

  TBeveledCubeBlock=class(TCustomBlock)
  private
    FBevelSize   : TPointXYZFloat;
    FBevelSizeStyle : TBevelSizeStyle;
    FCurvePoints : Integer;
    FCurveRound  : Boolean;
    FStyle       : TBevelCubeStyle;

    IList        : Integer;
    IListBorder  : Integer;

    procedure BevelChanged(Sender:TObject);
    procedure SetBevelSize(const Value: TPointXYZFloat);
    procedure SetBevelSizeStyle(const Value: TBevelSizeStyle);
    procedure SetCurvePoints(const Value: Integer);
    procedure SetCurveRound(const Value: Boolean);
    procedure SetStyle(const Value: TBevelCubeStyle);
  protected
    procedure DeleteLists; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property BevelSize:TPointXYZFloat read FBevelSize write SetBevelSize;
    property BevelSizeStyle:TBevelSizeStyle read FBevelSizeStyle
                                write SetBevelSizeStyle default bsPercentMin;
    property CurvePoints:Integer read FCurvePoints write SetCurvePoints default 8;
    property CurveRound:Boolean read FCurveRound write SetCurveRound default True;
    property Style:TBevelCubeStyle read FStyle write SetStyle default bsRound;
  end;

  TSliceEdges=class(TPersistent)
  private
    FInnerBottom : TBlockEdge;
    FInnerTop    : TBlockEdge;
    FOuterBottom : TBlockEdge;
    FOuterTop    : TBlockEdge;

    procedure SetInnerBottom(const Value:TBlockEdge);
    procedure SetInnerTop(const Value:TBlockEdge);
    procedure SetOuterBottom(const Value:TBlockEdge);
    procedure SetOuterTop(const Value:TBlockEdge);
  public
    Constructor Create(AOwner: TCustomBlock; Changed:TNotifyEvent);
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property InnerBottom:TBlockEdge read FInnerBottom write SetInnerBottom;
    property InnerTop:TBlockEdge read FInnerTop write SetInnerTop;
    property OuterBottom:TBlockEdge read FOuterBottom write SetOuterBottom;
    property OuterTop:TBlockEdge read FOuterTop write SetOuterTop;
  end;

  TPieSliceBlock=class(TCustomCoverBlock)
  private
    FAngle        : Double;
    FStacks       : Double;
    FDonutPercent : Double;
    FEdges        : TSliceEdges;
    FStartAngle   : Double;

    IList         : Integer;
    IListSide1    : Integer;
    IListSide2    : Integer;
    IListPenSide1 : Integer;
    IListPenSide2 : Integer;
    FInnerSize: TPointXYFloat;

    procedure DataChanged(Sender:TObject);
    function IsStacksStored:Boolean;
    procedure SetAngle(const Value: Double);
    procedure SetStacks(const Value: Double);
    procedure SetDonut(const Value: Double);
    procedure SetEdges(const Value: TSliceEdges);
    procedure SetStartAngle(const Value: Double);
    procedure SetInnerSize(const Value: TPointXYFloat);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Angle:Double read FAngle write SetAngle;
    property DonutPercent:Double read FDonutPercent write SetDonut;
    property Edges:TSliceEdges read FEdges write SetEdges;
    property InnerSize:TPointXYFloat read FInnerSize write SetInnerSize;
    property Stacks:Double read FStacks write SetStacks stored IsStacksStored;
    property StartAngle:Double read FStartAngle write SetStartAngle;
  end;

  TRectangleBlock=class;

  TRectangleCorners=class(TPersistent)
  private
    FLeftTop     : TColor;
    FRightTop    : TColor;
    FLeftBottom  : TColor;
    FRightBottom : TColor;

    IOwner : TRectangleBlock;

    procedure SetLeftTop(const Value:TColor);
    procedure SetRightTop(const Value:TColor);
    procedure SetLeftBottom(const Value:TColor);
    procedure SetRightBottom(const Value:TColor);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property LeftTop:TColor read FLeftTop write SetLeftTop default clDefault;
    property RightTop:TColor read FRightTop write SetRightTop default clDefault;
    property LeftBottom:TColor read FLeftBottom write SetLeftBottom default clDefault;
    property RightBottom:TColor read FRightBottom write SetRightBottom default clDefault;
  end;

  TRectangleSides=class(TPersistent)
  private
    FLeft   : TColor;
    FRight  : TColor;
    FTop    : TColor;
    FBottom : TColor;

    IOwner : TRectangleBlock;

    procedure SetLeft(const Value:TColor);
    procedure SetRight(const Value:TColor);
    procedure SetTop(const Value:TColor);
    procedure SetBottom(const Value:TColor);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Left:TColor read FLeft write SetLeft default clDefault;
    property Right:TColor read FRight write SetRight default clDefault;
    property Top:TColor read FTop write SetTop default clDefault;
    property Bottom:TColor read FBottom write SetBottom default clDefault;
  end;

  TRectangleBlock=class(TCustomBlock)
  private
    FCenter   : TColor;
    FCorners  : TRectangleCorners;
    FSides    : TRectangleSides;

    IAnyCorner : Boolean;
    IAnySide   : Boolean;
    IList      : Integer;
    IListFlat  : Integer;
    IListPen   : Integer;

    procedure ChangedCorner;
    procedure ChangedSide;
    procedure SetCenter(const Value: TColor);
    procedure SetCorners(const Value:TRectangleCorners);
    procedure SetSides(const Value:TRectangleSides);
  protected
    procedure DeleteLists; override;
    procedure PrepareForGallery; override;
    procedure ReadState(Reader: TReader); override;
    function UsesDepth:Boolean; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure Draw; override;
  published
    property Center:TColor read FCenter write SetCenter default clDefault;
    property Corners:TRectangleCorners read FCorners write SetCorners;
    property Sides:TRectangleSides read FSides write SetSides;
  end;

  TGradientBlock=class(TRectangleBlock)
  private
    FGradient: TTeeGradient;

    function GetGradient: TTeeGradient;
    procedure GradientChanged(Sender:TObject);
    procedure SetGradient(const Value: TTeeGradient);
  protected
    procedure PrepareForGallery; override;
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
  published
    property Gradient:TTeeGradient read GetGradient write SetGradient;
  end;

  TObjectProperties=class(TStringList)
  private
    IBlocks : TBlocks;
    IOwner  : TCustomBlock;

    function GetValue(const AName:String):String;
    procedure SetValue(const AName,AValue:String);
  public
    function PropertyValue(const AName:String):String;
    function IndexOfProperty(const AName:String):Integer;

    property Blocks:TBlocks read IBlocks;
    property Value[const Name:String]:String read GetValue write SetValue;
  end;

  TCustomObjectBlock=class(TCustomBlock)
  private
    FItems      : TBlocks;
    FProperties : TObjectProperties;

    IBoundsMin  : TPoint3DFloat;
    IBoundsMax  : TPoint3DFloat;
    IPropsReady : Boolean;

    function Get(Index:Integer):TCustomBlock;
    function GetItems:TBlocks;
    function GetProperties:TObjectProperties;
    function ItemsStored:Boolean;
    procedure SetItems(const Value:TBlocks);
    procedure SetProperties(const Value:TObjectProperties);
  protected
    FLink  : String;
    FLinkBlock : TCustomBlock;

    function CompleteLinkFile:String;
    procedure DeleteLists; override;

    procedure DoDrawItems; virtual;
    procedure StartTransform; override;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    function GetChildOwner: TComponent; override;
    function ItemsReady:Boolean;
    procedure LoadItems(const ASource,AFile:String); virtual;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    function SaveChildren:Boolean; dynamic;
    procedure SetBlocks(const Value:TBlocks); override;
    procedure SetLink(const Value:String); virtual;
    procedure SetLinkBlock(const Value:TCustomBlock);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean; override;
    procedure Clear; override;
    procedure Draw; override;
    function HasContents:Boolean; virtual;

    property Item[Index:Integer]:TCustomBlock read Get; default;

    // To be published:
    property Items:TBlocks read GetItems write SetItems stored ItemsStored;
    property LinkBlock:TCustomBlock read FLinkBlock write SetLinkBlock;
    property LinkFile:String read FLink write SetLink;
    property Properties:TObjectProperties read GetProperties write SetProperties;
  end;

  TObjectBlock=class(TCustomObjectBlock)
  published
    property Items;
    property LinkBlock;
    property LinkFile;
    property Properties;
  end;

  TBlockClass=class of TCustomBlock;

  TObjectBlockHandle=class(TObjectBlock)
  public
    HandleClass : TBlockClass;
    Locations   : TPoint3DArray;

    Destructor Destroy; override;

    function AddHandle(const ALocation:TPoint3DFloat; const DragAction:String;
                       const LeftClickAction:String=''):TCustomBlock; overload;
    function AddHandle(const x,y,z:Single; const DragAction:String;
                       const LeftClickAction:String=''):TCustomBlock; overload;
  end;

  TPyramidBlock=class(TCustomBlock)
  private
    FSide1 : TPointXYFloat;
    FSide2 : TPointXYFloat;

    IList : Integer;

    procedure Changed(Sender:TObject);
    procedure SetSide1(const Value:TPointXYFloat);
    procedure SetSide2(const Value:TPointXYFloat);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
  published
    property Side1:TPointXYFloat read FSide1 write SetSide1;
    property Side2:TPointXYFloat read FSide2 write SetSide2;
  end;

  TPointXYZColor=class(TPointXYZFloat)
  private
    procedure SetColor(const Value: TColor);
  protected
    FColor : TColor;
  public
    Constructor Create(const AOwner:TPersistent=nil; const AValue:Double=0;
                       const ChangedEvent:TNotifyEvent=nil); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Color:TColor read FColor write SetColor default clDefault;
  end;

  TTriangleBlock=class(TCustomBlock)
  private
    FPoint0 : TPointXYZColor;
    FPoint1 : TPointXYZColor;
    FPoint2 : TPointXYZColor;

    IList : Integer;

    procedure ChangedPoint(Sender: TObject);
    procedure SetPoint0(const Value: TPointXYZColor);
    procedure SetPoint1(const Value: TPointXYZColor);
    procedure SetPoint2(const Value: TPointXYZColor);
  protected
    procedure DeleteLists; override;
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw; override;
  published
    property Point0:TPointXYZColor read FPoint0 write SetPoint0;
    property Point1:TPointXYZColor read FPoint1 write SetPoint1;
    property Point2:TPointXYZColor read FPoint2 write SetPoint2;
  end;

  TBlockList=class(TList)
  private
    IKeepBlocks : Boolean;

    Function Get(Index:Integer):TCustomBlock; {$IFDEF D10}inline;{$ENDIF}
    Procedure Put(Index:Integer; Const Value:TCustomBlock); {$IFDEF D10}inline;{$ENDIF}
  public
    Procedure Clear; override;
    function Exists(const ATitle:String):Boolean;
    procedure FreeAll;
    function Last:TCustomBlock;

    property Block[Index:Integer]:TCustomBlock read Get write Put; default;
  end;

  // Animations:

  TAnimateItem=class(TCollectionItem)
  private
    FAnimate    : TTeeAnimate;
    FDesc       : String;
    FPlayOnLoad : Boolean;

    IList : TStrings;

    procedure AnimateDestroyed(Sender:TObject);
    function GetAnimate:TTeeAnimate;
    function GetAnimationCount:Integer;
    function GetAnimations:TAnimations;
    function GetList:TStrings;
    function GetLoop:Boolean;
    function GetOnStop:TNotifyEvent;
    function GetPlaying:Boolean;
    function GetSpeed:Integer;

    procedure SetAnimations(const Value:TAnimations);
    procedure SetDesc(const Value:String);
    procedure SetList(const Value:TStrings);
    procedure SetLoop(const Value:Boolean);
    procedure SetOnStop(const Value:TNotifyEvent);
    procedure SetPlaying(const Value:Boolean);
    procedure SetSpeed(const Value:Integer);
  public
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property Animate:TTeeAnimate read GetAnimate;
    property AnimationCount:Integer read GetAnimationCount;
  published
    property Animations:TAnimations read GetAnimations write SetAnimations stored False;
    property Description:String read FDesc write SetDesc;
    property List:TStrings read GetList write SetList;  // <--- hide
    property Loop:Boolean read GetLoop write SetLoop default False;
    property Playing:Boolean read GetPlaying write SetPlaying stored False;
    property PlayOnLoad:Boolean read FPlayOnLoad write FPlayOnLoad default False;
    property Speed:Integer read GetSpeed write SetSpeed default 60;

    // Events
    property OnStop:TNotifyEvent read GetOnStop write SetOnStop;
  end;

  TAnimates=class(TOwnedCollection)
  private
    IParent : TCustomTeePanel;

    procedure FinishedLoading(const ASource:String);
    function Get(Index: Integer): TAnimateItem;
    procedure Put(Index: Integer; const Value: TAnimateItem);
  protected
    procedure PrepareList;
  public
    function Add:TAnimateItem;
    function IndexOf(ADescription:String):TAnimateItem;

    property Item[Index:Integer]:TAnimateItem read Get write Put; default;
  end;

  TBlockShadows=record
    Color        : TColor;
    Smooth       : Boolean;
    SmoothSize   : Integer;
    Transparency : TTeeTransparency;
    Visible      : Boolean;
  end;

  {.$DEFINE TEEPICKSELECT}

  {$IFDEF TEEPICKSELECT}
  TPickBuffer=Array[0..4095] of Longword;
  {$ENDIF}

  // Blocks
  TBlocks=class(TComponent)
  private
    FAnimates     : TAnimates;
    FDrawBlocks   : TBlocks;
    FEvents       : TStrings;
    FHideBorders  : Boolean;
    FHideTextures : Boolean;
    FProperties   : TObjectProperties;
    IProgramShader : TProgramShader;

    FOnItemsChanged : TNotifyEvent;
    FOnLoaded     : TNotifyEvent;

    IAutoPlayDone : Boolean;
    IParent       : TCustomTeePanel;
    ITransparency : Byte;

    function CheckFileExtension(var FileName:String):String;
    procedure CreateBlockRuntime(Reader: TReader;
                    ComponentClass: TComponentClass; var Component: TComponent);
    procedure FinishedLoading;
    function Get(Index:Integer):TCustomBlock; {$IFDEF D10}inline;{$ENDIF}
    function GetAnimates:TAnimates;
    function GetCurrentParents:TBlockList;
    function GetEvents:TStrings;
    function GetProperties:TObjectProperties;
    function GetProgramShader:TProgramShader;
    function GetShaderEnabled:Boolean;

    class function GetLibraryPath:String;
    function GetterLibraryPath:String;
    function InternalClicked(X,Y:Integer; ABlock:TCustomBlock; IncludeObjects,
                             IncludeSubObjects:Boolean):TCustomBlock; overload;

    procedure Put(Index:Integer; const Value:TCustomBlock); {$IFDEF D10}inline;{$ENDIF}
    procedure ReaderError(Reader: TReader; const Message: string; var Handled: Boolean);
    function RecursiveContains(const Value:TCustomBlock):Boolean;
    procedure SetAnimates(const Value: TAnimates);
    procedure SetEvents(const Value:TStrings);
    procedure SetHideBorders(const Value:Boolean);
    procedure SetHideTextures(const Value:Boolean);
    procedure SetLibraryPath(const Value:String);
    procedure SetParent(APanel:TCustomTeePanel);
    procedure SetProperties(const Value:TObjectProperties);
    procedure SetShaderEnabled(const Value:Boolean);
    procedure TryLoad(const ParentSource,Source:String);
  protected
    FCurrentSource : String;
    FOnRemoved     : TNotifyEvent;

    ICurrentParents : TBlockList;
    IDrawingReflection : Boolean;
    IFloor         : TCustomBlock;
    ILightNum      : Integer;
    IList          : TBlockList;
    ILoadThread    : TThread;
    IMaxLights     : Integer;
    IObject        : TCustomObjectBlock;
    IOldAntiAlias  : Boolean;
    ITranspBlocks  : TList;
    IUseCursor     : Integer;

    ICanvas : {$IFDEF BLOCKS}IGraphicsGL{$ELSE}TGLCanvas{$ENDIF};

    procedure InitLights;
    procedure DrawAfter;
    class procedure DoDrawItem(ABlock:TCustomBlock);
    procedure ItemsChanged(ABlock:TCustomBlock);
    procedure DeleteLists;
    function DoFindName(var AName:String):TCustomBlock;
    function DoPicking:Integer;
    procedure DoSetProperty(SenderBlock:TCustomBlock; Exp:String);
    procedure FixAnimationsLoaded;
    Procedure GetChildren(Proc:TGetChildProc; Root:TComponent); override;
    function GetChildOwner: TComponent; override;
    function HasEvents:Boolean;
    function InternalGetPropValue(Exp:String):Variant;
    function IsAnimatesStored:Boolean;
    class function IsBooleanProperty(const Prop:PPropInfo):Boolean;
    procedure PreparePicking({$IFDEF TEEPICKSELECT}var Buffer:TPickBuffer;{$ENDIF} X,Y:Integer);
    procedure ResetShown;

    property ShaderEnabled:Boolean read GetShaderEnabled write SetShaderEnabled;
    procedure SetBlockName(ABlock:TCustomBlock);

    property OnItemsChanged:TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
    property OnLoaded:TNotifyEvent read FOnLoaded write FOnLoaded;
    property ProgramShader:TProgramShader read GetProgramShader;
  public
    Shadows : TBlockShadows;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Add(ABlock:TCustomBlock):TCustomBlock;
    function CalcBounds(var AMin,AMax:TPoint3DFloat):Boolean; 
    class procedure CheckLibraryPath(const SubFolder:String; var AFile:String);
    procedure Clear;

    function ClickedBlock(X,Y:Integer; IncludeObjects:Boolean=False;
                          IncludeSubObjects:Boolean=True):TCustomBlock; overload;

    function ClickedBlock(ABlock:TCustomBlock; X,Y:Integer):TCustomBlock; overload;

    function CloneBlock(ABlock:TCustomBlock):TCustomBlock;
    function Count:Integer; {$IFDEF D10}inline;{$ENDIF}
    class function DoGetObjectProp(AInst:TObject; const AProp:String):TObject;
    procedure Draw(Picking:Boolean=False);
    function Exists(const ATitle:String):Boolean; {$IFDEF D10}inline;{$ENDIF}

    function HasAnimations:Boolean;
    function HasProperties:Boolean;

    function Find(var AName:String; UseTitle:Boolean=False; SubObjects:Boolean=False):TCustomBlock;
    function FindByName(const AName:String):TCustomBlock;
    function FindByTitle(const ATitle:String; SubObjects:Boolean=False):TCustomBlock;

    function IndexOf(ABlock:TCustomBlock):Integer;

    Procedure LoadFromFile(const FileName:String);
    procedure LoadFromStream(Stream:TStream); virtual;
    procedure LoadFromURL(const URL:String);
    class function LoadURLStream(const URL:String; out ErrorSt:String):TStream;

    class function ParseFileName(const FolderName,FileName:String):String;
    procedure Remove(ABlock:TCustomBlock);
    procedure RemoveTexture(AGraphic: TGraphic); overload;
    procedure RemoveTexture(Picture:TPicture); overload;
    procedure Repaint;

    Procedure SaveToFile(const FileName:String);
    Procedure SaveToStream(Stream:TStream; Sign:Boolean=False);

    property Block[Index:Integer]:TCustomBlock read Get write Put; default;
    property CurrentParents:TBlockList read GetCurrentParents;
    property CurrentSource:String read FCurrentSource;
    property DrawBlocks:TBlocks read FDrawBlocks;
    property HideBorders:Boolean read FHideBorders write SetHideBorders default True;
    property HideTextures:Boolean read FHideTextures write SetHideTextures default False;
    property LibraryPath:String read GetterLibraryPath write SetLibraryPath;
    property Parent:TCustomTeePanel read IParent write SetParent;

    property OnRemoved:TNotifyEvent read FOnRemoved write FOnRemoved;
  published
    property Animates:TAnimates read GetAnimates write SetAnimates
                                stored IsAnimatesStored;
    property Events:TStrings read GetEvents write SetEvents stored HasEvents;
    property Properties:TObjectProperties read GetProperties write SetProperties
                           stored HasProperties;
  end;

function RemoveAmpersand(const S:String):String;

procedure RegisterBlock(BlockClass:TBlockClass);
procedure RegisterBlocks(const BlockClasses:Array of TBlockClass);

type
  TBlockClasses=class(TList)
  private
    function Get(Index:Integer):TBlockClass;
  public
    class function BlockDescription(const AClass:String):String;
    class function Sorted:TStringList;

    property BlockClass[Index:Integer]:TBlockClass read Get; default;
  end;

  TPointSinCos=record
    X : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    Y : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
  end;

function AngleOf(const P0,P1,P2:TFloatPoint):Double;

Function PointFloat(const P:TPoint):TPointFloat; overload; {$IFDEF D9}inline;{$ENDIF}
Function PointFloat(const aX,aY:Single):TPointFloat; overload; {$IFDEF D9}inline;{$ENDIF}
Function PointFloat(const aX,aY,aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}
Function PointFloat(const P:TPoint; const aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}
Function PointFloat(const P:TPoint3D; const aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}

var
  BlockClasses:TBlockClasses=nil;
  TeeNoThreads:Boolean=False;

const
  TwoPi  = 2*Pi;
  HalfPi = Pi*0.5;

function CalculateNormal(const A,B,C:TPoint3DFloat):TPoint3DFloat;

// Registry
function TeeMakerReadRegistry(const AKey,AName,ADefault:String):String;
procedure TeeMakerWriteRegistry(const AKey,AName,AValue:String);

// Strings
function LastPosOf(const ASub:Char; const AStr:String):Integer;
function RemoveTrailingSlash(const S:String):String;
procedure SplitValue(const Value:String; var ALeft,ARight:String);

// File name
function RemoveFileExtension(const FileName:String):String;

// Properties
function IsColorProperty(AObject: TObject; const AName: String): Boolean;

const
  TeeMakerKey='Software\Steema Software\TeeMaker';
  TeeMakerLibraryTag='$(TEEMAKER)\';
  CM_BLOCKBIRTH=20000;

type
  TMakerSystem=class(TComponent)
  private
    function GetNow:TDateTime; {$IFDEF D9}inline;{$ENDIF} // Cannot be static: D2011 bcc32 compiler bug
    function GetRandom:Double; {$IFDEF D9}inline;{$ENDIF} // Cannot be static: D2011 bcc32 compiler bug
  public
    property Now:TDateTime read GetNow;  // Cannot be class static: D2011 bcc32 compiler bug
    property Random:Double read GetRandom; // Cannot be class static: D2011 bcc32 compiler bug
  end;

  TBounds=record
    Min : TPoint3DFloat;
    Max : TPoint3DFloat;
  end;

// TPoint3DFloat
function AddPoints(const A,B:TPoint3DFloat):TPoint3DFloat; overload;
function AddPoints(const Bounds:TBounds; const Value:TPoint3DFloat):TBounds; overload;
function AddMultiply(const A,B:TPoint3DFloat; const Value:Double):TPoint3DFloat;
function AbsSubtract(const A,B:TPoint3DFloat):TPoint3DFloat;
function Center(const A,B:TPoint3DFloat):TPoint3DFloat;
function MaxValue(const P:TPoint3DFloat):Double;
function MinValue(const P:TPoint3DFloat):Double;
function Subtract(const A,B:TPoint3DFloat):TPoint3DFloat;

type
  TActionEventProc=procedure;

  TActionEvent=class(TObject)
    ID   : String;
    Path : String;
    Event : TActionEventProc;
  end;

  TMakerEvents=class(TList)
  public
    procedure AddEvent(const ActionID,ActionPath:String);
    function IDToPath(const ID:String):String;
    function PathToID(const Path:String):String;
  end;

var
  MakerSystem:TMakerSystem;
  MakerEvents:TMakerEvents;

var
  TeeOnChangeVisual : TChangeVisual=nil;
  TeeOnEditVisual   : TEditVisual=nil;
  TeeVisualEditorClass:TVisualEditorClass=nil;

implementation

uses
  Math, TeeHtml,
  Consts,

  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}

  JPEG,

  {$IFDEF CLX}
  TeeGIF,
  {$ELSE}
  {$IFDEF D105}
  GIFImg,
  {$ELSE}
  GIFImage,
  {$ENDIF}

  {$IFDEF CLX}
  TeePNG, TeeJPEG,
  {$ELSE}

  {$IFDEF D12}
  PNGImage,
  {$ELSE}
  TeePNGImage,
  {$ENDIF}

  {$ENDIF}

  {$ENDIF}

  TeeTarga, TeeLoadError, Registry, TeeURL,

  {$IFDEF D6}
  DateUtils,
  {$ENDIF}

  TeeFilters, TeeMakerConst;

procedure TeeMakerWriteRegistry(const AKey,AName,AValue:String);
begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_CURRENT_USER;

    if OpenKey(TeeMakerKey+'\'+AKey,True) then
       WriteString(AName,AValue);
  finally
    Free;
  end;
end;

function TeeMakerReadRegistry(const AKey,AName,ADefault:String):String;
begin
  result:=ADefault;

  with TRegistry.Create do
  try
    RootKey:=HKEY_CURRENT_USER;

    if OpenKeyReadOnly(TeeMakerKey+'\'+AKey) then
       if ValueExists(AName) then
          result:=ReadString(AName);
  finally
    Free;
  end;
end;

function RemoveFileExtension(const FileName:String):String;
begin
  result:=ExtractFileExt(FileName);

  if result='' then
     result:=FileName
  else
     result:=Copy(FileName,1,Length(FileName)-Length(result));
end;

{ TBlockClasses }

class function TBlockClasses.BlockDescription(const AClass:String):String;
begin
  result:=AClass;

  if UpperCase(Copy(result,1,1))='T' then
     System.Delete(result,1,1);

  if UpperCase(Copy(result,Length(result)-4,5))='BLOCK' then
     System.Delete(result,Length(result)-4,5);
end;

class function TBlockClasses.Sorted:TStringList;
var t : Integer;
begin
  result:=TStringList.Create;

  for t:=0 to BlockClasses.Count-1 do
      result.AddObject(BlockDescription(BlockClasses[t].ClassName),TObject(t));

  result.Sorted:=True;
end;

function TBlockClasses.Get(Index:Integer):TBlockClass;
begin
  result:=TBlockClass(Items[Index]);
end;

procedure RegisterBlock(BlockClass:TBlockClass);
begin
  TeeActivateGroup;

  RegisterClass(BlockClass);

  if BlockClasses.IndexOf(BlockClass)=-1 then
     BlockClasses.Add(BlockClass);
end;

procedure RegisterBlocks(const BlockClasses:Array of TBlockClass);
var t : Integer;
begin
  for t:=Low(BlockClasses) to High(BlockClasses) do
      RegisterBlock(BlockClasses[t]);
end;

function RemoveAmpersand(const S:String):String;
begin
  result:=S;

  while Pos('&',result)>0 do
        Delete(result,Pos('&',result),1);
end;

// Threading

type
  TBlockThread=class(TThread)
  private
    Format   : TBlockFormat;
    TeePanel : TCustomTeePanel;
    ParentSource : String;
    Source : String;
  end;

  TLoadPictureThread=class(TBlockThread)
  private
    procedure ThreadTerminated(Sender:TObject);
  protected
    procedure Execute; override;
  end;

procedure TLoadPictureThread.ThreadTerminated(Sender:TObject);
begin
  if (not Assigned(TeePanel)) or (not (csDestroying in TeePanel.ComponentState)) then
  with Format.Texture do
  if Assigned(FPicture) then
     if PictureTransparent then
        if FPicture.Graphic<>nil then
           FPicture.Graphic.Transparent:=True;

  with Format.Texture do
  if Assigned(FPicture) then
     FPicture.ILoadThread:=nil;

  if Assigned(TeePanel) then
  begin
    Application.ProcessMessages;
    TeePanel.Invalidate;
  end;
end;

procedure TLoadPictureThread.Execute;
var tmpPic : TBlockPicture;
begin
  tmpPic:=Format.Texture.FPicture;

  if Assigned(tmpPic) then
  begin
    tmpPic.TryLoad(ParentSource,Source);
    Format.Texture.CheckPicTransp;
  end;
end;

type
  TLoadThread=class(TBlockThread)
  private
    Items  : TBlocks;

    procedure ThreadTerminated(Sender:TObject);
  protected
    procedure Execute; override;
  end;

procedure TLoadThread.ThreadTerminated(Sender:TObject);
begin
  Items.ILoadThread:=nil;

  //  Items.FinishedLoading;

  if Assigned(TeePanel) then
     TeePanel.Invalidate;
end;

procedure TLoadThread.Execute;
begin
  Items.IObject.LoadItems(ParentSource,Source);
end;

{ TBlocks }

Constructor TBlocks.Create(AOwner: TComponent);
begin
  inherited;

  IList:=TBlockList.Create;
  FDrawBlocks:=Self;

  Shadows.Transparency:=80;
  Shadows.SmoothSize:=5;

  IMaxLights:=-1;
  FHideBorders:=True;
end;

Destructor TBlocks.Destroy;
begin
  IProgramShader.Free;
  ICurrentParents.Free;

  if Assigned(ILoadThread) then
  begin
    ILoadThread.Terminate;

    TerminateThread(ILoadThread.Handle,1);

    FreeAndNil(ILoadThread);
  end;

  FOnRemoved:=nil;

  ITranspBlocks.Free;
  IList.Free;

  FAnimates.Free;
  FEvents.Free;
  FProperties.Free;

  inherited;
end;

function TBlocks.GetProgramShader:TProgramShader;
begin
  if not Assigned(IProgramShader) then
     IProgramShader:=TProgramShader.Create;

  result:=IProgramShader;
end;

class function TBlocks.DoGetObjectProp(AInst:TObject; const AProp:String):TObject;
var tmp  : PPropInfo;
begin
  result:=nil;

  tmp:=GetPropInfo(AInst, AProp);

  if Assigned(tmp) then
     if tmp.PropType^.Kind=tkClass then
        result:=GetObjectProp(AInst, AProp);
end;

function TBlocks.GetCurrentParents:TBlockList;
begin
  if not Assigned(ICurrentParents) then
  begin
    ICurrentParents:=TBlockList.Create;
    ICurrentParents.IKeepBlocks:=True;
  end;

  result:=ICurrentParents;
end;

procedure TBlocks.SetParent(APanel:TCustomTeePanel);
begin
  IParent:=APanel;

  if Assigned(FAnimates) then
     FAnimates.IParent:=APanel;
end;

function TBlocks.Count:Integer;
begin
  result:=IList.Count;
end;

procedure TBlocks.TryLoad(const ParentSource, Source: String);
var tmp : String;
begin
  tmp:=Source;

  if ExtractFileExt(tmp)='' then
     tmp:=tmp+TeeMakerExtension;

  if TeeIsURL(tmp) then
     LoadFromURL(tmp)
  else
  if FileExists(tmp) then
     LoadFromFile(tmp)
  else
  if TeeIsURL(ParentSource) then
     LoadFromURL(ParentSource+'/'+tmp)
  else
  begin
    tmp:=ParentSource+'\'+ExtractFileName(tmp);

    if FileExists(tmp) then
       LoadFromFile(tmp);
  end;
end;

class function TBlocks.ParseFileName(const FolderName,FileName:String):String;
var tmp : String;
begin
  tmp:=Trim(FileName);

  if UpperCase(Copy(tmp,1,12))=TeeMakerLibraryTag then
     result:=TBlocks.GetLibraryPath+'\'+FolderName+'\'+Copy(tmp,13,Length(tmp))
  else
     result:=tmp;
end;

function RemoveTrailingSlash(const S:String):String;
begin
  result:=S;

  while Copy(result,Length(result),1)='\' do
        Delete(result,Length(result),1);
end;

procedure SplitValue(const Value:String; var ALeft,ARight:String);
var i : Integer;
begin
  i:=Pos('=',Value);

  if i>0 then
  begin
    ALeft:=Trim(Copy(Value,1,i-1));
    ARight:=Trim(Copy(Value,i+1,Length(Value)));
  end
  else
  begin
    ALeft:=Trim(Value);
    ARight:='';
  end;
end;

class procedure TBlocks.CheckLibraryPath(const SubFolder:String; var AFile:String);
var tmp : String;
begin
  AFile:=Trim(AFile);

  if Copy(AFile,1,1)<>'$' then  // (does not begin with tag)
  begin
    if not TeeIsURL(AFile) then
    begin
      tmp:=RemoveTrailingSlash(UpperCase(Trim(GetLibraryPath)))+'\'+UpperCase(SubFolder);

      if Copy(UpperCase(AFile),1,Length(tmp))=tmp then
      begin
        // Delete path
        AFile:=Copy(AFile,Length(tmp)+1,Length(AFile));

        while Copy(AFile,1,1)='\' do
              Delete(AFile,1,1);

        // Add tag
        AFile:=TeeMakerLibraryTag+AFile;
      end;
    end;
  end;
end;

function TBlocks.Exists(const ATitle:String):Boolean;
begin
  result:=IList.Exists(ATitle);
end;

function TBlocks.FindByTitle(const ATitle:String; SubObjects:Boolean=False):TCustomBlock;
var tmp : String;
begin
  tmp:=ATitle;
  result:=Find(tmp,True,SubObjects);
end;

function TBlocks.FindByName(const AName:String):TCustomBlock;
var tmp : String;
begin
  tmp:=AName;
  result:=Find(tmp);
end;

function TBlocks.Find(var AName: String; UseTitle:Boolean=False; SubObjects:Boolean=False): TCustomBlock;
var
  CheckIndex : Boolean;

  function FindBlock(ABlocks:TBlocks; const ABlockDesc:String):TCustomBlock;
  var t : Integer;
      tmpIndex : Integer;
  begin
    result:=nil;

    if ABlockDesc<>'' then
    begin
      if CheckIndex then
         tmpIndex:=StrToIntDef(ABlockDesc,-1)
      else
         tmpIndex:=-1;

      if tmpIndex=-1 then
      begin
        with ABlocks do
        for t:=0 to Count-1 do
        begin
          if (UseTitle and (UpperCase(Block[t].Title)=ABlockDesc)) or
             ((not UseTitle) and (UpperCase(Block[t].Name)=ABlockDesc)) then
          begin
            result:=Block[t];
            break;
          end;

          if SubObjects and (Block[t] is TCustomObjectBlock) then
          begin
            result:=TCustomObjectBlock(Block[t]).Items.Find(AName,UseTitle,SubObjects);

            if Assigned(result) then
               break;
          end;
        end;
      end
      else
        result:=ABlocks[tmpIndex];
    end;
  end;

var i    : Integer;
    i2   : Integer;
    tmpB : TBlocks;
    tmpBlock : TCustomBlock;
    tmpName : String;
    tmpExt  : {$IFDEF D6}Extended{$ELSE}Double{$ENDIF};
begin
  tmpBlock:=nil;

  AName:=UpperCase(Trim(AName));

  i:=Pos('.',AName);
  i2:=Pos(':',AName); // <-- obsolete?

  if (i2>0) then
     if (i=0) or (i2<i) then
        i:=i2;

  tmpB:=Self;

  CheckIndex:=(i>0) and (not TryStrToFloat(AName,tmpExt));

  if CheckIndex then
  begin
    repeat
      tmpName:=UpperCase(Trim(Copy(AName,1,i-1)));

      tmpBlock:=FindBlock(tmpB,tmpName);

      if Assigned(tmpBlock) then
      begin
        if tmpBlock is TCustomObjectBlock then
           tmpB:=TCustomObjectBlock(tmpBlock).Items
        else
        begin
          Delete(AName,1,i);

          {$IFDEF D12}
          Exit(tmpBlock);
          {$ELSE}
          result:=tmpBlock;
          Exit;
          {$ENDIF}
        end
      end
      else
      begin
        result:=nil;
        Exit;
      end;

      Delete(AName,1,i);

      i:=Pos('.',AName);
      i2:=Pos(':',AName); // <-- obsolete?

      if (i2>0) then
         if (i=0) or (i2<i) then
            i:=i2;

    until i=0;
  end;

  result:=tmpBlock;

  tmpBlock:=FindBlock(tmpB,AName);

  if Assigned(tmpBlock) then
  begin
    result:=tmpBlock;
    AName:='';
  end;
end;

function TBlocks.IndexOf(ABlock:TCustomBlock):Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
  if Block[t]=ABlock then
  begin
    result:=t;
    Exit;
  end;

  result:=-1;
end;

procedure TBlocks.DeleteLists;
var t : Integer;
begin
  for t:=0 to Count-1 do
      Block[t].DeleteLists;
end;

procedure TBlocks.Remove(ABlock: TCustomBlock);
begin
  IList.Remove(ABlock);
  ABlock.IBlocks:=nil;

  if Assigned(FOnRemoved) then
     FOnRemoved(ABlock);
end;

procedure TBlocks.ItemsChanged(ABlock:TCustomBlock);
begin
  if Assigned(FOnItemsChanged) then
     FOnItemsChanged(ABlock);
end;

function TBlocks.GetAnimates: TAnimates;
begin
  if not Assigned(FAnimates) then
  begin
    FAnimates:=TAnimates.Create(IParent,TAnimateItem);
    FAnimates.IParent:=IParent;
  end;

  result:=FAnimates;
end;

{ TBlockList }

function TBlockList.Last:TCustomBlock;
begin
  if Count=0 then result:=nil
             else result:=Get(Count-1);
end;

procedure TBlockList.FreeAll;
begin
  while Count>0 do
        Get(0).Free;
end;

function TBlockList.Exists(const ATitle:String):Boolean;
var t : Integer;
begin
  for t:=0 to Count-1 do
  if Block[t].Title=ATitle then
  {$IFDEF D12}
     Exit(True);
  {$ELSE}
  begin
    result:=True;
    Exit;
  end;
  {$ENDIF}

  result:=False;
end;

function TBlockList.Get(Index: Integer): TCustomBlock;
begin
  result:=TCustomBlock(List[Index]);
end;

Procedure TBlockList.Put(Index:Integer; Const Value:TCustomBlock);
begin
  List[Index]:=Value;
end;

Procedure TBlockList.Clear;
begin
  if not IKeepBlocks then
     FreeAll;

  inherited;
end;

Procedure TBlocks.GetChildren(Proc:TGetChildProc; Root:TComponent);
var t : Integer;
    tt : Integer;
begin
  inherited;

  for t:=0 to IList.Count-1 do
      Proc(Block[t]);

  if Assigned(FAnimates) then
  for t:=0 to FAnimates.Count-1 do
  with FAnimates[t].Animations do
      for tt:=0 to Count-1 do
          Proc(Animation[tt]);
end;

function TBlocks.GetChildOwner: TComponent;
begin
  if csDesigning in ComponentState then
     result:=IParent.Owner
  else
     result:=Self;  // Self ? nil? <-- WARNING : ULTRA-SLOW due to TComponent.Notification 
end;

procedure TBlocks.SetBlockName(ABlock:TCustomBlock);
var tmp : String;
begin
  if ABlock.Name='' then
  begin
    tmp:=BlockClasses.BlockDescription(ABlock.ClassName);

    if {$IFDEF D12}CharInSet(tmp[1],['0'..'9']){$ELSE}tmp[1] in ['0'..'9']{$ENDIF} then
       tmp:='T'+tmp;

    ABlock.Name:=TeeGetUniqueName(ABlock.Owner,tmp);
  end;
end;

procedure TBlocks.ReaderError(Reader: TReader; const Message: string; var Handled: Boolean);
begin
  Handled:=GlobalContinueLoad or TAskLoadError.AskContinueLoad(nil,Message);
end;

procedure TBlocks.CreateBlockRuntime(Reader: TReader;
    ComponentClass: TComponentClass; var Component: TComponent);
begin
  Component:=ComponentClass.Create(nil);

  // Animations need to have an owner, even at runtime: ...
  if Component is TTeeAnimation then
  begin
    Self.InsertComponent(Component);

    {
    if Component is TPropertyAnimation then
    with TPropertyAnimation(Component) do
    begin
      if Instance=nil then
      begin
        ??
      end;
    end;
    }
  end;
end;

procedure TBlocks.FixAnimationsLoaded;
var t    : Integer;
    tt   : Integer;
    tmp  : TComponent;
    tmpA : TTeeAnimation;
begin
  //Necessary? (see Animation ReadState)
  if Assigned(FAnimates) then
  for t:=0 to FAnimates.Count-1 do
  with FAnimates[t] do
  begin
    for tt:=0 to List.Count-1 do
    begin
      tmp:=Self.FindComponent(List[tt]);

      if not Assigned(tmp) then
         tmp:=Self.Parent.FindComponent(List[tt]);

      if (not Assigned(tmp)) and Assigned(Self.Parent.Owner) then
         tmp:=Self.Parent.Owner.FindComponent(List[tt]);

      if Assigned(tmp) then
      begin
        tmpA:=tmp as TTeeAnimation;

        Animations.Add(tmpA);

        tmpA.Animate:=FAnimate;
      end;
    end;
  end;
end;

procedure TBlocks.LoadFromStream(Stream:TStream);
var Reader : TReader;
    t      : Integer;
    SpeedRead : Boolean;
begin
  Reader:=TReader.Create(Stream, 1024);
  try
    GlobalContinueLoad:=False;

    Reader.OnError:=ReaderError;

    IAutoPlayDone:=False;

    //SpeedRead:=(GetChildOwner=nil);  <-- animations runtime loading fails !!

    SpeedRead:=False;

    if SpeedRead then
       Reader.OnCreateComponent:=CreateBlockRuntime;

    Reader.ReadRootComponent(Self);

    // Reset Name and Picture.Transparent property:
    for t:=0 to Count-1 do
    with Block[t] do
    begin
      if Format.Texture.PictureTransparent then
         Format.Texture.PictureTransparent:=True;

      SetBlockName(Block[t]);
    end;

    FixAnimationsLoaded;

    FinishedLoading;
  finally
    Reader.Free;
  end;
end;

{ TCylinderBlock }

Constructor TCylinderBlock.Create(AOwner: TComponent);
begin
  inherited;

  FAngle:=360;
  FSlices:=32;
  FStacks:=1;

  ICurrentFormat:=Format;

  FBottomEdge:=TBlockEdge.Create(Self,0,Changed);
  FTopEdge:=TBlockEdge.Create(Self,0,Changed);
end;

Destructor TCylinderBlock.Destroy;
begin
  FTopEdge.Free;
  FBottomEdge.Free;
  FBrushCover.Free;
  IStackData:=nil;
  inherited;
end;

procedure TCylinderBlock.DeleteLists;
begin
  inherited;

  IStackData:=nil;

  DeleteList(IList);
  DeleteList(ICoverList1);
  DeleteList(ICoverList2);
  DeleteList(IListCover);
end;

function TCylinderBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(-1.0,1,0,'Angle,MinMax:0;360').Format.Color:=clFuchsia;
    AddHandle(-1,1,0.5,'StartAngle,MinMax:0;360').Format.Color:=clGreen;
  end;
end;

type
  TGLCanvasAccess=class(TGLCanvas);

procedure TCylinderBlock.Draw;
var
  tmpN : Double;
  tmpStartAngle : Double;

  tmpMax : Integer;

  procedure CalcRadius(AEdge:TBlockEdge; var rx,rz:Double);
  var tmp : Double;
  begin
    if Assigned(AEdge) and AEdge.Active then
    begin
      tmp:=AEdge.X;
      rx:=rx+tmp;
      rz:=rz+tmp;
    end;

    rx:=0.01*(100-rx);
    rz:=0.01*(100-rz);
  end;

  procedure AddRoundEdge(const AEdge:TBlockEdge; var P:TPoint3DArray;
                         const ASin,ACos: {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
                         Stack1,Stack2:Integer);
  var
    P1,P2,P3 : TPoint3DFloat;

    procedure DoAddRoundEdge;
    var InvCurve : Single;
        t        : Integer;
        mu,
        mu2,
        mum1,
        mum1mu,
        mum12    : Single;
    begin
      if AEdge.Style=resRound then
      begin
        if not Assigned(P) then
           SetLength(P,AEdge.Slices+1);

        InvCurve:=1/AEdge.Slices;

        for t:=0 to AEdge.Slices do
        begin
          mu:=t*InvCurve;
          mu2:=Sqr(mu);
          mum1:=1-mu;
          mum12:=Sqr(mum1);
          mum1mu:=2*mum1*mu;

          with P[t] do
          begin
            X:=P1.x*mum12 + P2.x*mum1mu + P3.x*mu2;
            Y:=P1.y*mum12 + P2.y*mum1mu + P3.y*mu2;
            Z:=P1.z*mum12 + P2.z*mum1mu + P3.z*mu2;
          end;
        end;
      end
      else
      begin
        if not Assigned(P) then
           SetLength(P,2);

        P[0]:=P1;
        P[1]:=P3;
      end;
    end;

  var rx,
      rz : Double;
  begin
    if (IConeX=0) and (IConeY=0) then
    begin
      with IStackData[Stack2] do
      begin
        P1.X:=X*ACos;
        P1.Y:=Y;
        P1.Z:=Z*ASin;
      end;

      P2.X:=ACos;

      if AEdge=FTopEdge then
         P2.Y:=1
      else
         P2.Y:=-1;

      P2.Z:=ASin;

      rx:=1-AEdge.X*0.01;
      rz:=rx;

      P3.X:=rx*ACos;
      P3.Y:=P2.Y;
      P3.Z:=rz*ASin;
    end
    else
    begin
      with IStackData[Stack2+1] do
      begin
        P1.X:=X*ACos;
        P1.Y:=Y;
        P1.Z:=Z*ASin;
      end;

      with IStackData[Stack1+1] do
      begin
        P2.X:=X*ACos;
        P2.Y:=Y;
        P2.Z:=Z*ASin;
      end;

      if AEdge=TopEdge then
      begin
        rx:=IConeX;
        rz:=IConeY;
      end
      else
      begin
        rx:=0;
        rz:=0;
        P2.Y:=-1;
      end;

      CalcRadius(AEdge,rx,rz);

      P3.X:=rx*ACos;
      P3.Y:=P2.Y;
      P3.Z:=rz*ASin;
    end;

    DoAddRoundEdge;
  end;

  procedure DoDraw(DrawPartialCover,DoFill:Boolean);
  var
    s3,c3   : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    normal0 : TPoint3DFloat;
    tmpXText : Double;
    PTopZero,
    PTopBis  : TPoint3DArray;
    PBottomZero,
    PBottomBis  : TPoint3DArray;
    tmpInvStacks : Double;

    procedure DrawSlice(ASlice:Integer);
    var
      s3bis : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
      c3bis : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
      normal1 : TPoint3DFloat;
      tmpOneLessBis : Double;

      procedure AppendEdge(const AEdge:TBlockEdge; var PZero,PBis:TPoint3DArray;
                           Stack1,Stack2:Integer);
      var
        tmpMaxS : Integer;
        tmpInvS : Double;
        Invert  : Boolean;

        procedure AddPoint(Index:Integer);
        var tmpYText : Double;
        begin
          if Invert then
             tmpYText:=1-(tmpMaxS-Index)*tmpInvS
          else
             tmpYText:=(tmpMaxS-Index)*tmpInvS;

          glNormal3fv(@normal0);
          Format.Texture.Coord(tmpXText,tmpYText);
          glVertex3fv(@PZero[Index]);

          glNormal3fv(@normal1);
          Format.Texture.Coord(tmpOneLessBis,tmpYText);
          glVertex3fv(@PBis[Index]);
        end;

      var t   : Integer;
          tmp : TPoint3DArray;
      begin
        AddRoundEdge(AEdge,PBis,s3Bis,c3Bis,Stack1,Stack2);

        if AEdge.Style=resRound then
           tmpMaxS:=AEdge.Slices
        else
           tmpMaxS:=1;

        tmpInvS:=(AEdge.Y*0.02)/tmpMaxS;

        Invert:=AEdge=FBottomEdge;

        if Invert then
        for t:=tmpMaxS downto 0 do
            AddPoint(t)
        else
        for t:=0 to tmpMaxS do
            AddPoint(t);

        tmp:=PZero;
        PZero:=PBis;
        PBis:=tmp;
      end;

    var tmpStart,
        tmpEnd,
        t       : Integer;
        tmpYText : Single;
        tmpBis  : Single;
    begin
      tmpBis:=(ASlice-1)/tmpN;

      SinCos((tmpBis+tmpStartAngle)*TwoPi, s3Bis, c3Bis);

      with normal1 do
      begin
        X:=c3Bis;
        Y:=0;
        Z:=s3Bis;
      end;

      tmpOneLessBis:=1-tmpBis;

      tmpEnd:=Length(IStackData)-1;

      if FTopEdge.Active then
         Dec(tmpEnd,2);

      glBegin(GL_QUAD_STRIP);

      if FBottomEdge.Active then
      begin
        AppendEdge(FBottomEdge,PBottomZero,PBottomBis,0,1);
        tmpStart:=1;
      end
      else
        tmpStart:=0;

      if (IConeX=0) and (IConeY=0) then
      for t:=tmpStart to tmpEnd do
      begin
        tmpYText:=(tmpEnd-t)*tmpInvStacks;

        glNormal3fv(@normal0);
        Format.Texture.Coord(tmpXText,tmpYText);
        glVertex3f(c3,IStackData[t].Y,s3);

        glNormal3fv(@normal1);
        Format.Texture.Coord(tmpOneLessBis,tmpYText);
        glVertex3f(c3Bis,IStackData[t].Y,s3Bis);
      end
      else
      for t:=tmpStart to tmpEnd do
      with IStackData[t] do
      begin
        tmpYText:=(tmpEnd-t)*tmpInvStacks;

        glNormal3fv(@normal0);
        Format.Texture.Coord(tmpXText,tmpYText);
        glVertex3f(X*c3,Y,Z*s3);

        glNormal3fv(@normal1);
        Format.Texture.Coord(tmpOneLessBis,tmpYText);
        glVertex3f(X*c3Bis,Y,Z*s3Bis);
      end;

      // Round
      if FTopEdge.Active then
         AppendEdge(FTopEdge,PTopZero,PTopBis,tmpEnd+1,tmpEnd);

      glEnd;

      s3:=s3Bis;
      c3:=c3Bis;
      normal0:=normal1;
      tmpXText:=tmpOneLessBis;
    end;

  var
    minRx : Double;
    minRz : Double;

    procedure CalcStacks;
    var tmpMaxYStack : Double;
        tmp     : Double;
        tmpOff  : Double;
        tmpCone : Double;
        tmpStart,
        tmpEnd,
        t : Integer;
    begin
      tmpInvStacks:=1/(Length(IStackData)-1);

      if FBottomEdge.Active then
      begin
        tmpOff:=(2-(100-FBottomEdge.Y)*0.02);
        tmpStart:=1;
      end
      else
      begin
        tmpOff:=0;
        tmpStart:=0;
      end;

      tmpEnd:=Length(IStackData)-1;

      if FTopEdge.Active then
      begin
        tmpMaxYStack:=(2-tmpOff-FTopEdge.Y*0.02)*tmpInvStacks;
        Dec(tmpEnd);
      end
      else
      begin
        tmpMaxYStack:=(2-tmpOff)*tmpInvStacks;
      end;

      for t:=tmpStart to tmpEnd do
      with IStackData[t] do
      begin
        Y:=tmpOff+(t*tmpMaxYStack)-1;

        tmpCone:=(FStacks-t)*tmpInvStacks;

        X:=minRx+(1-minRx)*tmpCone;
        Z:=minRz+(1-minRz)*tmpCone;
      end;

      if FTopEdge.Active then
      with IStackData[tmpEnd+1] do
      begin
        Y:=1;
        tmp:=1-FTopEdge.Point.Y*0.01;
        X:=minRx*tmp;
        Z:=minRz*tmp;
      end;
    end;

    procedure CheckEdges;
    var tmp : Integer;
    begin
      if not Assigned(IStackData) then
      begin
        tmp:=Stacks+1;

        if FTopEdge.Active then
           Inc(tmp);

        if FBottomEdge.Active then
           Inc(tmp);

        SetLength(IStackData,tmp);
        CalcStacks;
      end;

      if FTopEdge.Active and (not Assigned(PTopZero)) then
         AddRoundEdge(FTopEdge,PTopZero,s3,c3,FStacks,FStacks-1);

      if FBottomEdge.Active and (not Assigned(PBottomZero)) then
         AddRoundEdge(FBottomEdge,PBottomZero,s3,c3,0,1);
    end;

  var j,
      initSlice   : Integer;
      tmpOff      : Double;
      DoDrawCover : Boolean;
  begin
    DoDrawCover:=False;

    if (FAngle=360) or DrawPartialCover then
    begin
      tmpOff:=0;

      if (FAngle<>360) and DrawPartialCover and DoFill then
         if Assigned(FBrushCover) then
            DoDrawCover:=True;
    end
    else
      tmpOff:=(FSlices+tmpMax-1)/tmpN;

    SinCos((tmpOff+tmpStartAngle) * TwoPi, s3, c3);

    with normal0 do
    begin
      X:=c3;
      Y:=0;
      Z:=s3;
    end;

    if IConeX<>0 then
       minRx:=(100-IConeX)*0.01
    else
       minRx:=1;

    if IConeY<>0 then
       minRz:=(100-IConeY)*0.01
    else
       minRz:=1;

    if DoDrawCover then
    begin
      ICurrentFormat.Finish;
      BrushCover.Start;

      if IListCover=0 then
      begin
        IListCover:=CreateNewList;

        CheckEdges;
        DrawSlice(FSlices+tmpMax);

        glEndList;
      end
      else
        glCallList(IListCover);

      BrushCover.Finish;
      ICurrentFormat.Start;
    end;

    if IList=0 then
    begin
      IList:=CreateNewList;

      CheckEdges;

      if DoDrawCover then
      begin
        initSlice:=FSlices+tmpMax-1;
        tmpXText:=0;
      end
      else
      begin
        initSlice:=FSlices+tmpMax;
        tmpXText:=1-((FSlices+tmpMax)/tmpN);
      end;

      for j:=initSlice downto 1 do
          DrawSlice(j);

      glEndList;

      PTopBis:=nil;
      PTopZero:=nil;
      PBottomBis:=nil;
      PBottomZero:=nil;
    end
    else
      glCallList(IList);
  end;

  procedure DoDrawCover(ABrush:TBlockFormat; const Position:Integer; Back:Boolean;
                        rx,rz:Double; var AList:Integer; AEdge:TBlockEdge);
  var tmpOff  : Double;
      tmpInvN : Double;

    procedure DrawPoint(const Index:Integer);
    var tmpSin,
        tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        tmp    : Single;
    begin
      SinCos(tmpOff-Index*tmpInvN,tmpSin,tmpCos);

      if Back then tmp:=0.5+0.5*tmpSin
              else tmp:=0.5-0.5*tmpSin;

      Format.Texture.Coord(tmp,0.5+0.5*tmpCos);
      glVertex3f(rx*tmpSin,Position,rz*tmpCos);
    end;

    var
      rxInner,
      rzInner : Double;

    procedure DrawHolePoint(const Index:Integer);
    var tmpSin,
        tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        tmpX,
        tmpZ : Double;
    begin
      SinCos(tmpOff-Index*tmpInvN,tmpSin,tmpCos);

      tmpX:=rxInner*tmpSin;
      tmpZ:=rzInner*tmpCos;

      Format.Texture.Coord(0.5-0.5*tmpX,0.5+0.5*tmpZ);
      glVertex3f(tmpX,Position,tmpZ);

      Format.Texture.Coord(0.5-0.5*tmpSin,0.5+0.5*tmpCos);
      glVertex3f(rx*tmpSin,Position,rz*tmpCos);
    end;

    procedure SetParams;
    begin
      tmpOff:=TwoPi+HalfPi-tmpStartAngle*TwoPi;

      tmpInvN:=TwoPi/tmpN;

      CalcRadius(AEdge,rx,rz);
    end;

    procedure DrawTheCover;
    var t : Integer;
    begin
      SetParams;

      if (ICoverHole.X<>0) or (ICoverHole.Y<>0) then
      begin
        rxInner:=ICoverHole.X*0.01;
        rzInner:=ICoverHole.Y*0.01;

        glBegin(GL_QUAD_STRIP);

        glNormal3i(0,0,-Position);

        if Back then
        for t:=Slices downto 0 do
            DrawHolePoint(t)
        else
        for t:=0 to Slices do
            DrawHolePoint(t);

        glEnd;
      end
      else
      begin
        if FAngle>=180 then
           glBegin(GL_TRIANGLE_FAN)
        else
           glBegin(GL_POLYGON);

        glNormal3i(0,Position,0);

        if FAngle>=180 then
        begin
          Format.Texture.Coord(0.5,0.5);
          glVertex3f(0,Position,0);
        end;

        if Back then
        begin
          for t:=Slices downto 0 do
              DrawPoint(t);

          DrawPoint(Slices);
        end
        else
        begin
          for t:=0 to Slices do
              DrawPoint(t);

          DrawPoint(0);
        end;

        glEnd;
      end;
    end;

    procedure DrawCoverBorder;
    var t : Integer;
    begin
      SetParams;

      glBegin(GL_LINE_LOOP);

      for t:=0 to Slices do
          DrawPoint(t);

      glEnd;
    end;

  begin
    if Assigned(ABrush) then
    begin
      if ABrush<>ICurrentFormat then
         ICurrentFormat.Finish;

      ABrush.Start;
    end
    else
      ABrush:=ICurrentFormat;

    if AList=0 then
    begin
      AList:=CreateNewList;
      DrawTheCover;
      glEndList;
    end
    else
      glCallList(AList);

    if ABrush<>ICurrentFormat then
    begin
      if ABrush.PreparePen then
         DrawCoverBorder;

      ABrush.Finish;
      ICurrentFormat.Start;
    end;
  end;

const
  Inv360=1/360;
  Inv180=1/180;

var OldStyle : TTeeCanvasSurfaceStyle;
    CanCull  : Boolean;
    Draw1    : Boolean;
    Draw2    : Boolean;
    tmpIsCone : Boolean;
    DrawFlatCover: Boolean;
begin
  if FAngle=360 then tmpMax:=0
                else tmpMax:=1;

  tmpN:=Slices/(Max(0,FAngle)*Inv360);
  tmpStartAngle:=FStartAngle*Inv180;

  DrawFlatCover:=(not Assigned(FBrushCover)) or FBrushCover.Solid;

  if ICurrentFormat.Solid then
  begin
    tmpIsCone:=(IConeX=100) and (IConeY=100);

    Draw1:=(not tmpIsCone) and ((not Assigned(FBrush1)) or FBrush1.FSolid);
    Draw2:=(not Assigned(FBrush2)) or FBrush2.Solid;

    CanCull:=(not ShouldDrawInterior) and
             (tmpIsCone or Draw1) and Draw2 and
             ((FAngle=360) or DrawFlatCover)
             and (ICoverHole.X=0) and (ICoverHole.Y=0);

    if CanCull then
       glEnable(GL_CULL_FACE);

    DoDraw(DrawFlatCover,True);

    if not IHideCovers then
    begin
      if Draw2 then
         DoDrawCover(FBrush2,-1,False,0,0,ICoverList1,FBottomEdge);

      if Draw1 then
         DoDrawCover(FBrush1,1,True,IConeX,IConeY,ICoverList2,FTopEdge);
    end;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if ICurrentFormat.PreparePen then
  begin
{$IFDEF BLOCKS}
    OldStyle:=ICanvas.getDrawStyle;
    ICanvas.setDrawStyle(tcsWire);
{$ELSE}
    OldStyle:=ICanvas.DrawStyle;
    ICanvas.DrawStyle:=tcsWire;
{$ENDIF}


    DoDraw(DrawFlatCover,False);

{$IFDEF BLOCKS}
    ICanvas.setDrawStyle(OldStyle);
{$ELSE}
    ICanvas.DrawStyle:=OldStyle;
{$ENDIF}
    ICurrentFormat.FinishPen;
  end;
end;

procedure TCylinderBlock.Changed(Sender:TObject);
begin
  DeleteLists;
end;

procedure TCylinderBlock.SetSlices(const Value: Integer);
begin
  FSlices:=Value;
  Changed(Self);
end;

procedure TCylinderBlock.SetStacks(const Value: Integer);
begin
  FStacks:=Value;
  Changed(Self);
end;

procedure TCylinderBlock.SetStartAngle(const Value: Integer);
begin
  FStartAngle:=Value;
  Changed(Self);
end;

function TCylinderBlock.GetCover:TBlockFormat;
begin
  if not Assigned(FBrushCover) then
  begin
    FBrushCover:=TBlockFormat.Create(Self);
    FBrushCover.Color:=Format.Color;
  end;

  result:=FBrushCover;
end;

function TCylinderBlock.HasBrushCover:Boolean;
begin
  result:=Assigned(FBrushCover);
end;

procedure TCylinderBlock.SetCover(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     BrushCover.Assign(Value)
  else
     FreeAndNil(FBrushCover);

  Repaint;
end;

procedure TCylinderBlock.SetAngle(const Value: Integer);
begin
  FAngle:=Value;
  Changed(Self);
end;

procedure TCylinderBlock.Assign(Source: TPersistent);
begin
  if Source is TCylinderBlock then
  with TCylinderBlock(Source) do
  begin
    Self.FAngle:=FAngle;
    Self.BottomEdge:=FBottomEdge;
    Self.BrushCover:=FBrushCover;
    Self.FSlices:=FSlices;
    Self.FStacks:=FStacks;
    Self.FStartAngle:=FStartAngle;
    Self.TopEdge:=FTopEdge;
  end;

  inherited;
end;

procedure TCylinderBlock.SetBottomEdge(const Value: TBlockEdge);
begin
  FBottomEdge.Assign(Value);
end;

procedure TCylinderBlock.SetTopEdge(const Value: TBlockEdge);
begin
  FTopEdge.Assign(Value);
end;

{ TTorusBlock }

Constructor TTorusBlock.Create(AOwner: TComponent);
begin
  inherited;
  FRings:=30;
  FSides:=20;
  FRadius:=TPointXYFloat.Create(Self,66,RadiusChanged);
end;

Destructor TTorusBlock.Destroy;
begin
  FRadius.Free;
  inherited;
end;

procedure TTorusBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListSide1);
  DeleteList(IListSide2);
end;

procedure TTorusBlock.Assign(Source:TPersistent);
begin
  if Source is TTorusBlock then
  with TTorusBlock(Source) do
  begin
    Self.Radius:=FRadius;
    Self.FRings:=FRings;
    Self.FSides:=FSides;
    Self.FStartAngle:=FStartAngle;
    Self.FTotalAngle:=FTotalAngle;
  end;

  inherited;
end;

function TTorusBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(0.5,1,0,'StartAngle,MinMax:0;360').Format.Color:=clYellow;
    AddHandle(1,1,0.5,'TotalAngle,MinMax:0;360').Format.Color:=clAqua;
    AddHandle(-0.5,1,0,'Radius.X,MinMax:0;100').Format.Color:=clFuchsia;
    AddHandle(-1,1,0.5,'Radius.Y,MinMax:0;100').Format.Color:=clGreen;
  end;
end;

procedure TTorusBlock.Draw;
var
  tmpXRadius,
  tmpYRadius,
  tmpInnX,
  tmpInnY : Double;
  CanCull : Boolean;
  SideAngles : Array of TPointSinCos;

  procedure CalcRadiusData;
  var SideStep : Double;
      t : Integer;
  begin
    tmpInnX:=1-(Radius.X*0.01);
    tmpXRadius:=1-tmpInnX;

    tmpInnY:=1-(Radius.Y*0.01);
    tmpYRadius:=1-tmpInnY;

    SetLength(SideAngles,Sides);
    SideStep:=360 / (Sides-1);

    for t:=0 to Sides-1 do
    with SideAngles[t] do
         SinCos((t*SideStep)*TeePiStep,X,Y);
  end;

  procedure DrawSide(var AList:Integer; AFormat:TBlockFormat;
                     const Angle:Double; Inverted:Boolean);
  var spsi,
      cpsi : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};

    procedure DrawSideTriangle(Index:Integer);
    begin
      with SideAngles[Index] do
           glVertex3f( cpsi * ( tmpXRadius + Y * tmpInnX),
                       spsi * ( tmpYRadius + Y * tmpInnY),
                       X );
    end;

  var t : Integer;
  begin
    if Assigned(AFormat) then
    begin
      Format.Finish;
      AFormat.Start;
    end;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if AList=0 then
    begin
      if Length(SideAngles)=0 then
         CalcRadiusData;

      AList:=CreateNewList;

      glBegin(GL_TRIANGLE_FAN);

      SinCos(Angle*TeePiStep, spsi, cpsi);

      glVertex3f(cpsi*(tmpXRadius + tmpInnX), spsi*(tmpYRadius + tmpInnY), 0);

      if Inverted then
      for t:=Sides-1 downto 0 do
          DrawSideTriangle(t)
      else
      for t:=0 to Sides-1 do
          DrawSideTriangle(t);

      glEnd;

      glEndList;
    end
    else
      glCallList(AList);

    if CanCull then
       glDisable(GL_CULL_FACE);

    if Assigned(AFormat) then
    begin
      AFormat.Finish;
      Format.Start;
    end;
  end;

var
  tmpTotalAngle : Double;

  procedure DoList;
  var
    vertex,
    text,
    normal : TPoint3DArray;

    procedure CalcData;
    var dpsi : Double;
        tmpInd,
        tmpIndex,
        t,
        tt   : Integer;
        spsi,
        cpsi : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    begin
      if Length(SideAngles)=0 then
         CalcRadiusData;

      SetLength(vertex, 3 * (Sides+1) * (Rings+1));
      SetLength(normal, Length(vertex));
      SetLength(text, Length(vertex));

      dpsi:=tmpTotalAngle / (Rings-1);

      for tt:=0 to Rings-1 do
      begin
        SinCos((StartAngle + (tt*dpsi))*TeePiStep, spsi, cpsi);

        tmpInd:=3*tt*Sides;

        for t:=0 to Sides-1 do
        with SideAngles[t] do
        begin
          tmpIndex:=tmpInd + 3 * t;

          vertex[tmpIndex].X:= cpsi * (tmpXRadius + (Y * tmpInnX));
          vertex[tmpIndex].Y:= spsi * (tmpYRadius + (Y * tmpInnY));
          vertex[tmpIndex].Z:= X;

          normal[tmpIndex].X:= cpsi * Y;
          normal[tmpIndex].Y:= spsi * Y;
          normal[tmpIndex].Z:= X;

          text[tmpIndex].X:=Format.Texture.ITextureSize.X*(1+normal[tmpIndex].X)*0.5;
          text[tmpIndex].Y:=Format.Texture.ITextureSize.Y*(1+normal[tmpIndex].Y)*0.5;
          text[tmpIndex].Z:=normal[tmpIndex].Z;
        end;
      end;
    end;

    Procedure DrawAll;
    var Index,
        t,
        tt,
        tmp : Integer;
    begin
      glBegin(GL_QUADS);

      for t:=0 to Sides-2 do
      begin
        for tt:=0 to Rings-2 do
        begin
          Index:=3 * ( tt*Sides + t );

          glNormal3fv( @normal[ Index + 3 ]);
          glTexCoord3fv( @text[ Index + 3 ]);
          glVertex3fv( @vertex[ Index + 3 ]);

          glNormal3fv( @normal[ Index ] );
          glTexCoord3fv( @text[ Index ] );
          glVertex3fv( @vertex[ Index ] );

          tmp:=Index + 3 * Sides;

          glNormal3fv( @normal[ tmp ]);
          glTexCoord3fv( @text[ tmp ]);
          glVertex3fv( @vertex[ tmp ]);

          Inc(tmp,3);

          glNormal3fv( @normal[ tmp ]);
          glTexCoord3fv( @text[ tmp ]);
          glVertex3fv( @vertex[ tmp ]);
        end;
      end;

      glEnd;
    end;

  begin
    if IList=0 then
    begin
      IList:=CreateNewList;

      if Length(vertex)=0 then
         CalcData;

      DrawAll;

      glEndList;

      text:=nil;
      vertex:=nil;
      normal:=nil;
    end
    else
      glCallList(IList);
  end;

var OldStyle : TTeeCanvasSurfaceStyle;
    Draw1 : Boolean;
    Draw2 : Boolean;
begin
  if TotalAngle=0 then
     tmpTotalAngle:=360
  else
     tmpTotalAngle:=TotalAngle;

  Draw1:=(tmpTotalAngle<>360) and ((not Assigned(FBrush1)) or FBrush1.Solid);
  Draw2:=(tmpTotalAngle<>360) and ((not Assigned(FBrush2)) or FBrush2.Solid);

  CanCull:=(not ShouldDrawInterior) and ((tmpTotalAngle=360) or (Draw1 and Draw2));

  if Format.Solid then
  begin
    if CanCull then
       glEnable(GL_CULL_FACE);

    DoList;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if Draw1 then
     DrawSide(IListSide1,FBrush1,StartAngle,False);

  if Draw2 then
     DrawSide(IListSide2,FBrush2,StartAngle+tmpTotalAngle,True);

  if Format.PreparePen then
  begin
{$IFDEF BLOCKS}
    OldStyle:=ICanvas.getDrawStyle;
    ICanvas.setDrawStyle(tcsWire);
{$ELSE}
    OldStyle:=ICanvas.DrawStyle;
    ICanvas.DrawStyle:=tcsWire;
{$ENDIF}

    DoList;

{$IFDEF BLOCKS}
    ICanvas.setDrawStyle(OldStyle);
{$ELSE}
    ICanvas.DrawStyle:=OldStyle;
{$ENDIF}
    Format.FinishPen;
  end;

  SideAngles:=nil;
end;

function TTorusBlock.GetEditor:String;
begin
  result:='TTorusEditor';
end;

procedure TTorusBlock.SetRadius(const Value: TPointXYFloat);
begin
  FRadius.Assign(Value);
end;

procedure TTorusBlock.RadiusChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TTorusBlock.SetRings(const Value: Integer);
begin
  if FRings<>Value then
  begin
    FRings := Value;
    DeleteLists;
  end;
end;

procedure TTorusBlock.SetSides(const Value: Integer);
begin
  if FSides<>Value then
  begin
    FSides := Value;
    DeleteLists;
  end;
end;

procedure TTorusBlock.SetStartAngle(const Value: Double);
begin
  FStartAngle := Value;
  DeleteLists;
end;

procedure TTorusBlock.SetTotalAngle(const Value: Double);
begin
  FTotalAngle := Value;
  DeleteLists;
end;

var
  InternalLock: TRTLCriticalSection;

class function TBlocks.LoadURLStream(const URL:String; out ErrorSt:String):TStream;

  procedure RaiseError(AError:Integer);
  begin
    FreeAndNil(result);

    ErrorSt:='Web URL address not found: '+URL;

    if AError<>0 then
       ErrorSt:=ErrorSt+#13+'Error: '+TeeURLErrorMessage(AError);
  end;

var tmp : Integer;
begin
  ErrorSt:='';

  result:=TMemoryStream.Create;

  EnterCriticalSection(InternalLock);
  try
    tmp:=TeeHtml.GraphicDownload(URL,result);
  finally
    LeaveCriticalSection(InternalLock);
  end;

  if (tmp=0) and (result.Size>0) then
  begin
    result.Position:=0;

    if result.Size>100 then
       if StringContains(TeeMsg_CannotFind,String(TMemoryStream(result).Memory))>0 then
          RaiseError(0);
  end
  else
    RaiseError(tmp);
end;

procedure TBlocks.LoadFromURL(const URL:String);
var tmpStream : TStream;
    tmpError  : String;
begin
  FCurrentSource:=Copy(URL,1,LastDelimiter('/',URL));

  if Copy(FCurrentSource,Length(FCurrentSource),1)='/' then
     System.Delete(FCurrentSource,Length(FCurrentSource),1);

  tmpError:='';

  tmpStream:=LoadURLStream(URL,tmpError);

  if Assigned(tmpStream) then
  begin
    try
      LoadFromStream(tmpStream);
    finally
      tmpStream.Free;
    end;
  end
  else
    ShowMessage(tmpError);
end;

function TBlocks.CheckFileExtension(var FileName:String):String;
begin
  result:=ExtractFileExt(FileName);

  if result='' then
  begin
    result:=TeeMakerExtension;
    FileName:=FileName+result;
  end;
end;

procedure TBlocks.LoadFromFile(const FileName: String);
var FileStream : TFileStream;
    tmpOutput  : TMemoryStream;
    tmpExt     : String;
    tmp        : String;
begin
  tmp:=ParseFileName(TeeMsg_ObjectsLibrary,FileName);
  tmpExt:=CheckFileExtension(tmp);

  FCurrentSource:=RemoveTrailingSlash(ExtractFilePath(ExpandFileName(tmp)));

  FileStream:=TFileStream.Create(tmp,fmOpenRead+fmShareDenyWrite);
  try
    if UpperCase(tmpExt)='.TXT' then
    begin
      tmpOutput:=TMemoryStream.Create;
      try
        ObjectTextToBinary(FileStream,tmpOutput);

        tmpOutput.Position:=0;
        LoadFromStream(tmpOutput);
      finally
        tmpOutput.Free;
      end;
    end
    else
      LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TBlocks.Put(Index:Integer; const Value:TCustomBlock);
begin
  IList.Put(Index,Value);
end;

function TBlocks.Get(Index:Integer):TCustomBlock;
begin
  // (inlined)
  result:=TCustomBlock(IList[Index]);
end;

Procedure TBlocks.SaveToStream(Stream:TStream; Sign:Boolean=False);
var Writer: TWriter;
begin
  if Assigned(FAnimates) then
     FAnimates.PrepareList;

  Writer:=TWriter.Create(Stream, 1024);
  try
    Writer.WriteRootComponent(Self);
  finally
    Writer.Free;
  end;
end;

procedure TBlocks.SaveToFile(const FileName: String);
var FileStream : TFileStream;
    Input      : TMemoryStream;
    tmp        : String;
    tmpExt     : String;
begin
  tmp:=FileName;
  tmpExt:=CheckFileExtension(tmp);

  FileStream:=TFileStream.Create(tmp,fmCreate);
  try
    if UpperCase(tmpExt)='.TXT' then
    begin
      Input:=TMemoryStream.Create;
      try
        SaveToStream(Input,True);
        Input.Position:=0;
        ObjectBinaryToText(Input,FileStream);
      finally
        Input.Free;
      end;
    end
    else
      SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TBlocks.HasEvents:Boolean;
begin
  result:=Assigned(FEvents) and (FEvents.Count>0);
end;

function TBlocks.GetEvents: TStrings;
begin
  if not Assigned(FEvents) then
     FEvents:=TStringList.Create;

  result:=FEvents;
end;

procedure TBlocks.SetEvents(const Value: TStrings);
begin
  if Assigned(Value) then
     Events.Assign(Value)
  else
     FreeAndNil(FEvents);
end;

function TBlocks.GetProperties: TObjectProperties;
begin
  if not Assigned(FProperties) then
  begin
    FProperties:=TObjectProperties.Create;
    FProperties.IBlocks:=Self;
    FProperties.IOwner:=IObject;
  end;

  result:=FProperties;
end;

procedure TBlocks.SetProperties(const Value: TObjectProperties);
begin
  if Assigned(Value) then
     Properties.Assign(Value)
  else
     FreeAndNil(FProperties);
end;

function TBlocks.CloneBlock(ABlock:TCustomBlock):TCustomBlock;
begin
  result:=ABlock.Clone;
  result.Parent:=Self;
end;

procedure TBlocks.Assign(Source:TPersistent);
var t : Integer;
begin
  if Source is TBlocks then
  with TBlocks(Source) do
  begin
    Self.FHideBorders:=FHideBorders;
    Self.HideTextures:=HideTextures;

    Self.Clear;

    for t:=0 to IList.Count-1 do
        Self.IList.Add(Self.CloneBlock(Block[t]));

    Self.SetAnimates(FAnimates);
    Self.SetEvents(FEvents);
    Self.SetProperties(FProperties);

    Self.FCurrentSource:=FCurrentSource;

  end
  else
    inherited;
end;

function TBlocks.HasProperties:Boolean;
begin
  result:=Assigned(FProperties) and (FProperties.Count>0);
end;

function TBlocks.HasAnimations:Boolean;
var t : Integer;
begin
  result:=False;

  if Assigned(FAnimates) then
  for t:=0 to FAnimates.Count-1 do
  if FAnimates[t].AnimationCount>0 then
  begin
    result:=True;
    break;
  end;
end;

class procedure TBlocks.DoDrawItem(ABlock:TCustomBlock);
var
  tmpNumX,
  tmpNumY,
  tmpNumZ,

  tmpX,
  tmpY,
  tmpZ : Integer;

  Old,
  tmpP : TPoint3DFloat;
begin
  with ABlock do
  if Assigned(FTile) and
     ((FTile.Point.X>1) or (FTile.Point.Y>1) or (FTile.Point.Z>1)) then
  begin
    Old:=Location.Point;

    tmpP:=Size.Point;

    if Assigned(FTile.FOffset) then
    with tmpP do
    begin
      X:=X+FTile.Offset.Point.X;
      Y:=Y+FTile.Offset.Point.Y;
      Z:=Z+FTile.Offset.Point.Z;
    end;

    with FTile.Point do
    begin
      tmpNumX:=Round(X)-1;
      tmpNumY:=Round(Y)-1;
      tmpNumZ:=Round(Z)-1;
    end;

    CheckOnShow;

    Format.Start;

    for tmpZ:=0 to tmpNumZ do
      for tmpX:=0 to tmpNumX do
        for tmpY:=0 to tmpNumY do
        with Location.Point do
        begin
          X:=Old.X+tmpX*tmpP.X;
          Y:=Old.Y+tmpY*tmpP.Y;
          Z:=Old.Z+tmpZ*tmpP.Z;

          StartTransform;
          Draw;
          EndTransform;
        end;

    Location.Point:=Old;

    Format.Finish;
  end
  else
    DrawBlock;
end;

procedure TBlocks.DrawAfter;
var t   : Integer;
    tmp : TCustomBlock;
begin
  for t:=0 to Count-1 do
  begin
    tmp:=Block[t];

    if tmp.ShouldDraw(True) then
    begin
      tmp.ICanvas:=ICanvas;
      tmp.Draw;
    end;
  end;
end;

procedure TBlocks.Draw(Picking:Boolean=False);
var t   : Integer;
    tmp : TCustomBlock;
begin
  // This is done here because it can't be done at FinishedLoading method
  // as it can be called from a secondary Thread (and TTimer does not work).
  if not IAutoPlayDone then
  begin
    if Assigned(FAnimates) then
    for t:=0 to FAnimates.Count-1 do
    with FAnimates[t] do
    if PlayOnLoad then
       Animate.Play;

    IAutoPlayDone:=True;
  end;

  if Picking then
  begin
    for t:=0 to Count-1 do
    begin
      tmp:=Block[t];

      if tmp.ShouldDraw then
      begin
        tmp.ICanvas:=ICanvas;
        tmp.IPicking:=Picking;
        DoDrawItem(tmp);
      end;
    end;
  end
  else
  begin
    // Pre-step: Sort blocks, by Transparency !!

    if Assigned(ITranspBlocks) then
       ITranspBlocks.Clear;

    for t:=0 to Count-1 do
    begin
      tmp:=Block[t];

      if tmp.ShouldDraw then
      begin
        tmp.ICanvas:=ICanvas;
        tmp.IPicking:=Picking;

        with tmp.Format do
        if (Transparency<>0) or FTexture.FTransp then
        begin
          if not Assigned(ITranspBlocks) then
             ITranspBlocks:=TList.Create;

          ITranspBlocks.Add(tmp);
        end
        else
           DoDrawItem(tmp);
      end;
    end;

    if Assigned(ITranspBlocks) then
    begin
     //glDepthMask(GL_FALSE);
     //TGLCanvasAccess(Parent.Canvas).ActivateBlend(True);
     //glBlendFunc(GL_SRC_ALPHA, GL_ONE);

     with ITranspBlocks do
     for t:=0 to Count-1 do
         DoDrawItem(TCustomBlock(List[t]));

     //glDepthMask(GL_TRUE);
    end;
  end;
end;

procedure TBlocks.RemoveTexture(Picture: TPicture);
begin
  RemoveTexture(Picture.Graphic);
end;

procedure TBlocks.RemoveTexture(AGraphic: TGraphic);
begin
  Assert(Assigned(IParent),'Error: IParent not assigned');
  TGLCanvasAccess(IParent.Canvas).RemoveTexture(AGraphic);
end;

{$IFOPT D+}
{.$DEFINE TEEDEBUGPICK}
{$ENDIF}

procedure TBlocks.PreparePicking({$IFDEF TEEPICKSELECT}var Buffer:TPickBuffer;{$ENDIF} X,Y:Integer); 
begin
  {$IFDEF TEEPICKSELECT}
  glSelectBuffer(SizeOf(Buffer),@Buffer);
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'SelectBuffer: '+IntToStr(TGLCanvasAccess(Parent.Canvas).ISavedError));

  glRenderMode(GL_SELECT);
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'RenderMode');

  glInitNames;
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'InitNames');
  {$ELSE}

  //glDrawBuffer(GL_BACK);
  glDisable(GL_DITHER);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_FOG);
  glDisable(GL_COLOR_MATERIAL);

  glClearColor(1,1,1,1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  {$ENDIF}

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;

  with Parent do
  begin
    TGLCanvasAccess(ICanvas).{$IFDEF BLOCKS}iDoProjection{$ELSE}DoProjection{$ENDIF}({$IFDEF TEEDEBUGPICK}False{$ELSE}True{$ENDIF},x,y);
    TGLCanvasAccess(ICanvas).{$IFDEF BLOCKS}iSetModelView{$ELSE}SetModelView{$ENDIF};
  end;

  glDisable(GL_LIGHTING);
  glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE_ARB);

  glShadeModel(GL_FLAT);
  //gluQuadricNormals(TGLCanvasAccess(Parent.Canvas).Quadric,GLU_FLAT);

  with TGLCanvasAccess(Parent.Canvas) do
  begin
    glDisable(GLTextureStyle);

    IOldAntiAlias:=IAntiAlias;
    SetAntiAlias(False);
  end;
end;

function TBlocks.DoPicking:Integer;
begin
  glMatrixMode(GL_PROJECTION);
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'Projection');
  glPopMatrix;

  glFlush;
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'Flush');

  {$IFDEF TEEPICKSELECT}
  result:=glRenderMode(GL_RENDER);
  Assert(TGLCanvas(Parent.Canvas).CheckGLError,'RenderMode');
  {$ELSE}
  result:=0;
  {$ENDIF}

  glEnable(GL_LIGHTING);
  TGLCanvasAccess(Parent.Canvas).SetAntiAlias(IOldAntiAlias);

  glEnable(GL_COLOR_MATERIAL);

  if TGLCanvas(Parent.Canvas).ShadeQuality then
     glShadeModel(GL_SMOOTH);
end;

function TBlocks.ClickedBlock(ABlock:TCustomBlock; X,Y:Integer):TCustomBlock;
begin
  result:=InternalClicked(X,Y,ABlock,True,True);
end;

function TBlocks.ClickedBlock(X,Y:Integer; IncludeObjects:Boolean=False;
                              IncludeSubObjects:Boolean=True):TCustomBlock;
begin
  result:=InternalClicked(X,Y,nil,IncludeObjects,IncludeSubObjects);
end;

function TBlocks.InternalClicked(X,Y:Integer; ABlock:TCustomBlock;
                                 IncludeObjects,IncludeSubObjects:Boolean):TCustomBlock;
{$IFDEF TEEPICKSELECT}
var
  Buffer : TPickBuffer;
  names,
  tmp    : Integer;

  function GetBlock:TCustomBlock;
  var t : Integer;
  begin
    if Assigned(ABlock) then
       result:=ABlock
    else
       result:=Get(Buffer[tmp+3]-1);

    for t:=1 to names-1 do  // -1 ??
        result:=TCustomObjectBlock(result).FItems[Buffer[tmp+3+t]-1];
  end;
{$ENDIF}

  procedure LoadBlock(ABlock:TCustomBlock); forward;

  procedure LoadBlocks(ABlocks:TBlocks);
  var t   : Integer;
      tmp : TCustomBlock;
  begin
    with ABlocks do
    for t:=0 to Count-1 do
    begin
      tmp:=Block[t];

      if tmp.Visible then
      begin
        {$IFDEF TEEPICKSELECT}
        glPushName(t+1);
        {$ELSE}
        tmp.IPicking:=True;
        //tmp.DeleteLists;
        {$ENDIF}

        LoadBlock(tmp);

        {$IFDEF TEEPICKSELECT}
        glPopName;
        {$ELSE}
        tmp.IPicking:=False;
        {$ENDIF}
      end;
    end;
  end;

  procedure LoadBlock(ABlock:TCustomBlock);
  var tmpBlocks : TBlocks;
  begin
    ABlock.ICanvas:=ICanvas;

    if ABlock.IsObjectBlock then
    begin
      if TCustomObjectBlock(ABlock).ItemsReady and
         (IncludeObjects or
          (IncludeSubObjects and TCustomObjectBlock(ABlock).HasContents)) then
      begin
        ABlock.StartTransform;

        tmpBlocks:=TCustomObjectBlock(ABlock).GetItems;
        tmpBlocks.ICanvas:=ICanvas;

        LoadBlocks(tmpBlocks);

        ABlock.EndTransform;
      end
      else
        DoDrawItem(ABlock);
    end
    else
      DoDrawItem(ABlock);
  end;

  procedure DoLoadBlocks;
  var t : Integer;
      tmpParents : TBlockList;
      tmpParent  : TBlocks;
  begin
    if Assigned(IObject) then
    begin
      tmpParents:=TBlockList.Create;
      tmpParents.IKeepBlocks:=True;

      tmpParent:=IObject.FItems;

      if Assigned(tmpParent) then
      while Assigned(tmpParent.IObject) do
      begin
        tmpParent:=tmpParent.IObject.Parent;

        if Assigned(tmpParent.IObject) then
           tmpParents.Insert(0,tmpParent.IObject);
      end;

      glPushMatrix;

      for t:=0 to tmpParents.Count-1 do
          tmpParents[t].DefaultTransform;
    end
    else
      tmpParents:=nil;

    if Assigned(ABlock) then
    begin
      {$IFNDEF TEEPICKSELECT}
      ABlock.IPicking:=True;
      //ABlock.DeleteLists;
      {$ENDIF}

      LoadBlock(ABlock);

      {$IFNDEF TEEPICKSELECT}
      ABlock.IPicking:=False;
      {$ENDIF}
    end
    else
       LoadBlocks(Self);

    if Assigned(tmpParents) then
    begin
      glPopMatrix;
      tmpParents.Free;
    end;
  end;

  {$IFDEF TEEPICKSELECT}
  function GetPickedBlock(Hits:Integer):TCustomBlock;
  var t     : Integer;
      tmpZ,
      z1,z2 : Double;
  begin
    result:=nil;

    if Hits>0 then
    begin
      tmp:=0;
      tmpZ:=0;

      for t:=0 to Hits-1 do
      begin
        names:=Buffer[tmp];

        z1:=(Buffer[tmp+1] and $7FFFFFFF);
        z2:=(Buffer[tmp+2] and $7FFFFFFF);

        if tmp=0 then
        begin
          result:=GetBlock;

          if z1<z2 then
             tmpZ:=z1
          else
             tmpZ:=z2
        end
        else
        if z1<tmpZ then
        begin
          tmpZ:=z1;
          result:=GetBlock;
        end
        else
        if z2<tmpZ then
        begin
          tmpZ:=z2;
          result:=GetBlock;
        end;

        Inc(tmp,3+names);
      end;
    end;
  end;
  {$ENDIF}

  {$IFNDEF TEEPICKSELECT}
  function DisableShaders(ABlocks:TBlocks):TProgramShader;
  begin
    with ABlocks do
    if ShaderEnabled then
    begin
      result:=IProgramShader;
      result.Enabled:=False;
    end
    else
    if Assigned(IObject) then
       result:=DisableShaders(IObject.IBlocks)
    else
       result:=nil;
  end;
  {$ENDIF}

{$IFNDEF TEEPICKSELECT}
var tmpColor : Cardinal;
    viewport : THomogeneousIntVector;

    {$IFDEF TEEDEBUGPICK}
    tmpBitmap : TBitmap;
    {$ENDIF}

{$ENDIF}
    OldShader   : TProgramShader;
    OldTextures : Boolean;
begin
  if Assigned(ICanvas) {$IFNDEF BLOCKS}and Assigned(ICanvas.View3DOptions){$ENDIF} then
  begin
    OldShader:=DisableShaders(Self);

    OldTextures:=FHideTextures;
    FHideTextures:=True;

    PreparePicking({$IFDEF TEEPICKSELECT}Buffer,{$ENDIF}X,Y);

    if Assigned(IFloor) and IFloor.Visible then
    begin
      {$IFNDEF TEEPICKSELECT}
      IFloor.IPicking:=True;
      {$ENDIF}

      DoDrawItem(IFloor);

      {$IFNDEF TEEPICKSELECT}
      IFloor.IPicking:=False;
      {$ENDIF}
    end;

    DoLoadBlocks;

    {$IFDEF TEEPICKSELECT}
    result:=GetPickedBlock(DoPicking);
    {$ELSE}
    DoPicking;

    glGetIntegerv(GL_VIEWPORT, @viewport);
    Assert(ICanvas.CheckGLError,'Get Viewport');

    if (Y<2) or (X<viewport[0]) or (X>=viewport[2]) or (Y<=viewport[1]) or (Y>=viewport[3]) then
       result:=nil
    else
    begin
      glReadBuffer(GL_BACK);
      glReadPixels(Round(X), Round(viewport[3]-Y-1), 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @tmpColor);

      Assert(TGLCanvas(Parent.Canvas).CheckGLError,
         'ReadPixels '+IntToStr(TGLCanvasAccess(Parent.Canvas).ISavedError));

      if tmpColor=$FFFFFFFF then
         result:=nil
      else
      begin
        {$IFDEF TEEDEBUGPICK}
        tmpBitmap:=TBitmap.Create;
        ICanvas.BufferToBitmap(tmpBitmap,ICanvas.Bounds);

        result:=TCustomBlock(tmpColor);

        try
          Self.IParent.Cursor:=result.Cursor;
        except
          on Exception do
             tmpBitmap.SaveToFile(GetLibraryPath+'\pick.bmp');
        end;

        tmpBitmap.Free;
        {$ELSE}
        result:=TCustomBlock(tmpColor);
        {$ENDIF}
      end;
      {$ENDIF}
    end;

    FHideTextures:=OldTextures;

    // Re-Enable shaders
    if Assigned(OldShader) then
       OldShader.Enabled:=True;
  end
  else
    result:=nil;
end;

procedure TBlocks.FinishedLoading;
begin
  if Assigned(FAnimates) then
     FAnimates.FinishedLoading(FCurrentSource);

  if Assigned(FDrawBlocks) and (FDrawBlocks<>Self) then
     FDrawBlocks.FinishedLoading;

  if Assigned(FOnLoaded) then
     FOnLoaded(Self);
end;

procedure TBlocks.InitLights; 
var t : Integer;
    params : Array[0..4] of GLInt;
begin
  if IMaxLights=-1 then
  begin
    IMaxLights:=0;

    glGetIntegerv(GL_MAX_LIGHTS, @params);
    Assert(TGLCanvas(Parent.Canvas).CheckGLError,
       'GetMaxLights '+IntToStr(TGLCanvasAccess(Parent.Canvas).ISavedError));

    IMaxLights:=params[0];
  end;

  ILightNum:=3;

  for t:=ILightNum to IMaxLights-1 do
      glDisable(GL_LIGHT0+t);

  TGLCanvasAccess(IParent.Canvas).ApplyPolygonOffset(not HideBorders);
end;

function TBlocks.IsAnimatesStored:Boolean;
begin
  result:=Assigned(FAnimates) and (FAnimates.Count>0);
end;

function TBlocks.RecursiveContains(const Value:TCustomBlock):Boolean;
var t : Integer;
begin
  result:=False;

  for t:=0 to Count-1 do
  if Block[t]=Value then
  begin
    result:=True;
    break;
  end
  else
  if Block[t] is TCustomObjectBlock then
  begin
    if TCustomObjectBlock(Block[t]).Items.RecursiveContains(Value) then
    begin
      result:=True;
      break;
    end;
  end;
end;

procedure TBlocks.SetAnimates(const Value: TAnimates);
begin
  if Assigned(Value) then
     Animates.Assign(Value)
  else
     FreeAndNil(FAnimates);
end;

// For Delphi 2005, and C++ Builder (hpp generation)
function TBlocks.GetterLibraryPath:String;
begin
  result:=GetLibraryPath;
end;

var
  ILibraryPath : String='';

class function TBlocks.GetLibraryPath:String;
begin
  if ILibraryPath='' then
     ILibraryPath:=TeeMakerReadRegistry('',TeeMakerLibRegistry,'Makers');

  result:=ILibraryPath;
end;

procedure TBlocks.SetHideBorders(const Value:Boolean);
begin
  if FHideBorders<>Value then
  begin
    FHideBorders:=Value;
    DeleteLists;
  end;
end;

procedure TBlocks.SetHideTextures(const Value:Boolean);
begin
  if FHideTextures<>Value then
  begin
    FHideTextures:=Value;
    Repaint;
  end;
end;

procedure TBlocks.SetLibraryPath(const Value:String);
begin
  ILibraryPath:=Value;
  TeeMakerWriteRegistry('',TeeMakerLibRegistry,Value);
end;

{ TTile }

Destructor TTile.Destroy;
begin
  FreeAndNil(FOffset);
  inherited;
end;

function TTile.GetOffset:TPointXYZFloat;
begin
  if not Assigned(FOffset) then
     FOffset:=TPointXYZFloat.Create(IOwner,0);

  result:=FOffset;
end;

procedure TTile.SetOffset(const Value: TPointXYZFloat);
begin
  if Assigned(Value) then
     Offset.Assign(Value)
  else
     FreeAndNil(FOffset);

  (IOwner as TVisualBlock).Repaint;
end;

procedure TTile.Assign(Source:TPersistent);
begin
  if Source is TTile then
  begin
    Offset:=TTile(Source).FOffset;
  end;

  inherited;
end;

{ TRotationXYZ }

Destructor TRotationXYZ.Destroy;
begin
  FreeAndNil(FCenter);
  inherited;
end;

function TRotationXYZ.GetCenter:TPointXYZFloat;
begin
  if not Assigned(FCenter) then
     FCenter:=TPointXYZFloat.Create(IOwner,0);

  result:=FCenter;
end;

function TRotationXYZ.GetPoint:TPoint3DFloat;
begin
  if Assigned(FOnGet) then
     result:=FOnGet
  else
     result:=Point;
end;

procedure TRotationXYZ.SetCenter(const Value: TPointXYZFloat);
begin
  if Assigned(Value) then
     Center.Assign(Value)
  else
     FreeAndNil(FCenter);

  (IOwner as TVisualBlock).Repaint;
end;

procedure TRotationXYZ.SetFaceToViewer(const Value: Boolean);
begin
  if FFaceToViewer<>Value then
  begin
    FFaceToViewer:=Value;
    (IOwner as TVisualBlock).Repaint;
  end;
end;

procedure TRotationXYZ.Assign(Source:TPersistent);
begin
  if Source is TRotationXYZ then
  begin
    Center:=TRotationXYZ(Source).FCenter;
    FFaceToViewer:=FFaceToViewer;
  end;

  inherited;
end;

{ TBlockTexture }

Constructor TBlockTexture.Create;
begin
  inherited;
  ITextureSize.X:=1;
  ITextureSize.Y:=1;
end;

Destructor TBlockTexture.Destroy;
begin
  IAlphaBitmap.Free;

  TryFreePicture;

  FScale.Free;
  FTranslate.Free;

  inherited;
end;

procedure TBlockTexture.Assign(Source:TPersistent);
begin
  if Source is TBlockTexture then
  with TBlockTexture(Source) do
  begin
    Self.FAlpha:=FAlpha;
    Self.FAlphaInvert:=FAlphaInvert;

    if Self.FPictureLink<>FPictureLink then
    begin
      TryFreePicture;

      Self.FPictureLink:=FPictureLink;
    end;

    if FPictureLink='' then
       Self.SetPicture(FPicture);

    Self.Scale:=FScale;
    Self.Translate:=FTranslate;
    Self.FTransp:=FTransp;

    if FAlpha then
       FreeAndNil(Self.IAlphaBitmap);
  end
  else
    inherited;
end;

procedure TBlockTexture.Coord(const X,Y:Single);
begin
  glTexCoord2f(ITextureSize.X*X,ITextureSize.Y*Y);
end;

procedure TBlockTexture.TryFreePicture; 
var tmp : TBlockFormat;
begin
  if Assigned(FPicture) and (not ISharedPicture) then
  begin
    if FPictureLink='' then
       tmp:=nil
    else
       tmp:=OtherHasSamePictureLink(True);

    if Assigned(tmp) and (@tmp.FTexture.FPicture.Graphic=@FPicture.Graphic) then
       tmp.FTexture.ISharedPicture:=False
    else
    begin
      if Assigned(IFormat.IOwner) and Assigned(IFormat.IOwner.Parent) then
      with IFormat.IOwner.Parent do
      if Assigned(IParent) then
         RemoveTexture(FPicture);

      if Assigned(FPicture.ILoadThread) then
      begin
        FPicture.ILoadThread.Terminate;

        TerminateThread(FPicture.ILoadThread.Handle,1);

        FreeAndNil(FPicture.ILoadThread);
      end;

      FPicture.Free;
    end;
  end;

  FPicture:=nil;
  ISharedPicture:=False;
end;

function TBlockTexture.OtherHasSamePictureLink(SharedOnly:Boolean):TBlockFormat;
var
  tmp : String;

  function OtherBlocksHasSame(ABlocks:TBlocks):TBlockFormat;
  var t : Integer;
      tmpB : TCustomBlock;
  begin
    result:=nil;

    with ABlocks.IList do
    for t:=0 to Count-1 do
    begin
      tmpB:=TCustomBlock(List[t]); // <-- speed opt.  (Block[t])

      if tmpB<>Self.IFormat.IOwner then // <-- correct?
         if tmpB.IsObjectBlock then // <-- speed opt.  (is TCustomObjectBlock then)
         begin
           if Assigned(TCustomObjectBlock(tmpB).FItems) then
           begin
             result:=OtherBlocksHasSame(TCustomObjectBlock(tmpB).FItems);

             if Assigned(result) then
                break;
           end;
         end
         else
         with tmpB.FFormat.FTexture do
         if FPictureLink<>'' then  // <-- speed opt.
            if (SharedOnly and ISharedPicture) or
               ((not SharedOnly) and (not ISharedPicture)) then
              if UpperCase(FPictureLink)=tmp then
              begin
                result:=tmpB.FFormat;
                break;
              end;
    end;
  end;

begin
  tmp:=UpperCase(FPictureLink);

  if Assigned(IFormat.IOwner) and Assigned(IFormat.IOwner.IBlocks) then
     result:=OtherBlocksHasSame(IFormat.IOwner.IBlocks.FDrawBlocks)
  else
     result:=nil;
end;

function TBlockTexture.HasTexture:Boolean;
begin
  result:=Assigned(FPicture) and Assigned(FPicture.Graphic) and
          (not Assigned(FPicture.ILoadThread));
end;

function TBlockTexture.IsPictureStored:Boolean;
begin
  result:=Assigned(FPicture) and (FPictureLink='');
end;

procedure TBlockTexture.SetAlpha(const Value: Boolean);
begin
  FAlpha:=Value;

  if not FAlpha then
     FreeAndNil(IAlphaBitmap);

  Changed(Self);
end;

function TBlockTexture.GetScale:TPointXYZFloat;
begin
  if not Assigned(FScale) then
     FScale:=TPointXYZFloat.Create(IFormat.IOwner,1,Changed);

  result:=FScale;
end;

procedure TBlockTexture.SetScale(const Value: TPointXYZFloat);
begin
  if Assigned(Value) then
     Scale.Assign(Value)
  else
     FreeAndNil(FScale);
end;

function TBlockTexture.GetTranslate:TPointXYZFloat;
begin
  if not Assigned(FTranslate) then
     FTranslate:=TPointXYZFloat.Create(IFormat.IOwner,0,Changed);

  result:=FTranslate;
end;

procedure TBlockTexture.SetTranslate(const Value: TPointXYZFloat);
begin
  if Assigned(Value) then
     Translate.Assign(Value)
  else
     FreeAndNil(FTranslate);
end;

function TBlockTexture.GetPicture: TBlockPicture;

  function GetParentSource:String;
  var tmp : TBlocks;
  begin
    result:='';

    tmp:=IFormat.IOwner.Parent;

    if Assigned(tmp) then
    begin
      while Assigned(tmp) and Assigned(tmp.IObject) do
      begin
        if Assigned(tmp.IObject.FItems) then
        begin
          result:=tmp.IObject.FItems.FCurrentSource;

          if result<>'' then
             Exit;
        end;

        tmp:=tmp.IObject.Parent;
      end;

      if result='' then
         if Assigned(IFormat.IOwner.Parent) then
            result:=IFormat.IOwner.Parent.FDrawBlocks.FCurrentSource;
    end;
  end;

  function SourceOfParent(AParent:TBlocks):String;
  var tmpObj : TCustomObjectBlock;
  begin
    result:='';

    tmpObj:=AParent.IObject;

    while Assigned(tmpObj) do
    if Assigned(tmpObj.Parent) then
    begin
      result:=tmpObj.Parent.FCurrentSource;

      if result<>'' then
         break
      else
         tmpObj:=tmpObj.Parent.IObject;
    end;

    if result='' then
       result:=AParent.FDrawBlocks.FCurrentSource;
  end;

var tmp : TBlockFormat;
begin
  if not Assigned(FPicture) then
  begin
    FPicture:=TBlockPicture.Create;
    FPicture.OnChange:=Changed;
  end;

  if FPicture.Graphic=nil then
     if FPictureLink<>'' then
     if not Assigned(FPicture.ILoadThread) then
     if not FPicture.IBadFile then
     begin
       tmp:=OtherHasSamePictureLink(False);

       if Assigned(tmp) then
       begin
         FPicture.Free;
         ISharedPicture:=True;  // <-- before assigning FPicture

         FPicture:=tmp.FTexture.Picture;
       end
       else
       begin
         //OutputDebugString(PAnsiChar(PictureLink));

         ISharedPicture:=False;

         if TeeNoThreads then
         begin
           FPicture.TryLoad(GetParentSource,TBlocks.ParseFileName(TeeMsg_TexturesLibrary,PictureLink));
           CheckPicTransp;
         end
         else
         begin
           FPicture.ILoadThread:=TLoadPictureThread.Create(True);

           with TLoadPictureThread(FPicture.ILoadThread) do
           begin
             Priority:=tpHigher;
             FreeOnTerminate:=True;

             Format:=Self.IFormat;

             if Assigned(Format.IOwner.Parent) then
             begin
               TeePanel:=Format.IOwner.Parent.IParent;
               ParentSource:=SourceOfParent(Format.IOwner.Parent);
             end;

             Source:=TBlocks.ParseFileName(TeeMsg_TexturesLibrary,PictureLink);
             OnTerminate:=ThreadTerminated;

             {$IFDEF D14}
             Start;
             {$ELSE}
             Resume;
             {$ENDIF}
           end;
         end;
       end;
     end;

  result:=FPicture;
end;

procedure TBlockTexture.SetPicture(const Value: TBlockPicture);
begin
  AssignPicture(Value);
end;

procedure TBlockTexture.AssignPicture(const Value: TPicture);
begin
  TryFreePicture;

  FPictureLink:='';

  if Assigned(Value) then
  begin
    Picture.Assign(Value);

    ISharedPicture:=False;

    if Assigned(IAlphaBitmap) then
       IAlphaBitmap.Assign(FPicture.Graphic);
  end;

  Changed(Self);
end;

procedure TBlockTexture.Changed;
begin
  if Assigned(IFormat.IOwner) then
  begin
    if Assigned(IFormat.IOwner.ICanvas) then
    with TGLCanvasAccess(IFormat.IOwner.ICanvas) do
       if ICanNPOTS then
          IFormat.IOwner.DeleteLists;

    IFormat.IOwner.Repaint;
  end;

  if Assigned(IChanged) then
     IChanged(Self);
end;

procedure TBlockTexture.SetPictureLink(const Value: String);
begin
  if FPictureLink<>Value then
  begin
    TryFreePicture;

    FPictureLink:=Value; // <-- after TryFreePicture

    TBlocks.CheckLibraryPath(TeeMsg_TexturesLibrary,FPictureLink);

    FreeAndNil(IAlphaBitmap);
    Changed(Self);
  end;
end;

procedure TBlockTexture.CheckPicTransp;
begin
  if Assigned(FPicture) then
  with FPicture do
    if not IBadFile then
       if Assigned(Graphic) then
       begin
         if FAlpha then
         begin
           if not Assigned(IAlphaBitmap) then
           begin
             IAlphaBitmap:=TAlphaBitmap.Create;
             IAlphaBitmap.Assign(Graphic);

             if FAlphaInvert then
                TInvertFilter.ApplyTo(TAlphaBitmap(IAlphaBitmap));

             Graphic:=IAlphaBitmap;
           end;
         end;

         if Graphic.Transparent<>Self.FTransp then
         begin
           Graphic.Transparent:=Self.FTransp;

           if Assigned(IFormat.IOwner.Parent) then
              IFormat.IOwner.Parent.DrawBlocks.RemoveTexture(FPicture);
         end;
       end;
end;

procedure TBlockTexture.SetTransp(const Value: Boolean);
begin
  FTransp:=Value;
  CheckPicTransp;
  Changed(Self);
end;

function TBlockTexture.IsRotationStored: Boolean;
begin
  result:=FRotation<>0;
end;

procedure TBlockTexture.SetRotation(const Value: Double);
begin
  FRotation:=Value;
  IFormat.IOwner.Repaint;
end;

procedure TBlockTexture.SetAutomatic(Value: Boolean);
const
  Mode=GL_OBJECT_LINEAR;
//  Mode=GL_EYE_LINEAR;
//  Mode=GL_SPHERE_MAP;
begin
  if Value then
  begin
    glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, Mode);
    glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, Mode);

//    glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, Mode);
//    glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, Mode);

//    glTexGendv(GL_T, GL_OBJECT_PLANE, @FScale.Point);

    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
//    glEnable(GL_TEXTURE_GEN_R);
//    glEnable(GL_TEXTURE_GEN_Q);
  end
  else
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
//    glDisable(GL_TEXTURE_GEN_R);
//    glDisable(GL_TEXTURE_GEN_Q);
  end;
end;

procedure TBlockTexture.SetEmbeddedPicture;
var tmp : TGraphic;
begin
  FPictureLink:='';

  if ISharedPicture then
  begin
    tmp:=FPicture.Graphic;

    FPicture:=TBlockPicture.Create;
    FPicture.Graphic:=tmp;

    ISharedPicture:=False;
  end;
end;

procedure TBlockTexture.SetAlphaInvert(const Value: Boolean);
begin
  FAlphaInvert:=Value;
  FreeAndNil(IAlphaBitmap);
  TryFreePicture;

  if FPictureLink<>'' then
     GetPicture;

  Changed(Self);
end;

{ TBlockFormat }

Constructor TBlockFormat.Create(AOwner: TCustomBlock);
begin
  inherited Create;

  IOwner:=AOwner;

  FColor:=clDefault;
  FSolid:=True;

  FTexture:=TBlockTexture.Create;
  FTexture.IFormat:=Self;

  FParentTexture:=True;
end;

Destructor TBlockFormat.Destroy;
begin
  FreeAndNil(FTexture);
  FBorder.Free;
  inherited;
end;

function TBlockFormat.GetBorder:TBlockBorder;
begin
  if not Assigned(FBorder) then
  begin
    FBorder:=TBlockBorder.Create;
    FBorder.IOwner:=Self;
    FBorder.IChanged:=IChanged;
  end;

  result:=FBorder;
end;

procedure TBlockFormat.SetDirectColor(const AColor:TColor); 
var tmp : Integer;
begin
  if IOwner.IPicking then
     tmp:=Integer(IOwner)
  else
     tmp:=(IOwner.IBlocks.FDrawBlocks.ITransparency shl 24)+AColor;

  glColor4ubv(@tmp);
end;

function TBlockFormat.GetRealColor:TColor;
begin
  if IOwner.IPicking then
     result:=TColor(Addr(IOwner))
  else
  begin
    result:=FColor;

    if result=clDefault then
    begin
      if IOwner.FFormat<>Self then
         result:=IOwner.FFormat.FColor;

      if result=clDefault then
         if Assigned(IOwner.Parent) and Assigned(IOwner.Parent.IObject) then
            result:=IOwner.Parent.IObject.FFormat.GetRealColor;

      // Last chance, return White color:
      if result=clDefault then
         result:=clWhite;
    end;
  end;
end;

procedure TBlockFormat.PrepareColor; 
begin
  PrepareColor(GetRealColor);
end;

procedure TBlockFormat.PrepareColor(const AColor:TColor); 
begin
  ICurrentColor:=(IOwner.IBlocks.FDrawBlocks.ITransparency shl 24)+AColor;
  glColor4ubv(@ICurrentColor);
end;

var
  ILineStippleEnabled:Boolean=False;

function TBlockFormat.InternalPreparePen:Boolean;

  procedure DoSetBorder;
  begin
    with FBorder do
    begin
      if Style=psSolid then
      begin
        if ILineStippleEnabled then
        begin
          glDisable(GL_LINE_STIPPLE);
          ILineStippleEnabled:=False;
        end;
      end
      else
      begin
        if not ILineStippleEnabled then
        begin
          glEnable(GL_LINE_STIPPLE);
          ILineStippleEnabled:=True;
        end;

        case Style of
          psSolid   : glLineStipple(1,$FFFF);
          psDot     : glLineStipple(1,$5555);
          psDash    : glLineStipple(1,$00FF);
          psDashDot : glLineStipple(1,$55FF);
        else
          glLineStipple(1,$1C47);
        end;
      end;

      glLineWidth(Width);

      if Transparency<>0 then
         InitTransparency(Min(255,Self.Transparency+Transparency));

      glDisable(TGLCanvasAccess(IOwner.IOwner.ICanvas).GLTextureStyle);
      SetDirectColor(Color);
    end;

{$IFNDEF BLOCKS}
    Assert(IOwner.ICanvas.CheckGLError,'DoSetBorder: '+TeeStr(TGLCanvasAccess(IOwner.ICanvas).ISavedError));
{$ENDIF}
  end;

begin
  result:=True;

  if Assigned(FBorder) then
  begin
    if FBorder.Visible then
       DoSetBorder
    else
       result:=False;
  end
  else
  begin // Default black pen:

    if ILineStippleEnabled then
    begin
      glDisable(GL_LINE_STIPPLE);
      ILineStippleEnabled:=False;
    end;

    glLineWidth(1);

    glDisable(TGLCanvasAccess(IOwner.ICanvas).GLTextureStyle);
    SetDirectColor(clBlack);
  end;
end;

function TBlockFormat.PreparePen:Boolean;
begin
  result:=(not IOwner.IPicking) and Assigned(IOwner.Parent);

  if result then
  begin
    with IOwner.Parent.FDrawBlocks do
         result:=(not Shadows.Visible) and (not HideBorders);

    if result then
       result:=InternalPreparePen;
  end;
end;

procedure TBlockFormat.DoSetBrush;

  procedure SetTheBrushGraphic(const AGraphic:TGraphic; const AColor:TColor=clNone);
  var tmp : GLUInt;
      {$IFOPT C+}
      tmpError : Boolean;
      {$ENDIF}
  begin
    if AColor=clNone then
       PrepareColor
    else
       PrepareColor(AColor);

    if AGraphic.Width<>0 then
    begin
      if TGLCanvasAccess(IOwner.ICanvas).{$IFDEF BLOCKS}iFindTexture{$ELSE}FindTexture{$ENDIF}(AGraphic,tmp) then
      begin
        glEnable(TGLCanvasAccess(IOwner.ICanvas).GLTextureStyle);

        {$IFOPT C+}
        tmpError:=TGLCanvasAccess(IOwner.ICanvas).CheckGLError;
        Assert(tmpError,'Enable Texture 2D: '+TeeStr(TGLCanvasAccess(IOwner.ICanvas).ISavedError));
        {$ENDIF}

        if Texture.FAlpha then
        begin
          glGetBooleanv(GL_ARB_MULTISAMPLE,@IWasMultiSample);
          glDisable(GL_ARB_MULTISAMPLE);
        end;

        ITextureEnabled:=True;

        glActiveTexture(GL_TEXTURE0);

        {$IFNDEF LINUX}
        glBindTexture(TGLCanvasAccess(IOwner.ICanvas).GLTextureStyle, tmp);
        Assert(TGLCanvasAccess(IOwner.ICanvas).CheckGLError,'BindTexture: '+TeeStr(TGLCanvasAccess(IOwner.ICanvas).ISavedError));
        {$ENDIF}

        with IOwner.IBlocks.DrawBlocks do
        if ShaderEnabled then
        with IProgramShader do
        begin
          SetUniform('MakerTexture',0);
          SetUniform('MakerTextured',1);
        end;

        if TGLCanvasAccess(IOwner.ICanvas).ICanNPOTS then
        with AGraphic do
        begin
          Texture.ITextureSize.X:=Width-1;
          Texture.ITextureSize.Y:=Height-1;
        end
        else
        begin
          Texture.ITextureSize.X:=1;
          Texture.ITextureSize.Y:=1;
        end;
      end;

      with Texture do
      //if Assigned(FScale) or Assigned(FTranslate) or (FRotation<>0) then
      begin
        glMatrixMode(GL_TEXTURE);
        glPushMatrix;

        if Assigned(FTranslate) then
        with FTranslate.Point do
             glTranslatef(ITextureSize.X*X,ITextureSize.Y*Z,-Y);

        if Assigned(FScale) then
        with FScale.Point do
             glScalef(X,-Y,Z)
        else
          glScalef(1,-1,1);

        if FRotation<>0 then
        begin
          glTranslatef(ITextureSize.X*0.5,ITextureSize.Y*0.5,0);
          glRotatef(FRotation,0,0,1);
          glTranslatef(-ITextureSize.X*0.5,-ITextureSize.Y*0.5,0);
        end;

        glMatrixMode(GL_MODELVIEW);

        IRotatedTexture:=True;
      end;
    end;
  end;

  function GetParentTexture(const ABlock:TCustomBlock):TBlockPicture;
  begin
    result:=nil;

    if Assigned(ABlock) then
    with ABlock do
    begin
      result:=Format.FTexture.FPicture;

      if (not Assigned(result)) or (not Assigned(result.Graphic)) then
         if Format.FTexture.PictureLink<>'' then
            result:=FTexture.GetPicture
         else
            result:=GetParentTexture(Parent.IObject);
    end
  end;

var tmp : TBlockPicture;
begin
  glDisable(TGLCanvasAccess(IOwner.ICanvas).GLTextureStyle);
  ITextureEnabled:=False;

  with IOwner.IBlocks.DrawBlocks do
  if ShaderEnabled then
     IProgramShader.SetUniform('MakerTextured',0);

  if FSolid then
  begin
    if IOwner.Parent.FDrawBlocks.Shadows.Visible then
       PrepareColor(IOwner.Parent.FDrawBlocks.Shadows.Color)
    else
    begin
      if Assigned(FTexture.FPicture) and Assigned(FTexture.FPicture.ILoadThread) then
         PrepareColor
      else
      begin
        if not IOwner.Parent.FDrawBlocks.HideTextures then
        begin
          tmp:=FTexture.FPicture;

          if Assigned(tmp) and (FTexture.PictureLink='$(DEPTHBUFFER)') then
             tmp.Graphic:=nil;

          if (not Assigned(tmp)) or (not Assigned(tmp.Graphic)) then
          begin
            if FTexture.FPictureLink<>'' then
               tmp:=FTexture.GetPicture
            else
            if FParentTexture then
               tmp:=GetParentTexture(IOwner);
          end;

          if Assigned(tmp) and Assigned(tmp.Graphic) and
             (not Assigned(tmp.ILoadThread)) then
             SetTheBrushGraphic(tmp.Graphic)
          else
             PrepareColor;
        end
        else
          PrepareColor;
      end;
    end;
  end;
end;

procedure TBlockFormat.PolylineList(out AList:Integer; const P:TPoint3DArray);
var tmpL : Integer;
    t    : Integer;
begin
  AList:=glGenLists(1);
  glNewList(AList,GL_COMPILE);

  glBegin(GL_LINE_LOOP);

  tmpL:=Length(P)-1;

  for t:=0 to tmpL do
  with P[t] do
       glVertex3f(X,-Y,Z);

  glEnd;

  glEndList;
end;

procedure TBlockFormat.ConcavePolygon(var AList:Integer; {$IFDEF D6}const{$ENDIF} P:Array of TPoint3DFloat;
                                      Invert:Boolean=False);
                                      
begin
  AList:=IOwner.CreateNewList;
  IOwner.ICanvas.Polygon3DConcave(P,Invert);
  glEndList;
end;

procedure TBlockFormat.ConvexPolygon(var AList:Integer; const Points: Array of TPoint3DFloat;
                                     Invert:Boolean=False);
var
  IRange : TPoint3DFloat;
  tmpMin : TPoint3DFloat;

  procedure AddPoint(const Index:Integer);
  begin
    with Points[Index] do
    begin
      glTexCoord3f((X-tmpMin.X)*IRange.X,(Y-tmpMin.Y)*IRange.Y,(Z-tmpMin.Z)*IRange.Z);
      glVertex3f(X,-Y,Z);
    end;
  end;

var t    : Integer;
    tmpL : Integer;
    tmpP : TPoint3DFloat;
    tmpMax : TPoint3DFloat;
begin
  tmpL:=Length(Points);

  if tmpL>0 then  // <-- pending to remove?
  begin
    if Solid then  // <-- pending to remove?
    begin
      AList:=IOwner.CreateNewList;

      TGLCanvasAccess(IOwner.ICanvas).{$IFDEF BLOCKS}iCalcMinMax{$ELSE}CalcMinMax{$ENDIF}(Points,tmpMin,tmpMax);

      if tmpMax.X=tmpMin.X then
         IRange.X:=0
      else
         IRange.X:=1/(tmpMax.X-tmpMin.X);

      if tmpMax.Y=tmpMin.Y then
         IRange.Y:=0
      else
         IRange.Y:=1/(tmpMax.Y-tmpMin.Y);

      if tmpMax.Z=tmpMin.Z then
         IRange.Z:=0
      else
         IRange.Z:=1/(tmpMax.Z-tmpMin.Z);

      glBegin(GL_POLYGON);

      if tmpL>2 then
      begin
        tmpP:=CalculateNormal(Points[0],Points[1],Points[2]);

        with tmpP do
        if Invert then // <-- ?
           glNormal3f(X,Y,Z)
        else
           glNormal3f(X,Y,-Z);
      end;

      if Invert then
      for t:=tmpL-1 downto 0 do
          AddPoint(t)
      else
      for t:=0 to tmpL-1 do
          AddPoint(t);

      glEnd;

      glEndList;
    end;

{$IFNDEF BLOCKS}
    Assert(IOwner.ICanvas.CheckGLError,'ConvexPolygon');
{$ENDIF}
  end;
end;

procedure TBlockFormat.Start;
const
  Inv255=1/255;
var
  tmp : Integer;
begin
  if IOwner.IPicking then
  begin
    tmp:=Integer(IOwner);
    glColor4ubv(@tmp);
  end
  else
  begin
    IWasMultiSample:=False;

    InitTransparency(Transparency);

    DoSetBrush;

    if Shininess<>0 then
    begin
      {$IFDEF BLOCKS}
      IOldShin:=IOwner.ICanvas.getShininess;
      IOwner.ICanvas.setShininess(Shininess*Inv255);
      {$ELSE}
      IOldShin:=IOwner.ICanvas.Shininess;
      IOwner.ICanvas.Shininess:=Shininess*Inv255;
      {$ENDIF}
    end;

    if FBright then
       glDisable(GL_LIGHTING);
  end;
end;

procedure TBlockFormat.CheckBlend;
begin
  TGLCanvasAccess(IOwner.ICanvas).

  {$IFDEF BLOCKS}
  iActivateBlend(IOwner.IBlocks.FDrawBlocks.ITransparency<>255);
  {$ELSE}
  ActivateBlend(IOwner.IBlocks.FDrawBlocks.ITransparency<>255);
  {$ENDIF}
end;

procedure TBlockFormat.Finish;
begin
  if not IOwner.IPicking then
  begin
    FinishTransparency(Transparency);

    if Shininess<>0 then
    {$IFDEF BLOCKS}
      IOwner.ICanvas.setShininess(IOldShin);
    {$ELSE}
      IOwner.ICanvas.Shininess:=IOldShin;
    {$ENDIF}

    if IRotatedTexture then
    begin
      glMatrixMode(GL_TEXTURE);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);

      IRotatedTexture:=False;
    end;

    if FBright then
       glEnable(GL_LIGHTING);

    if IWasMultiSample then
       glEnable(GL_ARB_MULTISAMPLE);

    with IOwner.IBlocks.DrawBlocks do
    if ShaderEnabled then
       IProgramShader.SetUniform('MakerTextured',0);
  end;
end;

procedure TBlockFormat.InitTransparency(const Value:Byte);
var tmp : Integer;
begin
  with IOwner.Parent.FDrawBlocks do
  if Shadows.Visible then
  begin
    if not Shadows.Smooth then
    begin
      tmp:=Min(200,Shadows.Transparency+Value);

      if tmp<>0 then
      begin
        ITransparency:=255-tmp;
        CheckBlend;
      end
    end;
  end
  else
  begin
    tmp:=Value;

    if tmp<>0 then
    begin
      IOwner.IBlocks.FDrawBlocks.ITransparency:=255-tmp;
      CheckBlend;
    end
    else
    if FTexture.FTransp then
    begin
      with TGLCanvasAccess(IOwner.ICanvas) do
           {$IFDEF BLOCKS}iActivateBlend(True){$ELSE}ActivateBlend(True){$ENDIF};

      if Self.FTransparency=0 then
         glColor4ub(255,255,255,255)
      else
         glColor4ub(255,255,255,255-Value);
    end;
  end;
end;

procedure TBlockFormat.FinishTransparency(const Value:Integer);
begin
  with IOwner.Parent.FDrawBlocks do
    if Shadows.Visible then
    begin
      ITransparency:=255;
      CheckBlend;
    end
    else
    if Value<>0 then
    begin
      ITransparency:=255;
      CheckBlend;
    end
    else
    if FTexture.FTransp then
       with TGLCanvasAccess(IOwner.ICanvas) do
            {$IFDEF BLOCKS}iActivateBlend(False){$ELSE}ActivateBlend(False){$ENDIF};
end;

procedure TBlockFormat.SetBorder(const Value:TBlockBorder);
begin
  if Assigned(Value) then
     Border.Assign(Value)
  else
     FreeAndNil(FBorder);

  IOwner.Repaint;
end;

procedure TBlockFormat.SetColor(const Value: TColor);
begin
  if FColor<>Value then
  begin
    FColor:=ColorToRGB(Value);
    IOwner.Repaint;

    if Assigned(OnColorChanged) then
       OnColorChanged(Self);
  end;
end;

procedure TBlockFormat.SetDrawInside(const Value: Boolean);
begin
  FDrawInside:=Value;
  IOwner.Repaint;
end;

procedure TBlockFormat.SetParentTexture(const Value:Boolean);
begin
  if FParentTexture<>Value then
  begin
    FParentTexture:=Value;
    IOwner.Repaint;
  end;
end;

procedure TBlockFormat.SetShininess(const Value: Integer);
begin
  FShininess:=Value;
  IOwner.Repaint;
end;

procedure TBlockFormat.SetBright(const Value: Boolean);
begin
  if FBright<>Value then
  begin
    FBright:=Value;
    IOwner.Repaint;
  end;
end;

procedure TBlockFormat.SetSolid(const Value: Boolean);
begin
  FSolid:=Value;

  if Assigned(IChanged) then
     IChanged(Self);

  IOwner.Repaint;
end;

procedure TBlockFormat.Assign(Source:TPersistent);
begin
  if Source is TBlockFormat then
  with TBlockFormat(Source) do
  begin
    Self.SetBorder(FBorder);
    Self.FBright:=FBright;
    Self.FColor:=FColor;
    Self.FDrawInside:=FDrawInside;
    Self.FTransparency:=Transparency;
    Self.FParentTexture:=FParentTexture;
    Self.SetTexture(FTexture);
    Self.FShininess:=FShininess;
    Self.FSolid:=FSolid;
  end
  else
    inherited;
end;

class function TBlockFormat.ColorToGL(const AColor:TColor):TRGBAlpha;
begin
  result.Red:=Byte(AColor);
  result.Green:=Byte(AColor shr 8);
  result.Blue:=Byte(AColor shr 16);
  result.Alpha:=1;
end;

procedure TBlockFormat.SetTexture(const Value: TBlockTexture);
begin
  FTexture.Assign(Value);
end;

procedure TBlockFormat.SetTransparency(const Value: Byte);
var t : Integer;
begin
  if FTransparency<>Value then
  begin
    FTransparency:=Value;

    if IOwner is TCustomObjectBlock then
    with TCustomObjectBlock(IOwner).Items do
    for t:=0 to Count-1 do
        Block[t].Format.Transparency:=Value;

    IOwner.Repaint;

    if Assigned(OnColorChanged) then
       OnColorChanged(Self);
  end;
end;

procedure TBlockFormat.FinishPen; 
begin
  if not IOwner.IPicking then
  begin
    if ITextureEnabled then
       glEnable(TGLCanvasAccess(IOwner.ICanvas).GLTextureStyle);

    glColor4ubv(@ICurrentColor);

    CheckBlend;
  end;
end;

{ TBlockBounds }

function TBlockBounds.GetLeft:Double;
begin
  with IBlock do
       result:=Location.X-(Size.X*0.5);
end;

procedure TBlockBounds.SetLeft(const Value:Double);
begin
  with IBlock do
  begin
    Size.X:=Right-Value;
    Location.X:=Value+Size.X*0.5;
  end;
end;

function TBlockBounds.GetRight:Double;
begin
  with IBlock do
       result:=Location.X+(Size.X*0.5);
end;

procedure TBlockBounds.SetRight(const Value:Double);
begin
  with IBlock do
  begin
    Size.X:=Value-Left;
    Location.X:=Value-Size.X*0.5;
  end;
end;

function TBlockBounds.GetTop:Double;
begin
  with IBlock do
       result:=Location.Z+(Size.Z*0.5);
end;

procedure TBlockBounds.SetTop(const Value:Double);
begin
  with IBlock do
  begin
    Size.Z:=Value-Bottom;
    Location.Z:=Value-Size.Z*0.5;
  end;
end;

function TBlockBounds.GetBottom:Double;
begin
  with IBlock do
       result:=Location.Z-(Size.Z*0.5);
end;

procedure TBlockBounds.SetBottom(const Value:Double);
begin
  with IBlock do
  begin
    Size.Z:=Top-Value;
    Location.Z:=Value+Size.Z*0.5;
  end;
end;

function TBlockBounds.GetFront:Double;
begin
  with IBlock do
       result:=Location.Y-(Size.Y*0.5);
end;

procedure TBlockBounds.SetFront(const Value:Double);
begin
  with IBlock do
  begin
    Size.Y:=Back-Value;
    Location.Y:=Value+Size.Y*0.5;
  end;
end;

function TBlockBounds.GetBack:Double;
begin
  with IBlock do
       result:=Location.Y+(Size.Y*0.5);
end;

procedure TBlockBounds.SetBack(const Value:Double);
begin
  with IBlock do
  begin
    Size.Y:=Value-Front;
    Location.Y:=Value-Size.Y*0.5;
  end;
end;

{ TVisualBlock }

Constructor TVisualBlock.Create(AOwner: TComponent);
begin
  inherited;
  FLocation:=TPointXYZFloat.Create(Self,0);
  FSize:=TPointXYZFloat.Create(Self,100,SizeChanged);

  IsCloneable:=True;  // Experimental
end;

Destructor TVisualBlock.Destroy;
begin
  FSize.Free;
  FLocation.Free;
  inherited;
end;

procedure TVisualBlock.Assign(Source: TPersistent);
begin
  if Source is TVisualBlock then
  with TVisualBlock(Source) do
  begin
    Self.FLocation.Assign(Location);
    Self.FSize.Assign(FSize);

    Self.IsCloneable:=IsCloneable;
  end
  else
    inherited;
end;

procedure TVisualBlock.SetLocation(const Value:TPointXYZFloat);
begin
  FLocation.Assign(Value);
  Repaint;
end;

procedure TVisualBlock.SetSize(const Value: TPointXYZFloat);
begin
  FSize.Assign(Value);
  Repaint;
end;

procedure TVisualBlock.AddPathPoint(const X,Y:Single; const AColor:TColor=clDefault);
begin
  // Dummy
end;

procedure TVisualBlock.ClearPath;
begin
  // Dummy
end;

procedure TVisualBlock.SetBounds(const R:TRect; const Z0,Z1:Single;
            const Height:TCoordinate);
begin
  with Location.Point,R do
  begin
    X:=(Left+Right)*0.5;
    Z:=Height-((Top+Bottom)*0.5);
    Y:=(Z0+Z1)*0.5;
  end;

  with Size.Point,R do
  begin
    X:=Abs(Right-Left);
    Z:=Abs(Bottom-Top);
    Y:=Abs(Z1-Z0);
  end;
end;

procedure TVisualBlock.SizeChanged(Sender:TObject);
begin
end;

{ TVisualEditor }

procedure TVisualEditor.MarkDirty;
begin
  if Assigned(OnDirty) then
     OnDirty(Self);
end;

procedure TVisualEditor.RefreshBlock(const AVisual:TVisualBlock);
begin
end;

{ TCustomBlock }

Constructor TCustomBlock.Create(AOwner: TComponent);
begin
  inherited;
  FVisible:=True;
  FFormat:=TBlockFormat.Create(Self);
end;

Destructor TCustomBlock.Destroy;
begin
  FBounds.Free;

  if Assigned(FTile) then
  begin
    FreeAndNil(FTile.FOffset);
    FTile.Free;
  end;

  FScale.Free;
  FRotation.Free;
  FFormat.Free;
  FActions.Free;

  if Assigned(Parent) then
     Parent.Remove(Self);

  DeleteLists;

  inherited;
end;

function TCustomBlock.ShouldDraw(After:Boolean=False):Boolean;
begin
  result:=Visible and (not After);
end;

function TCustomBlock.GetEditor:String;
begin
  result:='';
end;

function TCustomBlock.GetRotation:TRotationXYZ;
begin
  if not Assigned(FRotation) then
     FRotation:=TRotationXYZ.Create(Self,0);

  result:=FRotation;
end;

function TCustomBlock.HasRotation:Boolean;
begin
  result:=Assigned(FRotation);
end;

function TCustomBlock.HasScale: Boolean;
begin
  result:=Assigned(FScale);
end;

function TCustomBlock.HasTile: Boolean;
begin
  result:=Assigned(FTile);
end;

procedure TCustomBlock.DeleteLists;
begin
  Repaint;
end;

function TCustomBlock.ShouldDrawInterior: Boolean;
begin
  result:=FFormat.FDrawInside and (not IBlocks.FDrawBlocks.Shadows.Visible);
end;

function TCustomBlock.GetActions: TBlockActions;
begin
  if not Assigned(FActions) then
     FActions:=TBlockActions.Create(Self,TBlockActionItem);

  result:=FActions;
end;

function TCustomBlock.HasActions(AEvent:String=''):Boolean;
begin
  result:=Assigned(FActions);

  if result then
     if AEvent='' then
        result:=FActions.Count>0
     else
        result:=Assigned(FActions.OfEvent(AEvent));
end;

procedure TCustomBlock.SetCursor(const Value:TCursor);
begin
  if FCursor<>Value then
  begin
    FCursor:=Value;

    if Assigned(IBlocks) then
       if Assigned(IBlocks.FDrawBlocks) then
          if FCursor=crDefault then
             Dec(IBlocks.FDrawBlocks.IUseCursor)
          else
             Inc(IBlocks.FDrawBlocks.IUseCursor);
  end;
end;

procedure TCustomBlock.SetActions(const Value: TBlockActions);
begin
  if Assigned(Value) then
     Actions.Assign(Value)
  else
     FreeAndNil(FActions);
end;

function TCustomBlock.IsActionsStored: Boolean;
begin
  result:=Assigned(FActions) and (FActions.Count>0);
end;

function TCustomBlock.CreateNewList: Integer;
begin
  result:=glGenLists(1);
  glNewList(result,GL_COMPILE_AND_EXECUTE);
end;

procedure TCustomBlock.DeleteList(var AList: Integer);
begin
  if AList<>0 then
  begin
    glDeleteLists(AList,1);
    AList:=0;
  end;
end;

procedure TCustomBlock.InternalTransform;
var t : Integer;
begin
  with Parent.DrawBlocks.CurrentParents do
  for t:=0 to Count-1 do
      Block[t].DefaultTransform;

  DefaultTransform;

  //if not IsObjectBlock then
  with Size.Point do
       //glMatrixScalefEXT(GL_MODELVIEW, X*0.5, Z*0.5, Y*0.5);
       glScalef(X*0.5, Z*0.5, Y*0.5);
end;

procedure TCustomBlock.ModelMatrix(out AMatrix:TMatrix4d); // <-- BCB error D2011, cannot be a function
begin
  glMatrixMode(GL_MODELVIEW);

  {$IFOPT C+}
  if Assigned(Parent.IParent) and Assigned(Parent.IParent.Canvas) then
     Assert(TGLCanvas(Parent.IParent.Canvas).CheckGLError,'MatrixMode');
  {$ENDIF}

  glPushMatrix;
  glLoadIdentity;

  InternalTransform;

  glGetDoublev(GL_MODELVIEW_MATRIX, @AMatrix);
end;

function TCustomBlock.InternalProjectPoint(const Matrix:TMatrix4d; const X,Y,Z:Single):TPoint3DFloat;
var norm : Single;
begin
  result.X:= Matrix[0,0] * X + Matrix[1,0] * Y + Matrix[2,0] * Z + Matrix[3,0];
  result.Z:= Matrix[0,1] * X + Matrix[1,1] * Y + Matrix[2,1] * Z + Matrix[3,1];
  result.Y:= -Matrix[0,2] * X - Matrix[1,2] * Y - Matrix[2,2] * Z - Matrix[3,2];
  norm:= 1/( Matrix[0,3] * X - Matrix[1,3] * Y + Matrix[2,3] * Z + Matrix[3,3]);

  with result do
  begin
    X:=X*norm;
    Y:=Y*norm;
    Z:=Z*norm;
  end;
end;

function TCustomBlock.ProjectPoint(const X,Y,Z:Single):TPoint3DFloat;
var tmp : TMatrix4d;
begin
  if Assigned(Parent) then
  begin
    ModelMatrix(tmp);
    result:=InternalProjectPoint(tmp,X,Y,Z);

    glPopMatrix;
    //  glMatrixMode(GL_PROJECTION);
  end
  else
  begin
    result.X:=0;
    result.Y:=0;
    result.Z:=0;
  end;
end;

(*
var tmpSin,
    tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    tmp    : Double;
begin
  if Assigned(FScale) then
  with FScale.Point do
  begin
    APoint.X:=APoint.X*X;
    APoint.Y:=APoint.Y*Y;
    APoint.Z:=APoint.Z*Z;
  end;

  if Assigned(FRotation) then
  with FRotation.Point do
  begin
    if X<>0 then
    begin
      SinCos(X*TeePiStep,tmpSin,tmpCos);

      tmp:=APoint.X;
      APoint.X:=tmp*tmpCos-APoint.Y*tmpSin;
      APoint.Y:=tmp*tmpSin+APoint.Y*tmpCos;
    end;

    if Y<>0 then
    begin
      SinCos(Y*TeePiStep,tmpSin,tmpCos);

      tmp:=APoint.Y;
      APoint.Y:=tmp*tmpCos+APoint.Y*tmpSin;
      APoint.Z:=tmp*-tmpSin+APoint.Z*tmpCos;
    end;

    if Z<>0 then
    begin
      SinCos(Z*TeePiStep,tmpSin,tmpCos);

      tmp:=APoint.Z;
      APoint.Z:=tmp*tmpCos-APoint.Z*tmpSin;
      APoint.X:=tmp*tmpSin+APoint.X*tmpCos;
    end;
  end;

  with Location.Point do
  begin
    APoint.X:=APoint.X+X;
    APoint.Y:=APoint.Y+Y;
    APoint.Z:=APoint.Z+Z;
  end;
end;
*)

procedure TCustomBlock.DrawSelected;
begin
  ICanvas.{$IFDEF BLOCKS}iCube{$ELSE}Cube{$ENDIF}(-1,1,-1,1,-1,1,True);
end;

function TCustomBlock.Clicked(X,Y:Integer):TCustomBlock;
begin
  if Visible then
     result:=Parent.ClickedBlock(Self,X,Y)
  else
     result:=nil;
end;

{ TGradientBlock }

Destructor TGradientBlock.Destroy;
begin
  FGradient.Free;
  inherited;
end;

procedure TGradientBlock.Assign(Source:TPersistent);
begin
  if Source is TGradientBlock then
     Gradient:=TGradientBlock(Source).FGradient;

  inherited;
end;

procedure TGradientBlock.PrepareForGallery;
begin
  inherited;
  Gradient.StartColor:=clBlue;
  Gradient.EndColor:=clRed;
end;

procedure TGradientBlock.GradientChanged(Sender:TObject);

  procedure DoVertical(const Color1,Color2:TColor);
  begin
    Corners.FLeftTop:=Color1;
    Corners.FRightTop:=Color1;
    Corners.FLeftBottom:=Color2;
    Corners.FRightBottom:=Color2;

    Sides.FTop:=clDefault;
    Sides.FBottom:=clDefault;

    if IAnySide then
    begin
      Sides.FLeft:=Gradient.MidColor;
      Sides.FRight:=Gradient.MidColor;
    end
    else
    begin
      Sides.FLeft:=clDefault;
      Sides.FRight:=clDefault;
    end;
  end;

  procedure DoHorizontal(const Color1,Color2:TColor);
  begin
    Corners.FLeftTop:=Color1;
    Corners.FLeftBottom:=Color1;
    Corners.FRightTop:=Color2;
    Corners.FRightBottom:=Color2;

    Sides.FLeft:=clDefault;
    Sides.FRight:=clDefault;

    if IAnySide then
    begin
      Sides.FTop:=Gradient.MidColor;
      Sides.FBottom:=Gradient.MidColor;
    end
    else
    begin
      Sides.FTop:=clDefault;
      Sides.FBottom:=clDefault;
    end;
  end;

begin
  IAnyCorner:=True;
  IAnySide:=(Gradient.MidColor<>clNone);

  FCenter:=clDefault;

  with Gradient do
  case Direction of
    gdBottomTop : DoVertical(StartColor,EndColor);
    gdTopBottom : DoVertical(EndColor,StartColor);
    gdLeftRight : DoHorizontal(StartColor,EndColor);
    gdRightLeft : DoHorizontal(EndColor,StartColor);
   gdFromCenter,
   gdRadial     : begin
                    DoHorizontal(EndColor,EndColor);
                    DoVertical(EndColor,EndColor);
                    FCenter:=StartColor;
                  end;
   gdFromTopLeft : begin
                     Corners.FLeftTop:=EndColor;
                     Corners.FLeftBottom:=StartColor;
                     Corners.FRightTop:=StartColor;
                     Corners.FRightBottom:=StartColor;
                   end;
 gdFromBottomLeft: begin
                     Corners.FLeftTop:=StartColor;
                     Corners.FLeftBottom:=EndColor;
                     Corners.FRightTop:=StartColor;
                     Corners.FRightBottom:=StartColor;
                   end;
   gdDiagonalUp:   begin
                     Corners.FLeftTop:=StartColor;
                     Corners.FLeftBottom:=StartColor;
                     Corners.FRightTop:=EndColor;
                     Corners.FRightBottom:=StartColor;
                   end;
 gdDiagonalDown:   begin
                     Corners.FLeftTop:=StartColor;
                     Corners.FLeftBottom:=StartColor;
                     Corners.FRightTop:=StartColor;
                     Corners.FRightBottom:=EndColor;
                   end;
  end;

  DeleteLists;
end;

type
  TTeeGradientAccess=class(TTeeGradient);

procedure TGradientBlock.SetGradient(const Value: TTeeGradient);
begin
  if Assigned(Value) then
     Gradient.Assign(Value)
  else
     FreeAndNil(FGradient);

  Repaint;
end;

function TGradientBlock.GetGradient: TTeeGradient;
begin
  if not Assigned(FGradient) then
  begin
    FGradient:=TTeeGradient.Create(GradientChanged);
    TTeeGradientAccess(FGradient).IDefVisible:=True;
    FGradient.Visible:=True;
  end;

  result:=FGradient;
end;

{ TObjectProperties }

function TObjectProperties.PropertyValue(const AName:String):String;
var //tmp : String;
    t   : Integer;
    //tmpLeft,
    //tmpRight : String;
begin
  t:=IndexOfProperty(AName);
  if t=-1 then
     result:=''
  else
     result:=UpperCase(Values[AName]);

  {
  result:='';

  tmp:=UpperCase(PropName);

  with Props do
  for t:=0 to Count-1 do
  begin
    SplitValue(Strings[t],tmpLeft,tmpRight);

    if UpperCase(tmpLeft)=tmp then
    begin
      result:=tmpRight;
      break;
    end;
  end;
  }
end;

function TObjectProperties.IndexOfProperty(const AName:String):Integer;
var tmpPropName : String;
    t           : Integer;
    ALeft  : String;
    ARight : String;
begin
  result:=-1;

  tmpPropName:=UpperCase(AName);

  for t:=0 to Count-1 do
  begin
    SplitValue(Strings[t],ALeft,ARight);

    if UpperCase(ALeft)=tmpPropName then
    begin
      result:=t;
      break;
    end;
  end;
end;

function TObjectProperties.GetValue(const AName:String):String;
var tmp    : Integer;
    ALeft  : String;
    ARight : String;
begin
  tmp:=IndexOfProperty(AName);

  if tmp=-1 then
     result:=''
  else
  begin
    SplitValue(Strings[tmp],ALeft,ARight);

    if ARight='' then
       result:=''
    else
       result:=IBlocks.InternalGetPropValue(ARight);
  end;
end;

procedure TObjectProperties.SetValue(const AName,AValue:String);
var tmpName : String;
    tmp     : String;
    t       : Integer;
    i       : Integer;
begin
  tmpName:=UpperCase(AName);

  with IBlocks.Properties do
  for t:=0 to Count-1 do
  begin
    tmp:=Strings[t];
    i:=Pos('=',tmp);

    if (i>0) and (UpperCase(Copy(tmp,1,i-1))=tmpName) then
    begin
      IBlocks.DoSetProperty(IOwner,Copy(tmp,i+1,Length(tmp))+'='+AValue);
      Exit;
    end;
  end;

  // Raise Exception.Create('Property: '+AName+' not found in Object: '+TitleOrName);
end;

type
  TPointXYZFloatAccess=class(TPointXYZFloat);

{ TCustomObjectBlock }

Constructor TCustomObjectBlock.Create(AOwner: TComponent);
begin
  inherited;

  IsObjectBlock:=True;

  TPointXYZFloatAccess(Size).InitDefault(0);
end;

Destructor TCustomObjectBlock.Destroy;
begin
  LinkBlock:=nil;

  FreeAndNil(FItems);
  FProperties.Free;
  
  inherited;
end;

procedure TCustomObjectBlock.Assign(Source: TPersistent);
begin
  if Source is TCustomObjectBlock then
  with TCustomObjectBlock(Source) do
  begin
    Self.FLink:=FLink;

    if FLink='' then
       Self.Items:=Items
    else
       FreeAndNil(Self.FItems);

    Self.LinkBlock:=LinkBlock;
  end;

  inherited;
end;

function TCustomObjectBlock.HasContents:Boolean;
begin
  result:=(LinkFile='') and (not Assigned(LinkBlock)) and (Items.Count>0);
end;

function LastPosOf(const ASub:Char; const AStr:String):Integer;
var t : Integer;
begin
  result:=0;

  if AStr<>'' then
  begin
    t:=Length(AStr);

    while AStr[t]<>ASub do
    begin
      Dec(t);

      if t=0 then
         Exit;
    end;

    result:=t;
  end;
end;

function TBlocks.DoFindName(var AName:String):TCustomBlock;
var tmp    : TBlocks;
    tmpObj : TCustomObjectBlock;
    tmpSt    : String;
    tmpProp  : TBlocks;
begin
  result:=nil;

  tmp:=Self; //ABlock.Parent;

  {
  tmpSt:=Properties.PropertyValue(AName);

  if tmpSt<>'' then
     AName:=UpperCase(tmpSt);

  }

  tmpProp:=tmp;

  repeat
    tmpSt:=tmpProp.Properties.PropertyValue(AName);

    if tmpSt<>'' then
    begin
      AName:=UpperCase(tmpSt);
      break;
    end
    else
    if Assigned(tmpProp.IObject) and Assigned(tmpProp.IObject.Parent) then
       tmpProp:=tmpProp.IObject.Parent
    else
       break;

  until not Assigned(tmpProp);

  if Copy(AName,1,1)='@' then
  begin
    result:=TObject(StrToInt(Copy(AName,2,Length(AName)))) as TCustomBlock;
    AName:='';
  end
  else
  while not Assigned(result) do
  begin
    result:=tmp.Find(AName);

    if not Assigned(result) then
    begin
      tmpObj:=tmp.IObject;

      if Assigned(tmpObj) and tmpObj.ItemsReady then
      begin
        tmp:=tmpObj.Parent;

        if not Assigned(tmp) then
           Break;
      end
      else
        Break;
    end;
  end;
end;

class function TBlocks.IsBooleanProperty(const Prop:PPropInfo):Boolean;
begin
  result:= (Prop.PropType^.Kind=tkEnumeration) and
           (Prop.PropType^.Name='Boolean');
end;

function DoGetObject(AObj:TObject; const AProp:String):TObject;
var tmpP : PPropInfo;
begin
  tmpP:=GetPropInfo(AObj,AProp);

  if Assigned(tmpP) and (tmpP.PropType^.Kind=tkClass) then
     result:=GetObjectProp(AObj,AProp)
  else
     result:=nil;
end;

function TBlocks.InternalGetPropValue(Exp:String):Variant;

  function GetTextValue(AStrings:TStrings):String;
  var tmpSt : String;
      tmp   : Integer;
  begin
    tmpSt:=AStrings.Text;

    if tmpSt<>'' then
    begin
      repeat
        tmp:=Length(tmpSt);

        if (tmpSt[tmp]=#13) or (tmpSt[tmp]=#10) then
           Delete(tmpSt,tmp,1)
        else
           break;

      until tmp<2;
    end;

    result:=tmpSt;
  end;

var ARightProp  : String;
    ARightBlock : TObject;

  procedure TryFindMaker;
  var i : Integer;
      tmpName : String;
  begin
    i:=Pos('.',ARightProp);

    if i>0 then
    begin
      tmpName:=UpperCase(Copy(ARightProp,1,i-1));

      if tmpName=UpperCase(Parent.Name) then
      begin
        ARightBlock:=Parent;
        Delete(ARightProp,1,i);
      end;
    end;
  end;

var tmp         : TObject;
    tmpNot      : Boolean;
    tmpR        : PPropInfo;
    tmpBool     : Boolean;
    tmpValue    : Double;
begin
  if Copy(Exp,1,4)='NOT ' then
  begin
    tmpNot:=True;
    Delete(Exp,1,4);
    Exp:=Trim(Exp);
  end
  else
    tmpNot:=False;

  ARightProp:=Exp;

  ARightBlock:=DoFindName(ARightProp);

  if not Assigned(ARightBlock) then
     if Assigned(Parent) then
        TryFindMaker;

  tmpR:=TPropertyAnimation.Fixup(ARightBlock,ARightProp);

  if Assigned(tmpR) then
  begin
    if IsBooleanProperty(tmpR) then
    begin
      tmpBool:=Boolean(GetOrdProp(ARightBlock,ARightProp));

      if tmpNot then
         tmpBool:=not tmpBool;

      result:=tmpBool;
    end
    else
    if (ARightBlock is TStrings) and (UpperCase(ARightProp)='TEXT') then
       result:=GetTextValue(TStrings(ARightBlock))
    else
    begin
      tmp:=DoGetObject(ARightBlock,ARightProp);

      if tmp is TStrings then
         result:=GetTextValue(TStrings(tmp))
      else
         result:=GetPropValue(ARightBlock,ARightProp);
    end;
  end
  else
  if TryStrToFloat(ARightProp,tmpValue) then
     result:=tmpValue
  else
     Raise Exception.Create('Property '+Exp+' not found !');
end;

procedure TBlocks.DoSetProperty(SenderBlock:TCustomBlock; Exp:String);
var
  ALeftPropOrig : String;
  ALeftOrig     : TObject;

  procedure CreateAnimation(IsColor:Boolean; const AStart,AEnd:Double; ADuration:Integer);
  var tmpAnim : TNumberAnimation;
      tmpItem : TAnimateItem;
  begin
    if IsColor then
       tmpAnim:=TColorsAnimation.Create(nil)
    else
       tmpAnim:=TNumberAnimation.Create(nil);

    tmpAnim.Duration:=ADuration;
    tmpAnim.StartValue:=AStart;
    tmpAnim.EndValue:=AEnd;
    tmpAnim.KeepEndValue:=True;
    tmpAnim.UseStartValue:=True;
    tmpAnim.PropertyName:=ALeftPropOrig;
    tmpAnim.Instance:=ALeftOrig as TComponent;

    tmpItem:=Animates.Add;

    with tmpItem.Animate do
    begin
      FreeOnStop:=True;
      Animations.Add(tmpAnim);
      Play;
    end;
  end;

  procedure TrySetCustomProperty(ABlock:TObject; const ALeft,ARight:String);
  var tmp : String;
  begin
    if ABlock is TCustomObjectBlock then
    begin
      tmp:=TCustomObjectBlock(ABlock).Items.Properties.PropertyValue(ALeft);

      if tmp<>'' then
         TCustomObjectBlock(ABlock).Items.DoSetProperty(TCustomObjectBlock(ABlock),tmp+'='+ARight);
    end;
  end;

  procedure TrySetProp(ALeft:TObject; ALeftProp:String; ANot,AInc,ADec:Boolean;
                       ARight:TObject; ARightProp:String);
  var
    tmpDuration   : Integer;

    procedure SetLeftValue(const AValue:Variant);

      procedure SetAnimated(const Factor:Single);
      var tmpStart : Double;
      begin
        tmpStart:=GetPropValue(ALeft,ALeftProp);
        CreateAnimation(IsColorProperty(ALeft,ALeftProp),
          tmpStart,tmpStart+Factor*AValue,tmpDuration);
      end;

      procedure SetIncDec(const AFactor:Single);
      var tmp : TObject;
      begin
        tmp:=DoGetObject(ALeft,ALeftProp);

        if tmp is TPointXYZFloat then
           with TPointXYZFloat(tmp) do Value:=Value+AFactor*AValue // <-- animated?
        else
        begin
          if tmpDuration>0 then
             SetAnimated(AFactor)
          else
             SetPropValue(ALeft,ALeftProp,GetPropValue(ALeft,ALeftProp)+AFactor*AValue);
        end;
      end;

    var tmp : TObject;
    begin
      if AInc then
         SetIncDec(1)
      else
      if ADec then
         SetIncDec(-1)
      else
      if (ALeft is TStrings) and (UpperCase(ALeftProp)='TEXT') then
         TStrings(ALeft).Text:=AValue
      else
      begin
        tmp:=DoGetObject(ALeft,ALeftProp);

        if tmp is TStrings then
           TStrings(tmp).Text:=AValue
        else
          if tmpDuration>0 then
          begin
            CreateAnimation(IsColorProperty(ALeft,ALeftProp),
               GetPropValue(ALeft,ALeftProp),AValue,tmpDuration);
          end
          else
             SetPropValue(ALeft,ALeftProp,AValue);
      end;
    end;

    procedure SetRightProperty(tmpR:PPropInfo; ARight:TObject; const ARightProp:String);
    var tmpBool : Boolean;
    begin
      if IsBooleanProperty(tmpR) then
      begin
        tmpBool:=Boolean(GetOrdProp(ARight,ARightProp));

        if ANot then
           tmpBool:=not tmpBool;

        SetOrdProp(ALeft,ALeftProp,Ord(tmpBool));
      end
      else
        SetLeftValue(GetPropValue(ARight,ARightProp));
    end;

  var tmpR : PPropInfo;
      i    : Integer;
  begin
    i:=Pos(',ANIMATED,',ARightProp);

    if i>0 then
    begin
      tmpDuration:=StrToInt(Copy(ARightProp,i+10,Length(ARightProp)));
      Delete(ARightProp,i,Length(ARightProp)-i+1);
    end
    else
      tmpDuration:=0;

    if Assigned(ARight) then
    begin
      tmpR:=TPropertyAnimation.Fixup(ARight,ARightProp);

      if Assigned(tmpR) then
         SetRightProperty(tmpR,ARight,ARightProp)
      else
      begin
        if ARight is TCustomObjectBlock then
        begin
          ARightProp:=TCustomObjectBlock(ARight).Items.Properties.PropertyValue(ARightProp);

          if ARightProp<>'' then
          begin
            ARight:=TCustomObjectBlock(ARight).Items.DoFindName(ARightProp);

            TPropertyAnimation.Fixup(ARight,ARightProp);

            SetLeftValue(GetPropValue(ARight,ARightProp));
          end
        end
        else
        // Raise Exception !
          ;
      end;
    end
    else
    begin
      tmpR:=GetPropInfo(ALeft,ARightProp);

      if Assigned(tmpR) then
         SetRightProperty(tmpR,ALeft,ARightProp)
      else
         SetLeftValue(ARightProp);
    end;
  end;

var
  ALeft,
  ALeftProp : String;
  ALeftBlock : TObject;

  procedure FindLeft(APos:Integer);
  begin
    ALeftProp:=Copy(ALeft,1,APos-1);
    ALeftBlock:=DoFindName(ALeftProp);

    if ALeftProp<>'' then
       ALeftProp:=ALeftProp+'.';

    ALeftProp:=ALeftProp+Copy(ALeft,APos+1,Length(ALeft));
  end;

var i  : Integer;
    i2 : Integer;

    ARight : String;
    ARightProp : String;
    ARightBlock : TObject;

    tmpInc,
    tmpDec,
    tmpNot : Boolean;
begin
  i:=Pos('=',Exp);

  if i>0 then
  begin
    ALeft:=Trim(Copy(Exp,1,i-1));
    ARight:=Trim(Copy(Exp,i+1,Length(Exp)));

    i:=LastPosOf('.',ALeft);
    i2:=LastPosOf(':',ALeft); // <-- obsolete?

    if i2>0 then
       if (i=0) or (i2>i) then
          i:=i2;

    if i>0 then
    begin
      FindLeft(i);

      if not Assigned(ALeftBlock) then
      begin
        ALeftBlock:=SenderBlock;
        ALeftProp:=ALeft;
      end;
    end
    else
    begin
      ALeftBlock:=SenderBlock;
      ALeftProp:=ALeft;
    end;

    if Assigned(ALeftBlock) then
    begin
      ALeftOrig:=ALeftBlock;
      ALeftPropOrig:=ALeftProp;

      if not Assigned(TPropertyAnimation.Fixup(ALeftBlock,ALeftProp)) then
      begin
        i:=Pos('.',ALeft);

        if i>0 then
        begin
          FindLeft(i);

          if not Assigned(TPropertyAnimation.Fixup(ALeftBlock,ALeftProp)) then
          begin
          end;
        end
        else
        begin
          TrySetCustomProperty(ALeftBlock,ALeftProp,ARight);
          Exit;
        end;
      end;

      if Copy(ARight,1,4)='NOT ' then
      begin
        tmpNot:=True;
        Delete(ARight,1,4);
        ARight:=Trim(ARight);
      end
      else
        tmpNot:=False;

      if Copy(ARight,1,2)='-=' then
      begin
        tmpDec:=True;
        Delete(ARight,1,2);
        ARight:=Trim(ARight);
      end
      else
        tmpDec:=False;

      if Copy(ARight,1,2)='+=' then
      begin
        tmpInc:=True;
        Delete(ARight,1,2);
        ARight:=Trim(ARight);
      end
      else
        tmpInc:=False;

      ARightProp:=ARight;
      ARightBlock:=DoFindName(ARightProp);

      TPropertyAnimation.Fixup(ARightBlock,ARightProp);

      if not Assigned(ARightBlock) then
         ARightProp:=ARight;

      if Assigned(ALeftBlock) then
         TrySetProp(ALeftBlock,ALeftProp,tmpNot,tmpInc,tmpDec,ARightBlock,ARightProp);
    end
    else
      TrySetCustomProperty(SenderBlock,ALeft,ARight);
  end
  else
    Raise Exception.Create('Missing "=" symbol in expression (A=B)');
end;

function TCustomObjectBlock.SaveChildren:Boolean;
begin
  result:=HasContents;
end;

Procedure TCustomObjectBlock.GetChildren(Proc:TGetChildProc; Root:TComponent);
var t : Integer;
begin
  inherited;

  if SaveChildren then
     for t:=0 to Items.Count-1 do
         Proc(Item[t]);
end;

function TCustomObjectBlock.GetChildOwner: TComponent;
begin
  if (csDesigning in ComponentState) and Assigned(FItems) then
  begin
    if Assigned(FItems.IParent) then
       result:=FItems.IParent.Owner
    else
    if Assigned(FItems.IObject) then
       result:=FItems.IObject.Owner
    else
       result:=FItems;
  end
  else
     result:=FItems;  // Self ? nil? <-- WARNING : ULTRA-SLOW due to TComponent.Notification
end;

procedure TCustomObjectBlock.SetBlocks(const Value:TBlocks);
begin
  inherited;

  if Assigned(FItems) then
  begin
    FItems.FDrawBlocks:=Parent;

    if Assigned(Parent) then
       FItems.Parent:=Parent.IParent;
  end;
end;

function TCustomObjectBlock.Get(Index:Integer):TCustomBlock;
begin
  result:=FItems.Block[Index];
end;

procedure TCustomObjectBlock.SetItems(const Value: TBlocks);
begin
  if Assigned(Value) then
     Items.Assign(Value)
  else
     FreeAndNil(FItems);

  Repaint;
end;

procedure TCustomObjectBlock.SetLinkBlock(const Value: TCustomBlock);

  function ValidLink(const Value:TCustomBlock):Boolean;
  begin
    result:=Value<>Self;

    if result then
    if Value is TCustomObjectBlock then
       result:=not TCustomObjectBlock(Value).Items.RecursiveContains(Self);
  end;

begin
  if FLinkBlock<>Value then
  begin
    if (not Assigned(Value)) or ValidLink(Value) then
    begin
      if Assigned(FLinkBlock) then
         FLinkBlock.RemoveFreeNotification(Self);

      FLinkBlock:=Value;

      if Assigned(FLinkBlock) then
         FLinkBlock.FreeNotification(Self);

      FreeAndNil(FItems);
      Repaint;
    end
    else
      raise Exception.Create(TeeMsg_ErrorCannotLink);
  end;
end;

function TCustomObjectBlock.GetProperties:TObjectProperties;
begin
  if not Assigned(FProperties) then
     FProperties:=TObjectProperties.Create;

  result:=FProperties;
end;

procedure TCustomObjectBlock.SetProperties(const Value:TObjectProperties);
begin
  if Assigned(Value) then
     Properties.Assign(Value)
  else
     FreeAndNil(FProperties);
end;

procedure TCustomObjectBlock.LoadItems(const ASource,AFile:String);
begin
  FItems.TryLoad(ASource,AFile);
end;

function TCustomObjectBlock.GetItems: TBlocks;

  procedure DoGetLink;
  var tmpCurrentSource : String;
  begin
    if Assigned(Parent) then
    begin
      if Assigned(Parent.IObject) and
         (UpperCase(Parent.IObject.FLink)=UpperCase(FLink)) then
           raise Exception.Create('Recursive block loading not allowed: '+FLink);

      tmpCurrentSource:=Parent.FCurrentSource;
    end
    else
       tmpCurrentSource:='';

    if TeeNoThreads then
       LoadItems(tmpCurrentSource,TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,LinkFile))
    else
    if not Assigned(FItems.ILoadThread) then
    begin
      FItems.ILoadThread:=TLoadThread.Create(True);

      with TLoadThread(FItems.ILoadThread) do
      begin
        Format:=Self.Format;
        Items:=Self.Items;

        if Assigned(Parent) then
           TeePanel:=Parent.IParent;

        Priority:=tpHigher;
        FreeOnTerminate:=True;

        ParentSource:=tmpCurrentSource;
        Source:=TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,LinkFile);
        OnTerminate:=ThreadTerminated;

        {$IFDEF D14}
        Start;
        {$ELSE}
        Resume;
        {$ENDIF}
      end;
    end;
  end;

begin
  if not Assigned(FItems) then
  begin
    FItems:=TBlocks.Create(nil);
    FItems.IObject:=Self;
    FItems.FDrawBlocks:=Parent;

    if Assigned(Parent) then
       FItems.IParent:=Parent.IParent;

    if FLink<>'' then
       DoGetLink;
  end;

  result:=FItems;
end;

procedure TCustomObjectBlock.Notification(AComponent: TComponent;
                           Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FLinkBlock) and (AComponent=FLinkBlock) then
     LinkBlock:=nil;
end;

procedure TCustomObjectBlock.SetLink(const Value: String);
begin
  FLink:=Value;

  if FLink<>'' then
     TBlocks.CheckLibraryPath(TeeMsg_ObjectsLibrary,FLink);

  FreeAndNil(FItems);
  Repaint;
end;

procedure TCustomObjectBlock.DeleteLists;
begin
  inherited;

  if Assigned(FItems) then
     FItems.DeleteLists;
end;

procedure TCustomObjectBlock.StartTransform;
begin
  glPushMatrix;
  DefaultTransform;
end;

function TCustomObjectBlock.CompleteLinkFile:String;
var tmp : String;
begin
  if LinkFile='' then
     result:=''
  else
  begin
    result:=Parent.FCurrentSource;

    if TeeIsURL(LinkFile) then
       result:=LinkFile
    else
    if TeeIsURL(result) then
       result:=result+'/'+LinkFile
    else
    begin
      tmp:=TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,LinkFile);

      if Pos(':',tmp)>0 then  // absolute path
         result:=tmp
      else
         result:=result+'\'+tmp; // relative path to parent path
    end;
  end;
end;

procedure TCustomObjectBlock.DoDrawItems;
var t : Integer;
begin
  if not IPropsReady then
  try
    if Assigned(FProperties) then
       with FProperties do
       for t:=0 to Count-1 do
           Items.DoSetProperty(Self,Strings[t]);

  finally
    IPropsReady:=True;
  end;

  if Assigned(LinkBlock) then
  begin
    glPushMatrix;

    with LinkBlock.Location.Point do
         glTranslatef(-X,-Z,Y);

    TBlocks.DoDrawItem(LinkBlock);

    glPopMatrix;
  end
  else
  begin
    FItems.ICanvas:=ICanvas;
    FItems.FDrawBlocks:=IBlocks.FDrawBlocks;
    FItems.Draw(IPicking);
  end;
end;

procedure TCustomObjectBlock.Clear;
begin
  if Assigned(FItems) then
     FItems.Clear;
end;

procedure TCustomObjectBlock.Draw;
begin
  if not Assigned(LinkBlock) then
     if not Assigned(FItems) then
        GetItems;

  if Assigned(LinkBlock) or (not Assigned(FItems.ILoadThread)) then
     DoDrawItems;
end;

function TCustomObjectBlock.ItemsReady:Boolean;
begin
  result:=Assigned(FItems) and (not Assigned(FItems.ILoadThread)) and (FItems.Count>0);
end;

function TCustomObjectBlock.BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean;
begin
  result:=(Assigned(LinkBlock) and LinkBlock.BoundingBox(AMin,AMax)) or
          (ItemsReady and FItems.CalcBounds(AMin,AMax));

  IBoundsMin:=AMin;
  IBoundsMax:=AMax;

  with Size.Point do
  if result then
  begin
    X:=AMax.X-AMin.X;
    Y:=AMax.Y-AMin.Y;
    Z:=AMax.Z-AMin.Z;
  end
  else
  begin
    X:=0;
    Y:=0;
    Z:=0;
  end;
end;

function TCustomObjectBlock.ItemsStored:Boolean;
begin
  result:=HasContents and ItemsReady;
end;

function TCustomBlock.Editor(const AOwner:TComponent; Embeddable:Boolean=False):TControl;
var tmp   : TCustomFormClass;
    tmpClass : TClass;
    tmpOwner : TComponent;
begin
  result:=nil;

  tmpClass:=TeeGetClass('TBlockEditor');

  if Assigned(tmpClass) then
  begin
    tmp:=TCustomFormClass(tmpClass);

    tmpOwner:=AOwner;

    if not Assigned(tmpOwner) then
       tmpOwner:=Application;

    result:=tmp.Create(tmpOwner); // AV at design-time: Self
    result.Tag:=ObjectToTag(Self);

    if Embeddable then
    begin
      (result.FindComponent('PanelButtons') as TControl).Visible:=False;
      result.Align:=alClient;
    end;
  end;
end;

function TCustomBlock.GetParentComponent: TComponent;
begin
  result:=IBlocks.IObject;

  if not Assigned(result) then
     result:=IBlocks.Parent;
end;

function TCustomBlock.HasParent: Boolean;
begin
  result:=True;
end;

procedure FaceToViewer; 

var m : packed Array[0..15] of Double;

  procedure RotateToViewer(const Theta:Double; x,y,z:Double);
  var d : Double;
      st, ct : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
  begin
    d:= x*x + y*y + z*z;
    SinCos(Theta,st,ct);

    if (d > 0) then
    begin
      d:=1/d;
      x:=x*d;
      y:=y*d;
      z:=z*d;
    end;

    m[ 0] := 1; m[ 1] := 0; m[ 2] := 0; m[ 3] := 0;
    m[ 4] := 0; m[ 5] := 1; m[ 6] := 0; m[ 7] := 0;
    m[ 8] := 0; m[ 9] := 0; m[10] := 1; m[11] := 0;
    m[12] := 0; m[13] := 0; m[14] := 0; m[15] := 1;

    m[0] := x*x + ct*(1-x*x) + st*0;
    m[4] := x*y + ct*(0-x*y) + st*-z;
    m[8] := x*z + ct*(0-x*z) + st*y;

    m[1] := y*x + ct*(0-y*x) + st*z;
    m[5] := y*y + ct*(1-y*y) + st*0;
    m[9] := y*z + ct*(0-y*z) + st*-x;

    m[2] := z*x + ct*(0-z*x) + st*-y;
    m[6] := z*y + ct*(0-z*y) + st*x;
    m[10]:= z*z + ct*(1-z*z) + st*0;
  end;

begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @m);
  RotateToViewer(-ArcTan2(m[8], m[10]), 0, 1, 0);
  glMultMatrixd(@m);
end;

procedure TCustomBlock.StartTransform;
begin
  glPushMatrix;
  DefaultTransform;

  with Size.Point do
       //glMatrixScalefEXT(GL_MODELVIEW, X*0.5, Z*0.5, Y*0.5);
       glScalef(X*0.5, Z*0.5, Y*0.5);
end;

procedure TCustomBlock.GetRotationCenter(out AX,AY,AZ:Single; AddParents:Boolean);
var tmp : TPoint3DFloat;
    tmpBlock : TCustomObjectBlock;
begin
  with Location.Point do
  begin
    AX:=X;
    AY:=Y;
    AZ:=Z;
  end;

  if AddParents then
  begin
    if Assigned(Parent) then
    begin
      tmpBlock:=Parent.IObject;

      while Assigned(tmpBlock) do
      begin
        with tmpBlock.Location.Point do
        begin
          AX:=AX+X;
          AY:=AY+Y;
          AZ:=AZ+Z;
        end;

        if Assigned(tmpBlock.Parent) then
           tmpBlock:=tmpBlock.Parent.IObject
        else
           break;
      end;
    end;

    if IsObjectBlock then
    with TCustomObjectBlock(Self) do
    begin
      AX:=AX+(IBoundsMin.X+IBoundsMax.X)*0.5;
      AY:=AY+(IBoundsMin.Y+IBoundsMax.Y)*0.5;
      AZ:=AZ+(IBoundsMin.Z+IBoundsMax.Z)*0.5;
    end;
  end;

  if Assigned(FRotation) and Assigned(FRotation.FCenter) then
  begin
    tmp:=FRotation.FCenter.Point;

    // % of size, from: 0,0,0

    if tmp.X<>0 then
       AX:=AX+(tmp.X*Size.X*0.005);

    if tmp.Y<>0 then
       AY:=AY+(tmp.Y*Size.Y*0.005);

    if tmp.Z<>0 then
       AZ:=AZ+(tmp.Z*Size.Z*0.005);
  end;
end;

function TCustomBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=TObjectBlockHandle.Create(AOwner);
end;

procedure TCustomBlock.DefaultTransform;
var tmpX : Single;
    tmpY : Single;
    tmpZ : Single;

  procedure RotateTransform;
  begin
    with FRotation.GetPoint do
    begin
      if Y<>0 then
      begin
        glTranslatef(0,tmpZ,-tmpY);
        glRotatef(-Y, 1, 0, 0);

        if X<>0 then
        begin
          glTranslatef(tmpX,-tmpZ,0);
          glRotatef(X, 0, 1, 0);

          if Z<>0 then
          begin
            glTranslatef(0,tmpZ,tmpY);
            glRotatef(Z, 0, 0, 1);
            glTranslatef(-tmpX,-tmpZ,0);
          end
          else
            glTranslatef(-tmpX,0,tmpY);

        end
        else
        begin
          if Z<>0 then
          begin
            glTranslatef(tmpX,0,tmpY);
            glRotatef(Z, 0, 0, 1);
            glTranslatef(-tmpX,-tmpZ,0);
          end
          else
            glTranslatef(0,-tmpZ,tmpY);

        end;
      end
      else
      begin
        if X<>0 then
        begin
          glTranslatef(tmpX,0,-tmpY);
          glRotatef(X, 0, 1, 0);

          if Z<>0 then
          begin
            glTranslatef(0,tmpZ,tmpY);
            glRotatef(Z, 0, 0, 1);
            glTranslatef(-tmpX,-tmpZ,0);
          end
          else
            glTranslatef(-tmpX,0,tmpY);

        end
        else
        if Z<>0 then
        begin
          glTranslatef(tmpX,tmpZ,0);
          glRotatef(Z, 0, 0, 1);
          glTranslatef(-tmpX,-tmpZ,0);
        end;
      end;
    end;
  end;

var AX,AY,AZ : Single;
begin
  AX:=0;
  AY:=0;
  AZ:=0;

  if Assigned(FRotation) or Assigned(FScale) then
  begin
    GetRotationCenter(tmpX,tmpY,tmpZ,False);

    if IsObjectBlock then
      with TCustomObjectBlock(Self) do
      begin
        AX:=tmpX-Location.Point.X+(IBoundsMin.X+IBoundsMax.X)*0.5;
        AY:=tmpY-Location.Point.Y+(IBoundsMin.Y+IBoundsMax.Y)*0.5;
        AZ:=tmpZ-Location.Point.Z+(IBoundsMin.Z+IBoundsMax.Z)*0.5;
      end;
  end;

  if Assigned(FRotation) then
  begin
    if IsObjectBlock then
       glTranslatef(AX,AZ,-AY);

    RotateTransform;

    if IsObjectBlock then
       glTranslatef(-AX,-AZ,AY);
  end;

  with Location.Point do
       glTranslatef(X,Z,-Y);

  if Assigned(FScale) then
  begin
    if IsObjectBlock then
       glTranslatef(AX,AZ,-AY);

    with FScale.Point do
         glScalef(X,Y,Z);

    if IsObjectBlock then
       glTranslatef(-AX,-AZ,AY);
  end;
end;

procedure TCustomBlock.Dragged;
begin
  if Assigned(FOnDragging) then
     FOnDragging(Self);
end;

procedure TCustomBlock.CheckOnShow;
begin
  if not IShown then
  begin
    if Assigned(FOnShow) then
       FOnShow(Self);

    if Assigned(Parent) and Assigned(Parent.IParent) then
       Parent.IParent.Perform(CM_BLOCKBIRTH,ObjectToTag(Self),0);

    IShown:=True;
  end;
end;

procedure TCustomBlock.DrawBlock(AItems:TBlocks);
begin
  IBlocks:=AItems;
  ICanvas:=AItems.ICanvas;
  DrawBlock;
  IBlocks:=nil;
end;

procedure TCustomBlock.DrawBlock;
var Old : TPoint3DFloat;
    tmp : Boolean;
begin
  CheckOnShow;

  Format.Start;

  tmp:=Assigned(FRotation) and FRotation.FFaceToViewer;

  if tmp then
  begin
    ICanvas.DisableRotation;

    Old:=Location.Point;
    Location.Point.X:=Parent.Parent.Width*0.5-Old.X-(Size.Point.X*0.5);
    Location.Point.Y:=0;
    Location.Point.Z:=-(Parent.Parent.Height*0.5-Old.Z-(Size.Point.Z*0.5));
  end;

  StartTransform;
  Draw;
  EndTransform;

  if tmp then
  begin
    ICanvas.EnableRotation;
    Location.Point:=Old;
  end;

  Format.Finish;
end;

procedure TCustomBlock.Repaint;
begin
  if Assigned(IBlocks) then
     IBlocks.Repaint;
end;

function TCustomBlock.GetBlockIndex:Integer;
begin
  result:=IBlocks.IList.IndexOf(Self);
end;

procedure TCustomBlock.SetBlockIndex(AIndex:Integer);
begin
  // TODO
end;

function TCustomBlock.GetBounds:TBlockBounds;
begin
  if not Assigned(FBounds) then
  begin
    FBounds:=TBlockBounds.Create;
    FBounds.IBlock:=Self;
  end;

  result:=FBounds;
end;

function TCustomBlock.GetCursor:TCursor;
var tmp : TCustomBlock;
begin
  result:=FCursor;

  if result=crDefault then
  begin
    tmp:=Self;

    repeat
      if Assigned(tmp.IBlocks) then
      begin
        tmp:=tmp.IBlocks.IObject;

        if Assigned(tmp) then
        begin
          result:=tmp.Cursor;

          if result<>crDefault then
             break
        end
        else
          break;
      end
      else
        break;
        
    until not Assigned(tmp);
  end;
end;

procedure TCustomBlock.SetBlockBounds(const Value:TBlockBounds);
begin
 // Do nothing
end;

procedure TCustomBlock.SetBlocks(const Value: TBlocks);
begin
  if IBlocks<>Value then
  begin
    if Assigned(IBlocks) then
       IBlocks.IList.Remove(Self);

    IBlocks:=Value;

    if Assigned(IBlocks) then
       IBlocks.IList.Add(Self);
  end;
end;

type
  TBrushAccess=class(TTeeBrush);

procedure TCustomBlock.SetFormatting(const APen:TTeePen; const ABrush:TTeeBrush;
                                     SetBrushImage:Boolean;
                                     AColor:TColor; ATransparency:Byte);
begin
  Format.FColor:=AColor;
  Format.Transparency:=ATransparency;

  with Format.Border do
  begin
    FColor:=APen.Color;
    FStyle:=APen.Style;

    //FTransp:=APen.Transparency;

    FVisible:=APen.Visible;
    FWidth:=APen.Width;
  end;

  if SetBrushImage then
     if TBrushAccess(ABrush).HasImage then
        Format.Texture.Picture.Assign(ABrush.Image)
     else
        Format.Texture.Picture:=nil;
end;

Function TCustomBlock.GetOwner:TPersistent;
begin
  if csWriting in ComponentState then
     result:=Self
  else
     result:=inherited GetOwner;
end;

procedure TCustomBlock.SetParentComponent(AParent: TComponent);
const
  PropName='Blocks';

var tmpOb : TObject;
begin
  if AParent is TBlocks then
     Parent:=TBlocks(AParent)
  else
  if AParent is TObjectBlock then
     Parent:=TObjectBlock(AParent).Items
  else
  begin
    tmpOb:=TBlocks.DoGetObjectProp(AParent, PropName);

    if tmpOb is TBlocks then
       Parent:=TBlocks(tmpOb);
  end;
end;

procedure TCustomBlock.PrepareForGallery;
begin
end;

procedure TCustomBlock.ReadState(Reader: TReader);
begin
  inherited;

  if Reader.Parent is TBlocks then
     Parent:=TBlocks(Reader.Parent)
  else
  if Reader.Parent is TCustomObjectBlock then
     Parent:=TCustomObjectBlock(Reader.Parent).Items;
end;

procedure TCustomBlock.SetTitle(const Value: String);
begin
  if FTitle<>Value then
  begin
    FTitle:=Value;
    Repaint;
  end;
end;

procedure TCustomBlock.SetRotation(const Value: TRotationXYZ);
begin
  if Assigned(Value) then
     Rotation.Assign(Value)
  else
     FreeAndNil(FRotation);

  Repaint;
end;

procedure TCustomBlock.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Repaint;
end;

function TCustomBlock.UsesDepth:Boolean;
begin
  result:=True;
end;

function TCustomBlock.TitleOrName:String;
begin
  result:=Title;

  if result='' then
  begin
    result:=Name;

    if result='' then
       result:=ClassName;
  end;
end;

procedure TCustomBlock.Assign(Source: TPersistent);
begin
  if Source is TCustomBlock then
  with TCustomBlock(Source) do
  begin
    Self.SetActions(FActions);
    Self.SetRotation(FRotation);
    Self.FTitle:=Title;
    Self.Tile:=FTile;
    Self.FVisible:=Visible;
    Self.Format.Assign(Format);
    Self.Scale:=FScale;

    Self.DeleteLists;
  end;

  inherited;
end;

function TCustomBlock.GetScale:TPointXYZFloat;
begin
  if not Assigned(FScale) then
     FScale:=TPointXYZFloat.Create(Self,1);

  result:=FScale;
end;

procedure TCustomBlock.SetScale(const Value: TPointXYZFloat);
begin
  if Assigned(Value) then
     Scale.Assign(Value)
  else
     FreeAndNil(FScale);
end;

function TCustomBlock.GetTile:TTile;
begin
  if not Assigned(FTile) then
     FTile:=TTile.Create(Self,1);

  result:=FTile;
end;

procedure TCustomBlock.SetTile(const Value: TTile);
begin
  if Assigned(Value) then
  begin
    Tile.Assign(Value);
    FTile.SetOffset(Value.FOffset);
  end
  else
  begin
    if Assigned(FTile) then
    begin
      FreeAndNil(FTile.FOffset);
      FreeAndNil(FTile);
    end;
  end;

  Repaint;
end;

{ TCustomCoverBlock }

Destructor TCustomCoverBlock.Destroy;
begin
  FBrush2.Free;
  FBrush1.Free;
  inherited;
end;

procedure TCustomCoverBlock.Set1(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Brush1.Assign(Value)
  else
     FreeAndNil(FBrush1);

  Repaint;
end;

procedure TCustomCoverBlock.Set2(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Brush2.Assign(Value)
  else
     FreeAndNil(FBrush2);

  Repaint;
end;

function TCustomCoverBlock.Get1:TBlockFormat;
begin
  if not Assigned(FBrush1) then
     FBrush1:=TBlockFormat.Create(Self);

  result:=FBrush1;
end;

function TCustomCoverBlock.Get2:TBlockFormat;
begin
  if not Assigned(FBrush2) then
     FBrush2:=TBlockFormat.Create(Self);

  result:=FBrush2;
end;

function TCustomCoverBlock.HasBrush1: Boolean;
begin
  result:=Assigned(FBrush1);
end;

function TCustomCoverBlock.HasBrush2: Boolean;
begin
  result:=Assigned(FBrush2);
end;

procedure TCustomCoverBlock.Assign(Source: TPersistent);
begin
  if Source is TCustomCoverBlock then
  begin
    Self.Brush1:=TCustomCoverBlock(Source).FBrush1;
    Self.Brush2:=TCustomCoverBlock(Source).FBrush2;
  end;

  inherited;
end;

{ TEllipsoidBlock }

Constructor TEllipsoidBlock.Create(AOwner: TComponent);
begin
  inherited;
  FSides:=32;
  FStacks:=32;
  FTotal:=100;
end;

Destructor TEllipsoidBlock.Destroy;
begin
  FCover.Free;
  inherited;
end;

procedure TEllipsoidBlock.Assign(Source:TPersistent);
begin
  if Source is TEllipsoidBlock then
  with TEllipsoidBlock(Source) do
  begin
    Self.SetCover(FCover);
    Self.FEccentricity:=FEccentricity;
    Self.FSides:=FSides;
    Self.FStacks:=FStacks;
    Self.FTotal:=FTotal;
    Self.FTotalAngle:=FTotalAngle;
  end;

  inherited;
end;

procedure TEllipsoidBlock.DeleteLists;
begin
  inherited;

  DeleteList(IListSphere);
  DeleteList(IListCover);
end;

function TEllipsoidBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(0.5,1,0,'Total,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(1,1,0.5,'TotalAngle,MinMax:0;360,Invert').Format.Color:=clAqua;
  end;
end;

function TEllipsoidBlock.IsTotalStored:Boolean;
begin
  result:=FTotal<>100;
end;

procedure TEllipsoidBlock.SetCover(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Cover.Assign(Value)
  else
     FreeAndNil(FCover);

  Repaint;
end;

procedure TEllipsoidBlock.SetEccentricity(const Value: Double);
begin
  FEccentricity:=Value;
  DeleteLists;
end;

procedure TEllipsoidBlock.SetSides(const Value: Integer);
begin
  FSides := Value;
  DeleteLists;
end;

procedure TEllipsoidBlock.SetStacks(const Value: Integer);
begin
  FStacks := Value;
  DeleteLists;
end;

procedure TEllipsoidBlock.SetTotal(const Value: Double);
begin
  FTotal := Value;
  DeleteLists;
end;

procedure TEllipsoidBlock.SetTotalAngle(const Value: Double);
begin
  FTotalAngle := Value;
  DeleteLists;
end;

function TEllipsoidBlock.GetCover:TBlockFormat;
begin
  if not Assigned(FCover) then
     FCover:=TBlockFormat.Create(Self);

  result:=FCover;
end;

function TEllipsoidBlock.HasCover: Boolean;
begin
  result:=Assigned(FCover);
end;

procedure TEllipsoidBlock.Draw;
var
  tmpTotalAngle : Double;

  procedure CalcLists;
  var
    tmpS : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    tmpInvN : Single;
    tmpSinCos : packed Array of TPointSinCos;

    procedure CalcListSphere;
    var i,j   : Integer;
        ex    : Double;
        ez    : Double;

        tmpEcc,
        tmpXText,
        tmpYText,
        tmpYTextBis : Double;

        s1,c1 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        s2,c2 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        s3,c3 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    begin
      SinCos(-HalfPi, s1, c1);

      tmpYText:=1;

      for j:=1 to FStacks do
      begin
        SinCos(-HalfPi + j*tmpS, s2, c2);

        if Eccentricity>0 then
        begin
          tmpEcc:=2*(1-Eccentricity)*j/(FStacks);
          c2:=c2*tmpEcc;
        end;

        tmpYTextBis:=1- j/FStacks;

        glBegin(GL_QUAD_STRIP);

        for i:=0 to FSides do
        begin
          tmpXText:=1- i*tmpInvN;

          s3:=tmpSinCos[i].X;
          c3:=tmpSinCos[i].Y;

          ex:=c1 * c3;
          ez:=c1 * s3;

          glNormal3f(ex,s1,ez);
          Format.Texture.Coord(tmpXText,tmpYText);
          glVertex3f(ex,s1,ez);

          ex:=c2 * c3;
          ez:=c2 * s3;

          glNormal3f(ex,s2,ez);
          Format.Texture.Coord(tmpXText,tmpYTextBis);
          glVertex3f(ex,s2,ez);
        end;

        glEnd;

        s1:=s2;
        c1:=c2;

        tmpYText:=tmpYTextBis;
      end;
    end;

    procedure CalcListCover;
    var ex    : Double;
        ez    : Double;
        s2,c2 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        t     : Integer;
    begin
      glBegin(GL_TRIANGLE_FAN);
      glNormal3i(0,0,1);

      SinCos(-HalfPi + FStacks * tmpS, s2, c2);

      glVertex3f(0,s2,0);

      for t:=0 to FSides do
      begin
        with tmpSinCos[t] do
        begin
          ex:=c2*Y;
          ez:=c2*X;
        end;

        Format.Texture.Coord(ex,ez);
        glVertex3f(ex,s2,ez);
      end;

      glEnd;
    end;

  var t : Integer;
  begin
    tmpS:=Pi / (FStacks/(FTotal*0.01));
    tmpInvN:=1 / (FSides/(tmpTotalAngle/360));

    // Precalc sin & cos:
    SetLength(tmpSinCos,FSides+1);

    for t:=0 to FSides do
    with tmpSinCos[t] do
         SinCos(t*tmpInvN*TwoPi, X, Y);

    IListSphere:=glGenLists(1);
    glNewList(IListSphere,GL_COMPILE);
    CalcListSphere;
    glEndList;

    IListCover:=glGenLists(1);
    glNewList(IListCover,GL_COMPILE);
    CalcListCover;
    glEndList;

    tmpSinCos:=nil;
  end;

var OldStyle : TTeeCanvasSurfaceStyle;
    CanCull  : Boolean;
begin
  if FTotalAngle=0 then
     tmpTotalAngle:=360
  else
     tmpTotalAngle:=FTotalAngle;

  if IListSphere=0 then
     CalcLists;

  CanCull:=(not ShouldDrawInterior) and (FTotal=100) and (tmpTotalAngle=360);

  if Format.Solid then
  begin
    if CanCull then
       glEnable(GL_CULL_FACE);

    glCallList(IListSphere);

    if (FTotal<>100) and Assigned(FCover) and FCover.Solid then
    begin
      Format.Finish;
      FCover.Start;

      glCallList(IListCover);

      FCover.Finish;
      Format.Start;
    end;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if Format.PreparePen then
  begin
  {$IFDEF BLOCKS}
    OldStyle:=ICanvas.getDrawStyle;
    ICanvas.setDrawStyle(tcsWire);
  {$ELSE}
    OldStyle:=ICanvas.DrawStyle;
    ICanvas.DrawStyle:=tcsWire;
  {$ENDIF}

    glCallList(IListSphere);

  {$IFDEF BLOCKS}
    ICanvas.setDrawStyle(OldStyle);
  {$ELSE}
    ICanvas.DrawStyle:=OldStyle;
  {$ENDIF}
    Format.FinishPen;
  end;
end;

function TCustomBlock.BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean;
var tmp : Single;
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
end;

procedure TCustomBlock.CalcBounds(var AMin,AMax:TPoint3DFloat);
var
  Matrix : TMatrix4d;

  procedure Inflate(const x,y,z:Double);
  begin
    with InternalProjectPoint(Matrix,x,y,z) do
    begin
      if x<AMin.x then Amin.x:=x else
      if x>Amax.x then Amax.x:=x;

      if y<Amin.y then Amin.y:=y else
      if y>Amax.y then Amax.y:=y;

      if z<Amin.z then Amin.z:=z else
      if z>Amax.z then Amax.z:=z;
    end;
  end;

begin
  ModelMatrix(Matrix);

  AMin:=InternalProjectPoint(Matrix,1,1,1);
  AMax:=AMin;

  Inflate(1,1,-1);
  Inflate(1,-1,1);
  Inflate(1,-1,-1);
  Inflate(-1,1,1);
  Inflate(-1,1,-1);
  Inflate(-1,-1,1);
  Inflate(-1,-1,-1);

  glPopMatrix;
end;

procedure TCustomBlock.Clear;
begin
end;

function TCustomBlock.Clone:TCustomBlock;
var tmp : TBlockClass;
begin
  tmp:=TBlockClass(ClassType);
  result:=tmp.Create(Owner);
  result.Assign(Self);
end;

procedure TCustomBlock.EndTransform;
begin
  glPopMatrix;
end;

procedure TCustomBlock.InitTitle(const Prefix:String);
var tmpSt : String;
    tmp2  : String;
    tmpC  : Integer;
begin
  tmpSt:=RemoveAmpersand(Prefix);

  if Assigned(Parent) and Parent.IList.Exists(tmpSt) then
  begin
    tmpC:=0;

    repeat
      tmp2:=tmpSt+' '+TeeStr(tmpC);
      Inc(tmpC);
    until not Parent.IList.Exists(tmp2);
  end
  else tmp2:=tmpSt;

  Title:=tmp2;
end;

{ TCubeSides }

Constructor TCubeSides.Create(AOwner: TCustomBlock);
begin
  inherited Create;
  IOwner:=AOwner;
end;

Destructor TCubeSides.Destroy;
begin
  FFront.Free;
  FBack.Free;
  FBottom.Free;
  FRight.Free;
  FTop.Free;
  FLeft.Free;

  inherited;
end;

procedure TCubeSides.Assign(Source:TPersistent);
begin
  if Source is TCubeSides then
  with TCubeSides(Source) do
  begin
    Self.SetLeft(FLeft);
    Self.SetTop(FTop);
    Self.SetRight(FRight);
    Self.SetBottom(FBottom);
    Self.SetBack(FBack);
    Self.SetFront(FFront);
  end
  else
    inherited;
end;

function TCubeSides.IsLeftStored:Boolean;
begin
  result:=Assigned(FLeft);
end;

function TCubeSides.IsTopStored:Boolean;
begin
  result:=Assigned(FTop);
end;

function TCubeSides.IsRightStored:Boolean;
begin
  result:=Assigned(FRight);
end;

function TCubeSides.IsBottomStored:Boolean;
begin
  result:=Assigned(FBottom);
end;

function TCubeSides.IsBackStored:Boolean;
begin
  result:=Assigned(FBack);
end;

function TCubeSides.IsFrontStored:Boolean;
begin
  result:=Assigned(FFront);
end;

procedure TCubeSides.FormatChanged(Sender:TObject);
begin
  IOwner.DeleteLists;
end;

function TCubeSides.CreateFormat:TBlockFormat;
begin
  result:=TBlockFormat.Create(IOwner);
  result.IChanged:=FormatChanged;
  IAnyUsed:=True;
end;

function TCubeSides.GetLeft:TBlockFormat;
begin
  if not Assigned(FLeft) then
     FLeft:=CreateFormat;

  result:=FLeft;
end;

function TCubeSides.GetTop:TBlockFormat;
begin
  if not Assigned(FTop) then
     FTop:=CreateFormat;

  result:=FTop;
end;

function TCubeSides.GetRight:TBlockFormat;
begin
  if not Assigned(FRight) then
     FRight:=CreateFormat;

  result:=FRight;
end;

function TCubeSides.GetBottom:TBlockFormat;
begin
  if not Assigned(FBottom) then
     FBottom:=CreateFormat;

  result:=FBottom;
end;

function TCubeSides.GetBack:TBlockFormat;
begin
  if not Assigned(FBack) then
     FBack:=CreateFormat;

  result:=FBack;
end;

function TCubeSides.GetFront:TBlockFormat;
begin
  if not Assigned(FFront) then
     FFront:=CreateFormat;

  result:=FFront;
end;

function TCubeSides.AllVisible:Boolean;
begin
  result:=not IAnyUsed;

  if not result then
     result:=( (not Assigned(FLeft)) or FLeft.Solid ) and
             ( (not Assigned(FTop)) or FTop.Solid ) and
             ( (not Assigned(FRight)) or FRight.Solid ) and
             ( (not Assigned(FBottom)) or FBottom.Solid ) and
             ( (not Assigned(FBack)) or FBack.Solid ) and
             ( (not Assigned(FFront)) or FFront.Solid );
end;

procedure TCubeSides.FreeAndCheck(var ASide:TBlockFormat);
begin
  FreeAndNil(ASide);

  IAnyUsed:=Assigned(FLeft) or Assigned(FTop) or Assigned(FRight)
            or Assigned(FBottom) or Assigned(FBack) or Assigned(FFront);

  IOwner.Repaint;
end;

procedure TCubeSides.SetLeft(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Left.Assign(Value)
  else
     FreeAndCheck(FLeft);
end;

procedure TCubeSides.SetTop(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Top.Assign(Value)
  else
     FreeAndCheck(FTop);
end;

function TCubeSides.SideOf(Index: Integer): TBlockFormat;
begin
  case Index of
    0: result:=FLeft;
    1: result:=FTop;
    2: result:=FRight;
    3: result:=FBottom;
    4: result:=FFront;
  else
    result:=FBack;
  end;
end;

procedure TCubeSides.SetRight(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Right.Assign(Value)
  else
     FreeAndCheck(FRight);
end;

procedure TCubeSides.SetBottom(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Bottom.Assign(Value)
  else
     FreeAndCheck(FBottom);
end;

procedure TCubeSides.SetBack(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Back.Assign(Value)
  else
     FreeAndCheck(FBack);
end;

procedure TCubeSides.SetFront(const Value:TBlockFormat);
begin
  if Assigned(Value) then
     Front.Assign(Value)
  else
     FreeAndCheck(FFront);
end;

{ TCubeBlock }

Constructor TCubeBlock.Create(AOwner: TComponent);
begin
  inherited;
  FSides:=TCubeSides.Create(Self);
end;

Destructor TCubeBlock.Destroy;
begin
  FSides.Free;
  inherited;
end;

procedure TCubeBlock.SetSides(const Value:TCubeSides);
begin
  FSides.Assign(Value);
end;

procedure TCubeBlock.Draw;
var
  OldResult : Boolean;
  IOld : TBlockFormat;

  procedure FinishOld;
  begin
    if Assigned(IOld) then
    begin
      glDisable(TGLCanvasAccess(ICanvas).GLTextureStyle);
      IOld.Finish;
    end;
  end;

  function PrepareFormat(AFormat:TBlockFormat):Boolean;
  begin
    if not Assigned(AFormat) then
       AFormat:=Format;

    result:=AFormat.Solid;

    if result and (AFormat<>IOld) then
    begin
      FinishOld;
      AFormat.Start;
      IOld:=AFormat;
    end;
  end;

  function PreparePen(AFormat:TBlockFormat):Boolean;
  begin
    if not Assigned(AFormat) then
       AFormat:=Format;

    if AFormat<>IOld then
    begin
      result:=AFormat.PreparePen;

      if result then
         IOld:=AFormat;
    end
    else
      result:=OldResult;
  end;

  procedure DrawBorders;
  begin
    IOld:=nil;

    OldResult:=PreparePen(Format);

    if PreparePen(FSides.FBack) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( -1, 1, -1);
        glVertex3s( 1, 1, -1);
        glVertex3s( 1, -1, -1);
        glVertex3s( -1, -1, -1);
      glEnd;
    end;

    if PreparePen(FSides.FFront) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( -1, -1, 1);
        glVertex3s( -1, 1, 1);
        glVertex3s( 1, 1, 1);
        glVertex3s( 1, -1, 1);
      glEnd;
    end;

    if PreparePen(FSides.FRight) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( 1, -1, -1);
        glVertex3s( 1, -1, 1);
        glVertex3s( 1, 1, 1);
        glVertex3s( 1, 1, -1);
      glEnd;
    end;

    if PreparePen(FSides.FLeft) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( -1, -1, -1);
        glVertex3s( -1, 1, -1);
        glVertex3s( -1, 1, 1);
        glVertex3s( -1, -1, 1);
      glEnd;
    end;

    if PreparePen(FSides.FBottom) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( 1, -1, 1);
        glVertex3s( 1, -1, -1);
        glVertex3s( -1, -1, -1);
        glVertex3s( -1, -1, 1);
      glEnd;
    end;

    if PreparePen(FSides.FTop) then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex3s( 1, 1, 1);
        glVertex3s( 1, 1, -1);
        glVertex3s( -1, 1, -1);
        glVertex3s( -1, 1, 1);
      glEnd;
    end;
  end;

var CanCull: Boolean;
begin
  IOld:=Format;

  CanCull:=(not ShouldDrawInterior) and FSides.AllVisible;

  if CanCull then
     glEnable(GL_CULL_FACE);

  if PrepareFormat(FSides.FBack) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i( 0, 0, -1);
     Coord(1,0);
     glVertex3s( -1, 1, -1);
     Coord(0,0);
     glVertex3s( 1, 1, -1);
     Coord(0,1);
     glVertex3s( 1, -1, -1);
     Coord(1,1);
     glVertex3s( -1, -1, -1);
    glEnd;
  end;

  if PrepareFormat(FSides.FLeft) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i(-1,  0,  0);
     Coord(1,1);
     glVertex3s( -1, -1, 1);
     Coord(1,0);
     glVertex3s( -1, 1, 1);
     Coord(0,0);
     glVertex3s( -1, 1, -1);
     Coord(0,1);
     glVertex3s( -1, -1, -1);
    glEnd;
  end;

  if PrepareFormat(FSides.FFront) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i( 0, 0, 1);
     Coord(1,1);
     glVertex3s( 1, -1, 1);
     Coord(1,0);
     glVertex3s( 1, 1, 1);
     Coord(0,0);
     glVertex3s( -1, 1, 1);
     Coord(0,1);
     glVertex3s( -1, -1, 1);
    glEnd;
  end;

  if PrepareFormat(FSides.FRight) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i( 1,  0,  0);
     Coord(1,0);
     glVertex3s( 1, 1, -1);
     Coord(0,0);
     glVertex3s( 1, 1, 1);
     Coord(0,1);
     glVertex3s( 1, -1, 1);
     Coord(1,1);
     glVertex3s( 1, -1, -1);
    glEnd;
  end;

  if PrepareFormat(FSides.FBottom) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i( 0, -1,  0);
     Coord(0,0);
     glVertex3s( -1,  -1, 1);
     Coord(0,1);
     glVertex3s( -1,  -1, -1);
     Coord(1,1);
     glVertex3s( 1, -1, -1);
     Coord(1,0);
     glVertex3s( 1, -1, 1);
    glEnd;
  end;

  if PrepareFormat(FSides.FTop) then
  with IOld.Texture do
  begin
    glBegin(GL_QUADS);
    glNormal3i( 0, 1,  0);
     Coord(1,0);
     glVertex3s(  1, 1, -1);
     Coord(0,0);
     glVertex3s( -1, 1, -1);
     Coord(0,1);
     glVertex3s( -1, 1, 1);
     Coord(1,1);
     glVertex3s(  1, 1, 1);
    glEnd;
  end;

  if CanCull then
     glDisable(GL_CULL_FACE);

  if IOld<>Format then
  begin
    FinishOld;

    Format.Start;
  end;

  if not Parent.DrawBlocks.Shadows.Visible then
  begin
    if IListPen=0 then
    begin
      IListPen:=CreateNewList;
      DrawBorders;
      glEndList;
    end
    else
      glCallList(IListPen);

    if OldResult then
       Format.FinishPen;
  end;
end;

procedure TCubeBlock.Assign(Source: TPersistent);
begin
  if Source is TCubeBlock then
  begin
    Sides:=TCubeBlock(Source).FSides;
  end;

  inherited;
end;

procedure TCubeBlock.DeleteLists;
begin
  inherited;
  DeleteList(IListPen);
end;

{ TSphereBlock }

Constructor TSphereBlock.Create(AOwner: TComponent);
begin
  inherited;
  Radius:=Size.X;
end;

function TSphereBlock.GetRadius:Double;
begin
  result:=Size.Point.X;
end;

procedure TSphereBlock.SetRadius(const Value: Double);
begin
  Size.Point.X:=Value;
  Size.Point.Y:=Value;
  Size.Point.Z:=Value;

  Repaint;
end;

{ TEllipseBlock }

Constructor TEllipseBlock.Create(AOwner: TComponent);
begin
  inherited;
  FSlices:=90;
end;

procedure TEllipseBlock.SetSlices(const Value:Integer);
begin
  FSlices:=Value;
  DeleteLists;
end;

procedure TEllipseBlock.PrepareForGallery;
begin
  inherited;

  Size.Point.Y:=0;
end;

procedure TEllipseBlock.ReadState(Reader: TReader);
begin
  inherited;

  Size.Point.Y:=0;
end;

procedure TEllipseBlock.Draw;
var PiStep : Double;
    t : Integer;
    tmpSinCos : packed Array of TPointSinCos;
begin
  if (IListSolid=0) or (IListBorder=0) then
  begin
    // Precalc Sin & Cos
    SetLength(tmpSinCos,Slices+1);
    PiStep:=TwoPi/Slices;

    for t:=0 to Slices do
    with tmpSinCos[t] do
        SinCos(t*piStep,X,Y);
  end;

  if Format.Solid then
  begin
    if IListSolid=0 then
    begin
      IListSolid:=CreateNewList;

      glBegin(GL_TRIANGLE_FAN);

      glNormal3i(0,0,1);
      Format.Texture.Coord(0.5,0.5);
      glVertex3f(0,0,0);

      for t:=0 to Slices do
      with tmpSinCos[t] do
      begin
        Format.Texture.Coord((X+1)*0.5,1-(Y+1)*0.5);
        glVertex2f(X,Y);
      end;

      glEnd;

      glEndList;
    end
    else
      glCallList(IListSolid);
  end;

  if Format.PreparePen then
  begin
    if IListBorder=0 then
    begin
      IListBorder:=CreateNewList;

      glBegin(GL_LINE_LOOP);

      for t:=0 to Slices-1 do
      with tmpSinCos[t] do
           glVertex2f(X,Y);

      glEnd;

      glEndList;
    end
    else
      glCallList(IListBorder);
  end;

  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Ellipse');
  {$ENDIF}

  tmpSinCos:=nil;
end;

procedure TEllipseBlock.Assign(Source: TPersistent);
begin
  if Source is TEllipseBlock then
     FSlices:=TEllipseBlock(Source).FSlices;

  inherited;
end;

procedure TEllipseBlock.DeleteLists;
begin
  inherited;

  DeleteList(IListSolid);
  DeleteList(IListBorder);
end;

{ TConeBlock }

Constructor TConeBlock.Create(AOwner: TComponent);
begin
  inherited;

  FConeSize:=TPointXYFloat.Create(Self,100,ConeSizeChanged);
  FStacks:=32;
end;

Destructor TConeBlock.Destroy;
begin
  FConeSize.Free;
  inherited;
end;

procedure TConeBlock.ConeSizeChanged(Sender:TObject);
begin
  DeleteLists;
end;

function TConeBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(0.5,1,0,'ConeSize.X,MinMax:0;100,Invert').Format.Color:=clYellow;
    AddHandle(1,1,0.5,'ConeSize.Y,MinMax:0;100,Invert').Format.Color:=clAqua;
  end;
end;

procedure TConeBlock.SetConeSize(const Value: TPointXYFloat);
begin
  FConeSize.Assign(Value);
end;

procedure TConeBlock.Draw;
begin
  if (IConeX<>FConeSize.X) or (IConeY<>FConeSize.Y) then
  begin
    DeleteLists;

    IConeX:=FConeSize.X;
    IConeY:=FConeSize.Y;
  end;

  inherited;
end;

procedure TConeBlock.Assign(Source: TPersistent);
begin
  if Source is TConeBlock then
  begin
    ConeSize:=TConeBlock(Source).FConeSize;
  end;

  inherited;
end;

{ TLightBlock }
Constructor TLightBlock.Create(AOwner: TComponent);
begin
  inherited;

  ShowLamp:=True;
  FSpot:=45;
  FSpotExp:=14;
  FUseDirection:=True;
  FDiffuse:=clWhite;
  FSpecular:=clWhite;
end;

Destructor TLightBlock.Destroy;
begin
  if Assigned(ILamp) then
  begin
    ILamp.IBlocks:=nil;
    ILamp.Free;
  end;

  inherited;
end;

procedure TLightBlock.Assign(Source:TPersistent);
begin
  if Source is TLightBlock then
  with TLightBlock(Source) do
  begin
    Self.FDiffuse:=FDiffuse;
    Self.FFixed:=FFixed;
    Self.ShowLamp:=ShowLamp;
    Self.FSpecular:=FSpecular;
    Self.FSpot:=FSpot;
    Self.FSpotExp:=FSpotExp;
    Self.FUseDirection:=FUseDirection;
  end;

  inherited;
end;

function TLightBlock.BoundingBox(out AMin,AMax:TPoint3DFloat):Boolean;
begin
  result:=False;
end;

procedure TLightBlock.PrepareForGallery;
begin
  inherited;
  Format.Color:=clWhite;
  ShowLamp:=True;
  Spot:=40;
end;

class procedure TLightBlock.GLColor(const AColor:TColor; var Value:GLMat);
const tmpInv=1/128.0;
begin
  Value[0]:=(1+Byte(AColor)-128)*tmpInv;
  Value[1]:=(1+Byte(AColor shr 8)-128)*tmpInv;
  Value[2]:=(1+Byte(AColor shr 16)-128)*tmpInv;
  Value[3]:=1;
end;

procedure TLightBlock.InitLight;
var tmpColor    : GLMat;
    tmpDiffuse  : GLMat;
    tmpSpecular : GLMat;
    tmpCenter   : TPoint3DFloat;
    tmpDir      : TPoint3DFloat;
begin
  if Parent.ILightNum<Parent.IMaxLights then
  begin
    ICanvas:=Parent.ICanvas;

    GLColor(Format.Color,tmpColor);
    GLColor(Diffuse,tmpDiffuse);
    GLColor(Specular,tmpSpecular);

    with Rotation.Point do
    begin
      tmpDir.X:=Sin(X*TeePiStep);
      tmpDir.Y:=-Cos(Y*TeePiStep);
      tmpDir.Z:=-Sin(Z*TeePiStep);
    end;

    with Location.Point do
    begin
      tmpCenter.X:=X;
      tmpCenter.Y:=-Y;
      tmpCenter.Z:=-Z;
    end;

    if Fixed then
       ICanvas.{$IFDEF BLOCKS}iDisableRotation{$ELSE}DisableRotation{$ENDIF};

    TGLCanvasAccess(ICanvas).InitLight(GL_LIGHT0+Parent.ILightNum,
                                      tmpColor,tmpDiffuse,tmpSpecular,
                                      tmpCenter,tmpDir,UseDirection,Spot,
                                      SpotExponent);

    if Fixed then
       ICanvas.{$IFDEF BLOCKS}iEnableRotation{$ELSE}EnableRotation{$ENDIF};

    Inc(Parent.ILightNum);
  end;
end;

procedure TLightBlock.Draw;
begin
  if ShowLamp then
  begin
    ILamp.IBlocks:=IBlocks;
    ILamp.ICanvas:=ICanvas;
    ILamp.Draw;
  end;
end;

function TLightBlock.GetShowLamp:Boolean;
begin
  result:=Assigned(ILamp) and ILamp.Visible;
end;

procedure TLightBlock.SetShowLamp(const Value:Boolean);
begin
  if not Assigned(ILamp) then
  begin
    ILamp:=TConeBlock.Create(nil);
    ILamp.Format.Border.Visible:=False;
    ILamp.IBlocks:=IBlocks;
  end;

  ILamp.Visible:=Value;
  Repaint;
end;

procedure TLightBlock.SetFixed(const Value: Boolean);
begin
  FFixed:=Value;
  Repaint;
end;

procedure TLightBlock.SetSpot(const Value: Integer);
begin
  FSpot:=Value;
  Repaint;
end;

procedure TLightBlock.SetSpotExp(const Value: Integer);
begin
  FSpotExp:=Value;
  Repaint;
end;

procedure TLightBlock.SetUseDirection(const Value: Boolean);
begin
  FUseDirection:=Value;
  Repaint;
end;

Procedure TLightBlock.SetDiffuse(const Value:TColor);
begin
  if FDiffuse<>Value then
  begin
    FDiffuse:=Value;
    Repaint;
  end;
end;

Procedure TLightBlock.SetSpecular(const Value:TColor);
begin
  if FSpecular<>Value then
  begin
    FSpecular:=Value;
    Repaint;
  end;
end;

{ TTetrahedronBlock }

procedure TTetrahedronBlock.Draw;
var CanCull : Boolean;
begin
  if Format.Solid then
  begin
    CanCull:=not ShouldDrawInterior;

    if CanCull then
       glEnable(GL_CULL_FACE);

    glBegin(GL_TRIANGLE_FAN);

    glNormal3i(0,0,1);
    glTexCoord3s(0,0,0);
    glVertex3s(-1,-1,1);

    glNormal3i(1,0,-1);
    glTexCoord3s(1,0,1);
    glVertex3s(1,-1,-1);

    glNormal3i(1,1,1);
    glTexCoord3s(1,1,0);
    glVertex3s(1,1,1);

    glNormal3i(0,1,-1);
    glTexCoord3s(0,1,1);
    glVertex3s(-1,1,-1);

    glEnd;

    glBegin(GL_TRIANGLE_FAN);

    glNormal3i(0,0,1);
    glTexCoord3s(0,0,0);
    glVertex3s(-1,-1,1);

    glNormal3i(1,1,1);
    glTexCoord3s(1,1,0);
    glVertex3s(-1,1,-1);

    glNormal3i(1,0,-1);
    glTexCoord3s(1,0,1);
    glVertex3s(1,-1,-1);

    glEnd;

    glBegin(GL_TRIANGLE_FAN);

    glNormal3i(0,0,1);
    glTexCoord3s(0,0,0);
    glVertex3s(1,1,1);

    glNormal3i(1,1,1);
    glTexCoord3s(1,1,0);
    glVertex3s(1,-1,-1);

    glNormal3i(1,0,-1);
    glTexCoord3s(1,0,1);
    glVertex3s(-1,1,-1);

    glEnd;

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if Format.PreparePen then
  begin
    glBegin(GL_LINE_STRIP);
     glVertex3s(-1,-1,1);
     glVertex3s(1,1,1);
     glVertex3s(1,-1,-1);
     glVertex3s(-1,1,-1);
     glVertex3s(-1,-1,1);
     glVertex3s(1,-1,-1);
    glEnd;

    glBegin(GL_LINES);
     glVertex3s(-1,1,-1);
     glVertex3s(1,1,1);
    glEnd;
  end;

  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Tetrahedron');
  {$ENDIF}
end;

type
  TPointXYFloatAccess=class(TPointXYFloat);

{ TTubeBlock }

Constructor TTubeBlock.Create(AOwner: TComponent);
begin
  inherited;

  TPointXYFloatAccess(FConeSize).InitDefault(50);
  FStacks:=1;
  FTube:=TBlockFormat.Create(Self);
end;

Destructor TTubeBlock.Destroy;
begin
  FTube.Free;
  inherited;
end;

procedure TTubeBlock.Assign(Source:TPersistent);
begin
  if Source is TTubeBlock then
  with TTubeBlock(Source) do
  begin
    Self.Tube:=FTube;
  end;

  inherited;
end;

procedure TTubeBlock.SetTube(const Value:TBlockFormat);
begin
  FTube.Assign(Value);
end;

procedure TTubeBlock.Draw;
var OldX : Double;
    OldY : Double;
begin
  OldX:=FConeSize.X;
  OldY:=FConeSize.Y;

  FConeSize.Point.X:=0;
  FConeSize.Point.Y:=0;

  ICoverHole.X:=OldX;
  ICoverHole.Y:=OldY;

  inherited;

  IHideCovers:=True;

  ICurrentFormat.Finish;
  ICurrentFormat:=Tube;
  Tube.Start;

  glPushMatrix;
  glScalef(OldX*0.01,1,OldY*0.01);

  inherited;

  glPopMatrix;

  Tube.Finish;

  ICurrentFormat:=Format;
  ICurrentFormat.Start;

  IHideCovers:=False;

  FConeSize.Point.X:=OldX;
  FConeSize.Point.Y:=OldY;
end;

{ TBeveledCubeBlock }

Constructor TBeveledCubeBlock.Create(AOwner: TComponent);
begin
  inherited;

  FBevelSize:=TPointXYZFloat.Create(Self,5,BevelChanged);
  FBevelSizeStyle:=bsPercent;

  FCurvePoints:=8;
  FCurveRound:=True;
end;

Destructor TBeveledCubeBlock.Destroy;
begin
  FBevelSize.Free;
  inherited;
end;

procedure TBeveledCubeBlock.BevelChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TBeveledCubeBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListBorder);
end;

procedure TBeveledCubeBlock.Assign(Source:TPersistent);
begin
  if Source is TBeveledCubeBlock then
  with TBeveledCubeBlock(Source) do
  begin
    Self.BevelSize:=FBevelSize;
    Self.FCurvePoints:=FCurvePoints;
    Self.FCurveRound:=FCurveRound;
    Self.FStyle:=FStyle;
  end;

  inherited;
end;

procedure TBeveledCubeBlock.SetBevelSize(const Value: TPointXYZFloat);
begin
  FBevelSize.Assign(Value);
end;

procedure TBeveledCubeBlock.SetBevelSizeStyle(const Value: TBevelSizeStyle);
begin
  if FBevelSizeStyle<>Value then
  begin
    FBevelSizeStyle:=Value;
    DeleteLists;
  end;
end;

procedure TBeveledCubeBlock.Draw;
var
  tmpNumPoints : Integer;

  OutX : TPointFloatArray;
  OutY : TPointFloatArray;
  OutZ : TPointFloatArray;

  ISizeX : Single;
  ISizeY : Single;
  ISizeZ : Single;

  procedure DrawStripX(const Pos1,Pos2:Single; XFac,XSub,YFac,YSub,NormalOff:Integer);
  var t : Integer;
      t1,t2,
      tmpX,tmpY,
      tmpTe : Single;
      tmpA,
      tmpB,
      tmpC : TPoint3DFloat;
  begin
    t1:=ISizeZ;

    tmpTe:=(ISizeY+ISizeZ)/(tmpNumPoints+1);

    for t:=0 to tmpNumPoints do
    begin
      if (t>0) and ((t-NormalOff)<=tmpNumPoints) then
      begin
        tmpA:=PointFloat(Pos1, XFac+XSub*OutX[t-1].X,         YFac+YSub*OutX[t-1].Y);
        tmpB:=PointFloat(Pos2, XFac+XSub*OutX[t-NormalOff].X, YFac+YSub*OutX[t-NormalOff].Y);
        tmpC:=PointFloat(Pos1, XFac+XSub*OutX[t].X,           YFac+YSub*OutX[t].Y);

        with CalculateNormal(tmpA,tmpB,tmpC) do
             glNormal3f(X,Y,Z);
      end;

      tmpX:=XFac+XSub*OutX[t].X;
      tmpY:=YFac+YSub*OutX[t].Y;

      Format.Texture.Coord(0.5+0.5*Pos1,t1);
      glVertex3f( Pos1, tmpX, tmpY);

      t2:=t1+tmpTe;

      Format.Texture.Coord(0.5+0.5*Pos2,t2);
      glVertex3f( Pos2, tmpX,tmpY);

      t1:=t2;
    end;
  end;

  procedure DrawStripZ(const Pos1,Pos2:Single; XFac,XSub,YFac,YSub,NormalOff:Integer);
  var t : Integer;
      t1,t2,
      tmpX,tmpY,
      tmpTe : Single;
      tmpA,
      tmpB,
      tmpC : TPoint3DFloat;
  begin
    t1:=1-ISizeX;

    tmpTe:=(ISizeX+ISizeY)/(tmpNumPoints+1);

    for t:=0 to tmpNumPoints+NormalOff do
    begin
      if (t>0) and ((t-NormalOff)<=tmpNumPoints) then
      begin
        tmpA:=PointFloat(XFac+XSub*OutZ[t-1].X,         YFac+YSub*OutZ[t-1].Y, Pos1);
        tmpB:=PointFloat(XFac+XSub*OutZ[t-NormalOff].X, YFac+YSub*OutZ[t-NormalOff].Y, Pos2);
        tmpC:=PointFloat(XFac+XSub*OutZ[t].X,           YFac+YSub*OutZ[t].Y, Pos1);

        with CalculateNormal(tmpA,tmpB,tmpC) do
             glNormal3f(X,Y,Z);
      end;

      tmpX:=XFac+XSub*OutZ[t].X;
      tmpY:=YFac+YSub*OutZ[t].Y;

      Format.Texture.Coord(0.5+0.5*Pos1,t1);
      glVertex3f( tmpX, tmpY, Pos1);

      t2:=t1-tmpTe;

      Format.Texture.Coord(0.5+0.5*Pos2,t2);
      glVertex3f( tmpX, tmpY, Pos2);

      t1:=t2;
    end;
  end;

  procedure DrawStripY(const Pos1,Pos2:Single; XFac,XSub,YFac,YSub,NormalOff:Integer);
  var t : Integer;
      t1,t2,
      tmpX,tmpY,
      tmpTe : Single;
      tmpA,
      tmpB,
      tmpC : TPoint3DFloat;
  begin
    t1:=ISizeZ;

    tmpTe:=(ISizeX+ISizeZ)/(tmpNumPoints+1);

    for t:=0 to tmpNumPoints+NormalOff do
    begin
      if (t>0) and ((t-NormalOff)<=tmpNumPoints) then
      begin
        tmpA:=PointFloat(XFac+XSub*OutY[t-1].X,         Pos1, YFac+YSub*OutY[t-1].Y);
        tmpB:=PointFloat(XFac+XSub*OutY[t-NormalOff].X, Pos2, YFac+YSub*OutY[t-NormalOff].Y);
        tmpC:=PointFloat(XFac+XSub*OutY[t].X,           Pos1, YFac+YSub*OutY[t].Y);

        with CalculateNormal(tmpA,tmpB,tmpC) do
             glNormal3f(X,Y,Z);
      end;

      tmpX:=XFac+XSub*OutY[t].X;
      tmpY:=YFac+YSub*OutY[t].Y;

      Format.Texture.Coord(t1, 0.5+0.5*Pos2);
      glVertex3f( tmpX, Pos1, tmpY );

      t2:=t1+tmpTe;

      Format.Texture.Coord(t2, 0.5+0.5*Pos1);
      glVertex3f( tmpX, Pos2, tmpY);

      t1:=t2;
    end;
  end;

  type
    TCornerData=record
      tmpS      : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
      tmpInvN   : Single;
      tmpSinCos : Array of TPointSinCos;
    end;

  var
    tmpCorner : TCornerData;

  procedure DrawCorner(const X,Y,Z:Single; RotateX,RotateY,RotateZ:Integer);

    procedure AddPoints;
    var i,j   : Integer;
        ex    : Single;
        ez    : Single;

        tmpXText,
        tmpYText,
        tmpYTextBis : Single;

        s1,c1 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
        s2,c2 : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    begin
      SinCos(-HalfPi, s1, c1);

      tmpYText:=0;

      with tmpCorner do
      for j:=0 to tmpNumPoints-1 do
      begin
        SinCos(-HalfPi + (j + 1) * tmpS, s2, c2);

        tmpYTextBis:= -(j+1)/tmpNumPoints;

        glBegin(GL_QUAD_STRIP);

        for i:=0 to tmpNumPoints do
        with tmpSinCos[i] do
        begin
          tmpXText:= -i*tmpInvN;

          ex:=c1 * Y;
          ez:=c1 * X;

          glNormal3f(ex,s1,ez);
          Format.Texture.Coord(tmpXText,tmpYText);
          glVertex3f(ex,s1,ez);

          ex:=c2 * Y;
          ez:=c2 * X;

          glNormal3f(ex,s2,ez);
          Format.Texture.Coord(tmpXText,tmpYTextBis);
          glVertex3f(ex,s2,ez);
        end;

        glEnd;

        s1:=s2;
        c1:=c2;

        tmpYText:=tmpYTextBis;
      end;
    end;

    procedure DrawCorner2;
    var t : Integer;
    begin
      glBegin(GL_QUAD_STRIP);

      for t:=0 to Length(OutX)-1 do
          glVertex3f(X,OutX[t].X,OutX[t].Y);

      glEnd;
    end;

  begin
    glPushMatrix;

    glTranslatef(X,Y,Z);

    with BevelSize.Point do
         glScalef(X*0.0105,Y*0.0105,Z*0.0105);

    if RotateX<>0 then
       glRotatef(RotateX,1,0,0);

    if RotateZ<>0 then
       glRotatef(RotateZ,0,0,1);

    if RotateY<>0 then
       glRotatef(RotateY,0,1,0);

    AddPoints;

    //DrawCorner2;

    glPopMatrix;
  end;

  procedure PrepareCorners;
  var t : Integer;
  begin
    with tmpCorner do
    begin
      tmpS:=HalfPi/tmpNumPoints;
      tmpInvN:=0.25/tmpNumPoints;

      // Precalc sin & cos:
      SetLength(tmpSinCos,tmpNumPoints+1);

      for t:=0 to tmpNumPoints do
      with tmpSinCos[t] do
           SinCos(t*tmpInvN*TwoPi, X, Y);
    end;
  end;

  procedure BevelBezier(const Pos1,Pos2:Single; out Output:TPointFloatArray);
  var InvCurve : Single;
      t        : Integer;
      mu       : Single;
      mum1     : Single;
  begin
    SetLength(Output,tmpNumPoints+1);

    with Output[0] do
    begin
      X:=0;
      Y:=Pos1;
    end;

    if Style=bsCut then
    begin
      Output[1].X:=Pos2;
      Output[1].Y:=Pos1;
      Output[2].X:=Pos2;
      Output[2].Y:=0;
    end
    else
    begin
      InvCurve:=1/tmpNumPoints;

      for t:=1 to tmpNumPoints do
      with Output[t] do
      begin
        mu:=t*InvCurve;
        mum1:=1-mu;

        if CurveRound then
        begin
          X:=Pos2*Sqr(mu);
          Y:=Pos1*Sqr(mum1);
        end
        else
        begin
          X:=Pos2*mu*mu*mu;
          Y:=Pos1*mum1*mum1*mum1;
        end;
      end;
    end;
  end;

var Min1SizeX,
    Min1SizeY,
    Min1SizeZ,
    OneMinSizeX,
    OneMinSizeY,
    OneMinSizeZ : Single;

  procedure DrawSides;
  begin
    // Back
    glNormal3i( 0, 0, -1);
     Format.Texture.Coord(1,0);
     glVertex3f( Min1SizeX,  OneMinSizeY, -1);
     Format.Texture.Coord(0,0);
     glVertex3f( OneMinSizeX, OneMinSizeY, -1);
     Format.Texture.Coord(0,1);
     glVertex3f( OneMinSizeX, Min1SizeY,    -1);
     Format.Texture.Coord(1,1);
     glVertex3f( Min1SizeX,  Min1SizeY,    -1);

    // Left
    glNormal3i(-1,  0,  0);
     Format.Texture.Coord(1,1);
     glVertex3f( -1, Min1SizeY,    OneMinSizeZ);
     Format.Texture.Coord(1,0);
     glVertex3f( -1, OneMinSizeY, OneMinSizeZ);
     Format.Texture.Coord(0,0);
     glVertex3f( -1, OneMinSizeY, Min1SizeZ);
     Format.Texture.Coord(0,1);
     glVertex3f( -1, Min1SizeY,    Min1SizeZ);

    // Front
    glNormal3i( 0, 0, 1);
     Format.Texture.Coord(1,1);
     glVertex3f( OneMinSizeX, Min1SizeY,    1);
     Format.Texture.Coord(1,0);
     glVertex3f( OneMinSizeX, OneMinSizeY, 1);
     Format.Texture.Coord(0,0);
     glVertex3f( Min1SizeX,  OneMinSizeY, 1);
     Format.Texture.Coord(0,1);
     glVertex3f( Min1SizeX,  Min1SizeY,    1);

    // Right
    glNormal3i( 1,  0,  0);
     Format.Texture.Coord(1,0);
     glVertex3f( 1, OneMinSizeY, Min1SizeZ);
     Format.Texture.Coord(0,0);
     glVertex3f( 1, OneMinSizeY, OneMinSizeZ);
     Format.Texture.Coord(0,1);
     glVertex3f( 1, Min1SizeY,    OneMinSizeZ);
     Format.Texture.Coord(1,1);
     glVertex3f( 1, Min1SizeY,    Min1SizeZ);

    // Bottom
    glNormal3i( 0, -1,  0);
     Format.Texture.Coord(0,0);
     glVertex3f( Min1SizeX,  -1, OneMinSizeZ);
     Format.Texture.Coord(0,1);
     glVertex3f( Min1SizeX,  -1, Min1SizeZ);
     Format.Texture.Coord(1,1);
     glVertex3f( OneMinSizeX, -1, Min1SizeZ);
     Format.Texture.Coord(1,0);
     glVertex3f( OneMinSizeX, -1, OneMinSizeZ);

    // Top
    glNormal3i( 0, 1,  0);
     Format.Texture.Coord(1,0);
     glVertex3f( OneMinSizeX, 1, Min1SizeZ);
     Format.Texture.Coord(0,0);
     glVertex3f( Min1SizeX,  1, Min1SizeZ);
     Format.Texture.Coord(0,1);
     glVertex3f( Min1SizeX,  1, OneMinSizeZ);
     Format.Texture.Coord(1,1);
     glVertex3f( OneMinSizeX, 1, OneMinSizeZ);
  end;

var
  CanCull : Boolean;
  tmp     : Single;
begin

  with FBevelSize.Point do
  if FBevelSizeStyle=bsPercentMin then
  begin
    if X<Y then
       tmp:=X
    else
       tmp:=Y;

    if Z<tmp then
       tmp:=Z;

    ISizeX:=tmp*0.01;
    ISizeY:=tmp*0.01;
    ISizeZ:=tmp*0.01;
  end
  else
  begin
    ISizeX:=X*0.01;
    ISizeY:=Y*0.01;
    ISizeZ:=Z*0.01;
  end;

  Min1SizeX:=-1+ISizeX;
  Min1SizeY:=-1+ISizeY;
  Min1SizeZ:=-1+ISizeZ;

  OneMinSizeX:=1-ISizeX;
  OneMinSizeY:=1-ISizeY;
  OneMinSizeZ:=1-ISizeZ;

  if Style=bsRound then
     tmpNumPoints:=CurvePoints
  else
  if Style=bsBevel then
     tmpNumPoints:=1
  else
     tmpNumPoints:=2;

  if Format.Solid then
  begin
    CanCull:=not ShouldDrawInterior;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if IList=0 then
    begin
      IList:=CreateNewList;

      glBegin(GL_QUADS);
      DrawSides;
      glEnd;

      BevelBezier(ISizeZ, ISizeY, OutX);
      BevelBezier(ISizeY, ISizeX, OutZ);
      BevelBezier(ISizeZ, ISizeX, OutY);

       // Rectangles X

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 0, -1, 0);  // Bottom Back
       DrawStripX(OneMinSizeX,Min1SizeX,-1,1,-1,1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 0, 1, 0);  // Top Back
       DrawStripX(Min1SizeX,OneMinSizeX,1,-1,-1,1,0);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 0, 1, 0);  // Top Front
       DrawStripX(OneMinSizeX,Min1SizeX,1,-1,1,-1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 0, -1, 0);  // Bottom Front
       DrawStripX(Min1SizeX,OneMinSizeX,-1,1,1,-1,0);
      glEnd;

       // Rectangles Z

      glBegin(GL_QUAD_STRIP);
       glNormal3i( -1, 0, 0);  // Left Bottom
       DrawStripZ(OneMinSizeZ,Min1SizeZ,-1,1,-1,1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( -1, 0, 0);  // Left Top
       DrawStripZ(Min1SizeZ,OneMinSizeZ,-1,1,1,-1,0);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 1, 0, 0);  // Right Bottom
       DrawStripZ(Min1SizeZ,OneMinSizeZ,1,-1,-1,1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 1, 0, 0);  // Right Top
       DrawStripZ(OneMinSizeZ,Min1SizeZ,1,-1,1,-1,0);
      glEnd;

       // Rectangles Y

      glBegin(GL_QUAD_STRIP);
       glNormal3i( -1, 0, 0);  // Left Back
       DrawStripY(Min1SizeY,OneMinSizeY,-1,1,-1,1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 1, 0, 0);  // Right Back
       DrawStripY(OneMinSizeY,Min1SizeY,1,-1,-1,1,-1);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( -1, 0, 0);  // Left Front
       DrawStripY(OneMinSizeY,Min1SizeY,-1,1,1,-1,0);
      glEnd;

      glBegin(GL_QUAD_STRIP);
       glNormal3i( 1, 0, 0);  // Right Front
       DrawStripY(Min1SizeY,OneMinSizeY,1,-1,1,-1,0);
      glEnd;

      // Corners

      if Style<>bsCut then
      begin
        PrepareCorners;

        // Left Back Bottom
        DrawCorner(Min1SizeX,Min1SizeY,Min1SizeZ,0,90,270);

        // Right Back Bottom
        DrawCorner(OneMinSizeX,Min1SizeY,Min1SizeZ,90,0,0);

        // Left Back Top
        DrawCorner(Min1SizeX,OneMinSizeY,Min1SizeZ,270,0,180);

        // Right Back Top
        DrawCorner(OneMinSizeX,OneMinSizeY,Min1SizeZ,0,180,180);

        // Right Front Top
        DrawCorner(OneMinSizeX,OneMinSizeY,OneMinSizeZ,270,0,0);

        // Left Front Top
        DrawCorner(Min1SizeX,OneMinSizeY,OneMinSizeZ,270,0,270);

        // Right Front Bottom
        DrawCorner(OneMinSizeX,Min1SizeY,OneMinSizeZ,270,90,0);

        // Left Front Bottom
        DrawCorner(Min1SizeX,Min1SizeY,OneMinSizeZ,270,90,270);

        tmpCorner.tmpSinCos:=nil;
      end;

      glEndList;

      OutZ:=nil;
      OutY:=nil;
      OutX:=nil;
    end
    else
      glCallList(IList);

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if Format.PreparePen then
  begin
    if IListBorder=0 then
    begin
      IListBorder:=CreateNewList;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Min1SizeX,  OneMinSizeY, -1);
        glVertex3f( OneMinSizeX, OneMinSizeY, -1);
        glVertex3f( OneMinSizeX, Min1SizeY,    -1);
        glVertex3f( Min1SizeX,  Min1SizeY,    -1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( Min1SizeX,  Min1SizeY,    1);
        glVertex3f( Min1SizeX,  OneMinSizeY, 1);
        glVertex3f( OneMinSizeX, OneMinSizeY, 1);
        glVertex3f( OneMinSizeX, Min1SizeY,    1);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( 1, Min1SizeY,    Min1SizeZ);
        glVertex3f( 1, Min1SizeY,    OneMinSizeZ);
        glVertex3f( 1, OneMinSizeY, OneMinSizeZ);
        glVertex3f( 1, OneMinSizeY, Min1SizeZ);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( -1, Min1SizeY,    Min1SizeZ);
        glVertex3f( -1, OneMinSizeY, Min1SizeZ);
        glVertex3f( -1, OneMinSizeY, OneMinSizeZ);
        glVertex3f( -1, Min1SizeY,    OneMinSizeZ);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( OneMinSizeX, -1, OneMinSizeZ);
        glVertex3f( OneMinSizeX, -1, Min1SizeZ);
        glVertex3f( Min1SizeX,  -1, Min1SizeZ);
        glVertex3f( Min1SizeX,  -1, OneMinSizeZ);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex3f( OneMinSizeX, 1, OneMinSizeZ);
        glVertex3f( OneMinSizeX, 1, Min1SizeZ);
        glVertex3f( Min1SizeX,  1, Min1SizeZ);
        glVertex3f( Min1SizeX,  1, OneMinSizeZ);
      glEnd;

      glEndList;
    end
    else
      glCallList(IListBorder);

    Format.FinishPen;
  end;

  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Beveled Cube');
  {$ENDIF}
end;

{ TBlockEdge }

Constructor TBlockEdge.Create(const AOwner:TPersistent; const AValue:Single=0;
                              const CanvasChanged:TNotifyEvent=nil);
begin
  inherited;
  FSlices:=16;
end;

function TBlockEdge.Active:Boolean;
begin
  with Point do
       result:=(X<>0) or (Y<>0);
end;

procedure TBlockEdge.SetSlices(const Value: Integer);
begin
  FSlices:=Value;
  DoChanged;
end;

procedure TBlockEdge.SetStyle(const Value:TBlockEdgeStyle);
begin
  FStyle:=Value;
  DoChanged;
end;

procedure TBlockEdge.Assign(Source: TPersistent);
begin
  if Source is TBlockEdge then
  with TBlockEdge(Source) do
  begin
    Self.FSlices:=FSlices;
    Self.FStyle:=FStyle;
  end;

  inherited;
end;

{ TSliceRoundness }

Constructor TSliceEdges.Create(AOwner: TCustomBlock; Changed:TNotifyEvent);
begin
  inherited Create;

  FOuterTop:=TBlockEdge.Create(AOwner,0,Changed);
  FOuterBottom:=TBlockEdge.Create(AOwner,0,Changed);

  FInnerTop:=TBlockEdge.Create(AOwner,0,Changed);
  FInnerBottom:=TBlockEdge.Create(AOwner,0,Changed);
end;

Destructor TSliceEdges.Destroy;
begin
  FInnerBottom.Free;
  FInnerTop.Free;
  FOuterBottom.Free;
  FOuterTop.Free;

  inherited;
end;

procedure TSliceEdges.Assign(Source: TPersistent);
begin
  if Source is TSliceEdges then
  with TSliceEdges(Source) do
  begin
    Self.OuterTop:=OuterTop;
    Self.OuterBottom:=OuterBottom;
    Self.InnerTop:=InnerTop;
    Self.InnerBottom:=InnerBottom;
  end
  else
    inherited;
end;

procedure TSliceEdges.SetOuterTop(const Value:TBlockEdge);
begin
  FOuterTop.Assign(Value);
end;

procedure TSliceEdges.SetOuterBottom(const Value:TBlockEdge);
begin
  FOuterBottom.Assign(Value);
end;

procedure TSliceEdges.SetInnerTop(const Value:TBlockEdge);
begin
  FInnerTop.Assign(Value);
end;

procedure TSliceEdges.SetInnerBottom(const Value:TBlockEdge);
begin
  FInnerBottom.Assign(Value);
end;

procedure TBeveledCubeBlock.SetCurvePoints(const Value: Integer);
begin
  FCurvePoints := Value;
  DeleteLists;
end;

procedure TBeveledCubeBlock.SetCurveRound(const Value: Boolean);
begin
  FCurveRound := Value;
  DeleteLists;
end;

procedure TBeveledCubeBlock.SetStyle(const Value: TBevelCubeStyle);
begin
  FStyle := Value;
  DeleteLists;
end;

{ TPieSliceBlock }

Constructor TPieSliceBlock.Create(AOwner: TComponent);
begin
  inherited;
  FAngle:=295;
  FStacks:=1;

  FInnerSize:=TPointXYFloat.Create(Self,0,DataChanged);
  FEdges:=TSliceEdges.Create(Self,DataChanged);
  FFormat.Border.IChanged:=DataChanged;
end;

Destructor TPieSliceBlock.Destroy;
begin
  FInnerSize.Free;
  FEdges.Free;
  inherited;
end;

procedure TPieSliceBlock.Assign(Source:TPersistent);
begin
  if Source is TPieSliceBlock then
  with TPieSliceBlock(Source) do
  begin
    Self.FAngle:=FAngle;
    Self.FStacks:=FStacks;
    Self.FDonutPercent:=FDonutPercent;
    Self.Edges:=FEdges;
    Self.InnerSize:=FInnerSize;
    Self.FStartAngle:=FStartAngle;
  end;

  inherited;
end;

procedure TPieSliceBlock.DataChanged(Sender:TObject);
begin
  DeleteLists;
end;

procedure TPieSliceBlock.SetAngle(const Value: Double);
begin
  FAngle:=Value;
  DeleteLists;
end;

procedure TPieSliceBlock.SetStacks(const Value: Double);
begin
  FStacks:=Value;
  DeleteLists;
end;

procedure TPieSliceBlock.SetDonut(const Value: Double);
begin
  FDonutPercent:=Value;
  DeleteLists;
end;

procedure TPieSliceBlock.SetEdges(const Value: TSliceEdges);
begin
  FEdges.Assign(Value);
end;

procedure TPieSliceBlock.SetStartAngle(const Value: Double);
begin
  FStartAngle:=Value;
  DeleteLists;
end;

procedure TPieSliceBlock.Draw;
var tmpXRadius : Single;
    tmpYRadius : Single;
    tmpSinCos  : Array of TPointSinCos;

  Procedure GetXY(AIndex:Integer; out AX,AY:Single);
  begin
    with tmpSinCos[AIndex] do
    begin
      AX:=tmpXRadius*X;
      AY:=tmpYRadius*Y;
    end;
  end;

var
  IDonutPercent : Single;

  procedure CalcRoundness(AnglePos:Integer; IsInner,Invert:Boolean;
                          var BezierPoints:TPoint3DArray;
                          AEdge:TBlockEdge; const AZ0,AZ1:Single;
                          const InitialLength:Integer);
  var tmpRoundX,
      InvCurve : Single;

      mu,
      mu2,
      mum1,
      mum1mu,
      mum12 : Single;

      P     : TFloatPoint;
      P1    : TPoint3DFloat;
      P2    : TPoint3DFloat;
      P3    : TPoint3DFloat;
      t     : Integer;
      tmpIndex : Integer;
  begin
    tmpRoundX:=AEdge.X*0.01;

    if IsInner then
    begin
      tmpXRadius:=1+tmpXRadius*tmpRoundX;
      tmpYRadius:=1+tmpYRadius*tmpRoundX;
    end;

    GetXY(AnglePos,P.X,P.Y);

    tmpXRadius:=1;
    tmpYRadius:=1;

    if IsInner then
    begin
      P3.X:=IDonutPercent*P.X;
      P3.Z:=-IDonutPercent*P.Y;
      P3.Y:=AZ1;
    end
    else
    begin
      P3.X:=P.X;
      P3.Y:=AZ0;
      P3.Z:=-P.Y;

      P2.X:=P3.X;
      P2.Z:=P3.Z;
    end;

    P2.Y:=AZ1;

    if not IsInner then
    begin
      tmpXRadius:=1-tmpXRadius*tmpRoundX;
      tmpYRadius:=1-tmpYRadius*tmpRoundX;
    end;

    GetXY(AnglePos,P.X,P.Y);

    tmpXRadius:=1;
    tmpYRadius:=1;

    if IsInner then
    begin
      P1.X:=IDonutPercent*P.X;
      P1.Z:=-IDonutPercent*P.Y;
      P1.Y:=AZ0;

      P2.X:=P1.X;
      P2.Z:=P1.Z;
    end
    else
    begin
      P1.X:=P.X;
      P1.Z:=-P.Y;
      P1.Y:=AZ1;
    end;

    if AEdge.Style=resRound then
    begin
      InvCurve:=1/AEdge.Slices;

      SetLength(BezierPoints,InitialLength+AEdge.Slices+1);

      for t:=0 to AEdge.Slices do
      begin
        mu:=t*InvCurve;
        mu2:=Sqr(mu);
        mum1:=1-mu;
        mum12:=Sqr(mum1);
        mum1mu:=2*mum1*mu;

        if Invert then tmpIndex:=AEdge.Slices-t
                  else tmpIndex:=t;

        with BezierPoints[tmpIndex+InitialLength] do
        begin
          x:=(P1.x*mum12 + P2.x*mum1mu + P3.x*mu2);
          y:=(P1.y*mum12 + P2.y*mum1mu + P3.y*mu2);
          z:=(P1.z*mum12 + P2.z*mum1mu + P3.z*mu2);
        end;
      end;
    end
    else
    begin
      SetLength(BezierPoints,InitialLength+2);

      if Invert then
      begin
        BezierPoints[InitialLength]:=P3;
        BezierPoints[InitialLength+1]:=P1;
      end
      else
      begin
        BezierPoints[InitialLength]:=P1;
        BezierPoints[InitialLength+1]:=P3;
      end;
    end;
  end;

var
  tmpInnerTop : Single;
  tmpInnerBottom : Single;

  procedure AddEdges(AIndex:Integer; var Points:TPoint3DArray);

    procedure AddEdge(IsInner,Invert:Boolean; AEdge:TBlockEdge; const AX,AY,AZ,AZ0:Single);
    var tmpL : Integer;
    begin
      if AEdge.Active then
         CalcRoundness(AIndex,IsInner,Invert,Points,AEdge,AZ0,AZ,Length(Points))
      else
      begin
        tmpL:=Length(Points);
        SetLength(Points,tmpL+1);

        with Points[tmpL] do
        begin
          X:=AX;
          Y:=AZ;
          Z:=-AY;
        end;
      end;
    end;

  var tmpX,
      tmpY,
      tmpS,
      x,y : Single;
      tmpL : Integer;
  begin
    Points:=nil;

    GetXY(AIndex,X,Y);

    with Edges do
    begin
      AddEdge(False,False,OuterTop,X,Y,-1,-1+0.02*OuterTop.Point.Y);
      AddEdge(False,True,OuterBottom,X,Y,1,1-0.02*OuterBottom.Point.Y);

      if FDonutPercent=0 then
      begin
        tmpL:=Length(Points);
        SetLength(Points,tmpL+2);

        with Points[tmpL] do
        begin
          X:=0;
          Y:=tmpInnerTop;
          Z:=0;
        end;

        with Points[tmpL+1] do
        begin
          X:=0;
          Y:=tmpInnerBottom;
          Z:=0;
        end;
      end
      else
      begin
        tmpX:=IDonutPercent*X;
        tmpY:=IDonutPercent*Y;

        tmpS:=0.01*(tmpInnerTop-tmpInnerBottom);

        AddEdge(True,True,InnerBottom,tmpX,tmpY,tmpInnerTop,tmpInnerTop-tmpS*InnerBottom.Point.Y);
        AddEdge(True,False,InnerTop,tmpX,tmpY,tmpInnerBottom,tmpInnerBottom+tmpS*InnerTop.Point.Y);
      end;
    end;
  end;

var
  NumSliceParts : Integer;

  procedure CheckParams;
  var piStep     : Double;
      tmpSinCos2 : Double;
      t          : Integer;
  begin
    if tmpSinCos=nil then
    begin
      NumSliceParts:=Round(FAngle*FStacks);

      if NumSliceParts=0 then
         NumSliceParts:=1;

      piStep:=FAngle*TeePiStep/NumSliceParts;

      tmpSinCos2:=(HalfPi+StartAngle*TeePiStep);

      SetLength(tmpSinCos,NumSliceParts+1);
      for t:=0 to NumSliceParts do
      with tmpSinCos[t] do
           SinCos(tmpSinCos2+(t*piStep),X,Y);

      tmpXRadius:=1;
      tmpYRadius:=1;

      IDonutPercent:=FDonutPercent*0.01;
    end;
  end;

  Procedure DrawSide(var AList,AListPen:Integer; AFormat:TBlockFormat; Start:Boolean);
  var
    tmpPoints : TPoint3DArray;
    tmpL : Integer;
    t : Integer;
  begin
    if AList=0 then
    begin
      CheckParams;

      if Start then
         AddEdges(0,tmpPoints)
      else
         AddEdges(NumSliceParts,tmpPoints);

      tmpL:=Length(tmpPoints)-1;

      if (FInnerSize.Point.X=0) and (FInnerSize.Point.Y=0) then
         AFormat.ConvexPolygon(AList,tmpPoints,not Start)
      else
      begin
        for t:=0 to tmpL do
            with tmpPoints[t] do
                 Y:=-Y;

        AFormat.ConcavePolygon(AList,tmpPoints,not Start);
      end;

      AFormat.PolylineList(AListPen,tmpPoints);

      tmpPoints:=nil;
    end
    else
      glCallList(AList);

    if AFormat.PreparePen then
    begin
      glCallList(AListPen);

      // Pending to remove this code:
      // (needs to fix above call to ConcavePolygon to *not* genList)
      if (FInnerSize.Point.X<>0) or (FInnerSize.Point.Y<>0) then
      begin
        CheckParams;

        tmpPoints:=nil;

        if Start then
           AddEdges(0,tmpPoints)
        else
           AddEdges(NumSliceParts,tmpPoints);

        tmpL:=Length(tmpPoints)-1;

        for t:=0 to tmpL do
            with tmpPoints[t] do
                 Y:=-Y;

        glBegin(GL_LINE_STRIP);

        for t:=0 to tmpL do
        with tmpPoints[t] do
             glVertex3f(X,Y,Z);

        if tmpL>1 then
        with tmpPoints[0] do
             glVertex3f(X,Y,Z);

        glEnd;

        tmpPoints:=nil;
      end;

      AFormat.FinishPen;
    end;
  end;

  procedure DrawPie;
  var
    Points0 : TPoint3DArray;
    Points1 : TPoint3DArray;
    tmpL    : Integer;

    procedure SetNormal(AIndex:Integer);
    var P0,P1,P2 : TPoint3DFloat;
    begin
      if AIndex=0 then
         P0:=Points1[tmpL-1]
      else
         P0:=Points1[AIndex-1];

      P1:=Points1[AIndex];
      P2:=Points0[AIndex];

      P0.Y:=-P0.Y;
      P1.Y:=-P1.Y;
      P2.Y:=-P2.Y;

      with CalculateNormal(P0,P1,P2) do
           glNormal3f(X,Y,Z)
    end;

  var
    tmpTex0 : Double;
    tmpTex1 : Double;
    tmpCoords : TDoubleArray;

    procedure AddPoint(AIndex:Integer);
    begin
      SetNormal(AIndex);

      Format.Texture.Coord(tmpTex0,tmpCoords[AIndex]);

      with Points0[AIndex] do
           glVertex3f(X,-Y,Z);

      Format.Texture.Coord(tmpTex1,tmpCoords[AIndex]);

      with Points1[AIndex] do
           glVertex3f(X,-Y,Z);
    end;

    procedure AddPoint0;
    begin
      SetNormal(0);

      Format.Texture.Coord(tmpTex0,1);

      with Points0[0] do
           glVertex3f(X,-Y,Z);

      Format.Texture.Coord(tmpTex1,1);

      with Points1[0] do
           glVertex3f(X,-Y,Z);
    end;

    procedure CalcTexCoords;

      function Distance(const A,B:TPoint3DFloat):Double;
      begin
        result:=Sqrt(Sqr(A.X-B.X)+Sqr(A.Z-B.Z)+Sqr(A.Y-B.Y));
      end;

    var tmp : Double;
        tmpInv : Double;
        t   : Integer;
    begin
      SetLength(tmpCoords,tmpL);

      tmpCoords[0]:=0;

      tmp:=0;

      for t:=1 to tmpL-1 do
      begin
        tmp:=tmp+Distance(Points0[t],Points0[t-1]);
        tmpCoords[t]:=tmp;
      end;

      tmp:=tmp+Distance(Points0[tmpL-1],Points0[0]);

      tmpInv:=1/tmp;

      for t:=0 to tmpL-1 do
          tmpCoords[t]:=tmpCoords[t]*tmpInv;
    end;

  var t      : Integer;
      tt     : Integer;
      tmp    : TPoint3DArray;
      tmpInv : Double;
  begin
    tmp:=nil;

    CheckParams;

    AddEdges(0,Points0);

    tmpL:=Length(Points0);

    CalcTexCoords;

    tmpTeX0:=0;
    tmpInv:=1/NumSliceParts;

    for t:=1 to NumSliceParts do
    begin
      AddEdges(t,Points1);

      tmpTeX1:=t*tmpInv;

      glBegin(GL_QUAD_STRIP);

      for tt:=0 to tmpL-1 do
          AddPoint(tt);

      AddPoint0;

      glEnd;
      
      tmp:=Points1;
      Points1:=Points0;
      Points0:=tmp;

      tmpTeX0:=tmpTeX1;
    end;

    Points1:=nil;
    Points0:=nil;
    tmpCoords:=nil;
  end;

  procedure SetChanged(ABrush:TBlockFormat);
  begin
    if not Assigned(ABrush.IChanged) then
       DeleteLists;

    ABrush.IChanged:=DataChanged;

    if Assigned(ABrush.FBorder) then
       ABrush.FBorder.IChanged:=DataChanged;

    if Assigned(ABrush.FTexture) then
       ABrush.FTexture.IChanged:=DataChanged;
  end;

  procedure DrawOutline(AEdge:TBlockEdge; const APos:Single);
  var t : Integer;
      X,Y : Single;
  begin
    if not AEdge.Active then
    if Format.PreparePen then
    begin
      CheckParams;

      glBegin(GL_LINE_STRIP);

      for t:=0 to NumSliceParts do
      begin
        GetXY(t,X,Y);
        glVertex3f(X,APos,-Y);
      end;

      glEnd;

      Format.FinishPen;
    end;
  end;

  procedure DoDrawSide(var AList,AListPen:Integer; AFormat:TBlockFormat; Start:Boolean);
  begin
    if Assigned(AFormat) then
    begin
      SetChanged(AFormat);

      Format.Finish;
      AFormat.Start;
      DrawSide(AList,AListPen,AFormat,Start);
      AFormat.Finish;
      Format.Start;
    end
    else
      DrawSide(AList,AListPen,Format,Start);
  end;

var DrawSide1 : Boolean;
    DrawSide2 : Boolean;
    CanCull   : Boolean;
begin
  with FInnerSize.Point do
  begin
    tmpInnerTop:=1-2*Y*0.01;
    tmpInnerBottom:=-1+2*X*0.01;
  end;

  DrawSide1:=(not Assigned(FBrush1)) or FBrush1.Solid;
  DrawSide2:=(not Assigned(FBrush2)) or FBrush2.Solid;

  if Format.Solid then
  begin
    CanCull:=(not ShouldDrawInterior) and Format.Solid and DrawSide1 and DrawSide2;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if IList=0 then
    begin
      IList:=CreateNewList;
      DrawPie;
      glEndList;
    end
    else
      glCallList(IList);

    if DrawSide1 then
       DoDrawSide(IListSide1,IListPenSide1,FBrush1,True);

    if DrawSide2 then
       DoDrawSide(IListSide2,IListPenSide2,FBrush2,False);

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;
  
  // Outline (Pen)
  with Edges do
  begin
    DrawOutline(OuterTop,1);
    DrawOutline(OuterBottom,-1);

    if FDonutPercent<>0 then
    begin
      CheckParams;

      tmpXRadius:=IDonutPercent;
      tmpYRadius:=tmpXRadius;

      DrawOutline(InnerTop,-tmpInnerBottom);
      DrawOutline(InnerBottom,-tmpInnerTop);
    end;
  end;

  tmpSinCos:=nil;
  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Pie3D');
  {$ENDIF}
end;

function TPieSliceBlock.IsStacksStored: Boolean;
begin
  result:=FStacks<>1;
end;

procedure TPieSliceBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListSide1);
  DeleteList(IListSide2);
  DeleteList(IListPenSide1);
  DeleteList(IListPenSide2);
end;

function TPieSliceBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(0.5,1,0,'Angle,MinMax:0;360').Format.Color:=clYellow;
    AddHandle(1,1,0.5,'StartAngle,MinMax:0;360').Format.Color:=clAqua;
  end;
end;

procedure TPieSliceBlock.SetInnerSize(const Value: TPointXYFloat);
begin
  FInnerSize.Assign(Value);
end;

{ TRectangleCorners }

procedure TRectangleCorners.Assign(Source:TPersistent);
begin
  if Source is TRectangleCorners then
  with TRectangleCorners(Source) do
  begin
    Self.FLeftTop:=FLeftTop;
    Self.FRightTop:=FRightTop;
    Self.FLeftBottom:=FLeftBottom;
    Self.FRightBottom:=FRightBottom;
  end
  else
    inherited;
end;

procedure TRectangleCorners.SetLeftTop(const Value:TColor);
begin
  FLeftTop:=Value;
  IOwner.ChangedCorner;
end;

procedure TRectangleCorners.SetRightTop(const Value:TColor);
begin
  FRightTop:=Value;
  IOwner.ChangedCorner;
end;

procedure TRectangleCorners.SetLeftBottom(const Value:TColor);
begin
  FLeftBottom:=Value;
  IOwner.ChangedCorner;
end;

procedure TRectangleCorners.SetRightBottom(const Value:TColor);
begin
  FRightBottom:=Value;
  IOwner.ChangedCorner;
end;

{ TRectangleBlock }

Constructor TRectangleBlock.Create(AOwner: TComponent);
begin
  inherited;

  FCorners:=TRectangleCorners.Create;
  FCorners.IOwner:=Self;

  FCorners.FLeftTop:=clDefault;
  FCorners.FLeftBottom:=clDefault;
  FCorners.FRightTop:=clDefault;
  FCorners.FRightBottom:=clDefault;

  FSides:=TRectangleSides.Create;
  FSides.IOwner:=Self;

  FSides.FLeft:=clDefault;
  FSides.FRight:=clDefault;
  FSides.FTop:=clDefault;
  FSides.FBottom:=clDefault;

  FCenter:=clDefault;
end;

Destructor TRectangleBlock.Destroy;
begin
  FSides.Free;
  FCorners.Free;
  inherited;
end;

procedure TRectangleBlock.Assign(Source:TPersistent);
begin
  if Source is TRectangleBlock then
  with TRectangleBlock(Source) do
  begin
    Self.FCenter:=FCenter;
    Self.Corners:=Corners;
    Self.Sides:=Sides;
  end;

  inherited;
end;

procedure TRectangleBlock.ChangedCorner;
begin
  with FCorners do
       IAnyCorner:=(FLeftTop<>clDefault) or (FLeftBottom<>clDefault) or
                   (FRightTop<>clDefault) or (FRightBottom<>clDefault);

  DeleteLists;
end;

procedure TRectangleBlock.ChangedSide;
begin
  with FSides do
       IAnySide:=(FLeft<>clDefault) or (FTop<>clDefault) or
                  (FRight<>clDefault) or (FBottom<>clDefault);

  DeleteLists;
end;

procedure TRectangleBlock.SetCorners(const Value:TRectangleCorners);
begin
  FCorners.Assign(Value);
end;

procedure TRectangleBlock.SetSides(const Value:TRectangleSides);
begin
  FSides.Assign(Value);
end;

procedure TRectangleBlock.Draw;

  procedure ChangeColor(const AColor:TColor);
  begin
    if AColor<>clDefault then
       Format.PrepareColor(AColor);
  end;

  procedure DoLeftBottom;
  begin
    ChangeColor(FCorners.FLeftBottom);
    Format.Texture.Coord(0,1);
    glVertex2s(-1,-1);
  end;

var tmpChange : Boolean;
begin
  if Format.Solid then
  begin
    tmpChange:=(not IPicking) and
               (
                ((not Assigned(Parent)) or (not Parent.FDrawBlocks.Shadows.Visible)) and
                (IAnyCorner or IAnySide or (FCenter<>clDefault))
               );

    if tmpChange then
    begin
      if IList=0 then
      begin
        IList:=CreateNewList;

        if FCenter=clDefault then
           glBegin(GL_POLYGON)
        else
           glBegin(GL_TRIANGLE_FAN);

        glNormal3i(0,0,1);

        if FCenter<>clDefault then
        begin
          ChangeColor(FCenter);
          Format.Texture.Coord(0.5,0.5);
          glVertex2s(0,0);
        end;

        DoLeftBottom;

        ChangeColor(FSides.FBottom);
        Format.Texture.Coord(0.5,1);
        glVertex2s(0,-1);

        ChangeColor(FCorners.FRightBottom);
        Format.Texture.Coord(1,1);
        glVertex2s(1,-1);

        ChangeColor(FSides.FRight);
        Format.Texture.Coord(1,0.5);
        glVertex2s(1,0);

        ChangeColor(FCorners.FRightTop);
        Format.Texture.Coord(1,0);
        glVertex2s(1,1);

        ChangeColor(FSides.FTop);
        Format.Texture.Coord(0.5,0);
        glVertex2s(0,1);

        ChangeColor(FCorners.FLeftTop);
        Format.Texture.Coord(0,0);
        glVertex2s(-1,1);

        ChangeColor(FSides.FLeft);
        Format.Texture.Coord(0,0.5);
        glVertex2s(-1,0);

        if FCenter<>clDefault then
           DoLeftBottom;

        glEnd;

        glEndList;
      end
      else
        glCallList(IList);
    end
    else
    begin
      if IListFlat=0 then
      begin
        IListFlat:=CreateNewList;

        glBegin(GL_QUADS);
        glNormal3i(0,0,1);

        Format.Texture.Coord(0,1);
        glVertex2f(-1,-1);

        Format.Texture.Coord(1,1);
        glVertex2f(1,-1);

        Format.Texture.Coord(1,0);
        glVertex2f(1,1);

        Format.Texture.Coord(0,0);
        glVertex2f(-1,1);

        glEnd;

        glEndList;
      end
      else
        glCallList(IListFlat);
    end;
  end;

  if Format.PreparePen then
  begin
    if IListPen=0 then
    begin
      IListPen:=CreateNewList;

      glBegin(GL_LINE_LOOP);

      glVertex2f(-1,-1);
      glVertex2f(1,-1);
      glVertex2f(1,1);
      glVertex2f(-1,1);

      glEnd;

      glEndList;
    end
    else
      glCallList(IListPen);

    Format.FinishPen;
  end;
end;

procedure TRectangleBlock.PrepareForGallery;
begin
  inherited;

  Size.Point.Y:=0;
end;

procedure TRectangleBlock.ReadState(Reader: TReader);
begin
  inherited;

  Size.Point.Y:=0;
end;

function TRectangleBlock.UsesDepth:Boolean;
begin
  result:=False;
end;

procedure TRectangleBlock.SetCenter(const Value: TColor);
begin
  FCenter := Value;
  DeleteLists;
end;

procedure TRectangleBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListFlat);
  DeleteList(IListPen);
end;

{ TTeeTextBlock }

Constructor TTeeTextBlock.Create(AOwner: TComponent);
begin
  inherited;

  IFont:=-1;

  FFont:=TTeeFont.Create(TextChanged);

  FLines:=TStringList.Create;
  TStringList(FLines).OnChange:=TextChanged;

  FLink:=TPropertyLink.Create(Self);

  Size.Point.Y:=5;

  Format.OnColorChanged:=ColorChanged;
end;

Destructor TTeeTextBlock.Destroy;
begin
  FLink.Free;
  FLines.Free;
  FFont.Free;

  inherited;
end;

function TTeeTextBlock.GetText:String;
begin
  result:=FLines.Text;
end;

procedure TTeeTextBlock.DeleteLists;
begin
  inherited;
  IFont:=-1;
end;

procedure TTeeTextBlock.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation=opRemove) and Assigned(FLink.Instance) and (AComponent=FLink.Instance) then
     FLink.Instance:=nil;
end;

procedure TTeeTextBlock.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TTeeTextBlock.SetLink(const Value: TPropertyLink);
begin
  FLink.Assign(Value);
end;

procedure TTeeTextBlock.SetLinkFormat(const Value: String);
begin
  FLinkFormat := Value;
  Repaint;
end;

procedure TTeeTextBlock.ColorChanged(Sender:TObject);
begin
  Font.Color:=Format.Color;
end;

procedure TTeeTextBlock.TextChanged(Sender:TObject);
begin
  IChanged:=True;
  IExtentOk:=False;
  IFont:=-1;
  Repaint;
end;

procedure TTeeTextBlock.SetAlign(const Value: TAlignment);
begin
  FAlign:=Value;
  Repaint;
end;

procedure TTeeTextBlock.SetText(const Value: String);
begin
  FLines.Text:=Value;
  Repaint;
end;

function TTeeTextBlock.DoGetPropValue(AInstance:TObject; const AProp:String):Variant;
begin
  if GetPropInfo(AInstance,AProp)<>nil then
     result:=GetPropValue(AInstance,AProp)
  else
  if AInstance is TCustomObjectBlock then
     result:=TCustomObjectBlock(AInstance).Items.Properties.Value[AProp]
  else
     result:=''; // <-??
end;

type
  TTeeFontAccess=class(TTeeFont);

procedure TTeeTextBlock.Draw;

  function CalcTextLine(const Index:Integer):String;
  begin
    if ILinkText='' then
       result:=Lines[Index]
    else
       result:=SysUtils.Format(Lines[Index],[ILinkText]);
  end;

  function TextExtent:TPoint;
  var t : Integer;
  begin
    result.X:=0;
    result.Y:=0;

    for t:=0 to Lines.Count-1 do
{$IFDEF BLOCKS}
    with ICanvas.iMeasureString(CalcTextLine(t)) do
{$ELSE}
    with ICanvas.ReferenceCanvas.TextExtent(CalcTextLine(t)) do
{$ENDIF}
    begin
      if cX>result.X then
         result.X:=cX;

      result.Y:=result.Y+cY;
    end;
  end;

  procedure CheckLink;
  var tmpText : String;
      tmpInfo : PPropInfo;
      tmpValue : Variant;
  begin
    with FLink do
    begin
      CheckInstancePrefix('MAKERSYSTEM.',MakerSystem);
      tmpValue:=PropertyValue(DoGetPropValue);

      if not VarIsNull(tmpValue) then
      begin
        if FLinkFormat='' then
           tmpText:=tmpValue
        else
        begin
          tmpInfo:=PropertyInfo;

          if tmpInfo.PropType^ = System.TypeInfo(TDateTime) then
             tmpText:=FormatDateTime(FLinkFormat,tmpValue)
          else
          case tmpInfo.PropType^.Kind of
            tkInteger,
            tkFloat,
            tkInt64 : tmpText:=FormatFloat(FLinkFormat,tmpValue);
          else
             tmpText:=tmpValue
          end;
        end;
      end;

      if ILinkText<>tmpText then
      begin
        ILinkText:=tmpText;
        IExtentOk:=False;
      end;
    end;
  end;

var tmpHasTexture : Boolean;
    tmpHasExtrusion : Boolean;
    tmpExtrusion  : Double;
    OldEx         : Double;
    t             : Integer;
    tmpLines      : Integer;
    tmpSize       : Integer;
    tmpText       : String;
    tmpText2      : AnsiString;
    tmpWasAnti    : Boolean;
begin
  CheckLink;

  tmpHasTexture:=(not Parent.DrawBlocks.Shadows.Visible) and Format.Texture.HasTexture;

  if tmpHasTexture then
     Format.Texture.SetAutomatic(True);

  if not Assigned(ICanvas) then
     Exit;  // <-- wrong ! ICanvas should be asserted !

  if Assigned(FFont) then
  {$IFDEF BLOCKS}
    ICanvas.setFont(FFont.Color, FFont.Name, FFont.Size, FFont.Style=TFontStyles(fsBold),
    FFont.Style=TFontStyles(fsItalic), FFont.Style=TFontStyles(fsUnderLine));
  {$ELSE}
    ICanvas.Font.Assign(FFont);
  {$ENDIF}

  tmpLines:=Lines.Count-1;

  if not IExtentOk then
  begin
    if tmpLines=0 then
    {$IFDEF BLOCKS}
    with ICanvas.iMeasureString(CalcTextLine(0)) do
    {$ELSE}
    with ICanvas.ReferenceCanvas.TextExtent(CalcTextLine(0)) do
    {$ENDIF}
       begin
         ITextExtent.X:=cx;
         ITextExtent.Y:=cy;
       end
    else
      ITextExtent:=TextExtent;

    IExtentOk:=True;
  end;

  glNormal3i(0,0,1);

  {$IFDEF BLOCKS}
  ICanvas.setFontStyle(FontStyle);
  {$ELSE}
  ICanvas.FontStyle:=FontStyle;
  {$ENDIF}

  if IFont=-1 then
  begin
  {$IFDEF BLOCKS}
    OldEx:=ICanvas.getFontExtrusion;
    ICanvas.setFontExtrusion(Size.Point.Y);
    IFont:=ICanvas.iFindFont;
    ICanvas.setFontExtrusion(OldEx);
  {$ELSE}
    OldEx:=ICanvas.FontExtrusion;
    ICanvas.FontExtrusion:=Size.Point.Y;
    IFont:=TGLCanvasAccess(ICanvas).FindFont;
    ICanvas.FontExtrusion:=OldEx;
  {$ENDIF}
  end;

  {$IFDEF BLOCKS}
  tmpExtrusion:=ICanvas.getFontCacheExtrusion(IFont);
  {$ELSE}
  tmpExtrusion:=TGLCanvasAccess(ICanvas).FontCache[IFont].Extrusion;
  {$ENDIF}
  tmpHasExtrusion:=tmpExtrusion>0;

  if tmpHasExtrusion then
     glEnable(GL_CULL_FACE);

  {$R-}
  {$IFDEF BLOCKS}
  glListBase(ICanvas.getFontCacheOffset(IFont)-32);
  {$ELSE}
  glListBase(TGLCanvasAccess(ICanvas).FontCache[IFont].Offset-32);
  {$ENDIF}

  if FontStyle=fsBitmap then
     glRasterPos2i(0,0);

  tmpWasAnti:=TGLCanvasAccess(ICanvas).IAntiAlias and (Font.Quality=fqNormal);
  if Font.Quality=fqNormal then
     TGLCanvasAccess(ICanvas).SetAntiAlias(False);

  tmpSize:=Font.Size;

  glTranslatef(-1,0 {+0.5*((tmpLines+1)*0.5)},0);

  with ITextExtent do
  if tmpExtrusion=0 then
       glScalef(3*tmpSize/X,
                (2*tmpSize*TeeTextHeightFactor)/Y,
                1)
  else
       glScalef(3*tmpSize/X,
                (2*tmpSize*TeeTextHeightFactor)/Y,
                2/tmpExtrusion);

  glTranslatef(0,-0.25+0.5*tmpLines,0.5*tmpExtrusion);

  for t:=0 to tmpLines do
  begin
    tmpText:=CalcTextLine(t);

    glPushMatrix;

    if Alignment=taCenter then
       glTranslatef(-ICanvas.{$IFDEF BLOCKS}iTextWidth(tmpText){$ELSE}TextWidth(tmpText){$ENDIF}*0.5,0,0)
    else
    if Alignment=taRightJustify then
       glTranslatef((ITextExtent.X*0.5)-ICanvas.{$IFDEF BLOCKS}iTextWidth(tmpText){$ELSE}TextWidth(tmpText){$ENDIF},0,0);

    tmpText2:=AnsiString(tmpText);

    glCallLists(Length(tmpText), GL_UNSIGNED_BYTE, PAnsiChar(tmpText2));
    glPopMatrix;

    {$IFNDEF BLOCKS}
    Assert(ICanvas.CheckGLError,'TextOut3D: '+TeeStr(TGLCanvasAccess(ICanvas).ISavedError));
    {$ENDIF}

    if tmpLines>0 then
       glTranslatef(0,-1,0);
  end;

  if tmpWasAnti then
     TGLCanvasAccess(ICanvas).SetAntiAlias(True);

  if tmpHasTexture then
     Format.Texture.SetAutomatic(False);

  if tmpHasExtrusion then
  begin
    glDisable(GL_CULL_FACE);
    glFrontFace(GL_CCW);
  end;
end;

procedure TTeeTextBlock.PrepareForGallery;
begin
  inherited;
  Text:=TeeMakerVersion;
  Font.Size:=80;
  Font.Color:=Format.Color;
  Size.X:=400;
end;

procedure TTeeTextBlock.SetFontStyle(const Value: TTeeFontStyle);
begin
  FFontStyle:=Value;
  IFont:=-1;
  Repaint;
end;

procedure TTeeTextBlock.SetFont(const Value: TTeeFont);
begin
  FFont.Assign(Value);
  IExtentOk:=False;
  IFont:=-1;
  Repaint;
end;

{procedure TTeeTextBlock.SetExtrusion(const Value: Double);
begin
  FExtrusion:=Value;
  IFont:=-1;
  Repaint;
end;}

procedure TTeeTextBlock.Assign(Source: TPersistent);
begin
  if Source is TeeBlocks.TTeeTextBlock then
  with TeeBlocks.TTeeTextBlock(Source) do
  begin
    Self.FAlign:=FAlign;
    //Self.FExtrusion:=FExtrusion;
    Self.Font:=FFont;
    Self.FFontStyle:=FFontStyle;
    Self.Lines:=FLines;
    Self.LinkText:=LinkText;
    Self.FLinkFormat:=FLinkFormat;
  end;

  inherited;
end;

{ TPyramidBlock }

Constructor TPyramidBlock.Create(AOwner: TComponent);
begin
  inherited;
  FSide1:=TPointXYFloat.Create(Self,50,Changed);
  FSide2:=TPointXYFloat.Create(Self,50,Changed);
end;

Destructor TPyramidBlock.Destroy;
begin
  FSide2.Free;
  FSide1.Free;
  inherited;
end;

procedure TPyramidBlock.Assign(Source: TPersistent);
begin
  if Source is TPyramidBlock then
  begin
    Side1:=TPyramidBlock(Source).Side1;
    Side2:=TPyramidBlock(Source).Side2;
  end;

  inherited;
end;

procedure TPyramidBlock.Changed(Sender:TObject);
begin
  DeleteLists;
end;

procedure TPyramidBlock.SetSide1(const Value:TPointXYFloat);
begin
  FSide1.Assign(Value);
end;

procedure TPyramidBlock.SetSide2(const Value:TPointXYFloat);
begin
  FSide2.Assign(Value);
end;

procedure TPyramidBlock.Draw;
var tmpX1 : Double;
    tmpX2 : Double;
    tmpZ1 : Double;
    tmpZ2 : Double;
    CanCull : Boolean;
begin
  tmpX1:=Side1.X*0.01;
  tmpZ1:=-Side1.Y*0.01;

  tmpX2:=Side2.X*0.01;
  tmpZ2:=-Side2.Y*0.01;

  if Format.Solid then
  begin
    CanCull:=not ShouldDrawInterior;

    if CanCull then
       glEnable(GL_CULL_FACE);

    if IList=0 then
    begin
      IList:=CreateNewList;

      glBegin(GL_QUADS);

      // Front
      glNormal3i( 0, 0, 1);
       Format.Texture.Coord(1,0);
       glVertex3s( -1, -1, 1);
       Format.Texture.Coord(1,1);
       glVertex3s( 1, -1, 1);
       Format.Texture.Coord(0,1);
       glVertex3f( 1-tmpX2, 1, 1+tmpZ1);
       Format.Texture.Coord(0,0);
       glVertex3f( -1+tmpX1, 1, 1+tmpZ1);

      glNormal3i(-1,  0,  0);
       Format.Texture.Coord(0,0);
       glVertex3f( -1+tmpX1, 1, -1-tmpZ2);
       Format.Texture.Coord(0,1);
       glVertex3s( -1, -1, -1);
       Format.Texture.Coord(1,1);
       glVertex3s( -1, -1, 1);
       Format.Texture.Coord(1,0);
       glVertex3f( -1+tmpX1, 1, 1+tmpZ1);

      // Back
      glNormal3i( 0, 0, -1);
       Format.Texture.Coord(0,0);
       glVertex3f( 1-tmpX2, 1, -1-tmpZ2);
       Format.Texture.Coord(1,0);
       glVertex3s( 1, -1, -1);
       Format.Texture.Coord(1,1);
       glVertex3s( -1, -1, -1);
       Format.Texture.Coord(0,1);
       glVertex3f( -1+tmpX1, 1, -1-tmpZ2);

      glNormal3i( 1,  0,  0);
       Format.Texture.Coord(0,0);
       glVertex3s( 1, -1, 1);
       Format.Texture.Coord(0,1);
       glVertex3s( 1, -1, -1);
       Format.Texture.Coord(1,1);
       glVertex3f( 1-tmpX2, 1, -1-tmpZ2);
       Format.Texture.Coord(1,0);
       glVertex3f( 1-tmpX2, 1, 1+tmpZ1);

      glNormal3i( 0, 1,  0);
       Format.Texture.Coord(0,0);
       glVertex3f( -1+tmpX1, 1, -1-tmpZ2);
       Format.Texture.Coord(0,1);
       glVertex3f( -1+tmpX1, 1, 1+tmpZ1);
       Format.Texture.Coord(1,1);
       glVertex3f( 1-tmpX2, 1, 1+tmpZ1);
       Format.Texture.Coord(1,0);
       glVertex3f( 1-tmpX2, 1, -1-tmpZ2);

      glNormal3i( 0, -1,  0);
       Format.Texture.Coord(0,0);
       glVertex3s( 1, -1, 1);
       Format.Texture.Coord(0,1);
       glVertex3s( -1, -1, 1);
       Format.Texture.Coord(1,1);
       glVertex3s( -1, -1, -1);
       Format.Texture.Coord(1,0);
       glVertex3s( 1, -1, -1);

      glEnd;

      glEndList;
    end
    else
      glCallList(IList);

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if Format.PreparePen then
  begin
    glBegin(GL_LINE_LOOP);
      glVertex3s( -1, -1, 1);
      glVertex3s( 1, -1, 1);
      glVertex3f( 1-tmpX2, 1, 1+tmpZ1);
      glVertex3f( -1+tmpX1, 1, 1+tmpZ1);
    glEnd;

    glBegin(GL_LINE_LOOP);
      glVertex3f( -1+tmpX1, 1, -1-tmpZ2);
      glVertex3s( -1, -1, -1);
      glVertex3s( 1, -1, -1);
      glVertex3f( 1-tmpX2, 1, -1-tmpZ2);
    glEnd;

    glBegin(GL_LINE_LOOP);
      glVertex3f( 1-tmpX2, 1, 1+tmpZ1);
      glVertex3f( 1-tmpX2, 1, -1-tmpZ2);
      glVertex3s( 1, -1, -1);
      glVertex3s( 1, -1, 1);
    glEnd;

    glBegin(GL_LINE_LOOP);
      glVertex3f( -1+tmpX1, 1, 1+tmpZ1);
      glVertex3s( -1, -1, 1);
      glVertex3s( -1, -1, -1);
      glVertex3f( -1+tmpX1, 1, -1-tmpZ2);
    glEnd;

    glBegin(GL_LINE_LOOP);
      glVertex3f( 1-tmpX2, 1, -1-tmpZ2);
      glVertex3f( 1-tmpX2, 1, 1+tmpZ1);
      glVertex3f( -1+tmpX1, 1, 1+tmpZ1);
      glVertex3f( -1+tmpX1, 1, -1-tmpZ2);
    glEnd;

    Format.FinishPen;
  end;

  {$IFNDEF BLOCKS}
  Assert(ICanvas.CheckGLError,'Pyramid');
  {$ENDIF}
end;

function TBlocks.Add(ABlock:TCustomBlock):TCustomBlock;
begin
  result:=ABlock;
  ABlock.Parent:=Self;
end;

procedure TBlocks.ResetShown;
var t : Integer;
begin
  for t:=0 to Count-1 do
  begin
    Block[t].IShown:=False;

    if Block[t] is TCustomObjectBlock then
       TCustomObjectBlock(Block[t]).FItems.ResetShown;
  end;
end;

function TBlocks.GetShaderEnabled:Boolean;
begin
  result:=Assigned(IProgramShader) and IProgramShader.Enabled;
end;

procedure TBlocks.SetShaderEnabled(const Value:Boolean);
begin
  if ShaderEnabled<>Value then
  begin
    ProgramShader.Enabled:=Value;
    Assert(ICanvas.CheckGLError,'UseProgram: '+IntToStr(TGLCanvasAccess(ICanvas).ISavedError));

    Repaint;
  end;
end;

function TBlocks.CalcBounds(var AMin,AMax:TPoint3DFloat):Boolean;
var First : Boolean;

  procedure AddBlock(Index:Integer);

    procedure Check(const A,B:Single; var AMin,AMax:Single);
    begin
      if A<AMin then AMin:=A else
      if A>AMax then AMax:=A;

      if B<AMin then AMin:=B else
      if B>AMax then AMax:=B;
    end;

  var tmpMin : TPoint3DFloat;
      tmpMax : TPoint3DFloat;
  begin
    with Block[Index] do
    if BoundingBox(tmpMin,tmpMax) then
    begin
      with tmpMin do
      begin
        X:=X+Location.X;
        Y:=Y+Location.Y;
        Z:=Z+Location.Z;
      end;

      with tmpMax do
      begin
        X:=X+Location.X;
        Y:=Y+Location.Y;
        Z:=Z+Location.Z;
      end;

      if First then
      begin
        AMin:=tmpMin;
        AMax:=tmpMax;
        First:=False;
      end
      else
      begin
        Check(tmpMin.X,tmpMax.X,AMin.X,AMax.X);
        Check(tmpMin.Y,tmpMax.Y,AMin.Y,AMax.Y);
        Check(tmpMin.Z,tmpMax.Z,AMin.Z,AMax.Z);
      end;
    end;
  end;

var t : Integer;
begin
  result:=Count>0;

  if result then
  begin
    First:=True;

    for t:=0 to Count-1 do
        AddBlock(t);
  end
  else
  begin
    AMin.X:=0;
    AMin.Y:=0;
    AMin.Z:=0;

    AMax:=AMin;
  end;
end;

procedure TBlocks.Clear;
begin
  if Assigned(ICurrentParents) then
     ICurrentParents.Clear;

  IList.Clear;

  if Assigned(FAnimates) then
     FAnimates.Clear;

  Repaint;
end;

procedure TBlocks.Repaint;
begin
  if Assigned(IParent) then
     IParent.Invalidate
  else
  if Assigned(FDrawBlocks) and Assigned(FDrawBlocks.IParent) then
     FDrawBlocks.IParent.Invalidate;
end;

procedure TCustomBlock.SetFormat(const Value:TBlockFormat);
begin
  FFormat.Assign(Value);
end;

procedure TCustomBlock.Move(const AX, AY, AZ: Single);
begin
  with Location.Point do
  begin
    X:=X+AX;
    Y:=Y+AY;
    Z:=Z+AZ;
  end;

  Repaint;
end;


{ TBlockPicture }

{ Load graphic file from registry "Textures" folder }
class function TBlockPicture.LoadGraphicResource(const FileName:String):TGraphic;
var tmp      : String;
    tmpClass : TGraphicClass;
begin
  result:=nil;

  tmp:=TBlocks.ParseFileName(TeeMsg_TexturesLibrary,FileName);

  tmpClass:=TBlockPicture.FileGraphicClass(tmp);

  if not Assigned(tmpClass) then
     Raise Exception.Create(SUnknownExtension+': '+FileName);

  if FileExists(tmp) then
  begin
    result:=tmpClass.Create;
    result.LoadFromFile(tmp);
  end;
end;

class function TBlockPicture.FileGraphicClass(const FileName:String):TGraphicClass;
var tmpExt : String;
begin
  tmpExt:=UpperCase(ExtractFileExt(FileName));
  Delete(tmpExt, 1, 1);

  if (tmpExt='JPG') or (tmpExt='JPEG') then
     result:=TJPEGImage
  else
  if (tmpExt='PNG') then
     result:={$IFDEF CLX}TBitmap{$ELSE}{$IFDEF D12}TPngImage{$ELSE}TPNGObject{$ENDIF}{$ENDIF}
  else
  if (tmpExt='BMP') then
     result:=TBitmap
  else
  if (tmpExt='GIF') then
     result:=TGIFImage
  else
  if (tmpExt='WMF') or (tmpExt='EMF') then
     result:=TMetafile
  else
  if (tmpExt='TGA') then
     result:=TTGAImage
  else
     result:=nil;
end;

procedure TBlockPicture.LoadFromURL(const URL:String);
var tmpStream : TStream;
    tmpGraphic : TGraphic;
    tmpGraphicClass : TGraphicClass;
    tmpError : String;
begin
  tmpError:='';
  LoadedSource:='';

  tmpStream:=TBlocks.LoadURLStream(URL,tmpError);

  if Assigned(tmpStream) then
  try
    tmpGraphicClass:=FileGraphicClass(URL);

    if not Assigned(tmpGraphicClass) then
       Raise EInvalidGraphic.CreateFmt(SUnknownExtension, [URL]);

    tmpGraphic:=tmpGraphicClass.Create;
    try
      try
        tmpGraphic.LoadFromStream(tmpStream);
        LoadedSource:=URL;
      except
        IBadFile:=True;
        FreeAndNil(tmpGraphic);
        raise;
      end;

      Graphic:=tmpGraphic;

    finally
      tmpGraphic.Free;
    end;
  finally
    tmpStream.Free;
  end
  else
  begin
    IBadFile:=True;
    ShowMessage(tmpError);
  end;
end;

procedure TBlockPicture.TryLoad(const ParentSource, Source: String);
var tmp : String;
begin
  IBadFile:=False;
  LoadedSource:='';

  if TeeIsURL(Source) then
     LoadFromURL(Source)
  else
  if FileExists(Source) then
  begin
    LoadFromFile(Source);
    LoadedSource:=Source;
  end
  else
  if TeeIsURL(ParentSource) then
     LoadFromURL(ParentSource+'/'+Source)
  else
  begin
    tmp:=ParentSource+'\'+ExtractFileName(Source);

    if FileExists(tmp) then
    begin
      LoadFromFile(tmp);
      LoadedSource:=tmp;
    end
    else
       IBadFile:=True;
  end;
end;

procedure TPyramidBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
end;

function TPyramidBlock.DesignHandles(AOwner:TComponent):TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(0.5,1,0,'Side1.X,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(1,1,0.5,'Side1.Y,MinMax:0;100').Format.Color:=clAqua;
    AddHandle(-0.5,1,0,'Side2.X,MinMax:0;100').Format.Color:=clYellow;
    AddHandle(-1,1,0.5,'Side2.Y,MinMax:0;100').Format.Color:=clAqua;
  end;
end;

{ TAnimateItem }

Destructor TAnimateItem.Destroy;
begin
  IList.Free;
  FAnimate.Free;
  inherited;
end;

procedure TAnimateItem.Assign(Source:TPersistent);
begin
  if Source is TAnimateItem then
  with TAnimateItem(Source) do
  begin
    //Self.Animate:=Animate; <-- not necessary?

    Self.FDesc:=FDesc;
    Self.FPlayOnLoad:=FPlayOnLoad;
  end
  else
    inherited;
end;

function TAnimateItem.GetOnStop:TNotifyEvent;
begin
  result:=Animate.OnStop;
end;

function TAnimateItem.GetPlaying:Boolean;
begin
  result:=Animate.Playing;
end;

function TAnimateItem.GetSpeed:Integer;
begin
  result:=Animate.Speed;
end;

function TAnimateItem.GetLoop:Boolean;
begin
  result:=Animate.Loop;
end;

procedure TAnimateItem.SetLoop(const Value:Boolean);
begin
  Animate.Loop:=Value;
end;

procedure TAnimateItem.SetOnStop(const Value:TNotifyEvent);
begin
  Animate.OnStop:=Value;
end;

procedure TAnimateItem.SetPlaying(const Value:Boolean);
begin
  if Value then
     Animate.Play
  else
     Animate.Stop;
end;

procedure TAnimateItem.SetSpeed(const Value:Integer);
begin
  Animate.Speed:=Value;
end;

function TAnimateItem.GetAnimationCount:Integer;
begin
  if Assigned(FAnimate) then
     result:=Animations.Count
  else
     result:=0;
end;

function TAnimateItem.GetAnimations:TAnimations;
begin
  result:=Animate.Animations;
end;

function TAnimateItem.GetList:TStrings;
begin
  if not Assigned(IList) then
     IList:=TStringList.Create;

  result:=IList;
end;

procedure TAnimateItem.SetList(const Value:TStrings);
begin
  GetList.Assign(Value);
end;

procedure TAnimateItem.SetDesc(const Value:String);
begin
  FDesc:=Value;
end;

procedure TAnimateItem.SetAnimations(const Value:TAnimations);
begin
  FAnimate.Animations.Assign(Value);
end;

procedure TAnimateItem.AnimateDestroyed(Sender:TObject);
begin
  FAnimate:=nil;
end;

type
  TAnimateAccess=class(TTeeAnimate);
  
function TAnimateItem.GetAnimate:TTeeAnimate;
begin
  if not Assigned(FAnimate) then
  begin
    FAnimate:=TTeeAnimate.Create(nil);
    FAnimate.Panel:=TAnimates(Collection).IParent;

    TAnimateAccess(FAnimate).FOnDestroy:=AnimateDestroyed;
  end;

  result:=FAnimate;
end;

{ TAnimates }

type
  TAnimationAccess=class(TTeeAnimation);
  TPropertyAnimationAccess=class(TPropertyAnimation);

procedure TAnimates.FinishedLoading(const ASource:String);
var t, tt : Integer;
    tmpA  : TTeeAnimation;
begin
  for t:=0 to Count-1 do
  with Item[t] do
  if AnimationCount>0 then
     for tt:=0 to Animations.Count-1 do
     begin
       tmpA:=Item[t].Animations[tt];

       if tmpA is TPropertyAnimation then
       with TPropertyAnimationAccess(tmpA) do
       case IsSpecial of
         1: Instance:=Self.IParent;
         2: ; // ?? Instance:=Self.IParent.Render;
         3: ; // ?? Instance:=Self.IParent.Blocks;
       end;

       TAnimationAccess(tmpA).FixupReferences(ASource);
     end;
end;

function TAnimates.Get(Index: Integer): TAnimateItem;
begin
  result:=TAnimateItem(inherited Items[Index]);
end;

procedure TAnimates.Put(Index: Integer; const Value: TAnimateItem);
begin
  inherited Items[Index]:=Value;
end;

function TAnimates.Add:TAnimateItem;

  function NameExists(AName:String):Boolean;
  var t : Integer;
  begin
    result:=False;

    AName:=UpperCase(AName);

    for t:=0 to Count-1 do
        if UpperCase(Item[t].Description)=AName then
        begin
          result:=True;
          break;
        end;
  end;

  function MakeName(Index:Integer):String;
  begin
    result:='Animation '+TeeStr(Index);
  end;

var t : Integer;
begin
  result:=(inherited Add as TAnimateItem);

  t:=1;
  while NameExists(MakeName(t)) do
        Inc(t);

  result.FDesc:=MakeName(t);
end;

function TAnimates.IndexOf(ADescription: String): TAnimateItem;
var t : Integer;
begin
  result:=nil;

  ADescription:=UpperCase(ADescription);

  for t:=0 to Count-1 do
  if UpperCase(Item[t].Description)=ADescription then
  begin
    result:=Item[t];
    break;
  end;
end;

procedure TAnimates.PrepareList;
var t, tt : Integer;
begin
  for t:=0 to Count-1 do
  begin
    Item[t].List.Clear;

    with Item[t].Animations do
    for tt:=0 to Count-1 do
    if Animation[tt].Name<>'' then
        Item[t].List.Add(Animation[tt].Name);
  end;
end;

function AngleOf(const P0,P1,P2:TFloatPoint):Double;
var tmpDist01,
    tmpDist12,
    tmpDist02,
    tmpAngle0 : Double;
    tmpAngle2 : Double;
begin
  tmpDist01:=Sqrt(Sqr(P1.X-P0.X)+Sqr(P1.Y-P0.Y));
  tmpDist12:=Sqrt(Sqr(P2.X-P1.X)+Sqr(P2.Y-P1.Y));
  tmpDist02:=Sqrt(Sqr(P2.X-P0.X)+Sqr(P2.Y-P0.Y));

  if (tmpDist01>tmpDist12) and (tmpDist01>tmpDist02) then
  begin
    tmpAngle2:=ArcCos((Sqr(tmpDist12)+Sqr(tmpDist02)-Sqr(tmpDist01))/(2*tmpDist12*tmpDist02));
    result:=ArcSin(tmpDist02*Sin(tmpAngle2)/tmpDist01);
  end
  else
  if (tmpDist12>tmpDist01) and (tmpDist12>tmpDist02) then
  begin
    tmpAngle0:=ArcCos((Sqr(tmpDist01)+Sqr(tmpDist02)-Sqr(tmpDist12))/(2*tmpDist01*tmpDist02));
    tmpAngle2:=ArcSin(tmpDist01*Sin(tmpAngle0)/tmpDist12);
    result:=Pi-(tmpAngle2+tmpAngle0);
  end
  else
    result:=ArcCos((Sqr(tmpDist01)+Sqr(tmpDist12)-Sqr(tmpDist02))/(2*tmpDist01*tmpDist12));
end;

Function PointFloat(const P:TPoint):TPointFloat; overload; {$IFDEF D9}inline;{$ENDIF}
begin
  with result do
  begin
    X:=P.X;
    Y:=P.Y;
  end;
end;

Function PointFloat(const aX,aY:Single):TPointFloat; overload; {$IFDEF D9}inline;{$ENDIF}
begin
  with result do
  begin
    X:=aX;
    Y:=aY;
  end;
end;

Function PointFloat(const aX,aY,aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}
begin
  with result do
  begin
    X:=aX;
    Y:=aY;
    Z:=aZ;
  end;
end;

Function PointFloat(const P:TPoint; const aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}
begin
  with result do
  begin
    X:=P.X;
    Y:=P.Y;
    Z:=aZ;
  end;
end;

Function PointFloat(const P:{$IFDEF D20}TeCanvas.{$ENDIF}TPoint3D; const aZ:Single):TPoint3DFloat; overload; {$IFDEF D9}inline;{$ENDIF}
begin
  with result do
  begin
    X:=P.X;
    Y:=P.Y;
    Z:=aZ;
  end;
end;

function CalculateNormal(const A,B,C:TPoint3DFloat):TPoint3DFloat;
var difAB,
    difBC,
    cross : TPoint3DFloat;
    d     : Single;
begin
  difAB.X:= b.X - a.X;
  difAB.Y:= b.Y - a.Y;
  difAB.Z:= b.Z - a.Z;

  difBC.X:= c.X - a.X;
  difBC.Y:= c.Y - a.Y;
  difBC.Z:= c.Z - a.Z;

{
  difAB.X:= a.X - b.X;
  difAB.Y:= a.Y - b.Y;
  difAB.Z:= a.Z - b.Z;

  difBC.X:= b.X - c.X;
  difBC.Y:= b.Y - c.Y;
  difBC.Z:= b.Z - c.Z;
}

  with cross do
  begin
    X:= (difAB.Y * difBC.Z) - (difBC.Y * difAB.Z);
    Y:= (difAB.Z * difBC.X) - (difBC.Z * difAB.X);
    Z:= (difAB.X * difBC.Y) - (difBC.X * difAB.Y);

    d:=Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z));
  end;

  if d=0 then
  begin
    result.X:=1;
    result.Y:=0;
    result.Z:=0;
  end
  else
  begin
    d:=1/d;

    result.X:=cross.X*d;
    result.Y:=cross.Y*d;
    result.Z:=cross.Z*d;
  end;
end;

{ TBlockBorder }

Constructor TBlockBorder.Create;
begin
  inherited;

  FWidth:=1;
  InitVisible(True);
end;

procedure TBlockBorder.Assign(Source: TPersistent);
begin
  if Source is TBlockBorder then
  with TBlockBorder(Source) do
  begin
    Self.FColor:=FColor;
    Self.FStyle:=FStyle;
    Self.FTransp:=FTransp;
    Self.FVisible:=FVisible;
    Self.FWidth:=FWidth;

    Self.Changed;
  end
  else
    inherited;
end;

procedure TBlockBorder.InitVisible(const Value:Boolean);
begin
  FVisible:=Value;
  IDefaultVisible:=Value;
end;

procedure TBlockBorder.Changed;
begin
  IOwner.IOwner.Repaint;

  if Assigned(IChanged) then
     IChanged(Self);
end;

function TBlockBorder.IsVisibleStored:Boolean;
begin
  result:=Visible<>IDefaultVisible;
end;

procedure TBlockBorder.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TBlockBorder.SetStyle(const Value: TPenStyle);
begin
  FStyle := Value;
  Changed;
end;

procedure TBlockBorder.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  Changed;
end;

procedure TBlockBorder.SetTransp(const Value: Byte);
begin
  FTransp:=Value;
  Changed;
end;

procedure TBlockBorder.SetVisible(const Value: Boolean);
begin
  FVisible:=Value;
  Changed;
end;

{ TRectangleSides }

procedure TRectangleSides.Assign(Source: TPersistent);
begin
  if Source is TRectangleSides then
  with TRectangleSides(Source) do
  begin
    Self.FLeft:=FLeft;
    Self.FRight:=FRight;
    Self.FTop:=FTop;
    Self.FBottom:=FBottom;
  end
  else
    inherited;
end;

procedure TRectangleSides.SetBottom(const Value: TColor);
begin
  FBottom:=Value;
  IOwner.ChangedSide;
end;

procedure TRectangleSides.SetLeft(const Value: TColor);
begin
  FLeft:=Value;
  IOwner.ChangedSide;
end;

procedure TRectangleSides.SetRight(const Value: TColor);
begin
  FRight:=Value;
  IOwner.ChangedSide;
end;

procedure TRectangleSides.SetTop(const Value: TColor);
begin
  FTop:=Value;
  IOwner.ChangedSide;
end;

{ TBlockActionItem }

Constructor TBlockActionItem.Create(Collection: TCollection);
begin
  inherited;
  FActions:=TStringList.Create;
end;

Destructor TBlockActionItem.Destroy;
begin
  FActions.Free;
  inherited;
end;

procedure TBlockActionItem.Add(const Action:String);
begin
  Actions.Add(Action);
end;

procedure TBlockActionItem.Assign(Source:TPersistent);
begin
  if Source is TBlockActionItem then
  with TBlockActionItem(Source) do
  begin
    Self.Actions:=Actions;
    Self.FTrigger:=Trigger;
  end
  else
    inherited;
end;

procedure TBlockActionItem.SetActions(const Value: TStrings);
begin
  FActions.Assign(Value);
end;

procedure TBlockActionItem.SetTrigger(const Value: String);
begin
  FTrigger:=Value;
end;

{ TBlockActions }

function TBlockActions.Get(Index: Integer): TBlockActionItem;
begin
  result:=TBlockActionItem(inherited Items[Index]);
end;

class function TBlockActions.ActionToIndex(const Action:String):Integer;
begin
  if Copy(Action,1,10)='ANIMATION:' then result:=0 else
  if Copy(Action,1,7) ='ACTION:'    then result:=1 else
  if Copy(Action,1,6) ='EVENT:'     then result:=2 else
  if Copy(Action,1,4) ='SET:'       then result:=3 else
  if Copy(Action,1,5) ='DRAG:'      then result:=4 else
  if Copy(Action,1,6) ='SOUND:'     then result:=5 else
  if Copy(Action,1,6) ='DELAY:'     then result:=6 else
  if Copy(Action,1,8) ='REPAINT:'   then result:=7
                                    else result:=8;
end;

function TBlockActions.Add(Trigger:String):TBlockActionItem;
begin
  if StrToIntDef(Trigger,-1)=-1 then
     Trigger:=MakerEvents.PathToID(Trigger);

  result:=OfEvent(Trigger);

  if not Assigned(result) then
  begin
    result:=inherited Add as TBlockActionItem;
    result.FTrigger:=Trigger;
  end;
end;

function TBlockActions.OfEvent(AEvent: String): TBlockActionItem;
var t : Integer;
begin
  result:=nil;

  AEvent:=UpperCase(Trim(AEvent));

  if Copy(AEvent,1,1)='"' then
     System.Delete(AEvent,1,1);

  if Copy(AEvent,Length(AEvent),1)='"' then
     System.Delete(AEvent,Length(AEvent),1);

  for t:=0 to Count-1 do
  if UpperCase(Action[t].FTrigger)=AEvent then
  begin
    result:=Action[t];
    break;
  end;
end;

procedure TBlockActions.Put(Index: Integer; const Value: TBlockActionItem);
begin
  inherited Items[Index]:=Value;
end;

{ TTriangleBlock }

Constructor TTriangleBlock.Create(AOwner: TComponent);
begin
  inherited;
  FPoint0:=TPointXYZColor.Create(Self,0,ChangedPoint);
  FPoint1:=TPointXYZColor.Create(Self,0,ChangedPoint);
  FPoint2:=TPointXYZColor.Create(Self,0,ChangedPoint);
end;

Destructor TTriangleBlock.Destroy;
begin
  FPoint2.Free;
  FPoint1.Free;
  FPoint0.Free;
  inherited;
end;

procedure TTriangleBlock.ChangedPoint(Sender: TObject);
begin
  DeleteLists;
end;

procedure TTriangleBlock.Assign(Source: TPersistent);
begin
  if Source is TTriangleBlock then
  with TTriangleBlock(Source) do
  begin
    Self.Point0:=Point0;
    Self.Point1:=Point1;
    Self.Point2:=Point2;
  end;

  inherited;
end;

procedure TTriangleBlock.Draw; 
var tmpNoColors : Boolean;

  procedure DrawPoints;
  begin
    glBegin(GL_TRIANGLES);

    with CalculateNormal(Point0.Point,Point1.Point,Point2.Point) do
         glNormal3f(X,Y,Z);

    if (not tmpNoColors) and (Point0.FColor<>clDefault) then
       Format.SetDirectColor(Point0.FColor);

    glVertex3fv(@Point0.Point);

    if not tmpNoColors then
       if Point1.FColor<>clDefault then
          Format.SetDirectColor(Point1.FColor)
       else
          Format.SetDirectColor(Format.GetRealColor);

    glVertex3fv(@Point1.Point);

    if not tmpNoColors then
       if Point2.FColor<>clDefault then
          Format.SetDirectColor(Point2.FColor)
       else
          Format.SetDirectColor(Format.GetRealColor);

    glVertex3fv(@Point2.Point);

    glEnd;
  end;

begin
  inherited;

  if Format.Solid then
  begin
    tmpNoColors:=IPicking or Parent.FDrawBlocks.Shadows.Visible;

    if tmpNoColors then
       DrawPoints
    else
    begin
      Format.Texture.SetAutomatic(True);

      if IList=0 then
      begin
        IList:=CreateNewList;
        DrawPoints;
        glEndList;
      end
      else
        glCallList(IList);

      Format.Texture.SetAutomatic(False);
    end;

    if Format.PreparePen then
    begin
      glBegin(GL_LINE_LOOP);

      glVertex3fv(@Point0.Point);
      glVertex3fv(@Point1.Point);
      glVertex3fv(@Point2.Point);

      glEnd;

      Format.FinishPen;
    end;
  end;
end;

procedure TTriangleBlock.SetPoint0(const Value: TPointXYZColor);
begin
  FPoint0.Assign(Value);
end;

procedure TTriangleBlock.SetPoint1(const Value: TPointXYZColor);
begin
  FPoint1.Assign(Value);
end;

procedure TTriangleBlock.SetPoint2(const Value: TPointXYZColor);
begin
  FPoint2.Assign(Value);
end;

procedure TTriangleBlock.PrepareForGallery;

  function RandomCoord:Double;
  const
    Range=1000000;
    HalfRange=500000;
  begin
    result:=(Random(Range)-HalfRange)/HalfRange;
  end;

begin
  inherited;

  Point0.Point:=PointFloat(RandomCoord,RandomCoord,RandomCoord);
  Point1.Point:=PointFloat(RandomCoord,RandomCoord,RandomCoord);
  Point2.Point:=PointFloat(RandomCoord,RandomCoord,RandomCoord);
end;

procedure TTriangleBlock.DeleteLists;
begin
  inherited;
  DeleteList(IList);
end;

{ TPointXYZColor }

Constructor TPointXYZColor.Create(const AOwner:TPersistent=nil; const AValue:Double=0;
                                  const ChangedEvent:TNotifyEvent=nil);
begin
  inherited;
  FColor:=clDefault;
end;

procedure TPointXYZColor.Assign(Source: TPersistent);
begin
  if Source is TPointXYZColor then
     FColor:=TPointXYZColor(Source).FColor;

  inherited;
end;

procedure TPointXYZColor.SetColor(const Value: TColor);
begin
  if FColor<>Value then
  begin
    FColor:=Value;
    DoChanged;
  end;
end;

{ TMakerSystem }

function TMakerSystem.GetNow:TDateTime;
begin
  result:=SysUtils.Now;
end;

function TMakerSystem.GetRandom:Double;
begin
  result:=System.Random;
end;

{ TObjectBlockHandle }

Destructor TObjectBlockHandle.Destroy;
begin
  Locations:=nil;
  inherited;
end;

function TObjectBlockHandle.AddHandle(const ALocation:TPoint3DFloat; const DragAction:String;
                       const LeftClickAction:String=''):TCustomBlock;
begin
  result:=AddHandle(ALocation.X,ALocation.Y,ALocation.Z,DragAction,LeftClickAction);
end;

function TObjectBlockHandle.AddHandle(const x,y,z:Single;
                                      const DragAction:String;
                                      const LeftClickAction:String=''):TCustomBlock;
begin
  if HandleClass=nil then
     HandleClass:=TSphereBlock;

  SetLength(Locations,Length(Locations)+1);
  Locations[Items.Count]:=Point3D(x,y,z);

  result:=HandleClass.Create(Self);

  with result do
  begin
    Format.Transparency:=200;
    Size.SetPoint(16,4,24);
  end;

  result.Cursor:=crHandPoint;

  result.OnDragging:=OnDragging;

  result.Actions.Add('mouse.enter').Add('set:Format.Transparency=0,Animated,5');
  result.Actions.Add('mouse.exit').Add('set:Format.Transparency=200,Animated,5');

  if DragAction<>'' then
  with result.Actions.Add(BlockAction_LeftDrag) do
  begin
    Add('Drag: Target.'+DragAction);
    Add('Event:');
  end;

  if LeftClickAction<>'' then
  with result.Actions.Add(BlockAction_LeftClick) do
  begin
    Add('Set: Target.'+LeftClickAction);
    Add('Event:');
  end;

  result.Parent:=Items;
end;

{ Support Procedures }

function IsColorProperty(AObject: TObject; const AName: String): Boolean;
var tmp : PPropInfo;
begin
  tmp:=GetPropInfo(AObject,AName);

  result:= Assigned(tmp) and
           (tmp.PropType^.Kind=tkInteger) and
           (tmp.PropType^.Name='TColor');
end;

// TPoint3DFloat

function AddPoints(const A,B:TPoint3DFloat):TPoint3DFloat; overload;
begin
  with result do
  begin
    X:=A.X+B.X;
    Y:=A.Y+B.Y;
    Z:=A.Z+B.Z;
  end;
end;

function AddPoints(const Bounds:TBounds; const Value:TPoint3DFloat):TBounds; overload;
begin
  result.Min:=AddPoints(Bounds.Min,Value);
  result.Max:=AddPoints(Bounds.Max,Value);
end;

function AddMultiply(const A,B:TPoint3DFloat; const Value:Double):TPoint3DFloat;
begin
  with result do
  begin
    X:=A.X+B.X*Value;
    Y:=A.Y+B.Y*Value;
    Z:=A.Z+B.Z*Value;
  end;
end;

function AbsSubtract(const A,B:TPoint3DFloat):TPoint3DFloat;
begin
  with result do
  begin
    X:=Abs(A.X-B.X);
    Y:=Abs(A.Y-B.Y);
    Z:=Abs(A.Z-B.Z);
  end;
end;

function MaxValue(const P:TPoint3DFloat):Double;
begin
  with P do
  if X>Y then
  begin
    if X>Z then
       result:=X
    else
       result:=Z
  end
  else
  if Y>Z then
     result:=Y
  else
     result:=Z;
end;

function MinValue(const P:TPoint3DFloat):Double;
begin
  with P do
  if X<Y then
  begin
    if X<Z then
       result:=X
    else
       result:=Z
  end
  else
  if Y<Z then
     result:=Y
  else
     result:=Z;
end;

// result := A-B
function Subtract(const A,B:TPoint3DFloat):TPoint3DFloat;
begin
  with result do
  begin
    X:=(A.X-B.X);
    Y:=(A.Y-B.Y);
    Z:=(A.Z-B.Z);
  end;
end;

function Center(const A,B:TPoint3DFloat):TPoint3DFloat;
begin
  with result do
  begin
    X:=(A.X+B.X)*0.5;
    Y:=(A.Y+B.Y)*0.5;
    Z:=(A.Z+B.Z)*0.5;
  end;
end;

procedure TMakerEvents.AddEvent(const ActionID,ActionPath:String);
var tmp : TActionEvent;
begin
  tmp:=TActionEvent.Create;
  tmp.ID:=ActionID;
  tmp.Path:=ActionPath;
//  tmp.Event:=ActionEvent;

  Add(tmp);
end;

function TMakerEvents.PathToID(const Path:String):String;
var t : Integer;
begin
  result:=UpperCase(Path);

  for t:=0 to Count-1 do
  with TActionEvent(Items[t]) do
      if result=UpperCase(Path) then
      begin
        result:=ID;
        exit;
      end;

  result:='';
end;

function TMakerEvents.IDToPath(const ID:String):String;
var t : Integer;
begin
  result:=UpperCase(ID);

  for t:=0 to Count-1 do
  with TActionEvent(Items[t]) do
      if result=UpperCase(ID) then
      begin
        result:=Path;
        exit;
      end;

  result:='';
end;

procedure FreeEvents;
var t : Integer;
begin
  for t:=0 to MakerEvents.Count-1 do
      TActionEvent(MakerEvents[t]).Free;

  MakerEvents.Free;
end;

initialization
  MakerEvents:=TMakerEvents.Create;

  with MakerEvents do
  begin
    AddEvent(BlockAction_LeftClick,Tee_MouseLeft);
    AddEvent(BlockAction_RightClick,Tee_MouseRight);
    AddEvent(BlockAction_LeftDrag,Tee_MouseLeftDrag);
    AddEvent(BlockAction_RightDrag,Tee_MouseRightDrag);
    AddEvent(BlockAction_MouseEnter,Tee_MouseEnter);
    AddEvent(BlockAction_MouseExit,Tee_MouseExit);
    AddEvent(BlockAction_LeftDoubleClick,Tee_MouseLeftDoubleClick);
    AddEvent(BlockAction_RightDoubleClick,Tee_MouseRightDoubleClick);
    AddEvent(BlockAction_WheelDrag,Tee_MouseWheelDrag);
    AddEvent(BlockAction_LifeBirth,Tee_LifeBirth);
    AddEvent(BlockAction_LifeDeath,Tee_LifeDeath);
  end;

  MakerSystem:=TMakerSystem.Create(nil);
  MakerSystem.Name:='MakerSystem';

  BlockClasses:=TBlockClasses.Create;

  RegisterBlocks( [
    TBeveledCubeBlock,
    TConeBlock,
    TCubeBlock,
    TCylinderBlock,
    TEllipseBlock,
    TEllipsoidBlock,
    TLightBlock,
    TGradientBlock,
    TObjectBlock,
    TPieSliceBlock,
    TPyramidBlock,
    TRectangleBlock,
    TSphereBlock,
    TTetrahedronBlock,
    TeeBlocks.TTeeTextBlock,
    TTorusBlock,
    TTriangleBlock,
    TTubeBlock]
      );

  InitializeCriticalSection(InternalLock);

  TPlaySoundAnimation.SetGlobalPath(TBlocks.GetLibraryPath+'\Sounds');

finalization
  DeleteCriticalSection(InternalLock);
  BlockClasses.Free;
  MakerSystem.Free;
  FreeEvents;
end.
