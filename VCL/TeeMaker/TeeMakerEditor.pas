{********************************************}
{ TeeMaker 2.0                               }
{ Copyright (c) 2002-2026 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeMakerEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages, OpenGL2,
  {$ELSE}
  OpenGLLinux,
  {$ENDIF}
  {$IFNDEF D15}
  FileCtrl,
  {$ENDIF}
  SysUtils, Classes,

  {$IFDEF D6}
  Types,
  {$ENDIF}

  {$IFDEF D17}
  System.UITypes,
  {$ENDIF}

  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls, QCheckLst, QExtCtrls,
  QMenus, QButtons, QImgList, QGrids, QActnList,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, CheckLst, ExtCtrls,
  Menus, Buttons, ImgList, ActnList, Grids,
  {$ENDIF}
  Math, TypInfo,

  TeeProcs, TeCanvas, TeeJPEG, TeeGIF, TeePNG,
  TeeComma, TeeOpenGL, TeeGLEditor, TeeGLCanvas, TeeEdiGrad,
  TeeEdiFont, TeeBrushDlg, TeeEdiPane,
  TeeBackImage, TeeBlocks, TeeAnimate, TeeAnimateEditor, TeeFilters,
  TeeAnimationGallery, TeeExport,
  TeeConst, TeeStore, TeeURL, TeeBlockEditor, TeeStringsEditor,
  TeeMakerControl, TeeRevolution, TeeRevolutionEditor, TeeEdit,
  TeeMakerLibrary, TeeColorPalette, TeeDraw3D, TeeExtruded, TeeKinematics,
  TeeCamera;

type
  TMakerTab=class
    Maker      : TMaker;
    Tab        : TTabSheet;
    Dirty      : Boolean;

    FileName   : String;
    URLName    : String;

    URLLoaded  : Boolean;

    OldAlign   : TAlign;
    OldBoundingBox : Boolean;
    OldBounds  : TRect;
    OldParent  : TWinControl;
    OldPopup   : TPopupMenu;

    Old_OnAfterDraw : TNotifyEvent;
    Old_OnBeforeDraw : TNotifyEvent;
    Old_OnDragging : TNotifyEvent;
    Old_OnClick : TNotifyEvent;
    Old_OnClicked : TMouseEvent;
    Old_OnDblClick : TNotifyEvent;
    Old_OnDragOver : TDragOverEvent;
    Old_OnDragDrop : TDragDropEvent;
    Old_OnEndDrag  : TEndDragEvent;
    Old_OnMouseDown : TMouseEvent;
    Old_OnMouseMove : TMouseMoveEvent;
    Old_OnDoLoad  : TLoadMakerEvent;
  end;

  TMakerDropBackup=class
  private
    Active      : Boolean;
    PictureLink : String;
    Block       : TCustomBlock;
    Color       : TColor;
    IsBlock     : Boolean;
    IsTexture   : Boolean;
    IsMaker     : Boolean;
    Texture     : TBlockTexture;
    Picture     : TPicture;
  public
    Destructor Destroy; override;
    procedure Restore(Maker:TMaker);
  end;

  TMakerEditor = class(TForm)
    TeeCommander1: TTeeCommander;
    SaveDialog1: TSaveDialog;
    PageEditor: TPageControl;
    TabBlocks: TTabSheet;
    PanelTree: TPanel;
    PanelEditor: TPanel;
    Panel3: TPanel;
    BAdd: TButton;                                
    PopupMenu1: TPopupMenu;
    SplitterEditor: TSplitter;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit7: TMenuItem;
    View1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Resetview1: TMenuItem;
    Borders1: TMenuItem;
    Textures1: TMenuItem;
    Smooth1: TMenuItem;
    N4: TMenuItem;
    View3DAxes: TMenuItem;
    StatusBar1: TStatusBar;
    N6: TMenuItem;
    Duplicate1: TMenuItem;
    Insert1: TMenuItem;
    Delete1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Duplicate2: TMenuItem;
    Savetoexternal1: TMenuItem;
    Delete2: TMenuItem;
    Boundingbox1: TMenuItem;
    Locallight1: TMenuItem;
    AntiAlias1: TMenuItem;
    Gradient1: TMenuItem;
    Backimage1: TMenuItem;
    EditLink: TMenuItem;
    Tools1: TMenuItem;
    Animations1: TMenuItem;
    PanelBig: TPanel;
    SplitterAnim: TSplitter;
    TreeBlocks: TTreeView;
    ImageOpen: TImage;
    Options1: TMenuItem;
    EditMode1: TMenuItem;
    Editor1: TMenuItem;
    N9: TMenuItem;
    Reopen1: TMenuItem;
    Open2: TMenuItem;
    TabLibrary: TTabSheet;
    N10: TMenuItem;
    Source1: TMenuItem;
    N11: TMenuItem;
    Print1: TMenuItem;
    Export1: TMenuItem;
    SplitterTree: TSplitter;
    TextureQuality1: TMenuItem;
    Images: TImageList;
    N2: TMenuItem;
    BlockCustomEdit: TMenuItem;
    Wireframe1: TMenuItem;
    TabExtras: TTabSheet;
    ButtonNewFolder: TButton;
    Makelocalcopy1: TMenuItem;
    CBAutoPlay: TCheckBox;
    Panel1: TPanel;
    SBMute: TSpeedButton;
    BChange: TButton;
    PageExtras: TPageControl;
    TabFloor: TTabSheet;
    TabOther: TTabSheet;
    TabCamera: TTabSheet;
    exturesfolder1: TMenuItem;
    PageControl2: TPageControl;
    TabLinks: TTabSheet;
    TreeLinks: TTreeView;
    TabEffects: TTabSheet;
    PageEffects: TPageControl;
    TabSheet8: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    BFogColor: TButtonColor;
    SBFogDensity: TScrollBar;
    CBFogEnabled: TCheckBox;
    SBFogStart: TScrollBar;
    SBFogEnd: TScrollBar;
    CBFogStyle: TComboFlat;
    CBFogNicest: TCheckBox;
    TabSheet6: TTabSheet;
    Label6: TLabel;
    CBShadows: TCheckBox;
    ShadowColor: TScrollBar;
    ShadowTransp: TScrollBar;
    Shadows1: TMenuItem;
    TabColors: TTabSheet;
    Events1: TMenuItem;
    Link1: TMenuItem;
    Gotolink1: TMenuItem;
    Doublebuffer1: TMenuItem;
    TabSheet1: TTabSheet;
    TreePictures: TListBox;
    ReopenDummy: TMenuItem;
    Object1: TMenuItem;
    Properties1: TMenuItem;
    N5: TMenuItem;
    SBLight: TSpeedButton;
    ComboNavigate: TComboFlat;
    SBRotateBlock: TSpeedButton;
    SBMoveBlock: TSpeedButton;
    SBSizeBlock: TSpeedButton;
    Rename1: TMenuItem;
    Panel5: TPanel;
    BChangePic: TButton;
    BViewPic: TButton;
    Panel6: TPanel;
    BViewObject: TButton;
    TabLighting: TTabSheet;
    TabSheet2: TTabSheet;
    Panel7: TPanel;
    LCachedFonts: TLabel;
    ListFonts: TListBox;
    TabBack: TTabSheet;
    N7: TMenuItem;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    PopupTabs: TPopupMenu;
    Closetab1: TMenuItem;
    Newtab1: TMenuItem;
    ReplaceItem: TMenuItem;
    VerticalSync1: TMenuItem;
    TabAdvanced: TTabSheet;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    CBThreading: TCheckBox;
    Button2: TButton;
    CBClickToFocus: TCheckBox;
    LShadowTransp: TLabel;
    Label25: TLabel;
    ShapeShadowColor: TShape;
    FullScreen1: TMenuItem;
    BDelete: TButton;
    CBShadowSmooth: TCheckBox;
    Panel8: TPanel;
    LBCameras: TListBox;
    PanelCamera: TPanel;
    Splitter1: TSplitter;
    Panel10: TPanel;
    SBAddCamera: TSpeedButton;
    SBRemoveCamera: TSpeedButton;
    BRenameCamera: TButton;
    Blocks1: TMenuItem;
    SourceMaker1: TMenuItem;
    Rename2: TMenuItem;
    Replace1: TMenuItem;
    CBFPS: TCheckBox;
    Panel9: TPanel;
    Label4: TLabel;
    Label22: TLabel;
    Label21: TLabel;
    Label18: TLabel;
    SpeedButton1: TSpeedButton;
    LReflection: TLabel;
    LReflectDist: TLabel;
    SBReflection: TScrollBar;
    SBReflectDistance: TScrollBar;
    LimitFloor: TComboFlat;
    EDefFloorTexture: TEdit;
    CBTransp3D: TCheckBox;
    Panel11: TPanel;
    Button1: TButton;
    SBEditMode: TSpeedButton;
    Kinematics1: TMenuItem;
    TabSheet5: TTabSheet;
    Label13: TLabel;
    LSpeed: TLabel;
    Label5: TLabel;
    Label14: TLabel;
    Label26: TLabel;
    RGNavigate: TRadioGroup;
    GroupBox2: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Joy1X: TLabel;
    Joy1Y: TLabel;
    Joy1Z: TLabel;
    CBUseJoystick1: TCheckBox;
    CameraInertia: TScrollBar;
    CBMouseWheel: TComboFlat;
    EWalkSpeed: TEdit;
    UDWalkSpeed: TUpDown;
    GroupBox3: TGroupBox;
    CBAnaglyph: TComboFlat;
    Label20: TLabel;
    SBAnaglyphDistance: TScrollBar;
    Panel12: TPanel;
    Label7: TLabel;
    TBAmbientLight: TTrackBar;
    LAmbientLight: TLabel;
    Label8: TLabel;
    ESmoothSize: TEdit;
    UDSmoothSize: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BDupliClick(Sender: TObject);
    procedure Orderbyname1Click(Sender: TObject);
    procedure NoOrder1Click(Sender: TObject);
    procedure CBFilterChange(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Resetview1Click(Sender: TObject);
    procedure Borders1Click(Sender: TObject);
    procedure Textures1Click(Sender: TObject);
    procedure Smooth1Click(Sender: TObject);
    procedure View3DAxesClick(Sender: TObject);
    procedure Duplicate1Click(Sender: TObject);
    procedure Insert1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Duplicate2Click(Sender: TObject);
    procedure Savetoexternal1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete2Click(Sender: TObject);
    procedure Edit7Click(Sender: TObject);
    procedure Boundingbox1Click(Sender: TObject);
    procedure Locallight1Click(Sender: TObject);
    procedure AntiAlias1Click(Sender: TObject);
    procedure Gradient1Click(Sender: TObject);
    procedure Backimage1Click(Sender: TObject);
    procedure EditLinkClick(Sender: TObject);
    procedure BEditLinkClick(Sender: TObject);
    procedure Animations1Click(Sender: TObject);
    procedure TreeBlocksEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure TreeBlocksChange(Sender: TObject; Node: TTreeNode);
    procedure EditMode1Click(Sender: TObject);
    procedure Editor1Click(Sender: TObject);
    procedure Open2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeLinksChange(Sender: TObject; Node: TTreeNode);
    procedure SBRotateBlockClick(Sender: TObject);
    procedure SBMoveBlockClick(Sender: TObject);
    procedure Source1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure SBSizeBlockClick(Sender: TObject);
    procedure TeeCommander1SetLabel(Sender: TTeeCommander;
      var Text: String);
    procedure TextureQuality1Click(Sender: TObject);
    procedure TreeBlocksGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeBlocksGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeBlocksDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeBlocksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure BlockCustomEditClick(Sender: TObject);
    procedure Wireframe1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure SBReflectionChange(Sender: TObject);
    procedure SBFogDensityChange(Sender: TObject);
    procedure CBFogEnabledClick(Sender: TObject);
    procedure BFogColorClick(Sender: TObject);
    procedure SBFogStartChange(Sender: TObject);
    procedure SBFogEndChange(Sender: TObject);
    procedure CBFogStyleChange(Sender: TObject);
    procedure ButtonNewFolderClick(Sender: TObject);
    procedure TreeBlocksDblClick(Sender: TObject);
    procedure Makelocalcopy1Click(Sender: TObject);
    procedure CBAutoPlayClick(Sender: TObject);
    procedure SBMuteClick(Sender: TObject);
    procedure TreeBlocksKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeBlocksAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure BChangeClick(Sender: TObject);
    procedure SBReflectDistanceChange(Sender: TObject);
    procedure CBShadowsClick(Sender: TObject);
    procedure ShadowColorChange(Sender: TObject);
    procedure ShadowTranspChange(Sender: TObject);
    procedure CBFogNicestClick(Sender: TObject);
    procedure SBLightClick(Sender: TObject);
    procedure RGNavigateClick(Sender: TObject);
    procedure PageExtrasChange(Sender: TObject);
    procedure exturesfolder1Click(Sender: TObject);
    procedure CBThreadingClick(Sender: TObject);
    procedure CBUseJoystick1Click(Sender: TObject);
    procedure PageEditorChange(Sender: TObject);
    procedure CBAnaglyphChange(Sender: TObject);
    procedure Shadows1Click(Sender: TObject);
    procedure SBAnaglyphDistanceChange(Sender: TObject);
    procedure TreeBlocksMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Events1Click(Sender: TObject);
    procedure Reopen1Click(Sender: TObject);
    procedure ComboURLDropDown(Sender: TObject);
    procedure Link1Click(Sender: TObject);
    procedure Gotolink1Click(Sender: TObject);
    procedure Doublebuffer1Click(Sender: TObject);
    procedure LimitFloorChange(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure ComboNavigateChange(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure TreePicturesDblClick(Sender: TObject);
    procedure BChangePicClick(Sender: TObject);
    procedure TreePicturesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BViewPicClick(Sender: TObject);
    procedure BViewObjectClick(Sender: TObject);
    procedure CameraInertiaChange(Sender: TObject);
    procedure CBClickToFocusClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure CloseAll1Click(Sender: TObject);
    procedure Closetab1Click(Sender: TObject);
    procedure PopupTabsPopup(Sender: TObject);
    procedure Newtab1Click(Sender: TObject);
    procedure CBMouseWheelChange(Sender: TObject);
    procedure ReplaceItemClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure X1Click(Sender: TObject);
    procedure VerticalSync1Click(Sender: TObject);
    procedure FullScreen1Click(Sender: TObject);
    procedure CBShadowSmoothClick(Sender: TObject);
    procedure LBCamerasClick(Sender: TObject);
    procedure SBAddCameraClick(Sender: TObject);
    procedure SBRemoveCameraClick(Sender: TObject);
    procedure SourceMaker1Click(Sender: TObject);
    procedure Rename2Click(Sender: TObject);
    procedure BRenameCameraClick(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure CBFPSClick(Sender: TObject);
    procedure CBTransp3DClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SBEditModeClick(Sender: TObject);
    procedure EWalkSpeedChange(Sender: TObject);
    procedure Kinematics1Click(Sender: TObject);
    procedure TBAmbientLightChange(Sender: TObject);
    procedure ESmoothSizeChange(Sender: TObject);
  private
    { Private declarations }
    ButtonOpen : TSpeedButton;
    IHistory   : TStrings;
    IColorPalette : TColorPalette;
    IFloorEditor  : TBlockEditor;

    IOnDragOver : TDragOverEvent;
    IOnDragDrop : TDragDropEvent;

    IDropBackup : TMakerDropBackup;

    IExtraBlocks : TObjectBlock;
    IMoveBlocks  : TObjectBlockHandle;
    ISizeBlocks  : TObjectBlockHandle;
    IRotateBlocks: TObjectBlockHandle;
    IDesignHandles : TObjectBlockHandle;
    ILightLamps    : TObjectBlockHandle;

    procedure ActivateAnimEditor(const AMaker:TMaker);
    Procedure AddAnimates(const Animates:TAnimates);
    procedure AddBlock(ParentNode:TTreeNode; const ABlock:TCustomBlock);
    procedure AddExtraBlocks(const HelpText:String);
    procedure AddMaker(const AMaker:TMaker);
    procedure AddNewMaker;
    procedure AnimateGallery(Sender: TObject; out Animation:TTeeAnimation; const AParent:TTeeAnimation);
    procedure AnimateGetName(const Animation:TTeeAnimation; out S:String);
    procedure AnimateHide(Sender: TObject);
    procedure AnimateModified(Sender: TObject);
    procedure AnimateSelected(Sender: TObject; out Animation:TTeeAnimation);
    procedure AnimationAddGroup(Sender: TObject);
    procedure AnimationChangeGroup(Sender: TObject);
    procedure AnimationRemoveGroup(Sender: TObject);
    procedure AnimationRenameGroup(Sender: TObject);
    procedure BlockCollision(Sender:TMovement; const ABlock:TCustomBlock;
                                              var ACollided:TCustomBlock;
                                              var APoint:TPoint3DFloat);
    function BlockUnderMouse:TCustomBlock;
    procedure BlocksLoaded(Sender: TObject);
    procedure BlocksItemsChanged(Sender: TObject);
    procedure CameraChanged(Sender: TObject);
    procedure CheckColors;
    procedure CheckLights;
    Procedure ClearBlocks(ClearCurrent:Boolean=False);
    procedure ColorPaletteChanged(Sender: TObject);
    procedure CopySelectedBlocks(const Collection:TBlocks);
    function CreateTempMaker:TMaker;
    function CurrentAnimate:TAnimateItem;
    procedure DeleteNodeAndBlock(const Node:TTreeNode);
    procedure DeleteSelectedBlocks;
    procedure DisableBlockButtons;
    procedure DoDragDrop(Source: TObject; X, Y: Integer; SetDirty:Boolean=True);
    Procedure DoLoad(const Strings:TStrings); overload;
    Function DoSaveDialog(const AMakerTab:TMakerTab):Boolean;
    function DragFromPalette(Source:TObject):Boolean;
    function DragObject(Source:TObject):Boolean;
    procedure EditorDirty(Sender:TObject);
    procedure EditPanel(Sender: TObject);
    procedure EditorSetAnimate(const AMaker:TMaker);
    function FileNameOrURLName:String;
    procedure FillBlockList;
    procedure FinishLoad(AName:String);
    procedure FinishSetup;
    function HasCurrentMaker:Boolean;

    procedure ListAnimDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListAnimDragDrop(Sender, Source: TObject; X, Y: Integer);

    function LoadNewObjectFile(const APath,AFile,AExt:String):TCustomObjectBlock;
    procedure Maker1AfterDraw(Sender:TObject);
    procedure Maker1BeforeDraw(Sender:TObject);
    procedure Maker1Click(Sender: TObject);
    procedure Maker1Clicked(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
    procedure Maker1DblClick(Sender: TObject);
    procedure Maker1DoLoad(Sender: TMaker; const FileName:String);
    procedure Maker1DragOver(Sender, Source: TObject; X, Y: Integer;
                             State: TDragState; var Accept: Boolean);
    procedure Maker1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Maker1EndDrag(Sender, Target: TObject; X, Y: Integer);

    procedure Maker1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    function InDesignTime:Boolean;
    //procedure InspectorChanged(Sender:TTeeInspector; Item:TInspectorItem);
    function IsMakerFile(const FileName:String):Boolean;

    procedure LibraryAddObject(Sender: TObject);
    procedure LibraryClickObject(Sender: TObject);
    procedure LibraryOpenObject(Sender:TObject);
    procedure LoadNewMaker(const AFileName:String);

    function NodeOfBlock(const ABlock:TCustomBlock):TTreeNode;
    function NewBlockOwner:TComponent;
    procedure RefreshAnimEditor;
    procedure RefreshLocation(const Sender:TCustomBlock);
    procedure RefreshRotation(const Sender:TCustomBlock);
    procedure RefreshSize(const Sender:TCustomBlock);
    procedure RemoveExtraBlocks;
    procedure RemoveMaker(Index:Integer);
    procedure ReplaceBlock(const OldBlock,NewBlock:TCustomBlock);
    function SaveBlocksToFile(const AMaker:TMaker; const FileName:String):Boolean;
    function SelectedLibraryBlock:TCustomBlock;
    procedure SetBoundTitle;
    procedure SetCamera(const ACamera:TMakerCamera);
    procedure SetExtraPositions;
    procedure SetMainCaption(Text:String);
    procedure SetNewBlockLocation(const ABlock:TCustomBlock; X,Y:Integer);
    procedure ShowHideAnimEditor(DoShow:Boolean);
    procedure ShowHideEditor(DoShow:Boolean);
    procedure ShowHideSaveButton(DoShow:Boolean);
    procedure UnmarkDirty(const AMakerTab:TMakerTab=nil);
    procedure UpdateCameraData(const AMaker:TMaker);
    procedure UpdateNavigate(const AMaker:TMaker);
    procedure ViewSource(const AStream:TStream);

    {$IFNDEF CLX}
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    {$ENDIF}

  protected
    Makers     : Array of TMakerTab;
    MakerClipboard : TBlocks;

    PageMakers : TPageControl;

    CanAddFirstEmptyMaker : Boolean;

    Kinematics : TKinematics;

    IAnimEditor: TTeeAnimateEditor;
    IAnimEditorHeight : Integer;
    IAnimGalleryBlock : TCustomBlock;

    IBackEditor  : TFormTeePanel;
    IBlockEditor : TBlockEditor;
    ICameraEditor: TCameraEditor;
    IGLEditor    : TFormTeeGLEditor;
    IKinematics  : TKinematicsEditor;
    ILibrary   : TMakerLibrary;
    IHome      : String;
    IModifying : Boolean;

    WasAnimations : Boolean;
    WasEditor     : Boolean;
    WasKinematics : Boolean;

    procedure ActivateMaker(const AMaker:TMaker); virtual;
    function AddNewBlockGallery:TCustomBlock;
    procedure AddReopen(AName:String); virtual;
    procedure CheckReopen; virtual;
    Function Current:TCustomBlock;
    function CurrentMaker:TMaker;
    function CurrentMakerTab:TMakerTab;
    procedure DoAddClick(const ABlock:TCustomBlock; const ATitle:String;
                         ParentNode:TTreeNode=nil; FindParentNode:Boolean=True;
                         SelectAdded:Boolean=True);
    Procedure DoLoad(const AFile:String; AddToHistory:Boolean=True); overload;
    Function DoSave(AskToSave:Boolean=False; AMakerTab:TMakerTab=nil):Boolean;
    procedure FinishAdd(const NewBlock:TCustomBlock);
    procedure InitFirstMaker;
    procedure Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure MarkDirty;
    procedure PageMakersChange(Sender: TObject);
    procedure SetAntiAlias(Value:Boolean);
    procedure SetEditMode(Value:Boolean);
  public
    { Public declarations }

    Constructor CreateMaker(const AOwner: TComponent; const AMaker:TMaker); virtual;

    class function ModalShow(const AOwner:TComponent; const AMaker:TMaker):Boolean; overload;
    class function ModalShow(const AOwner:TComponent; const FileName:String):Boolean; overload;
    class function ModalShow(const AOwner:TComponent; const ABlock:TCustomObjectBlock):Boolean; overload;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  TeeDraw3DEditor, TeeTextureSelector,

  {$IFNDEF BCB}
  TeePlayMP3,
  {$ENDIF}

  {$IFDEF CLX}
  Qt,
  {$ENDIF}

  TeeLoadBlock, TeePenDlg, TeeMakerConst, TeeProperties,
  TeePipe, TeeBlockGallery, TeeBlockFormat, TeeRain, TeeHelix,
  Registry, ShellAPI, TeeBlockReplacer, TeeWater, TeeNumberAnimation,
  TeeBlockAnimations, TeeActionAnimation, TeeMoveAnimation,
  TeePointItemEditor, TeeObjFormat, Tee3DSFormat, TeeClipBlock;

Constructor TMakerEditor.CreateMaker(const AOwner: TComponent; const AMaker: TMaker);
begin
  Create(AOwner);
  AddMaker(AMaker);
end;

type
  TMakerEditorClass=class of TMakerEditor;

class function TMakerEditor.ModalShow(const AOwner:TComponent; const AMaker:TMaker):Boolean;
var tmp : TMakerEditorClass;
begin
  tmp:=Self;

  with tmp.CreateMaker(AOwner,AMaker) do
  try
    FinishSetup;
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

class function TMakerEditor.ModalShow(const AOwner:TComponent; const ABlock:TCustomObjectBlock):Boolean;
var tmp : TMaker;
    Old : TBlocks;
begin
  Old:=ABlock.Parent;
  tmp:=TMaker.Create(nil);
  try
    ABlock.Parent:=tmp.Blocks;
    result:=ModalShow(AOwner,tmp);
  finally
    ABlock.Parent:=Old;
    tmp.Free;
  end;
end;

class function TMakerEditor.ModalShow(const AOwner:TComponent; const FileName:String):Boolean;
var Maker : TMaker;
begin
  Maker:=TMaker.Create(nil);
  try
    Maker.Blocks.LoadFromFile(FileName);
    result:=ModalShow(AOwner,Maker);
  finally
    Maker.Free;
  end;
end;

procedure TMakerEditor.UnmarkDirty(const AMakerTab:TMakerTab=nil);
begin
  if Assigned(AMakerTab) then
     AMakerTab.Dirty:=False
  else
     CurrentMakerTab.Dirty:=False;

  TeeCommander1.ButtonSave.Enabled:=False;
end;

function TMakerEditor.CurrentMakerTab:TMakerTab;
begin
  if Assigned(PageMakers) then
  begin
    if PageMakers.ActivePageIndex=-1 then
       result:=nil
    else
    if Length(Makers)>PageMakers.ActivePageIndex then
       result:=Makers[PageMakers.ActivePageIndex]
    else
       result:=nil;
  end
  else
  if Length(Makers)>0 then
     result:=Makers[0]
  else
     result:=nil;
end;

function TMakerEditor.CurrentMaker:TMaker;
var tmp : TMakerTab;
begin
  tmp:=CurrentMakerTab;

  if Assigned(tmp) then
     result:=tmp.Maker
  else
     result:=nil;
end;

type
  TCommAccess=class(TTeeCommander);

procedure TMakerEditor.EditPanel(Sender: TObject);
begin
  TDraw3DEditor.Edit(Self,CurrentMaker);
end;

type
  TBlocksAccess=class(TBlocks);
  TGLCanvasAccess=class(TGLCanvas);

procedure TMakerEditor.AddMaker(const AMaker:TMaker);
var tmp  : TMakerTab;
    tmpL : Integer;
begin
  tmp:=TMakerTab.Create;

  tmpL:=Length(Makers);
  SetLength(Makers,tmpL+1);
  Makers[tmpL]:=tmp;

  with tmp do
  begin
    OldAlign:=AMaker.Align;
    OldBoundingBox:=AMaker.Options.BoundingBox;
    OldBounds:=AMaker.BoundsRect;
    OldParent:=AMaker.Parent;
    OldPopup:=AMaker.PopupMenu;

    Maker:=AMaker;

    Maker.Align:=alClient;
  end;

  if Assigned(PageMakers) then
  begin
    tmp.Tab:=TTabSheet.Create(Self);

    tmp.Tab.Caption:=tmp.Maker.Name;

    tmp.Tab.PageControl:=PageMakers;
    PageMakers.ActivePage:=tmp.Tab;

    tmp.Maker.Parent:=tmp.Tab;
  end
  else
    tmp.Maker.Parent:=PanelBig;

  with tmp do
  begin
    Old_OnAfterDraw:= Maker.OnAfterDraw;
    Old_OnBeforeDraw:= Maker.OnBeforeDraw;
    Old_OnClick:= Maker.OnClick;
    Old_OnClicked:= Maker.OnClickedBlock;
    Old_OnDblClick:= Maker.OnDblClick;
    Old_OnDoLoad:= Maker.OnDoLoad;
    Old_OnDragDrop:= Maker.OnDragDrop;
    Old_OnEndDrag:= Maker.OnEndDrag;
    Old_OnDragOver:= Maker.OnDragOver;
    Old_OnMouseDown:= Maker.OnMouseDown;
    Old_OnMouseMove:= Maker.OnMouseMove;

    Maker.OnAfterDraw:= Maker1AfterDraw;
    Maker.OnBeforeDraw:= Maker1BeforeDraw;
    Maker.OnClick:= Maker1Click;
    Maker.OnClickedBlock:=Maker1Clicked;
    Maker.OnDoLoad:=Maker1DoLoad;
    Maker.OnDblClick:=Maker1DblClick;
    Maker.OnDragDrop:= Maker1DragDrop;
    Maker.OnDragOver:= Maker1DragOver;
    Maker.OnEndDrag:= Maker1EndDrag;
    Maker.OnMouseDown:= Maker1MouseDown;
    Maker.OnMouseMove:= Maker1MouseMove;

    TBlocksAccess(Maker.Blocks).OnItemsChanged:=BlocksItemsChanged;
    TBlocksAccess(Maker.Blocks).OnLoaded:=BlocksLoaded;

    Maker.Options.SelectMode:=True;

    if Maker.Blocks.Animates.Count=0 then
       Maker.Blocks.Animates.Add;
  end;

  ActivateMaker(tmp.Maker);
end;

procedure TMakerEditor.ActivateMaker(const AMaker:TMaker);
begin
  AMaker.Options.BoundingBox:=EditMode1.Checked;

  AntiAlias1.Checked:=AMaker.Render.Antialias;
  View3DAxes.Checked:=AMaker.Options.View3DAxes;
  Boundingbox1.Checked:=AMaker.Options.BoundingBox;

  TeeCommander1.Panel:=AMaker;

  TeeCommander1.ButtonSave.Enabled:=CurrentMakerTab.Dirty;

  ComboNavigate.ItemIndex:=Ord(AMaker.Options.Navigate.Mode);

  BFogColor.LinkProperty(AMaker.Options.Fog,'Color');  // Do not localize
  SBFogDensity.Position:=Round(100*AMaker.Options.Fog.Density);

  with AMaker.Options.Fog do
  begin
    CBFogNicest.Checked:=not Fast;
    CBFogEnabled.Checked:=Enabled;
    CBFogStyle.ItemIndex:=Ord(Style);
  end;

  TreeBlocks.Selected:=nil;
  TreeBlocks.Items.Clear;

  LBCameras.Items.Clear;
  LBCamerasClick(Self);

  DisableBlockButtons;

  AMaker.PopupMenu:=PopupMenu1;

  if PageEditor.Visible then
     PageEditorChange(Self);

  ActivateAnimEditor(AMaker);
end;

procedure TMakerEditor.BlocksLoaded(Sender: TObject);
begin
  TreeBlocks.Invalidate;
end;

function TMakerEditor.InDesignTime:Boolean;
begin
  result:=(csDesigning in CurrentMaker.ComponentState);
end;

function TMakerEditor.CreateTempMaker:TMaker;

  function MakerNameExists(tmp:Integer):Boolean;
  var t : Integer;
      tmpName : String;
  begin
    result:=False;

    tmpName:=UpperCase('Maker'+TeeStr(tmp));

    for t:=0 to Length(Makers)-1 do
    if UpperCase(Makers[t].Maker.Name)=tmpName then
    begin
      result:=True;
      break;
    end;
  end;

var tmpName : Integer;
begin
  result:=TMaker.Create(Self);

  tmpName:=1;
  while MakerNameExists(tmpName) do
        Inc(tmpName);

  result.Name:='Maker'+TeeStr(tmpName);
end;

procedure TMakerEditor.AddNewMaker;
begin
  AddMaker(CreateTempMaker);
end;

type
  TTeeAnimateAccess=class(TTeeAnimate);

procedure TMakerEditor.FormCreate(Sender: TObject);

  procedure VerifyLibraryPathRegistry;
  var tmp : String;
      tmpDir : String;
  begin
    tmp:=TeeMakerReadRegistry('',TeeMakerLibRegistry,'');

    if tmp='' then
    begin
      tmpDir:=RemoveTrailingSlash(ExtractFilePath(Application.ExeName));

      if not {$IFDEF D15}SysUtils.{$ENDIF}DirectoryExists(tmpDir+'\'+TeeMsg_MakerLibraryFolder) then
      begin
        ShowMessage(Format(TeeMsg_TeeMakerWelcome,[tmpDir]));

        tmpDir:=tmpDir+'\'+TeeMsg_MakerLibraryFolder;

        ForceDirectories(tmpDir+'\'+TeeMsg_MakerLibraryObjects);
        ForceDirectories(tmpDir+'\'+TeeMsg_MakerLibrarySounds);
        ForceDirectories(tmpDir+'\'+TeeMsg_MakerLibraryTextures);

        TeeMakerWriteRegistry('',TeeMakerLibRegistry,tmpDir);
      end;
    end;
  end;

begin
  CanAddFirstEmptyMaker:=True;
  
  MakerClipboard:=TBlocks.Create(nil);

  {$IFDEF D6}
  TreeBlocks.MultiSelect:=True;
  {$ENDIF}

  Doublebuffer1.Visible:=  Win32MajorVersion<6;

  PageEditor.DoubleBuffered:=True;

  BFogColor.FullOpen:=True;

  IDropBackup:=TMakerDropBackup.Create;

  IExtraBlocks:=TObjectBlock.Create(Self);
  IExtraBlocks.Items.Properties.Add('');

  IHistory:=TStringList.Create;

  IBlockEditor:=TBlockEditor.Create(Self);
  IBlockEditor.PanelButtons.Visible:=False;

  VerifyLibraryPathRegistry;

  ILibrary:=TMakerLibrary.Create(Self);
  ILibrary.Align:=alClient;

  ILibrary.TreeObjects.Images:=Images;
  ILibrary.TreeTextures.Images:=Images;
  ILibrary.OnOpenObject:=LibraryOpenObject;
  ILibrary.Addtoscene1.OnClick:=LibraryAddObject;
  ILibrary.TreeObjects.OnClick:=LibraryClickObject;

  TTeeVCL.AddFormTo(ILibrary,TabLibrary);

  IBlockEditor.OnDirty:=EditorDirty;
  IBlockEditor.BlocksTreeView:=TreeBlocks;
  IBlockEditor.OpenDialog1.InitialDir:=GetCurrentDir;
  IBlockEditor.Align:=alClient;

  TTeeVCL.AddFormTo(IBlockEditor,PanelEditor);

  PanelEditor.Visible:=False;

  EDefFloorTexture.Text:=TMakerFloor.DefaultTexture;
  
  {$IFDEF D6}
  {$IFNDEF CLX}
  TreeBlocks.MultiSelect:=True;
  {$ENDIF}
  {$ENDIF}

  N7.Visible:=Assigned(PageMakers);
  Close1.Visible:=N7.Visible;
  CloseAll1.Visible:=N7.Visible;
end;

procedure TMakerEditor.LibraryClickObject(Sender: TObject);
var tmp : String;
begin
  if Assigned(ILibrary.TreeObjects.Selected) then
  begin
    if ILibrary.IsBasicNode(ILibrary.TreeObjects.Selected) then
       tmp:=ILibrary.TreeObjects.Selected.Text
    else
    if ILibrary.IsBasicFolder(ILibrary.TreeObjects.Selected) then
       tmp:='Basic'
    else
    begin
      tmp:=ILibrary.SelectedLinkFile;

      if ILibrary.NodeIsFile(ILibrary.TreeObjects.Selected) then
         if not TeeIsURL(tmp) then
            tmp:=tmp+' ('+IntToStr(FileSize(tmp))+' bytes)';
    end;
  end
  else
    tmp:='';

  StatusBar1.SimpleText:=tmp;
end;

function FormatData(const Value:Double):String;
begin
  result:=FormatFloat('0.##',Value);
end;

procedure TMakerEditor.UpdateNavigate(const AMaker:TMaker);
begin
  with AMaker.Options.Navigate do
  begin
    LSpeed.Caption:=FormatData(FlySpeed);
    CameraInertia.Position:=RotateInertia;
    CBMouseWheel.ItemIndex:=Ord(MouseWheel);

    with Joystick1.Position do
    begin
      Joy1X.Caption:=FormatData(X);
      Joy1Y.Caption:=FormatData(Y);
      Joy1Z.Caption:=FormatData(Z);
    end;
  end;
end;

type
  TPropertyAnimationAccess=class(TPropertyAnimation);
  TMakerOptionsAccess=class(TMakerOptions);

procedure TMakerEditor.Maker1AfterDraw(Sender:TObject);

  procedure DrawAnimationBounds;

    function TryDrawBounds(AObject:TObject):Boolean;
    begin
      result:=Assigned(AObject) and (AObject is TCustomBlock);

      if result then
         TMakerOptionsAccess(CurrentMaker.Options).DrawBoundingBox(TCustomBlock(AObject),clAqua,False)
    end;

  begin
    if Assigned(IAnimEditor) and (IAnimEditor.Selected<>nil) then
    with IAnimEditor do
    if Selected is TPropertyAnimation then
    with TPropertyAnimationAccess(Selected) do
         if not TryDrawBounds(IRealInstance) then
            TryDrawBounds(Instance);
  end;

begin
  if PageEditor.Visible and (PageEditor.ActivePage=TabExtras) and
     (PageExtras.ActivePage=TabCamera) then
       UpdateNavigate(CurrentMaker);

  if CurrentMaker.Options.SelectMode then
     DrawAnimationBounds;
end;

procedure TMakerEditor.Maker1BeforeDraw(Sender:TObject);
begin
  if EditMode1.Checked then
     SetExtraPositions;
end;

procedure TMakerEditor.RefreshSize(const Sender:TCustomBlock);
begin
  IBlockEditor.RefreshSize;
  SetBoundTitle;
end;

procedure TMakerEditor.UpdateCameraData(const AMaker:TMaker);
var t : Integer;
begin
  if LBCameras.Items.Count=0 then
  with CurrentMaker.Options.Cameras do
  if Count>0 then
  begin
    for t:=0 to Count-1 do
        LBCameras.Items.Add(Camera[t].Title);

    LBCameras.ItemIndex:=0;
  end;

  SetCamera(CurrentMaker.Options.Cameras.Selected);
end;

procedure TMakerEditor.EditorDirty(Sender:TObject);
begin
  MarkDirty;
end;

procedure TMakerEditor.RemoveMaker(Index:Integer);
begin
  if Makers[Index].Maker.Owner=Self then
     Makers[Index].Maker.Free;

  Makers[Index].Free;
end;

type
  TMakerAccess=class(TMaker);

procedure TMakerEditor.FormDestroy(Sender: TObject);

  procedure SaveOptions;
  begin
    with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;

      if OpenKey(TeeMakerKey+'\BlockEditor',True) then
      begin
        WriteInteger('PanelTree',PanelEditor.Height);
        WriteInteger('PageTree',PageEditor.Width);

        if Assigned(IAnimEditor) then
           WriteInteger('PanelBig',IAnimEditor.Height);

        WriteBool('AnimateEditor',Animations1.Checked);

        CloseKey;
      end;

      if OpenKey(TeeMakerKey+'\Floor',True) then
      begin
        WriteString('FloorTexture',EDefFloorTexture.Text);
        CloseKey;
      end;
    finally
      Free;
    end;
  end;

  procedure RemoveMakers;
  var t : Integer;
  begin
    for t:=0 to Length(Makers)-1 do
        RemoveMaker(t);

    Makers:=nil;
  end;

var Maker : TMaker;
begin
  SaveOptions;

  Maker:=CurrentMaker;

  if Assigned(Maker) and (Maker.Owner<>Self) then
  with CurrentMakerTab do
  begin
    Maker.OnAfterDraw:=Old_OnAfterDraw;
    Maker.OnBeforeDraw:=Old_OnBeforeDraw;
    Maker.OnClick:=Old_OnClick;
    Maker.OnClickedBlock:=Old_OnClicked;
    Maker.OnDblClick:=Old_OnDblClick;
    Maker.OnDoLoad:=Old_OnDoLoad;
    Maker.OnDragDrop:=Old_OnDragDrop;
    Maker.OnEndDrag:=Old_OnEndDrag;
    Maker.OnDragOver:=Old_OnDragOver;
    Maker.OnMouseDown:=Old_OnMouseDown;
    Maker.OnMouseMove:=Old_OnMouseMove;

    TBlocksAccess(Maker.Blocks).OnItemsChanged:=nil;
    TBlocksAccess(Maker.Blocks).OnLoaded:=nil;

    Maker.Align:=OldAlign;
    Maker.Options.BoundingBox:=OldBoundingBox;
    Maker.BoundsRect:=OldBounds;
    Maker.Parent:=OldParent;
    Maker.PopupMenu:=OldPopup;

    Maker.Invalidate;
  end;

  TreeBlocks.Selected:=nil;

  IExtraBlocks.Free;

  RemoveMakers;

  IHistory.Free;

  if Assigned(IAnimEditor) then
     IAnimEditor.RestoreAnimate;

  IAnimEditor.Free;
  IDropBackup.Free;

  Kinematics.Free;
  
  MakerClipboard.Free;

  {$IFNDEF LINUX}
  DragAcceptFiles({$IFDEF CLX}QWidget_winId{$ENDIF}(Handle),False);
  {$ENDIF}
end;

type
  TBlockAccess=class(TCustomBlock);
  TCustomObjectBlockAccess=class(TCustomObjectBlock);

procedure TMakerEditor.AddBlock(ParentNode:TTreeNode; const ABlock:TCustomBlock);
var t : Integer;
begin
  ParentNode:=TreeBlocks.Items.AddChildObject(ParentNode,TBlockAccess(ABlock).TitleOrName,ABlock);
  TBlockAccess(ABlock).IData:=ParentNode;

  if ABlock is TCustomObjectBlock then
  with TCustomObjectBlockAccess(ABlock) do
       if LinkFile='' then
          for t:=0 to Items.Count-1 do
              AddBlock(ParentNode,Item[t]);
end;

function TMakerEditor.NodeOfBlock(const ABlock:TCustomBlock):TTreeNode;
begin
  if Assigned(ABlock) then
     result:=TTreeNode(TBlockAccess(ABlock).IData)
  else
     result:=nil;
end;

procedure TMakerEditor.FinishAdd(const NewBlock:TCustomBlock);
begin
  if not IModifying then
  begin
    TreeBlocks.Selected:=NodeOfBlock(NewBlock);

    IBlockEditor.PageControl1.Enabled:=True;
    MarkDirty;
  end;
end;

function TMakerEditor.AddNewBlockGallery:TCustomBlock;
begin
  result:=TBlockGallery.ModalShow(Self,NewBlockOwner);

  if Assigned(result) then
  begin
    TBlockAccess(result).PrepareForGallery;
    DoAddClick(result,TBlockClasses.BlockDescription(result.ClassName));
    TBlocksAccess(CurrentMaker.Blocks).SetBlockName(result);
    IBlockEditor.RefreshBlock(Current);
  end;
end;

procedure TMakerEditor.BAddClick(Sender: TObject);
begin
  AddNewBlockGallery;
end;

function IsFolder(Block:TCustomBlock):Boolean; overload;
begin
  if Assigned(Block) and (Block is TCustomObjectBlock) then
  with TCustomObjectBlock(Block) do
       result:=(not Assigned(LinkBlock)) and (LinkFile='')
  else
       result:=False;
end;

function IsFolder(Node:TTreeNode):Boolean; overload;
begin
  result:=Assigned(Node) and IsFolder(TCustomBlock(Node.Data));
end;

procedure TMakerEditor.DoAddClick(const ABlock:TCustomBlock; const ATitle:String;
                                  ParentNode:TTreeNode=nil; FindParentNode:Boolean=True;
                                  SelectAdded:Boolean=True);
var tmpFolder : TCustomObjectBlock;
    tmpSin,tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
begin
  TBlocksAccess(CurrentMaker.Blocks).SetBlockName(ABlock);

  if CurrentMaker.Options.Navigate.Mode<>nmObserve then
  with CurrentMaker.View3DOptions do
  begin
    SinCos(RotationFloat*Pi/180,tmpSin,tmpCos);

    with ABlock.Location do
    begin
      X:=HorizOffsetFloat+300*tmpSin;
      Z:=VertOffsetFloat;
      Y:=-300*tmpCos-ZOffset;
    end;
  end;

  if FindParentNode and (not Assigned(ParentNode)) then
  begin
    ParentNode:=TreeBlocks.Selected;

    if Assigned(ParentNode) then
    while Assigned(ParentNode.Parent) do
          ParentNode:=ParentNode.Parent;
  end;

  if IsFolder(ParentNode) then
  begin
    tmpFolder:=TCustomObjectBlock(ParentNode.Data);
    ABlock.Parent:=tmpFolder.Items;
    ABlock.InitTitle(ATitle);

    AddBlock(ParentNode, ABlock);
  end
  else
  begin
    if Current<>nil then
       ABlock.Parent:=Current.Parent
    else
       ABlock.Parent:=CurrentMaker.Blocks;

    ABlock.InitTitle(ATitle);

    AddBlock(nil, ABlock);
  end;

  if SelectAdded then
     FinishAdd(ABlock);
end;

function TMakerEditor.NewBlockOwner:TComponent;
begin
  result:=TBlocksAccess(CurrentMaker.Blocks).GetChildOwner;
end;

procedure TMakerEditor.MarkDirty;
begin
  if not CurrentMakerTab.Dirty then
  begin
    CurrentMakerTab.Dirty:=True;
    TeeCommander1.ButtonSave.Enabled:=True;
  end;

  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.DeleteNodeAndBlock(const Node:TTreeNode);
begin
  TCustomBlock(Node.Data).Free;
  Node.Free;

  TreeBlocksChange(Self,TreeBlocks.Selected);
end;

procedure TMakerEditor.BDeleteClick(Sender: TObject);
begin
  if TeeYesNo(Format(TeeMsg_SureToDelete,[Current.Title])) then
  begin
    DeleteNodeAndBlock(TreeBlocks.Selected);
    MarkDirty;
  end;
end;

procedure TMakerEditor.AddReopen(AName:String);
begin
end;

procedure TMakerEditor.CheckReopen;
begin
end;

function TMakerEditor.Current: TCustomBlock;
begin
  if Assigned(TreeBlocks.Selected) then
     result:=TCustomBlock(TreeBlocks.Selected.Data)
  else
     result:=nil;
end;

procedure TMakerEditor.FillBlockList;
var t: Integer;
begin
  if (CurrentMaker.Blocks.Count>0) and (CurrentMaker.Blocks[0]<>IExtraBlocks) then
  with TreeBlocks do
  begin
    Items.BeginUpdate;
    try
      OnChange:=nil;

      Items.Clear;

      with CurrentMaker.Blocks do
      for t:=0 to Count-1 do
          if Block[t]<>IExtraBlocks then
             AddBlock(nil, Block[t]);

      OnChange:=TreeBlocksChange;

      if Items.Count>0 then
         Selected:=Items[0];

      AlphaSort;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMakerEditor.SetMainCaption(Text:String);
begin
  if Text='' then
     Text:=TeeMsg_Untitled;

  Caption:=Format(TeeMsg_TeeMaker,[Text]);
end;

procedure TMakerEditor.RefreshAnimEditor;
begin
  // Refresh animation editor
  if Assigned(IAnimEditor) then
  begin
    IAnimEditor.ComboGroups.ItemIndex:=0;
    AnimationChangeGroup(Self);
  end;
end;

procedure TMakerEditor.InitFirstMaker;

  procedure PrepareCommander;
  begin
    ButtonOpen:=TeeCommander1.CreateButton(0,Open1Click,'Open',ImageOpen.Picture.Bitmap,0);
    ButtonOpen.NumGlyphs:=1;

    with TCommAccess(TeeCommander1) do
         SetChildOrder(ButtonOpen,0);

    TeeCommander1.ButtonDepth.Visible:=False;
    TeeCommander1.Button3D.Visible:=False;
    TeeCommander1.ButtonRotate.Visible:=False;

    TeeCommander1.ButtonEdit.OnClick:=EditPanel;

    //TeeCommander1.ButtonSave.Enabled:=not InDesignTime;
    ShowHideSaveButton(True);
    TeeCommander1.ButtonSave.OnClick:=Save1Click;

    TCommAccess(TeeCommander1).RepositionControls;
  end;

begin
  PrepareCommander;

  ILibrary.LibraryPath:=CurrentMaker.Blocks.LibraryPath;
  SaveDialog1.InitialDir:=ILibrary.LibraryPath;

  if CurrentMaker.CanFocus then
     CurrentMaker.SetFocus;

  UnmarkDirty;
  FinishSetup;
end;

procedure TMakerEditor.FormShow(Sender: TObject);

  procedure LoadOptions;
  begin
    with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;

      if OpenKeyReadOnly(TeeMakerKey+'\BlockEditor') then
      begin
        PanelEditor.Height:=ReadInteger('PanelTree');
        PageEditor.Width:=ReadInteger('PageTree');

        if ValueExists('PanelBig') then
        begin
          IAnimEditorHeight:=ReadInteger('PanelBig');

          if Assigned(IAnimEditor) then
             IAnimEditor.Height:=IAnimEditorHeight;
        end
        else
          IAnimEditorHeight:=0;

        if ValueExists('AnimateEditor') then
        begin
          Animations1.Checked:=not ReadBool('AnimateEditor');
          Animations1Click(Self);
        end;

        CloseKey;

        if Length(Makers)>0 then
        if OpenKeyReadOnly(TeeMakerKey+'\BlockEditor\Back') then
        begin
          CurrentMaker.AutoRepaint:=False;

          if ValueExists('GradientStart') then
             CurrentMaker.Gradient.StartColor:=ReadInteger('GradientStart');

          if ValueExists('GradientMiddle') then
             CurrentMaker.Gradient.MidColor:=ReadInteger('GradientMiddle');

          if ValueExists('GradientEnd') then
             CurrentMaker.Gradient.EndColor:=ReadInteger('GradientEnd');

          if ValueExists('Gradient') then
             CurrentMaker.Gradient.Visible:=ReadBool('Gradient');

          if ValueExists('Color') then
             CurrentMaker.Color:=ReadInteger('Color');

          CurrentMaker.AutoRepaint:=True;
          
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;

begin
  PageExtras.ActivePage:=TabCamera;
  PageEditor.ActivePage:=TabBlocks;

  if (Length(Makers)=0) and CanAddFirstEmptyMaker then
  begin
    AddNewMaker;
    CurrentMakerTab.FileName:='';

    InitFirstMaker;
  end;

  RefreshAnimEditor;

  // Last thing to set:
  KeyPreview:=True;

  // Tell Windows to accept dragged files from Explorer...
  {$IFNDEF LINUX}
  DragAcceptFiles({$IFDEF CLX}QWidget_winId{$ENDIF}(Handle),True);
  {$ENDIF}

  LoadOptions;
end;

function TMakerEditor.HasCurrentMaker:Boolean;
begin
  result:=(not Assigned(PageMakers)) or (PageMakers.ActivePageIndex<>-1);
end;

procedure TMakerEditor.EditorSetAnimate(const AMaker:TMaker);
begin
  if Assigned(AMaker) then
    with AMaker.Blocks.Animates do
    if Count=0 then
       IAnimEditor.Animate:=nil
    else
       IAnimEditor.Animate:=Item[0].Animate
  else
    IAnimEditor.Animate:=nil
end;

procedure TMakerEditor.ActivateAnimEditor(const AMaker:TMaker);
begin
  if Assigned(IAnimEditor) then
  begin
    AddAnimates(AMaker.Blocks.Animates);
    EditorSetAnimate(AMaker);
    IAnimEditor.RefreshAnimate;
  end;
end;

procedure TMakerEditor.FinishSetup;
begin
  FillBlockList;

  IBlockEditor.PageControl1.Enabled:=(CurrentMaker.Blocks.Count>0);

  ActivateAnimEditor(CurrentMaker);

  UnmarkDirty;
end;

procedure TMakerEditor.FinishLoad(AName:String);
var tmp : String;
begin
  TBlocks.CheckLibraryPath(TeeMsg_ObjectsLibrary,AName);

  if Assigned(PageMakers) then
  begin
    tmp:=Trim(RemoveFileExtension(ExtractFileName(AName)));

    if tmp<>'' then
       CurrentMakerTab.Tab.Caption:=tmp;
  end
  else
     SetMainCaption(AName);

  CheckReopen;
  AddReopen(AName);

  FinishSetup;
end;

Procedure TMakerEditor.AddAnimates(const Animates:TAnimates);
var t : Integer;
begin
  IAnimEditor.ComboGroups.Clear;

  with Animates do
  begin
    for t:=0 to Count-1 do
        IAnimEditor.ComboGroups.Items.Add(Item[t].Description);

    if Count>0 then
    begin
      IAnimEditor.ComboGroups.ItemIndex:=0;
      AnimationChangeGroup(Self);
    end;
  end;
end;

Procedure TMakerEditor.ClearBlocks(ClearCurrent:Boolean=False);
begin
  TreeBlocks.Selected:=nil;
  TreeBlocks.Items.Clear;

  TreeLinks.Items.Clear;

  if ClearCurrent then
  begin
    CurrentMaker.Blocks.Clear;
    CurrentMaker.Blocks.Animates.Add;
  end;

  SBMoveBlock.Enabled:=HasCurrentMaker and Assigned(CurrentMaker.Selected) and (not SBLight.Down);
  SBRotateBlock.Enabled:=SBMoveBlock.Enabled;
  SBSizeBlock.Enabled:=SBMoveBlock.Enabled;

  DisableBlockButtons;

  RefreshAnimEditor;
end;

function TMakerEditor.FileNameOrURLName:String;
begin
  result:=CurrentMakerTab.FileName;

  if result='' then
     result:=CurrentMakerTab.URLName;
end;

Procedure TMakerEditor.DoLoad(const Strings:TStrings);
var t1,t2 : Cardinal;
    tmp   : TStringStream;
    tmpOutput : TMemoryStream;
begin
  Screen.Cursor:=crHourGlass;
  try
    t1:=GetTickCount;

    ClearBlocks(True);

    tmp:=TStringStream.Create(Strings.Text);
    try
      tmpOutput:=TMemoryStream.Create;
      try
        ObjectTextToBinary(tmp,tmpOutput);

        tmpOutput.Position:=0;
        CurrentMaker.Blocks.LoadFromStream(tmpOutput);
      finally
        tmpOutput.Free;
      end;
    finally
      tmp.Free;
    end;

    FinishLoad(FileNameOrURLName);

    t2:=GetTickCount;

    StatusBar1.SimpleText:=Format(TeeMsg_MakerLoaded,[TeeStr(t2-t1)]);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

Procedure TMakerEditor.DoLoad(const AFile:String; AddToHistory:Boolean=True);
var t1,
    t2  : Cardinal;
    tmp : String;
    tmpExt : String;
    tmpMaker : TMaker;
    tmpIsURL : Boolean;
    tmpObj   : TBaseObjBlock;
begin
  tmpIsURL:=TeeIsURL(AFile);

  if not tmpIsURL then
  begin
    tmp:=TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,AFile);

    if ExtractFileExt(tmp)='' then
       tmp:=tmp+TeeMakerExtension;

    if not FileExists(tmp) then
       Raise Exception.Create(Format(TeeMsg_CannotFindFile,[AFile]));
  end;

  Screen.Cursor:=crHourGlass;
  try
    t1:=GetTickCount;

    tmpMaker:=CreateTempMaker;
    try
      ClearBlocks;

      tmpExt:=UpperCase(ExtractFileExt(AFile));

      if tmpExt=UpperCase(TeeObjExtension) then
      begin
        tmpObj:=TObjBlock.Create(tmpMaker);
        tmpObj.LinkFile:=AFile;
        tmpMaker.Blocks.Add(tmpObj);
      end
      else
      if tmpExt=UpperCase(Tee3DSExtension) then
      begin
        tmpObj:=T3DSObject.Create(tmpMaker);
        tmpObj.LinkFile:=AFile;
        tmpMaker.Blocks.Add(tmpObj);
      end
      else
      if tmpIsURL then
         tmpMaker.Blocks.LoadFromURL(AFile)
      else
         tmpMaker.Blocks.LoadFromFile(AFile);

      AddMaker(tmpMaker);

      FinishLoad(AFile);

      t2:=GetTickCount;

      if AddToHistory then
         IHistory.Insert(0,AFile);

      StatusBar1.SimpleText:=Format(TeeMsg_MakerLoaded,[TeeStr(t2-t1)]);

      CurrentMakerTab.URLLoaded:=tmpIsURL;

      if tmpIsURL then
      begin
        CurrentMakerTab.FileName:='';
        CurrentMakerTab.URLName:=AFile;
      end
      else
      begin
        CurrentMakerTab.URLName:='';

        if tmpExt=UpperCase(TeeMakerExtension) then
           CurrentMakerTab.FileName:=ExpandFileName(TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,AFile));
      end;
    except
      on Exception do
      begin
        tmpMaker.Free;
        raise;
      end;
    end;

  finally
    Screen.Cursor:=crDefault;
  end;
end;

Function TeeYesNoCancel(Const Message:String; Owner:TControl=nil):Integer;
var x : Integer;
    y : Integer;
Begin
  Screen.Cursor:=crDefault;

  if Assigned(Owner) then
  begin
    x:=Owner.Width div 2;
    y:=Owner.Height div 2;
    result:=MessageDlgPos(Message,mtConfirmation,[mbYes,mbNo,mbCancel],0,x,y);
  end
  else result:=MessageDlg(Message,mtConfirmation,[mbYes,mbNo,mbCancel],0);
end;

{$IFNDEF D6}
function FileIsReadOnly(const FileName: string): Boolean;
begin
  Result := (GetFileAttributes(PChar(FileName)) and faReadOnly) <> 0;
end;
{$ENDIF}

function TMakerEditor.SaveBlocksToFile(const AMaker:TMaker; const FileName:String):Boolean;

  function RemovedReadOnly:Boolean;
  var tmp : Integer;
  begin
    result:=TeeYesNo(Format(TeeMsg_FileReadOnly,[FileName]));

    if result then
    begin
      tmp:=GetFileAttributes(PChar(FileName));
      tmp:=tmp and (not faReadOnly);
      SetFileAttributes(PChar(FileName),tmp);
    end;
  end;

var Old : TBlocks;
begin
  result:=(not FileExists(FileName)) or
          (not FileIsReadOnly(FileName)) or RemovedReadOnly;

  if result then
  begin
    Old:=IExtraBlocks.Parent;
    IExtraBlocks.Parent:=nil;

    AMaker.Blocks.SaveToFile(FileName);

    IExtraBlocks.Parent:=Old;

    if AMaker=CurrentMaker then
       TreeBlocksChange(Self,NodeOfBlock(Current));
  end;
end;

function TMakerEditor.DoSaveDialog(const AMakerTab:TMakerTab):Boolean;
begin
  result:=SaveDialog1.Execute;

  if result then
  begin
    AMakerTab.FileName:=SaveDialog1.FileName;

    // TODO: Check all blocks LinkFile property for path change...

    SaveBlocksToFile(AMakerTab.Maker,AMakerTab.FileName);

    CheckReopen;
    AddReopen(AMakerTab.FileName);

    ILibrary.ReFillTrees;
  end;
end;

procedure TMakerEditor.Doublebuffer1Click(Sender: TObject);
begin
  with Doublebuffer1 do
       Checked:=not Checked;

  CurrentMaker.BufferedDisplay:=Doublebuffer1.Checked;
end;

Function TMakerEditor.DoSave(AskToSave:Boolean=False; AMakerTab:TMakerTab=nil):Boolean;

  function AskSaveMaker:Integer;
  var tmp : String;
  begin
    if Assigned(AMakerTab.Tab) then
       tmp:=AMakerTab.Tab.Caption
    else
       tmp:=AMakerTab.FileName;

    result:=TeeYesNoCancel(Format(TeeMsg_SaveChanges,[tmp]));
  end;

begin
  result:=False;

  if not Assigned(AMakerTab) then
     AMakerTab:=CurrentMakerTab;

  if AMakerTab.FileName='' then
  begin
    if (AMakerTab.URLLoaded and (not AMakerTab.Dirty)) or InDesignTime or (AMakerTab.Maker.Owner<>Self) then
       result:=True
    else
    if (AMakerTab.Maker.Blocks.Count>0) or AMakerTab.Maker.Blocks.HasAnimations then
    begin
       if not AskToSave then
          result:=DoSaveDialog(AMakerTab)
       else
       begin
          case AskSaveMaker of
            mrYes: result:=DoSaveDialog(AMakerTab);
             mrNo: result:=True;
          end
       end
    end
    else
       result:=True;
  end
  else
  begin
    if AMakerTab.Dirty then
    begin
      Case AskSaveMaker of
        mrYes: begin
                 result:=SaveBlocksToFile(AMakerTab.Maker,AMakerTab.FileName);

                 if result then
                    UnmarkDirty(AMakerTab);
               end;
        mrNo: begin
                result:=True;
                UnmarkDirty(AMakerTab);
              end;
      end;
    end
    else
      result:=True;
  end;
end;

procedure TMakerEditor.Save1Click(Sender: TObject);
begin
  if CurrentMakerTab.FileName<>'' then
  begin
    if SaveBlocksToFile(CurrentMaker,CurrentMakerTab.FileName) then
       UnmarkDirty
    else
       Raise Exception.CreateFmt(TeeMsg_CannotSaveFile,[CurrentMakerTab.FileName]);
  end
  else
  if DoSave then
     UnmarkDirty;
end;

procedure TMakerEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var t : Integer;
begin
  if Assigned(PageMakers) then
  for t:=0 to PageMakers.PageCount-1 do
  begin
    CanClose:=DoSave(True,Makers[t]);

    if not CanClose then
       break;
  end
  else
    CanClose:=DoSave(True);
end;

procedure TMakerEditor.BDupliClick(Sender: TObject);
var tmp : TCustomBlock;
    tmpNew : TCustomBlock;
    tmpNode : TTreeNode;
begin
  tmp:=Current;

  if Assigned(tmp) then
  begin
    tmpNew:=TBlockClass(tmp.ClassType).Create(NewBlockOwner);
    TBlocksAccess(CurrentMaker.Blocks).SetBlockName(tmpNew);
    tmpNew.Parent:=tmp.Parent;

    tmpNode:=NodeOfBlock(tmp);

    if Assigned(tmpNode) then
       AddBlock(tmpNode.Parent,tmpNew)
    else
       AddBlock(nil,tmpNew);

    with tmpNew do
    begin
      Assign(tmp);

      Title:=SysUtils.Format(TeeMsg_CopyOfBlock,[Title]);

      with Location.Point do
           X:=X+Size.X+(Size.X*0.5);
    end;

    TreeBlocks.Selected:=NodeOfBlock(tmpNew);
    TreeBlocks.Selected.Text:=tmpNew.Title;
    TreeBlocksChange(Self,TreeBlocks.Selected);

    MarkDirty;
  end;
end;

procedure TMakerEditor.BEditLinkClick(Sender: TObject);
begin
  EditLinkClick(Self);
end;

procedure TMakerEditor.Orderbyname1Click(Sender: TObject);
begin
  TreeBlocks.SortType:=stText;
end;

procedure TMakerEditor.NoOrder1Click(Sender: TObject);
begin
  TreeBlocks.SortType:=stNone;
  FillBlockList;
end;

procedure TMakerEditor.CBFilterChange(Sender: TObject);
begin
  FillBlockList;
end;

procedure TMakerEditor.Saveas1Click(Sender: TObject);
var Old : String;
begin
  Old:=CurrentMakerTab.FileName;

  CurrentMakerTab.FileName:='';

  if DoSaveDialog(CurrentMakerTab) then
     UnmarkDirty
  else
     CurrentMakerTab.FileName:=Old;
end;

procedure TMakerEditor.New1Click(Sender: TObject);
begin
  if Assigned(PageMakers) or CloseQuery then
  begin
    if Assigned(PageMakers) then
       AddNewMaker
    else
       CurrentMakerTab.FileName:='';

    ClearBlocks(True);
    FinishLoad('');
    CurrentMaker.Invalidate;
  end;
end;

procedure TMakerEditor.Open1Click(Sender: TObject);
begin
  if IBlockEditor.OpenDialog1.Execute then
     LoadNewMaker(IBlockEditor.OpenDialog1.FileName);
end;

procedure TMakerEditor.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMakerEditor.Resetview1Click(Sender: TObject);
begin
  with CurrentMaker.View3DOptions do
  begin
    Orthogonal:=False;
    Rotation:=30;
    Elevation:=350;
    Tilt:=0;
    HorizOffset:=0;
    VertOffset:=0;
    Zoom:=64;
    Perspective:=50;
  end;
end;

procedure TMakerEditor.Borders1Click(Sender: TObject);
begin
  with Borders1 do Checked:=not Checked;
  CurrentMaker.Blocks.HideBorders:=not Borders1.Checked;
end;

procedure TMakerEditor.Textures1Click(Sender: TObject);
begin
  with Textures1 do Checked:=not Checked;
  CurrentMaker.Blocks.HideTextures:=not Textures1.Checked;
end;

procedure TMakerEditor.exturesfolder1Click(Sender: TObject);
var tmp : String;
begin
  with CurrentMaker.Blocks do
  begin
    tmp:=LibraryPath;

    if TTeeVCL.SelectFolder(TeeMsg_SelectLibraryFolder,'',tmp) then
       LibraryPath:=tmp;
  end;
end;

procedure TMakerEditor.Smooth1Click(Sender: TObject);
begin
  with Smooth1 do Checked:=not Checked;
  CurrentMaker.Render.ShadeQuality:=Smooth1.Checked;
end;

procedure TMakerEditor.View3DAxesClick(Sender: TObject);
begin
  with View3DAxes do Checked:=not Checked;
  CurrentMaker.Options.View3DAxes:=View3DAxes.Checked;
end;

procedure TMakerEditor.Duplicate1Click(Sender: TObject);
begin
  BDupliClick(Self);
end;

procedure TMakerEditor.Insert1Click(Sender: TObject);
begin
  BAddClick(Self);
end;

procedure TMakerEditor.Delete1Click(Sender: TObject);
begin
  BDeleteClick(Self);
end;

procedure TMakerEditor.Duplicate2Click(Sender: TObject);
begin
  Duplicate1Click(Self);
end;

procedure TMakerEditor.DeleteSelectedBlocks;
{$IFDEF D6}
var t : Integer;
    Nodes : Array of TTreeNode;
    {$IFDEF CLX}
    tmp : Integer;
    {$ENDIF}
{$ENDIF}
begin
  {$IFDEF D6}
  SetLength(Nodes,TreeBlocks.{$IFDEF CLX}SelCount{$ELSE}SelectionCount{$ENDIF});

  {$IFDEF CLX}
  tmp:=0;

  for t:=0 to TreeBlocks.Items.Count-1 do
  if TreeBlocks.Items[t].Selected then
  begin
    Nodes[tmp]:=TreeBlocks.Items[t];
    Inc(tmp);
  end;

  {$ELSE}
  for t:=0 to TreeBlocks.SelectionCount-1 do
      Nodes[t]:=TreeBlocks.Selections[t];
  {$ENDIF}

  for t:=0 to Length(Nodes)-1 do
      DeleteNodeAndBlock(Nodes[t]);

  Nodes:=nil;
  {$ELSE}
  DeleteNodeAndBlock(TreeBlocks.Selected);
  {$ENDIF}

  MarkDirty;
end;

procedure TMakerEditor.CopySelectedBlocks(const Collection:TBlocks);
{$IFDEF D6}
var t : Integer;
{$ENDIF}
begin
  {$IFDEF D6}

  {$IFDEF CLX}
  for t:=0 to TreeBlocks.Items.Count-1 do
  if TreeBlocks.Items[t].Selected then
      Collection.CloneBlock(TCustomBlock(TreeBlocks.Items[t].Data));
  {$ELSE}
  for t:=0 to TreeBlocks.SelectionCount-1 do
      Collection.CloneBlock(TCustomBlock(TreeBlocks.Selections[t].Data));
  {$ENDIF}

  {$ELSE}
  Collection.CloneBlock(Current);
  {$ENDIF}
end;

procedure TMakerEditor.Savetoexternal1Click(Sender: TObject);
var tmp : TCustomObjectBlock;
begin
  {$IFDEF D6}
  if TreeBlocks.{$IFDEF CLX}SelCount{$ELSE}SelectionCount{$ENDIF}>0 then
  {$ELSE}
  if TreeBlocks.Selected<>nil then
  {$ENDIF}
  begin
    tmp:=TObjectBlock.Create(NewBlockOwner);
    tmp.Parent:=CurrentMaker.Blocks;
    tmp.Title:=TeeMsg_ObjectBlock;

    CopySelectedBlocks(tmp.Items);

    AddBlock(nil, tmp);

    DeleteSelectedBlocks;

    FinishAdd(tmp);
  end;
end;   

procedure TMakerEditor.Copy1Click(Sender: TObject);
begin
  MakerClipboard.Clear;
  CopySelectedBlocks(MakerClipboard);
end;

procedure TMakerEditor.Paste1Click(Sender: TObject);
var t : Integer;
begin
  for t:=0 to MakerClipboard.Count-1 do
      AddBlock(nil, CurrentMaker.Blocks.CloneBlock(MakerClipboard[t]));
end;

procedure TMakerEditor.Delete2Click(Sender: TObject);
begin
  Delete1Click(Self);
end;

procedure TMakerEditor.Edit7Click(Sender: TObject);
var tmp : Boolean;
begin
  tmp:=TreeBlocks.Selected<>nil;
  Copy1.Enabled:=tmp;
  Duplicate2.Enabled:=tmp;
  Delete2.Enabled:=tmp;
  Paste1.Enabled:=MakerClipboard.Count>0;
end;

procedure TMakerEditor.Boundingbox1Click(Sender: TObject);
begin
  with Boundingbox1 do Checked:=not Checked;
  CurrentMaker.Options.BoundingBox:=Boundingbox1.Checked;
end;

procedure TMakerEditor.Locallight1Click(Sender: TObject);
begin
  Locallight1.Checked:=not Locallight1.Checked;

  if Locallight1.Checked then TeeFullLightModel:=GL_TRUE
                         else TeeFullLightModel:=GL_FALSE;

  // Force resetting Canvas:
  CurrentMaker.Canvas.UseBuffer:=True;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SetAntiAlias(Value:Boolean);
begin
{  if Value then
     TeeSmoothQuality:=GL_NICEST
  else
     TeeSmoothQuality:=GL_FASTEST;
}

  if HasCurrentMaker then
  begin
    CurrentMaker.Render.AntiAlias:=Value;

    // Force resetting Canvas:
    CurrentMaker.Canvas.UseBuffer:=True;

    TMakerAccess(CurrentMaker).DeleteAllLists;

    CurrentMaker.Invalidate;
  end;
end;

procedure TMakerEditor.AntiAlias1Click(Sender: TObject);
begin
  AntiAlias1.Checked:=not AntiAlias1.Checked;
  SetAntiAlias(AntiAlias1.Checked);

//  TBlocksAccess(CurrentMaker.Blocks).DeleteLists;
end;

procedure TMakerEditor.Maker1Click(Sender: TObject);
begin
  with CurrentMaker do
  if not Focused then
     SetFocus;
end;

procedure TMakerEditor.AnimateModified(Sender: TObject);
begin
  MarkDirty;
end;

procedure TMakerEditor.AnimateHide(Sender: TObject);
begin
  Animations1.Checked:=False;
  SplitterAnim.Visible:=Assigned(IKinematics) and IKinematics.Visible;
end;

procedure TMakerEditor.AnimateSelected(Sender: TObject; out Animation:TTeeAnimation);
begin
  if Assigned(PageMakers) and (PageMakers.ActivePageIndex<>-1) then
     CurrentMaker.Invalidate;
end;

type
  TAnimationAccess=class(TTeeAnimation);

procedure TMakerEditor.AnimateGallery(Sender: TObject; out Animation:TTeeAnimation; const AParent:TTeeAnimation);
begin
  with TAnimationGallery.Create(Self) do
  try
    Animate:=(Sender as TTeeAnimateEditor).Animate;
    ParentAnimation:=AParent;

    CreateSelector(TMakerPropertySelector);
    (Selector as TMakerPropertySelector).AddSelection(Self.CurrentMaker.Blocks);

    if Assigned(IAnimGalleryBlock) then
       with Selector.TreeObjects do
            Selected:=TMakerPropertySelector.NodeWithObject(Items,IAnimGalleryBlock);

    if ShowModal=mrOk then
    begin
      Animation:=GetAnimation(Self.NewBlockOwner);
      TAnimationAccess(Animation).FixupReferences(Self.CurrentMaker.Blocks.CurrentSource);

      if Animation is TPropertyAnimation then
      with TPropertyAnimationAccess(Animation) do
      if Instance=Self.CurrentMaker then
         IsSpecial:=1
      else
      if Instance=Self.CurrentMaker.Render then
         IsSpecial:=2
      else
      if Instance=Self.CurrentMaker.Blocks then
         IsSpecial:=3
      else
         IsSpecial:=0;
    end;
  finally
    Free;
  end;
end;

procedure TMakerEditor.Gradient1Click(Sender: TObject);
begin
  if TTeeGradientEditor.Edit(Self,CurrentMaker.Gradient) then
     MarkDirty;
end;

procedure TMakerEditor.Backimage1Click(Sender: TObject);
begin
  if TBackImageEditor.Edit(Self,CurrentMaker.BackImage) then
     MarkDirty;
end;

procedure TMakerEditor.EditLinkClick(Sender: TObject);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  tmp: String;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);

  StartupInfo.cb := SizeOf(StartupInfo);

  tmp:=ParamStr(0);
  tmp:=tmp+' "'+TCustomObjectBlockAccess(Current).CompleteLinkFile+'"';

  if not CreateProcess(nil, PChar(tmp), nil, nil, False, 0, nil, nil,
                      StartupInfo, ProcessInfo) then
     raise Exception.CreateFmt(TeeMsg_CannotExecute,[tmp]);
end;

procedure TMakerEditor.AnimationAddGroup(Sender: TObject);
var tmp : String;
begin
  tmp:=Format(TeeMsg_Animation,[TeeStr(CurrentMaker.Blocks.Animates.Count+1)]);

  if InputQuery(TeeMsg_Animations,TeeMsg_NewAnimation,tmp) then
  begin
    CurrentMaker.Blocks.Animates.Add.Description:=tmp;

    IAnimEditor.ComboGroups.Add(tmp);
    IAnimEditor.ComboGroups.ItemIndex:=IAnimEditor.ComboGroups.Items.Count-1;
    AnimationChangeGroup(Self);

    MarkDirty;
  end;
end;

procedure TMakerEditor.AnimationRenameGroup(Sender: TObject);
var tmp : String;
    Old : Integer;
begin
  tmp:=CurrentAnimate.Description;

  if InputQuery(TeeMsg_Animation,TeeMsg_RenameAnimation,tmp) then
  begin
    CurrentAnimate.Description:=tmp;

    with IAnimEditor.ComboGroups do
    begin
      Old:=ItemIndex;
      Items[ItemIndex]:=tmp;
      ItemIndex:=Old;

      AnimationChangeGroup(Self);
    end;
  end;
end;

procedure TMakerEditor.AnimationRemoveGroup(Sender: TObject);
var tmp  : String;
    tmpN : Integer;
begin
  tmp:=IAnimEditor.ComboGroups.Items[IAnimEditor.ComboGroups.ItemIndex];

  if TeeYesNo(Format(TeeMsg_SureToDelete,[tmp])) then
  begin
    tmpN:=IAnimEditor.ComboGroups.ItemIndex;

    CurrentMaker.Blocks.Animates[tmpN].Free;

    IAnimEditor.ComboGroups.Items.Delete(tmpN);
    IAnimEditor.SBRemoveGroup.Enabled:=IAnimEditor.ComboGroups.Items.Count>0;

    with IAnimEditor.ComboGroups do
    if tmpN>Items.Count-1 then
       ItemIndex:=Items.Count-1
    else
       ItemIndex:=tmpN;

    AnimationChangeGroup(Self);

    MarkDirty;
  end;
end;

function TMakerEditor.CurrentAnimate:TAnimateItem;
begin
  result:=CurrentMaker.Blocks.Animates[IAnimEditor.ComboGroups.ItemIndex];
end;

procedure TMakerEditor.AnimationChangeGroup(Sender: TObject);
var tmp : TTeeAnimation;
begin
  with IAnimEditor do
  begin
    SBRemoveGroup.Enabled:=ComboGroups.ItemIndex>0;

    if ComboGroups.ItemIndex=-1 then
       Animate:=nil
    else
       Animate:=CurrentAnimate.Animate;

    if Animate<>nil then
       CBAutoPlay.Checked:=CurrentAnimate.PlayOnLoad;
  end;

  if (IAnimEditor.Animate=nil) or (IAnimEditor.Animate.Animations.Count=0) then
  begin
    tmp:=nil;
    AnimateSelected(IAnimEditor,tmp);
  end;
end;

type
  TCustomBlockAccess=class(TCustomBlock);

procedure TMakerEditor.AnimateGetName(const Animation:TTeeAnimation; out S:String);
begin
  if Animation is TPropertyAnimation then
     with TPropertyAnimation(Animation) do
     if Assigned(Instance) then
        if (Instance is TCustomBlock) and (TCustomBlock(Instance).Title<>'') then
           S:=BlockTitlePath(TCustomBlock(Instance))+' '+PropertyName;
end;

procedure TMakerEditor.Animations1Click(Sender: TObject);
var tmp : TSpeedButton;
begin
  Animations1.Checked:=not Animations1.Checked;

  if Animations1.Checked then
  begin
    if not Assigned(IAnimEditor) then
    begin
      IAnimEditor:=TTeeAnimateEditor.Create(Self);

      IAnimEditor.OnGetAnimationName:=AnimateGetName;

      if HasCurrentMaker then
         EditorSetAnimate(CurrentMaker)
      else
         EditorSetAnimate(nil);

      IAnimEditor.OnShowGallery:=AnimateGallery;
      IAnimEditor.OnSelectedAnimation:=AnimateSelected;
      IAnimEditor.OnHide:=AnimateHide;
      IAnimEditor.OnModified:=AnimateModified;

      with IAnimEditor.ListAnim do
      begin
        IOnDragOver:=OnDragOver;
        OnDragOver:=ListAnimDragOver;

        IOnDragDrop:=OnDragDrop;
        OnDragDrop:=ListAnimDragDrop;
      end;

      IAnimEditor.PanelGroups.Visible:=True;

      IAnimEditor.Panel3.Hide;
      IAnimEditor.TeeInspector1.Hide;

      IAnimEditor.PanelAnimTop.Parent:=IAnimEditor.PanelAnim;
      IAnimEditor.ListAnim.Parent:=IAnimEditor.PanelAnim;
      IAnimEditor.ListAnim.Align:=alClient;

      IAnimEditor.PanelAnimTree.Hide;

      IAnimEditor.Splitter1.Hide;

      IAnimEditor.PanelAnim.Width:=200;

      CBAutoPlay.Parent:=IAnimEditor.PanelGroups;
      CBAutoPlay.Left:=IAnimEditor.SBRemoveGroup.Left+70;
      CBAutoPlay.Visible:=True;

      tmp:=TSpeedButton.Create(IAnimEditor);
      tmp.Parent:=IAnimEditor.PanelGroups;
      tmp.Caption:='...';
      tmp.Flat:=True;
      tmp.Top:=IAnimEditor.SBAddGroup.Top;
      tmp.Left:=IAnimEditor.SBRemoveGroup.Left+34;
      tmp.OnClick:=AnimationRenameGroup;

      IAnimEditor.SBAddGroup.OnClick:=AnimationAddGroup;
      IAnimEditor.SBRemoveGroup.OnClick:=AnimationRemoveGroup;

      IAnimEditor.ComboGroups.OnChange:=AnimationChangeGroup;

      if HasCurrentMaker then
         AddAnimates(CurrentMaker.Blocks.Animates);

      IAnimEditor.Align:=alBottom;

      if IAnimEditorHeight>0 then
         IAnimEditor.Height:=IAnimEditorHeight;

      TTeeVCL.AddFormTo(IAnimEditor, PanelBig);
    end;
  end;

  ShowHideAnimEditor(Animations1.Checked);
end;

procedure TMakerEditor.TreeBlocksEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  if S<>'' then
  begin
    Current.Title:=S;
    IBlockEditor.BlockTitle.Text:=Current.Title;
    MarkDirty;
  end;
end;

procedure TMakerEditor.TreeBlocksChange(Sender: TObject; Node: TTreeNode);

  procedure AddDesignTimeHandles;
  var tmp : TCustomBlock;
  begin
    if not Assigned(IDesignHandles) then
    begin
      IDesignHandles:=TObjectBlockHandle.Create(IExtraBlocks);
      IDesignHandles.Parent:=IExtraBlocks.Items;
    end;

    IDesignHandles.Items.Clear;

    if Assigned(CurrentMaker.Selected) then
    begin
      tmp:=TBlockAccess(CurrentMaker.Selected).DesignHandles(IExtraBlocks);

      if Assigned(tmp) then
      if TObjectBlockHandle(tmp).Items.Count>0 then
      begin
        tmp.Parent:=IDesignHandles.Items;
        AddExtraBlocks('');
      end
      else
        tmp.Free;
    end;
  end;

var tmp : Boolean;
    tmpSt : String;
    tmpParent : TTreeNode;
    tmpOb : TObject;
begin
  IModifying:=True;

  CurrentMaker.Selected:=Current;

  if Assigned(CurrentMaker.Selected) then
  begin
    tmpSt:=CurrentMaker.Selected.Name;

    if tmpSt='' then
    begin
      tmpOb:=CurrentMaker.Selected;
      tmpSt:='@'+IntToStr({$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(tmpOb));
    end;

    IExtraBlocks.Items.Properties[0]:='Target='+tmpSt;
  end;

  AddDesignTimeHandles;

  if Assigned(ILibrary) then
     ILibrary.Current:=CurrentMaker.Selected;

  BDelete.Enabled:=(Current<>nil);
  BChange.Enabled:=BDelete.Enabled and (not (Current is TCustomObjectBlock));

  // Set parents
  CurrentMaker.Blocks.CurrentParents.Clear;

  tmpParent:=Node;

  if Assigned(tmpParent) then
  while Assigned(tmpParent.Parent) do
  begin
    tmpParent:=tmpParent.Parent;
    CurrentMaker.Blocks.CurrentParents.Insert(0,TCustomBlock(tmpParent.Data));
  end;

  try
    if Current=nil then
    begin
      SBMoveBlock.Enabled:=False;
      SBRotateBlock.Enabled:=False;
      SBSizeBlock.Enabled:=False;

      DisableBlockButtons;

      SetBoundTitle;

      RemoveExtraBlocks;
    end
    else
    with Current do
    begin
      tmp:=CurrentMakerTab.Dirty;

      if Assigned(IBlockEditor) then
         if IBlockEditor.Current<>Current then
            IBlockEditor.RefreshBlock(Current);

      SBMoveBlock.Enabled:=True;
      SBRotateBlock.Enabled:=True;
      SBSizeBlock.Enabled:=True;

      if CurrentMakerTab.Dirty and (not tmp) then
         UnmarkDirty;

      if Name='' then
         StatusBar1.SimpleText:=TeeStr(Index)
      else
         StatusBar1.SimpleText:=Name;
    end;

    if View3DAxes.Checked or Boundingbox1.Checked then
       CurrentMaker.Invalidate;

    PanelEditor.Visible:=PageEditor.Visible and (Current<>nil);

    if Assigned(IAnimEditor) then
       if (Current is TAnimatedBlock) then
       begin
         IAnimEditor.Animate:=TAnimatedBlock(Current).Animate;
         IAnimEditor.RefreshAnimate;
       end
       else
         RefreshAnimEditor;

    // Trick to reset splitter position:
    SplitterTree.Align:=alTop;
    SplitterTree.Align:=alBottom;
  finally
    IModifying:=False;
  end;
end;

procedure TMakerEditor.SetEditMode(Value:Boolean);
begin
  if Value and (Menu=nil) then
     Menu:=MainMenu1;

  CurrentMaker.Options.SelectMode:=Value;

  if Value then
  begin
    CurrentMaker.PopupMenu:=PopupMenu1;

    TeeCommander1.Show;
    StatusBar1.Show;

    if not Editor1.Checked then
       Editor1Click(Self);

    if WasAnimations then
       Animations1Click(Self);

    if WasKinematics then
       Kinematics1Click(Self);

    if TreeBlocks.Selected<>nil then
       if not PanelEditor.Visible then
          TreeBlocksChange(Self,TreeBlocks.Selected);
  end
  else
  begin
    RemoveExtraBlocks;

    CurrentMaker.PopupMenu:=nil;

    WasEditor:=Editor1.Checked;

    if WasEditor then
       Editor1Click(Self);

    WasAnimations:=Animations1.Checked;
    if WasAnimations then
       Animations1Click(Self);

    WasKinematics:=Kinematics1.Checked;
    if WasKinematics then
       Kinematics1Click(Self);

    TeeCommander1.ButtonNormal.Down:=True;
    TeeCommander1.Hide;
    StatusBar1.Hide;

    DisableBlockButtons;

    CurrentMaker.Options.View3DAxes:=False;

    TBlocksAccess(CurrentMaker.Blocks).ResetShown;
  end;

  Edit7.Visible:=Value;
  N10.Visible:=Value;
  Source1.Visible:=Value;
  Tools1.Visible:=Value;
  N1.Visible:=Value;
  Save1.Visible:=Value;
  Saveas1.Visible:=Value;
  CurrentMaker.Options.BoundingBox:=Value;
  View3DAxes.Visible:=Value;
  Boundingbox1.Visible:=Value;
  Wireframe1.Visible:=Value;
  Object1.Visible:=Value;

  FullScreen1.Enabled:=not EditMode1.Checked;
end;

procedure TMakerEditor.EditMode1Click(Sender: TObject);
begin
  EditMode1.Checked:=not EditMode1.Checked;
  SBEditMode.Down:=EditMode1.Checked;
  
  SetEditMode(EditMode1.Checked);
end;

procedure TMakerEditor.ShowHideAnimEditor(DoShow:Boolean);
begin
  if Assigned(IAnimEditor) then
  begin
    IAnimEditor.Visible:=DoShow;
    SplitterAnim.Visible:=DoShow or (Assigned(IKinematics) and IKinematics.Visible);
  end;
end;

procedure TMakerEditor.ShowHideSaveButton(DoShow:Boolean);
begin
  TeeCommander1.ButtonSave.Visible:=DoShow and (not InDesignTime) and
                                    (CurrentMaker.Owner=Self);

  Save1.Visible:=TeeCommander1.ButtonSave.Visible;
end;

procedure TMakerEditor.ShowHideEditor(DoShow:Boolean);
begin
  PageEditor.Visible:=DoShow;
  SplitterEditor.Visible:=PageEditor.Visible;

  SBRotateBlock.Visible:=DoShow;
  SBMoveBlock.Visible:=DoShow;
  SBSizeBlock.Visible:=DoShow;
  SBLight.Visible:=DoShow;

  TeeCommander1.ButtonEdit.Visible:=DoShow;
  TeeCommander1.ButtonNormal.Visible:=DoShow;

  ShowHideSaveButton(DoShow);

  if Assigned(ButtonOpen) then
     ButtonOpen.Visible:=DoShow;

  TCommAccess(TeeCommander1).RepositionControls;

  if SplitterEditor.Visible then
     SplitterEditor.Left:=PageEditor.Width;

  if not DoShow then
  if HasCurrentMaker then
  begin
    CurrentMaker.Options.BoundingBox:=False;
    CurrentMaker.Options.View3DAxes:=False;
  end;
end;

procedure TMakerEditor.Editor1Click(Sender: TObject);
begin
  Editor1.Checked:=not Editor1.Checked;
  ShowHideEditor(Editor1.Checked);
end;

procedure TMakerEditor.Open2Click(Sender: TObject);
begin
  if UpperCase(ExtractFileExt(TCustomObjectBlock(Current).LinkFile))='.TEE' then
     IBlockEditor.BEditLinkClick(Self)
  else
     LoadNewMaker(TCustomObjectBlockAccess(Current).CompleteLinkFile);
end;

procedure TMakerEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_F11) and (not EditMode1.Checked) then
  begin
    SetEditMode(True);

    Key:=0;
  end
  else
  if CurrentMaker.Focused then
  begin
    if (Key=VK_SPACE) and Assigned(IAnimEditor) then
       if IAnimEditor.Animate.Playing then
       begin
         IAnimEditor.SBPause.Down:=True;
         IAnimEditor.SBPauseClick(Self);
       end
       else
          IAnimEditor.SBPlayClick(Self)
    else
       CurrentMaker.DoKeyDown(Key,Shift);
  end;
end;

procedure TMakerEditor.TreeLinksChange(Sender: TObject; Node: TTreeNode);
begin
  BViewObject.Enabled:=TreeLinks.Selected<>nil;

  if BViewObject.Enabled then
     TreeBlocks.Selected:=NodeOfBlock(TCustomBlock(TreeLinks.Selected.Data));
end;

procedure TMakerEditor.SetBoundTitle;

  function BoundPoint:TPointXYZFloat;
  begin
    with Current do
    if SBRotateBlock.Down then
       result:=Rotation
    else
    if SBMoveBlock.Down then
       result:=Location
    else
       result:=Size;
  end;

begin
  if (Current<>nil) and
     (SBRotateBlock.Down or SBMoveBlock.Down or SBSizeBlock.Down) then
  with BoundPoint do
     CurrentMaker.Options.BoundTitle:=Format('%.2f x %.2f x %.2f',[X,Y,Z])
  else
     CurrentMaker.Options.BoundTitle:='';

  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.RefreshRotation(const Sender: TCustomBlock);
begin
  IBlockEditor.RefreshRotation;
  SetBoundTitle;
end;

procedure TMakerEditor.SBRotateBlockClick(Sender: TObject);

  procedure AddRotateHandles;

    function SetHelix(Block:TCustomBlock):TCustomBlock;
    begin
      THelixBlock(Block).Twists:=1.2;
      Block.Size.Value:=16;
      result:=Block;
    end;

  begin
    if not Assigned(IRotateBlocks) then
    begin
      IRotateBlocks:=TObjectBlockHandle.Create(IExtraBlocks);

      with IRotateBlocks do
      begin
        Format.Color:=clPurple;
        HandleClass:=THelixBlock;
        OnDragging:=RefreshRotation;

        SetHelix(AddHandle(Point3D(-1.16, 0, 0),'Rotation.X','Rotation.X=+=1')).Rotation.X:=90;
        SetHelix(AddHandle(Point3D( 0, 0, 1.16),'Rotation.Y','Rotation.Y=+=1'));
        SetHelix(AddHandle(Point3D( 0, 1.16, 0),'Rotation.Z','Rotation.Z=+=1')).Rotation.Y:=90;

        Parent:=IExtraBlocks.Items;
      end;
    end;

    IRotateBlocks.Visible:=True;
  end;

begin
  if SBRotateBlock.Down then
  begin
    SBMoveBlock.Down:=False;
    SBSizeBlock.Down:=False;

    if Assigned(IMoveBlocks) then
       IMoveBlocks.Visible:=False;

    if Assigned(ISizeBlocks) then
       ISizeBlocks.Visible:=False;

    AddRotateHandles;
    AddExtraBlocks(TeeMsg_RotateBlockHelp);
  end
  else
    IRotateBlocks.Visible:=False;

  SetBoundTitle;
end;

procedure TMakerEditor.RemoveExtraBlocks;
begin
  IExtraBlocks.Parent:=nil;
  CurrentMaker.Options.SelectModeBlocks:=nil;
  StatusBar1.SimpleText:='';
end;

procedure TMakerEditor.RefreshLocation(const Sender: TCustomBlock);
begin
  IBlockEditor.RefreshLocation;
  SetBoundTitle;
end;

procedure TMakerEditor.SBMoveBlockClick(Sender: TObject);

  procedure AddMoveHandles;
  begin
    if not Assigned(IMoveBlocks) then
    begin
      IMoveBlocks:=TObjectBlockHandle.Create(IExtraBlocks);

      with IMoveBlocks do
      begin
        Format.Color:=clRed;
        HandleClass:=TArrowBlock;
        OnDragging:=RefreshLocation;

        AddHandle(Point3D(-1.16,0,0),'Location.X','Location.X=-=1');
        AddHandle(Point3D( 1.16,0,0),'Location.X','Location.X=+=1');
        AddHandle(Point3D( 0,1.24,0),'Location.Z,Invert','Location.Z=+=1');
        AddHandle(Point3D(0,-1.24,0),'Location.Z,Invert','Location.Z=-=1');
        AddHandle(Point3D( 0,0,1.44),'Location.Y,Invert','Location.Y=-=1');
        AddHandle(Point3D(0,0,-1.44),'Location.Y,Invert','Location.Y=+=1');

        Parent:=IExtraBlocks.Items;
      end;
    end;

    IMoveBlocks.Visible:=True;
  end;

begin
  if SBMoveBlock.Down then
  begin
    SBRotateBlock.Down:=False;
    SBSizeBlock.Down:=False;

    if Assigned(IRotateBlocks) then
       IRotateBlocks.Visible:=False;

    if Assigned(ISizeBlocks) then
       ISizeBlocks.Visible:=False;

    AddMoveHandles;
    AddExtraBlocks(TeeMsg_MoveBlockHelp);
  end
  else
  if Assigned(IMoveBlocks) then 
     IMoveBlocks.Visible:=False;

  SetBoundTitle;
end;

type
  TBlockFormatAccess=class(TBlockFormat);

procedure TMakerEditor.Maker1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var tmpBlock : TCustomBlock;
begin
  if EditMode1.Checked and (Button=mbLeft) then
  begin
    if TeeCommander1.ButtonNormal.Down then
       if SBRotateBlock.Down or SBMoveBlock.Down or SBSizeBlock.Down then
       begin
         with TMakerAccess(CurrentMaker) do
         if Assigned(Selected) and (BlockUnderMouse=Selected) then
            DoInitDrag(X,Y);
       end
       else
       if PageEditor.ActivePage=TabColors then
       begin
         tmpBlock:=TMakerAccess(CurrentMaker).BlockUnderMouse;
         TMakerAccess(CurrentMaker).CurrentOver:=tmpBlock;

         if Assigned(tmpBlock) then
            IColorPalette.CurrentColor:=TBlockFormatAccess(tmpBlock.Format).GetRealColor
         else
         if CurrentMaker.Gradient.Visible then
            IColorPalette.CurrentColor:=CurrentMaker.Gradient.EndColor
         else
            IColorPalette.CurrentColor:=CurrentMaker.Color;
       end;
  end
  else
  if EditMode1.Checked and (ssAlt in Shift) and (Button=mbRight) then
  begin
    TMakerEditor.ModalShow(Self,IExtraBlocks);
    CurrentMaker.CancelMouse:=True;
  end;
end;

procedure TMakerEditor.Maker1Clicked(Sender: TObject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
var tmpNode : TTreeNode;
    tmp     : TCustomBlock;
begin
  if Editor1.Checked then
  begin
    StatusBar1.SimpleText:=TBlockAccess(TCustomBlock(Sender)).TitleOrName;

    repeat
      tmpNode:=NodeOfBlock(TCustomBlock(Sender));

      if Assigned(tmpNode) then
         tmp:=nil
      else
      begin
        tmp:=TBlocksAccess(TCustomBlock(Sender).Parent).IObject;

        if Assigned(tmp) then
           Sender:=tmp;
      end;

    until Assigned(tmpNode) or (not Assigned(tmp));

    with TreeBlocks do
    if Selected<>tmpNode then
       Selected:=tmpNode;
  end;
end;

function TMakerEditor.BlockUnderMouse:TCustomBlock;
begin
  with CurrentMaker.GetCursorPos do
       result:=CurrentMaker.Blocks.ClickedBlock(X,Y);
end;

procedure TMakerEditor.Maker1DblClick(Sender: TObject);
var tmp : TCustomBlock;
begin
  if EditMode1.Checked then
  begin
    tmp:=BlockUnderMouse;

    if Assigned(tmp) and (tmp.Parent<>IExtraBlocks.Items) then
    begin
      TreeBlocks.Selected:=NodeOfBlock(tmp);
      BlockCustomEditClick(Self);

      CurrentMaker.CancelMouse:=True;
    end;
  end;
end;

procedure TMakerEditor.Maker1DoLoad(Sender: TMaker; const FileName:String);
begin
  DoLoad(FileName);
end;

function TMakerEditor.DragFromPalette(Source:TObject):Boolean;
begin
  result:=Assigned(IColorPalette) and (Source=IColorPalette.ColorPalette);
end;

function TMakerEditor.DragObject(Source:TObject):Boolean;
begin
  result:=ILibrary.IsDragOkFrom(Source,ILibrary.TreeObjects);
end;

procedure TMakerEditor.Maker1DragOver(Sender, Source: TObject; X, Y: Integer;
                             State: TDragState; var Accept: Boolean);

  function DragTexture:Boolean;
  begin
    result:=ILibrary.IsDragOkFrom(Source,ILibrary.TreeTextures);
  end;

var tmp : Boolean;
    tmpBlock : TCustomBlock;
begin
  tmp:=DragFromPalette(Source) or DragObject(Source) or DragTexture;

  tmpBlock:=CurrentMaker.Blocks.ClickedBlock(X,Y);

  if tmp and (tmpBlock<>TMakerAccess(CurrentMaker).CurrentOver) then
  begin
    TMakerAccess(CurrentMaker).CurrentOver:=tmpBlock;
    CurrentMaker.Invalidate;
  end;

  if State=dsDragLeave then
     IDropBackup.Restore(CurrentMaker)
  else
  if tmp then
     DoDragDrop(Source,X,Y,False);

  Accept:=tmp;
end;

procedure TMakerEditor.BlocksItemsChanged(Sender: TObject);
var tmp : TTreeNode;
    t   : Integer;
begin
  if Sender is TCustomObjectBlock then
  begin
    tmp:=NodeOfBlock(TCustomBlock(Sender));

    if Assigned(tmp) then
    begin
      TreeBlocks.Items.BeginUpdate;
      try
        tmp.DeleteChildren;

        with TCustomObjectBlock(Sender).Items do
        for t:=0 to Count-1 do
            AddBlock(tmp, Block[t]);
      finally
        TreeBlocks.Items.EndUpdate;
      end;

      if not IModifying then
      begin
        // NO !  This does Free tabs that call here (re-entrancy)
        //IBlockEditor.RefreshBlock(TCustomBlock(Sender));

        MarkDirty;
      end;
    end;
  end;
end;

procedure TMakerEditor.SetNewBlockLocation(const ABlock:TCustomBlock; X,Y:Integer);
begin
  ABlock.Location.Point:=CurrentMaker.Options.Navigate.MouseToLocation(X,Y);

  with ABlock.Location.Point do Z:=30;

  ABlock.Repaint;
end;

procedure TMakerEditor.DoDragDrop(Source: TObject; X, Y: Integer; SetDirty:Boolean);

  procedure SetPicTo(ATexture:TBlockTexture);
  var tmpPic : String;
  begin
    tmpPic:=ILibrary.SelectedTexture;

    with ATexture do
    if PictureLink<>tmpPic then
    begin
      if not SetDirty then
      begin
        IDropBackup.Restore(CurrentMaker);

        IDropBackup.Active:=True;
        IDropBackup.IsTexture:=True;
        IDropBackup.Texture:=ATexture;
        IDropBackup.PictureLink:=PictureLink;
      end;

      PictureLink:=tmpPic;

      if SetDirty then
         MarkDirty;
    end;
  end;

  procedure SetDragToBlock(ABlock:TCustomBlock);
  begin
    if DragFromPalette(Source) then
    begin
      if not SetDirty then
      begin
        IDropBackup.Restore(CurrentMaker);

        IDropBackup.Active:=True;
        IDropBackup.IsTexture:=False;
        IDropBackup.Block:=ABlock;
        IDropBackup.Color:=ABlock.Format.Color;
      end;

      ABlock.Format.Color:=IColorPalette.Selected;

      if SetDirty then
         MarkDirty;
    end
    else
      SetPicTo(ABlock.Format.Texture);
  end;

var tmp : TCustomBlock;
begin
  if DragObject(Source) then
  begin
    if not Assigned(IDropBackup.Block) then
    begin
      IDropBackup.Block:=SelectedLibraryBlock;
      IDropBackup.Active:=True;
      IDropBackup.IsTexture:=False;
      IDropBackup.IsMaker:=False;
      IDropBackup.IsBlock:=True;

      IDropBackup.Block.Parent:=CurrentMaker.Blocks;
    end;

    SetNewBlockLocation(IDropBackup.Block,X,Y);

    CurrentMaker.Selected:=IDropBackup.Block;

    with IDropBackup.Block.Location do
        CurrentMaker.Options.BoundTitle:=Format('%.2f x %.2f x %.2f',[X,Y,Z])
  end
  else
  begin
    tmp:=CurrentMaker.Blocks.ClickedBlock(X,Y);

    if Assigned(tmp) then
    begin
      TreeBlocks.Selected:=NodeOfBlock(tmp);
      SetDragToBlock(tmp);
    end
    else
    begin
      tmp:=CurrentMaker.Options.Floor.Block.Clicked(X,Y);

      if Assigned(tmp) then
         SetDragToBlock(tmp)
      else
      begin
        if DragFromPalette(Source) then
        begin
          if not SetDirty then
          begin
            IDropBackup.Restore(CurrentMaker);

            IDropBackup.Active:=True;
            IDropBackup.IsTexture:=False;
            IDropBackup.IsMaker:=True;

            if CurrentMaker.Gradient.Visible then
               IDropBackup.Color:=CurrentMaker.Gradient.EndColor
            else
               IDropBackup.Color:=CurrentMaker.Color;
          end;

          if CurrentMaker.Gradient.Visible then
             CurrentMaker.Gradient.EndColor:=IColorPalette.Selected
          else
             CurrentMaker.Color:=IColorPalette.Selected;

          if SetDirty then
             MarkDirty;
        end
        else
        begin
          if not SetDirty then
          begin
            IDropBackup.Restore(CurrentMaker);

            IDropBackup.Active:=True;
            IDropBackup.IsTexture:=True;
            IDropBackup.Texture:=nil;
            IDropBackup.Block:=nil;
            IDropBackup.IsMaker:=False;
            IDropBackup.PictureLink:='';
            IDropBackup.Picture:=TPicture.Create;
            IDropBackup.Picture.Assign(CurrentMaker.BackImage);
          end;

          CurrentMaker.BackImage.LoadFromFile(ILibrary.SelectedTexture);
        end;
      end;
    end;
  end;
end;

procedure TMakerEditor.Maker1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if not Assigned(Target) then
     IDropBackup.Restore(CurrentMaker);
end;

procedure TMakerEditor.Maker1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source=ILibrary.TreeObjects then
     LibraryAddObject(Self)
  else
     DoDragDrop(Source,X,Y);

  IDropBackup.Active:=False;
end;

type
  TBlockEditorAccess=class(TBlockEditor);

procedure TMakerEditor.Maker1MouseMove(Sender: TObject; Shift: TShiftState;
                                       X, Y: Integer);

  function FixDegree(const Value:Double):Double;
  begin
    result:=Round(Value) mod 360;

    if result<0 then
       result:=360-result;
  end;

  procedure DoEditMode;
  var
    OldPos : TPoint;

    procedure ChangeY(const Delta:Double);
    begin
      Current.Size.Y:=Max(0,Current.Size.Y-Delta*(Y-OldPos.Y));
    end;

    procedure ChangeZ(const Delta:Double);
    begin
      Current.Size.Z:=Max(0,Current.Size.Z+Delta*(Y-OldPos.Y));
    end;

  var tmp    : Double;
      tmpNew : Double;
      IZRotated : Boolean;
  begin
    if Current<>nil then
    begin
      CurrentMaker.Options.Navigate.Dragging:=True;

      OldPos:=TMakerAccess(CurrentMaker).DragPosition;

      IModifying:=True;
      TBlockEditorAccess(IBlockEditor).IModifying:=True;

      if ssShift in Shift then
         tmp:=3
      else
         tmp:=1;

      if SBRotateBlock.Down then
      begin
        if ssCtrl in Shift then
        begin
          tmpNew:=FixDegree(Current.Rotation.Z+tmp*(Y-OldPos.Y));

          IZRotated:=tmpNew<>Current.Rotation.Z;

          if IZRotated then
          begin
            Current.Rotation.Z:=tmpNew;
            IBlockEditor.BlockTilt.Position:=Round(Current.Rotation.Z);
            IBlockEditor.LTilt.Caption:=TeeStr(IBlockEditor.BlockTilt.Position);
          end;
        end
        else
        begin
          Current.Rotation.X:=FixDegree(Current.Rotation.X+tmp*(X-OldPos.X));
          IBlockEditor.BlockRotation.Position:=Round(Current.Rotation.X);
          IBlockEditor.LRotation.Caption:=TeeStr(IBlockEditor.BlockRotation.Position);

          Current.Rotation.Y:=FixDegree(Current.Rotation.Y-tmp*(Y-OldPos.Y));
          IBlockEditor.BlockElevation.Position:=Round(Current.Rotation.Y);
          IBlockEditor.LElevation.Caption:=TeeStr(IBlockEditor.BlockElevation.Position);
        end;
      end
      else
      if SBMoveBlock.Down then
      begin
        if ssCtrl in Shift then
        begin
          if SBLight.Down then
             with CurrentMaker.Render.Light0 do
                  Position.Y:=Position.Y+tmp*(Y-OldPos.Y)
          else
          begin
            Current.Location.Z:=Current.Location.Z+tmp*(Y-OldPos.Y);
            IBlockEditor.RefreshLocation;
          end;
        end
        else
        begin
          if SBLight.Down then
          begin
            with CurrentMaker.Render.Light0 do
            begin
              Position.X:=Position.X+tmp*(X-OldPos.X);
              Position.Z:=Position.Z-tmp*(Y-OldPos.Y);
            end;
          end
          else
          begin
            Current.Location.X:=Current.Location.X+tmp*(X-OldPos.X);
            Current.Location.Y:=Current.Location.Y-tmp*(Y-OldPos.Y);

            IBlockEditor.RefreshLocation;
          end;
        end;
      end
      else
      if SBSizeBlock.Down then
      begin
        if Current is TCustomObjectBlock then
           tmp:=tmp*0.01;

        if (ssCtrl in Shift) and TBlockAccess(Current).UsesDepth then
        begin
          if Current is TCustomObjectBlock then
          begin
            Current.Scale.Z:=Max(0.00001,Current.Scale.Z+tmp*(Y-OldPos.Y));
            IBlockEditor.BlockScaleZ.Position:=TBlockFormatEditor.FromScaleValue(Current.Scale.Z);
            IBlockEditor.LScaleZ.Caption:=FormatFloat('0.000',Current.Scale.Z);
          end
          else
             ChangeZ(tmp)
        end
        else
        begin
          if Current is TCustomObjectBlock then
          begin
            Current.Scale.X:=Max(0.00001,Current.Scale.X+tmp*(X-OldPos.X));
            IBlockEditor.BlockScaleX.Position:=TBlockFormatEditor.FromScaleValue(Current.Scale.X);
            IBlockEditor.LScaleX.Caption:=FormatFloat('0.000',Current.Scale.X);

            Current.Scale.Y:=Max(0.00001,Current.Scale.Y+tmp*(Y-OldPos.Y));
            IBlockEditor.BlockScaleY.Position:=TBlockFormatEditor.FromScaleValue(Current.Scale.Y);
            IBlockEditor.LScaleY.Caption:=FormatFloat('0.000',Current.Scale.Y);
          end
          else
          begin
            Current.Size.X:=Max(0,Current.Size.X+tmp*(X-OldPos.X));

            if TBlockAccess(Current).UsesDepth then
               ChangeY(tmp)
            else
               ChangeZ(tmp);

            IBlockEditor.RefreshSize;
          end;
        end;
      end;

      SetBoundTitle;

      OldPos.X:=X;
      OldPos.Y:=Y;

      TMakerAccess(CurrentMaker).DragPosition:=OldPos;

      IModifying:=False;
      TBlockEditorAccess(IBlockEditor).IModifying:=False;
      MarkDirty;

      CurrentMaker.CancelMouse:=True;
    end;
  end;

var tmpBlock : TCustomBlock;
begin
  if EditMode1.Checked then
  begin
    if TMakerAccess(CurrentMaker).DragInfo.PreDragging then
    begin
      with TMakerAccess(CurrentMaker).DragInfo do
      if (not Assigned(Source)) or (not TMakerAccess(CurrentMaker).ParentIsSelected(Source)) then
          DoEditMode;
    end
    else
    if not CurrentMaker.Options.Navigate.Dragging then
    begin
      tmpBlock:=CurrentMaker.Blocks.ClickedBlock(X,Y);

      if Assigned(tmpBlock) then
      begin
        CurrentMaker.Cursor:=crHandPoint;
        StatusBar1.SimpleText:=TBlockAccess(tmpBlock).TitleOrName;
      end
      else
      begin
        CurrentMaker.Cursor:=crDefault;
        StatusBar1.SimpleText:='';
      end;

      CurrentMaker.OriginalCursor:=CurrentMaker.Cursor;
    end;
  end;
end;

procedure TMakerEditor.LoadNewMaker(const AFileName:String);
begin
  if Assigned(PageMakers) or DoSave(True) then
  begin
    DoLoad(AFileName);
    CurrentMaker.SetFocus;
  end;
end;

procedure TMakerEditor.ViewSource(const AStream:TStream);
var tmp : TStrings;
    tmpOut : TMemoryStream;
begin
  AStream.Position:=0;

  tmpOut:=TMemoryStream.Create;
  try
    ObjectBinaryToText(AStream,tmpOut);

    tmp:=TStringList.Create;
    try
      tmpOut.Position:=0;
      tmp.LoadFromStream(tmpOut);

      if TStringsEditor.Edit(Self,tmp) then
         if TeeYesNo(TeeMsg_SureToReload,Self) then
            DoLoad(tmp);
    finally
      tmp.Free;
    end;

  finally
    tmpOut.Free;
  end;
end;

procedure TMakerEditor.Source1Click(Sender: TObject);
var tmpIn : TMemoryStream;
    Old   : TBlocks;
begin
  tmpIn:=TMemoryStream.Create;
  try
    Old:=IExtraBlocks.Parent;
    IExtraBlocks.Parent:=nil;

    CurrentMaker.Blocks.SaveToStream(tmpIn,True);

    IExtraBlocks.Parent:=Old;

    ViewSource(tmpIn);
  finally
    tmpIn.Free;
  end;
end;

procedure TMakerEditor.Print1Click(Sender: TObject);
begin
  TeeCommander1.ButtonPrint.Click;
end;

procedure TMakerEditor.Export1Click(Sender: TObject);
begin
  With TTeeExportFormBase.Create(Self) do
  try
    ExportPanel:=Self.CurrentMaker;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMakerEditor.SetExtraPositions;
const
  Size:TPoint3DFloat=(X:0.16; Y:0.04; Z:0.24);
  SizeR:TPoint3DFloat=(X:0.16; Y:0.16; Z:0.16);

  function SetPosition(Handles:TObjectBlock; Index:Integer;
                       const ASize:TPoint3DFloat):TCustomBlock;
  begin
    result:=Handles[Index];

    with TObjectBlockHandle(Handles).Locations[Index] do
         result.Location.Point:=CurrentMaker.Selected.ProjectPoint(X,Y,Z);

    result.Rotation:=CurrentMaker.Selected.Rotation;

    with CurrentMaker.Selected.Size do
         result.Size.SetPoint(ASize.X*X,ASize.Y*Y,ASize.Z*Z);
  end;

  procedure SetPositions(Handles:TObjectBlock);
  begin
    SetPosition(Handles,0,Size).Rotation.Z:=90;
    SetPosition(Handles,1,Size).Rotation.Z:=270;
    SetPosition(Handles,2,Size).Rotation.Z:=0;
    SetPosition(Handles,3,Size).Rotation.Z:=180;
    SetPosition(Handles,4,Size).Rotation.Y:=270;
    SetPosition(Handles,5,Size).Rotation.Y:=90;
  end;

  procedure SetLamps;

     procedure SetLamp(Index:Integer; ALight:TGLLightSource);
     begin
       with TConeBlock(ILightLamps[Index]) do
       begin
         with Location do
         begin
           Point.X:=ALight.Position.Point.X;
           Point.Y:=ALight.Position.Point.Y;
           Point.Z:=-ALight.Position.Point.Z;
         end;

         with Rotation.Point do
         begin
           X:=-180*ALight.Direction.Point.X;
           Y:=180*ALight.Direction.Point.Y;
           Z:=180*ALight.Direction.Point.Z;
         end;

         if Visible then
            Format.Color:=clYellow
         else
            Format.Color:=clDkGray;

         if ALight.SpotDegrees=180 then
         begin
           ConeSize.Point.X:=0;
           ConeSize.Point.Y:=0;
         end
         else
         begin
           ConeSize.Point.X:=ALight.SpotDegrees;
           ConeSize.Point.Y:=ALight.SpotDegrees;
         end;
       end;
     end;

     procedure AddLamp(Index:Integer; ALight:TGLLightSource);
     begin
       with ILightLamps.AddHandle(ALight.Position.Point,'Rotation.X','Visible=not Visible') do
       begin
         Format.Transparency:=40;
         Size.Value:=48;
       end;
     end;

  begin
    with CurrentMaker,Render do
    if Options.ShowLightLamps then
    begin
      if not Assigned(ILightLamps) then
      begin
        ILightLamps:=TObjectBlockHandle.Create(IExtraBlocks);
        ILightLamps.HandleClass:=TConeBlock;

        AddLamp(0,Light0);
        AddLamp(1,Light1);
        AddLamp(2,Light2);
      end;

      ILightLamps.Parent:=IExtraBlocks.Items;

      SetLamp(0,Light0);
      SetLamp(1,Light1);
      SetLamp(2,Light2);
    end
    else
    if Assigned(ILightLamps) then
       ILightLamps.Parent:=nil;
  end;

var t,tt : Integer;
begin
  if Assigned(CurrentMaker.Selected) then
  begin
    if SBSizeBlock.Down and Assigned(ISizeBlocks) then
       SetPositions(ISizeBlocks)
    else
    if SBMoveBlock.Down and Assigned(IMoveBlocks) then
       SetPositions(IMoveBlocks)
    else
    if SBRotateBlock.Down and Assigned(IRotateBlocks) then
    begin
      SetPosition(IRotateBlocks,0,SizeR).Rotation.X:=90;
      SetPosition(IRotateBlocks,1,SizeR);
      SetPosition(IRotateBlocks,2,SizeR).Rotation.Y:=90;
    end;

    if Assigned(IDesignHandles) then
    with IDesignHandles.Items do
    for t:=0 to Count-1 do
        if Block[t] is TObjectBlockHandle then
           for tt:=0 to TObjectBlockHandle(Block[t]).Items.Count-1 do
               SetPosition(TObjectBlockHandle(Block[t]),tt,SizeR)
        else
           SetPosition(IDesignHandles,t,SizeR);
  end
  else
    SetLamps;
end;

procedure TMakerEditor.AddExtraBlocks(const HelpText:String);
begin
  CurrentMaker.Options.SelectModeBlocks:=IExtraBlocks.Items;
  IExtraBlocks.Parent:=CurrentMaker.Blocks;
  StatusBar1.SimpleText:=HelpText;
  TeeCommander1.ButtonNormal.Down:=True;
end;

procedure TMakerEditor.SBSizeBlockClick(Sender: TObject);

  procedure AddResizeHandles;
  begin
    if not Assigned(ISizeBlocks) then
    begin
      ISizeBlocks:=TObjectBlockHandle.Create(IExtraBlocks);

      with ISizeBlocks do
      begin
        Format.Color:=clLime;
        HandleClass:=TArrowBlock;
        OnDragging:=RefreshSize;

        AddHandle(Point3D(-1.16,0,0),'Bounds.Left,MinMax:;Target.Bounds.Right','Bounds.Left=-=1');
        AddHandle(Point3D( 1.16,0,0),'Bounds.Right,MinMax:Target.Bounds.Left','Bounds.Right=+=1');
        AddHandle(Point3D( 0,1.24,0),'Bounds.Top,MinMax:Target.Bounds.Bottom,Invert','Bounds.Top=-=1');
        AddHandle(Point3D(0,-1.24,0),'Bounds.Bottom,MinMax:;Target.Bounds.Top,Invert','Bounds.Bottom=+=1');
        AddHandle(Point3D( 0,0,1.44),'Bounds.Front,MinMax:;Target.Bounds.Back,Invert','Bounds.Front=-=1');
        AddHandle(Point3D(0,0,-1.44),'Bounds.Back,MinMax:Target.Bounds.Front,Invert','Bounds.Back=+=1');

        Parent:=IExtraBlocks.Items;
      end;
    end;

    ISizeBlocks.Visible:=True;
  end;

begin
  if SBSizeBlock.Down then
  begin
    SBRotateBlock.Down:=False;
    SBMoveBlock.Down:=False;

    if Assigned(IMoveBlocks) then
       IMoveBlocks.Visible:=False;

    if Assigned(IRotateBlocks) then
       IRotateBlocks.Visible:=False;

    AddResizeHandles;
    AddExtraBlocks(TeeMsg_SizeBlockHelp);
  end
  else
    ISizeBlocks.Visible:=False;

  SetBoundTitle;
end;

procedure TMakerEditor.DisableBlockButtons;
begin
  SBRotateBlock.Down:=False;
  SBMoveBlock.Down:=False;
  SBSizeBlock.Down:=False;
end;

procedure TMakerEditor.TeeCommander1SetLabel(Sender: TTeeCommander;
  var Text: String);
begin
  if not TeeCommander1.ButtonNormal.Down then
     DisableBlockButtons;
end;

procedure TMakerEditor.TextureQuality1Click(Sender: TObject);
begin
  TextureQuality1.Checked:=not TextureQuality1.Checked;

  CurrentMaker.Render.TextureQuality:=TextureQuality1.Checked;

  TGLCanvasAccess(CurrentMaker.Canvas).SetTextureParams;

  CurrentMaker.Invalidate;
end;

function NodeImage(Node:TTreeNode):Integer;
begin
  if Node.HasChildren or IsFolder(Node) then
     if Node.Expanded then result:=1
                      else result:=0
  else
  if TCustomBlock(Node.Data) is TCustomObjectBlock then
     result:=3
  else
  if TCustomBlock(Node.Data).Visible then
     result:=2
  else
     result:=4;
end;

procedure TMakerEditor.TreeBlocksGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=NodeImage(Node);
end;

procedure TMakerEditor.TreeBlocksGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex:=NodeImage(Node);
end;

function ValidTarget(Node:TTreeNode):Boolean;
begin
  result:=Assigned(Node) and IsFolder(Node);
end;

procedure TMakerEditor.TreeBlocksDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tmp : TTreeNode;
    tmpDragged : TCustomBlock;
begin
  Accept:=False;

  tmp:=TreeBlocks.GetNodeAt(X,Y);

  if Assigned(tmp) then
  begin
    if tmp<>TreeBlocks.Selected then
    begin
      tmpDragged:=TCustomBlock(TTreeView(Sender).Selected.Data);

      if ValidTarget(tmp) then
         Accept:=(tmpDragged.Parent<>TCustomObjectBlock(tmp.Data).Items)
      else
      if Assigned(tmpDragged.Parent) and (not Assigned(tmp)) then
         Accept:=True;
    end;
  end
  else
    Accept:=True;
end;

procedure TMakerEditor.TreeBlocksDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp      : TTreeNode;
    tmpBlock : TCustomBlock;

    {$IFDEF D6}
    t        : Integer;
    {$ENDIF}
begin
  if Current<>nil then
  begin
    tmp:=TreeBlocks.GetNodeAt(X,Y);

    if ValidTarget(tmp) then
    begin
      // Add selected to folder

      CopySelectedBlocks(TCustomObjectBlock(tmp.Data).Items);
      DeleteSelectedBlocks;

      with TCustomObjectBlock(tmp.Data) do
           tmpBlock:=Items.Block[Items.Count-1];

      AddBlock(tmp,tmpBlock);

      TreeBlocks.Selected:=NodeOfBlock(tmpBlock);

      MarkDirty;
    end
    else
    if not Assigned(tmp) then
    begin
      // Dropped outside blocks (root)

      {$IFDEF D6}
      {$IFDEF CLX}
      for t:=0 to TreeBlocks.Items.Count-1 do
      if TreeBlocks.Items[t].Selected then
      begin
        TCustomBlock(TreeBlocks.Items[t].Data).Parent:=CurrentMaker.Blocks;
        TreeBlocks.Items[t].MoveTo(nil,naAdd);
      end;
      {$ELSE}
      for t:=0 to TreeBlocks.SelectionCount-1 do
      begin
        TCustomBlock(TreeBlocks.Selections[t].Data).Parent:=CurrentMaker.Blocks;
        TreeBlocks.Selections[t].MoveTo(nil,naAdd);
      end;
      {$ENDIF}
      {$ELSE}

      Current.Parent:=CurrentMaker.Blocks;
      TreeBlocks.Selected.MoveTo(nil,naAdd);
      {$ENDIF}

      MarkDirty;
    end;
  end;
end;

procedure TMakerEditor.BlockCustomEditClick(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=Current;

  if Assigned(tmp) then
  with tmp.Editor(Self) as TCustomForm do
  try
    if ShowModal=mrOk then
       MarkDirty;
  finally
    Free;
  end;
end;

procedure TMakerEditor.Wireframe1Click(Sender: TObject);
begin
  Wireframe1.Checked:=not Wireframe1.Checked;

  if Wireframe1.Checked then
     CurrentMaker.Render.DrawStyle:=tcsWire
  else
     CurrentMaker.Render.DrawStyle:=tcsSolid;
end;

procedure TMakerEditor.ScrollBar1Change(Sender: TObject);
var tmp : GLMat;
begin
  TeeMaterialSpecular :=ScrollBar1.Position*0.01;
  tmp:=ColorToGL(TeeMaterialSpecular);
  glMaterialfv(TeeColorPlanes,GL_SPECULAR,PGLFloat(@tmp));

  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.ScrollBar2Change(Sender: TObject);
var tmp : GLMat;
begin
  TeeMaterialDiffuse :=ScrollBar2.Position*0.01;
  tmp:=ColorToGL(TeeMaterialDiffuse);
  glMaterialfv(TeeColorPlanes,GL_DIFFUSE,PGLFloat(@tmp));

  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SBReflectionChange(Sender: TObject);
begin
  CurrentMaker.Options.Floor.Reflection:=SBReflection.Position;
  LReflection.Caption:=IntToStr(SBReflection.Position);
end;

procedure TMakerEditor.SBFogDensityChange(Sender: TObject);
begin
  CurrentMaker.Options.Fog.Density:=SBFogDensity.Position*0.001;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.CBFogEnabledClick(Sender: TObject);
begin
  CurrentMaker.Options.Fog.Enabled:=CBFogEnabled.Checked;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.BFogColorClick(Sender: TObject);
begin
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SBFogStartChange(Sender: TObject);
begin
  CurrentMaker.Options.Fog.StartPos:=SBFogStart.Position;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SBFogEndChange(Sender: TObject);
begin
  CurrentMaker.Options.Fog.EndPos:=SBFogEnd.Position;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.CBFogStyleChange(Sender: TObject);
begin
  CurrentMaker.Options.Fog.Style:=TFogStyle(CBFogStyle.ItemIndex);
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.ButtonNewFolderClick(Sender: TObject);
begin
  DoAddClick(TObjectBlock.Create(NewBlockOwner),TeeMsg_Folder);
  TreeBlocks.SetFocus;
  TreeBlocks.Selected.EditText;
end;

procedure TMakerEditor.TreeBlocksDblClick(Sender: TObject);
begin
  if (not IsFolder(TreeBlocks.Selected)) or (TreeBlocks.Selected.Count=0) then
     BlockCustomEditClick(Self);
end;

procedure TMakerEditor.Makelocalcopy1Click(Sender: TObject);

  procedure CheckName(AOwner,AComp:TComponent);
  var tmp : String;
      t   : Integer;
  begin
    if Assigned(AOwner) then
    begin
      tmp:=AComp.Name;
      t:=0;

      while AOwner.FindComponent(tmp)<>nil do
      begin
        if t>0 then
           tmp:=AComp.Name+TeeStr(t);

        Inc(t);
      end;

      if AComp.Name<>tmp then
      begin
        AComp.Name:=tmp;

        if Assigned(AComp.Owner) then
           AComp.Owner.RemoveComponent(AComp);
           
        AOwner.InsertComponent(AComp);
      end;
    end;
  end;

var t : Integer;
begin
  if TeeYesNo(TeeMsg_SureToConvertLink) then
  begin
    TCustomObjectBlockAccess(Current).FLink:='';

    with TCustomObjectBlock(Current) do
    for t:=0 to Items.Count-1 do
    begin
      CheckName(Current.Owner,Items[t]);
      AddBlock(TreeBlocks.Selected,Items[t]);
    end;

    MarkDirty;
  end;
end;

procedure TMakerEditor.CBAutoPlayClick(Sender: TObject);
begin
  if Assigned(IAnimEditor) then
  begin
    CurrentAnimate.PlayOnLoad:=CBAutoPlay.Checked;
    MarkDirty;
  end;
end;

procedure TMakerEditor.SBMuteClick(Sender: TObject);
var tmp : Integer;
begin
  if SBMute.Down then
     tmp:=0
  else
     tmp:=100;

  TPlaySoundAnimation.ChangeVolume(tmp);

  {$IFNDEF BCB}
  TPlayMP3Sound.ChangeVolume(tmp);
  {$ENDIF}
end;

procedure TMakerEditor.TreeBlocksKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
      VK_F2: if Assigned(TreeBlocks.Selected) then
                TreeBlocks.Selected.EditText;

  VK_DELETE: if Assigned(TreeBlocks.Selected) then
                if not TreeBlocks.IsEditing then
                   BDeleteClick(Self);
  end;
end;

procedure TMakerEditor.TreeBlocksAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var tmp : TObject;
    tmpColor : TColor;
begin
  tmp:=TObject(Node.Data);

  if cdsSelected in State then
     tmpColor:=clHighlightText
  else
     tmpColor:=clWindowText;

  if Assigned(tmp) then
     if tmp is TCustomObjectBlock then
     with TCustomObjectBlock(tmp) do
          if (not Assigned(LinkBlock)) and (LinkFile='') then
              if not HasContents then
                 tmpColor:=clSilver;

  {$IFNDEF CLX}
  TreeBlocks.Canvas.Font.Color:=tmpColor;
  {$ENDIF}

  DefaultDraw:=True;
end;

procedure TMakerEditor.ReplaceBlock(const OldBlock,NewBlock:TCustomBlock);
var tmpNode : TTreeNode;
    tmpName : String;
begin
  if (NewBlock is TCustomObjectBlock) or (NewBlock.ClassType<>OldBlock.ClassType) then
  begin
    if not (NewBlock is TCustomObjectBlock) then
       NewBlock.Assign(OldBlock);

    tmpNode:=NodeOfBlock(OldBlock);

    DoAddClick(NewBlock,NewBlock.Title,tmpNode.Parent);

    tmpName:=OldBlock.Name;
    tmpNode.Free;
    OldBlock.Free;

    NewBlock.Name:=tmpName;
  end
  else
    NewBlock.Free;
end;

procedure TMakerEditor.BChangeClick(Sender: TObject);
var tmp    : TCustomBlock;
    tmpNew : TCustomBlock;
begin
  tmp:=Current;

  if Assigned(tmp) then
  begin
    tmpNew:=TBlockGallery.ModalShow(Self,NewBlockOwner,TBlockClass(tmp.ClassType));

    if Assigned(tmpNew) then
       ReplaceBlock(tmp,tmpNew);
  end;
end;

procedure TMakerEditor.SBReflectDistanceChange(Sender: TObject);
begin
  CurrentMaker.Options.Floor.Distance:=SBReflectDistance.Position;
  LReflectDist.Caption:=IntToStr(SBReflectDistance.Position);
end;

procedure TMakerEditor.CBShadowsClick(Sender: TObject);
begin
  CurrentMaker.Options.DrawShadows:=CBShadows.Checked;

  ShadowColor.Position:=GetRValue(CurrentMaker.Blocks.Shadows.Color);
  ShadowColor.Enabled:=CurrentMaker.Options.DrawShadows;
  ShapeShadowColor.Brush.Color:=CurrentMaker.Blocks.Shadows.Color;

  ShadowTransp.Position:=CurrentMaker.Blocks.Shadows.Transparency;
  ShadowTransp.Enabled:=CurrentMaker.Options.DrawShadows;
  LShadowTransp.Caption:=IntToStr(CurrentMaker.Blocks.Shadows.Transparency);
end;

procedure TMakerEditor.ShadowColorChange(Sender: TObject);
begin
  with ShadowColor do
  begin
    CurrentMaker.Blocks.Shadows.Color:=RGB(Position,Position,Position);
    ShapeShadowColor.Brush.Color:=CurrentMaker.Blocks.Shadows.Color;
  end;

  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.ShadowTranspChange(Sender: TObject);
begin
  CurrentMaker.Blocks.Shadows.Transparency:=ShadowTransp.Position;
  LShadowTransp.Caption:=IntToStr(ShadowTransp.Position);
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.CBFogNicestClick(Sender: TObject);
begin
  CurrentMaker.Options.Fog.Fast:=not CBFogNicest.Checked;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SBLightClick(Sender: TObject);
begin
  SBSizeBlock.Enabled:=Assigned(CurrentMaker.Selected) and (not SBLight.Down);

  if SBSizeBlock.Down then
     SBSizeBlock.Down:=False;

  CurrentMaker.Options.ShowLightLamps:=SBLight.Down;

  SBMoveBlock.Enabled:=Assigned(CurrentMaker.Selected) or SBLight.Down;
  SBRotateBlock.Enabled:=SBMoveBlock.Enabled;

  if SBLight.Down then
  begin
    SBMoveBlock.Down:=True;
    TeeCommander1.ButtonNormal.Down:=True;

    AddExtraBlocks('Drag to move Light source');
  end
  else
  if Assigned(ILightLamps) then
    ILightLamps.Parent:=nil;
end;

procedure TMakerEditor.RGNavigateClick(Sender: TObject);
begin
  CurrentMaker.Options.Navigate.Mode:=TNavigateMode(RGNavigate.ItemIndex);
  ComboNavigate.ItemIndex:=RGNavigate.ItemIndex;
end;

procedure TMakerEditor.PageExtrasChange(Sender: TObject);

  procedure UpdateTreeLinks;

    procedure DoAddBlocks(Node:TTreeNode; ABlocks:TBlocks);
    var t : Integer;
    begin
      for t:=0 to ABlocks.Count-1 do
      if ABlocks.Block[t] is TCustomObjectBlock then
      with TCustomObjectBlock(ABlocks.Block[t]) do
      if (Items<>nil) and (Items.Count>0) then
      begin
        if LinkFile<>'' then
           DoAddBlocks(TreeLinks.Items.AddChildObject(Node,LinkFile,ABlocks.Block[t]),Items)
        else
           DoAddBlocks(Node,Items);
      end;
    end;

  var tmpNode : TTreeNode;
  begin
    TreeLinks.Items.Clear;

    if CurrentMaker.Blocks.Count>0 then
    begin
      if CurrentMakerTab.FileName='' then
         tmpNode:=nil
      else
         tmpNode:=TreeLinks.Items.AddChild(nil,CurrentMakerTab.FileName);

      DoAddBlocks(tmpNode,CurrentMaker.Blocks);

      if TreeLinks.Items.Count>0 then
         TreeLinks.Items[0].Expand(False);
    end;
  end;

  procedure UpdateTreePictures;

    procedure DoAddBlocks(ABlocks:TBlocks);
    var t : Integer;
        tmp : String;
    begin
      for t:=0 to ABlocks.Count-1 do
      begin
        tmp:=UpperCase(ABlocks.Block[t].Format.Texture.PictureLink);

        if tmp<>'' then
        with TreePictures.Items do
             if IndexOf(tmp)=-1 then
                AddObject(tmp,ABlocks.Block[t].Format);

        if ABlocks.Block[t] is TCustomObjectBlock then
        begin
          with TCustomObjectBlock(ABlocks.Block[t]) do
          if (Items<>nil) and (Items.Count>0) then
             DoAddBlocks(Items);
        end;
      end;
    end;

  begin
    TreePictures.Sorted:=False;
    TreePictures.Items.Clear;

    if CurrentMaker.Blocks.Count>0 then
       DoAddBlocks(CurrentMaker.Blocks);

    TreePictures.Sorted:=True;

    TreePicturesClick(Self);
  end;

  procedure AddFonts;
  var tmp : TGLCanvas;
      t,
      tmpNum : Integer;
  begin
    tmp:=TGLCanvas(CurrentMaker.Canvas);
    tmpNum:=TGLCanvasAccess(tmp).CachedFonts;

    LCachedFonts.Caption:=Format(TeeMsg_CachedFonts,[TeeStr(tmpNum)]);

    ListFonts.Items.Clear;

    for t:=0 to tmpNum-1 do
        with TGLCanvasAccess(tmp).FontCache[t] do
             ListFonts.Items.Add(Name+' '+TeeStr(Weight)+' '+FloatToStr(Extrusion));
  end;

begin
  if PageExtras.ActivePage=TabFloor then
  with CurrentMaker.Options.Floor do
  begin
    LimitFloor.ItemIndex:=Ord(Limit);

    SBReflection.Position:=Reflection;
    LReflection.Caption:=IntToStr(Reflection);

    SBReflectDistance.Position:=Distance;
    LReflectDist.Caption:=IntToStr(Distance);

    if not Assigned(IFloorEditor) then
    begin
      IFloorEditor:=TBlockEditor.Create(Self);
      IFloorEditor.Align:=alClient;
      TTeeVCL.AddFormTo(IFloorEditor,TabFloor);
    end;

    if IFloorEditor.Current<>CurrentMaker.Options.Floor.Block then
       IFloorEditor.RefreshBlock(CurrentMaker.Options.Floor.Block);
  end
  else
  if PageExtras.ActivePage=TabCamera then
  begin
    with CurrentMaker.Options.Navigate.Joystick1 do
    begin
      CBUseJoystick1.Enabled:=Present;
      CBUseJoystick1.Checked:=Present and Active;
    end;

    RGNavigate.ItemIndex:=Ord(CurrentMaker.Options.Navigate.Mode);
    ComboNavigate.ItemIndex:=RGNavigate.ItemIndex;

    UDWalkSpeed.Position:=Round(CurrentMaker.Options.Navigate.WalkSpeed);

    UpdateCameraData(CurrentMaker);
    UpdateNavigate(CurrentMaker);
  end
  else
  if PageExtras.ActivePage=TabBack then
  begin
    if Assigned(IBackEditor) then
    begin
      if IBackEditor.ThePanel<>CurrentMaker then
         IBackEditor.RefreshPanel(CurrentMaker);
    end
    else
    begin
      IBackEditor:=TFormTeePanel.InsertAt(Self,TabBack,CurrentMaker);

      IBackEditor.TabBorders.TabVisible:=False;
      IBackEditor.TabMargins.TabVisible:=False;
      IBackEditor.TabShadow.TabVisible:=False;
      IBackEditor.TabEmboss.TabVisible:=False;
      IBackEditor.CBImageInside.Visible:=False;
    end;
  end
  else
  if PageExtras.ActivePage=TabOther then
  begin
    CBThreading.Checked:=CurrentMaker.Options.UseThreads;
    CBClickToFocus.Checked:=CurrentMaker.Options.ClickToFocus;

    UpdateTreeLinks;
    UpdateTreePictures;
    AddFonts;
  end;
end;

procedure TMakerEditor.CBThreadingClick(Sender: TObject);
begin
  CurrentMaker.Options.UseThreads:=CBThreading.Checked;
end;

procedure TMakerEditor.CBUseJoystick1Click(Sender: TObject);
begin
  CurrentMaker.Options.Navigate.Joystick1.Active:=CBUseJoystick1.Checked;
end;

procedure TMakerEditor.CheckLights;
begin
  if not Assigned(IGLEditor) then
  begin
    IGLEditor:=TFormTeeGLEditor.Create(Self);
    IGLEditor.TabLights.Align:=alClient;
    IGLEditor.TabLights.Parent:=TabLighting;
  end;

  IGLEditor.RefreshOpenGL(CurrentMaker.Render);

  with CurrentMaker.Render do
  begin
    TBAmbientLight.Position:=AmbientLight;
    LAmbientLight.Caption:=IntToStr(AmbientLight);
  end;
end;

procedure TMakerEditor.PageEditorChange(Sender: TObject);
begin
  if PageEditor.ActivePage=TabBlocks then
  begin
    if TreeBlocks.Items.Count=0 then
       FillBlockList;
  end
  else
  if PageEditor.ActivePage=TabLibrary then
  begin
    ILibrary.LibraryPath:=CurrentMaker.Blocks.LibraryPath;
    ILibrary.TryFillTrees;
  end
  else
  if PageEditor.ActivePage=TabExtras then
     PageExtrasChange(Self)
  else
  if PageEditor.ActivePage=TabLighting then
     CheckLights
  else
  if PageEditor.ActivePage=TabColors then
     CheckColors;
end;

procedure TMakerEditor.CheckColors;
begin
  if not Assigned(IColorPalette) then
  begin
    IColorPalette:=TColorPalette.Create(Self);
    IColorPalette.Align:=alClient;
    TTeeVCL.AddFormTo(IColorPalette,TabColors);

    IColorPalette.OnCurrentChanged:=ColorPaletteChanged;
  end;
end;

procedure TMakerEditor.ColorPaletteChanged(Sender: TObject);
begin
  if Assigned(TMakerAccess(CurrentMaker).CurrentOver) then
     TMakerAccess(CurrentMaker).CurrentOver.Format.Color:=IColorPalette.CurrentColor
  else
  if CurrentMaker.Gradient.Visible then
     CurrentMaker.Gradient.EndColor:=IColorPalette.CurrentColor
  else
     CurrentMaker.Color:=IColorPalette.CurrentColor;
end;

procedure TMakerEditor.LibraryOpenObject(Sender:TObject);
begin
  LoadNewMaker(ILibrary.SelectedLinkFile);
end;

procedure TMakerEditor.ListAnimDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
begin
  Accept:=Source=TreeBlocks;

  if not Accept then
     IOnDragOver(Sender,Source,X,Y,State,Accept);
end;

procedure TMakerEditor.ListAnimDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source=TreeBlocks then
  begin
    IAnimGalleryBlock:=Current;
    IAnimEditor.SBAddClick(Self);
  end
  else
    IOnDragDrop(Sender,Source,X,Y);
end;

function TMakerEditor.LoadNewObjectFile(const APath,AFile,AExt:String):TCustomObjectBlock;
begin
  if AExt=TeeMakerExtension then
     result:=TObjectBlock.Create(NewBlockOwner)
  else
     result:=TObjBlock.Create(NewBlockOwner);

  result.LinkFile:=ILibrary.LinkFile(APath,AFile,AExt);
  result.Items;  // force load

  {
  if APath='' then
     result.LinkFile:=AFile
  else
     result.LinkFile:=ILibrary.LinkFile(APath,AFile,AExt);
  }
end;

function TMakerEditor.SelectedLibraryBlock:TCustomBlock;
var tmp : TTreeNode;
    tmpExt : String;
begin
  result:=nil;

  tmp:=ILibrary.TreeObjects.Selected;

  if Assigned(tmp) then
  begin
    if ILibrary.IsBasicNode(tmp) then
    begin
      result:=BlockClasses[{$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(tmp.Data)].Create(NewBlockOwner);
      TBlockAccess(result).PrepareForGallery;
    end
    else
    with ILibrary do
    if NodeIsFile(tmp) then
    begin
      if SelectedIsMaker then
         tmpExt:=TeeMakerExtension
      else
      if SelectedIsObj then
         tmpExt:=TeeObjExtension
      else
         tmpExt:=Tee3DSExtension;

      result:=LoadNewObjectFile(NodePath(TreeObjects,tmp.Parent),tmp.Text,tmpExt);
    end;
  end;
end;

procedure TMakerEditor.LibraryAddObject(Sender: TObject);
var tmp : TTreeNode;
    tmpBlock : TCustomBlock;
begin
  tmpBlock:=IDropBackup.Block;

  if not Assigned(tmpBlock) then
  begin
    tmpBlock:=SelectedLibraryBlock;

    with CurrentMaker.GetCursorPos do
         SetNewBlockLocation(tmpBlock,X,Y);
  end;

  if Assigned(tmpBlock) then
  begin
    tmp:=ILibrary.TreeObjects.Selected;

    if ILibrary.IsBasicNode(tmp) then
       DoAddClick(tmpBlock,tmp.Text)
    else
    with ILibrary do
    if NodeIsFile(tmp) then
       DoAddClick(tmpBlock,RemoveFileExtension(tmp.Text+TeeMakerExtension));
  end;
end;

{$IFNDEF CLX}
procedure TMakerEditor.WMDROPFILES(var Message: TWMDROPFILES);

  function GetFileName:String;
  var FileName : Array[0..255] of Char;
  begin
    if DragQueryFile(Message.Drop,$FFFFFFFF,nil,0)>0 then
    begin
      DragQueryFile(Message.Drop,0,@FileName,SizeOf(FileName));

      result:=FileName;
    end
    else
      result:='';

    DragFinish(Message.Drop);
  end;

  function ChangePic(ABlock:TCustomBlock; const APic:String):Boolean;
  begin
    result:=ABlock.Format.Texture.PictureLink<>APic;

    if result then
       ABlock.Format.Texture.PictureLink:=APic;
  end;
  
var tmp : TCustomBlock;
    tmpFile : String;
begin
  tmpFile:=GetFileName;

  if tmpFile<>'' then
  begin
    if TBlockPicture.FileGraphicClass(tmpFile)<>nil then
    begin
      if (PageEditor.ActivePage<>TabLibrary) or
         (not ILibrary.DropExternalFile(ILibrary.TreeTextures,tmpFile)) then
      begin
        tmp:=BlockUnderMouse;

        if Assigned(tmp) then
        begin
          TreeBlocks.Selected:=NodeOfBlock(tmp);

          if ChangePic(Current,tmpFile) then
             MarkDirty;
        end
        else
        begin
          with CurrentMaker.GetCursorPos do
               tmp:=CurrentMaker.Options.Floor.Block.Clicked(X,Y);

          if Assigned(tmp) then
             ChangePic(tmp,tmpFile)
          else
             CurrentMaker.BackImage.LoadFromFile(tmpFile);
        end;
      end;
    end
    else
    if IsMakerFile(tmpFile) then
    begin
      if PtInRect(CurrentMaker.ClientRect,CurrentMaker.GetCursorPos) then
         DoAddClick(LoadNewObjectFile('',tmpFile,TeeMakerExtension),RemoveFileExtension(tmpFile))
      else
      if PageEditor.ActivePage=TabLibrary then
         ILibrary.DropExternalFile(ILibrary.TreeObjects,tmpFile);
    end;
  end;
end;
{$ENDIF}

procedure TMakerEditor.CBAnaglyphChange(Sender: TObject);
begin
  CurrentMaker.Options.Anaglyph:=TAnaglyph(CBAnaglyph.ItemIndex);
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.Shadows1Click(Sender: TObject);
begin
  Shadows1.Checked:=not Shadows1.Checked;
  CurrentMaker.Options.DrawShadows:=Shadows1.Checked;
end;

procedure TMakerEditor.SBAnaglyphDistanceChange(Sender: TObject);
begin
  CurrentMaker.Options.AnaglyphDistance:=SBAnaglyphDistance.Position;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.TreeBlocksMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TreeBlocks.GetNodeAt(X,Y)=nil then
     TreeBlocks.Selected:=nil;
end;

function TMakerEditor.IsMakerFile(const FileName:String):Boolean;
begin
  result:=UpperCase(ExtractFileExt(FileName))=UpperCase(TeeMakerExtension);
end;

Procedure EnableControls(Enable:Boolean; Const ControlArray:Array of TMenuItem);
var t : Integer;
begin
  for t:=Low(ControlArray) to High(ControlArray) do
  if Assigned(ControlArray[t]) then
     ControlArray[t].Enabled:=Enable;
end;

procedure TMakerEditor.PopupMenu1Popup(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=Current;

  EnableControls(Assigned(tmp),[Delete1,Duplicate1,Link1,Savetoexternal1,Rename1]);

  ReplaceItem.Enabled:=Assigned(tmp) and (not IsFolder(tmp));

  Gotolink1.Visible:=Assigned(tmp) and (tmp is TCustomObjectBlock) and
                     (TCustomObjectBlockAccess(tmp).LinkBlock<>nil);

  Open2.Enabled:=Assigned(tmp) and (tmp is TCustomObjectBlock) and
                 (TCustomObjectBlockAccess(tmp).CompleteLinkFile<>'');

  Makelocalcopy1.Enabled:=Open2.Enabled;

  EditLink.Enabled:=Open2.Enabled and IsMakerFile(TCustomObjectBlock(tmp).LinkFile);

  BlockCustomEdit.Enabled:=Assigned(tmp);
end;

procedure TMakerEditor.Events1Click(Sender: TObject);
begin
  if TStringsEditor.Edit(Self,CurrentMaker.Blocks.Events) then
     MarkDirty;
end;

procedure TMakerEditor.Reopen1Click(Sender: TObject);
begin
  CheckReopen;
end;

procedure TMakerEditor.ComboURLDropDown(Sender: TObject);
begin
  CheckReopen;
end;

procedure TMakerEditor.Link1Click(Sender: TObject);
var tmp : TCustomObjectBlock;
    tmpNode : TTreeNode;
begin
  tmp:=TObjectBlock.Create(NewBlockOwner);
  tmp.LinkBlock:=Current;

  tmpNode:=NodeOfBlock(Current);

  DoAddClick(tmp,Format(TeeMsg_LinkToBlock,[tmp.LinkBlock.Title]),tmpNode.Parent,False);

  with tmp,Location.Point do
       X:=X+Size.X+(Size.X*0.5);

  TreeBlocks.Selected:=NodeOfBlock(tmp);
end;

procedure TMakerEditor.Gotolink1Click(Sender: TObject);
begin
  TreeBlocks.Selected:=NodeOfBlock(TCustomObjectBlock(Current).LinkBlock);
end;

procedure TMakerEditor.LimitFloorChange(Sender: TObject);
begin
  CurrentMaker.Options.Floor.Limit:=TMakerLimitFloor(LimitFloor.ItemIndex);
end;

procedure TMakerEditor.File1Click(Sender: TObject);
begin
  CheckReopen;

  Close1.Enabled:=Close1.Visible and (PageMakers.PageCount>1);
  CloseAll1.Enabled:=Close1.Enabled;
end;

procedure TMakerEditor.Properties1Click(Sender: TObject);
begin
  if TPropertiesEditor.ModalShow(Self,CurrentMaker.Blocks.Properties) then
     MarkDirty;
end;

procedure TMakerEditor.ComboNavigateChange(Sender: TObject);
begin
  CurrentMaker.Options.Navigate.Mode:=TNavigateMode(ComboNavigate.ItemIndex);
  RGNavigate.ItemIndex:=ComboNavigate.ItemIndex;
end;

procedure TMakerEditor.Rename1Click(Sender: TObject);
begin
  if TreeBlocks.Selected.EditText then
     MarkDirty;
end;

procedure TMakerEditor.TreePicturesDblClick(Sender: TObject);
begin
  BViewPicClick(Self);
end;

procedure TMakerEditor.BChangePicClick(Sender: TObject);
var
  OldPic : String;
  NewPic : String;

  procedure ReplacePicture(Blocks:TBlocks);
  var t : Integer;
  begin
    for t:=0 to Blocks.Count-1 do
    begin
      if Blocks[t].Format.Texture.PictureLink=OldPic then
         Blocks[t].Format.Texture.PictureLink:=NewPic;

      if Blocks[t] is TCustomObjectBlock then
         ReplacePicture(TCustomObjectBlock(Blocks[t]).Items);
    end;
  end;

var tmp : TBlockFormat;
begin
  tmp:=TBlockFormat(TreePictures.Items.Objects[TreePictures.ItemIndex]);

  OldPic:=tmp.Texture.PictureLink;

  if TTextureSelector.ModalShow(Self,tmp.Block.Parent,tmp) then
  begin
    NewPic:=tmp.Texture.PictureLink;

    ReplacePicture(CurrentMaker.Blocks);
    TreePictures.Items[TreePictures.ItemIndex]:=tmp.Texture.PictureLink;
    MarkDirty;
  end;
end;

procedure TMakerEditor.TreePicturesClick(Sender: TObject);
begin
  BChangePic.Enabled:=TreePictures.ItemIndex<>-1;
  BViewPic.Enabled:=BChangePic.Enabled;
end;

function GetWinSysDir:String;
var St : Array[0..MAX_PATH] of Char;
begin
  GetSystemDirectory(St,MAX_PATH);
  result:=StrPas(St);
end;

function FindDll(AName:String):String;
begin
  result:='';

  if ExtractFileExt(AName)='' then
     AName:=AName+'.dll';

  if FileExists(AName) then
     result:=AName
  else
  begin
    AName:=ExtractFileName(AName);

    if FileExists(AName) then
       result:=AName
    else
    if FileExists(GetWinSysDir+'\'+AName) then
       result:=GetWinSysDir+'\'+AName;
  end;
end;

procedure DoCopyFile(const Source,Dest:String);
begin
  CopyFile(PChar(Source),PChar(Dest),False);
end;

procedure CopyDll(const AName,DestFolder:String);
var tmpSource : String;
begin
  tmpSource:=FindDll(AName);

  if tmpSource='' then
     Raise Exception.Create('Cannot find DLL: '+AName)
  else
     DoCopyFile(tmpSource,DestFolder+'\'+ExtractFileName(tmpSource));
end;

type
  TBlockTextureAccess=class(TBlockTexture);

procedure TMakerEditor.Button2Click(Sender: TObject);

  procedure EmbeddPictures(Blocks:TBlocks);
  var t : Integer;
  begin
    for t:=0 to Blocks.Count-1 do
    begin
      if Blocks[t].Format.Texture.PictureLink<>'' then
         TBlockTextureAccess(Blocks[t].Format.Texture).SetEmbeddedPicture;

      if Blocks[t] is TCustomObjectBlock then
         EmbeddPictures(TCustomObjectBlock(Blocks[t]).Items);
    end;
  end;

  procedure EmbeddLinks(Blocks:TBlocks; ParentSource:String; const DestFolder:String);
  var t : Integer;
      tmpSource : String;
  begin
    for t:=0 to Blocks.Count-1 do
    if Blocks[t] is TCustomObjectBlock then
    with TCustomObjectBlock(Blocks[t]) do
    begin
      if LinkFile<>'' then
      begin
        // Copy sub-sub-objects to folder:

        tmpSource:=TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,LinkFile);

        if not FileExists(tmpSource) then
           if ParentSource<>'' then
              if FileExists(ParentSource+'\'+tmpSource) then
                 tmpSource:=ParentSource+'\'+tmpSource;

        DoCopyFile(tmpSource,DestFolder+'\'+ExtractFileName(tmpSource));

        EmbeddLinks(Items,ExtractFilePath(LinkFile),DestFolder);

        TCustomObjectBlockAccess(Blocks[t]).FLink:=ExtractFileName(LinkFile);

        // Embedd sub-objects intead of copying:
        // (note: pending to "rename" whole objects subtree to avoid duplications when loading it back)
        // TCustomObjectBlockAccess(Blocks[t]).FLink:='';

      end
      else
        EmbeddLinks(Items,ParentSource,DestFolder);
    end;
  end;

var tmpFolder : String;
    tmpFile   : String;
begin
  if DoSave(True) then
  begin
    tmpFolder:=ExtractFilePath(CurrentMakerTab.FileName);

    if tmpFolder='' then
       tmpFolder:=GetCurrentDir;

    if TTeeVCL.SelectFolder(TeeMsg_SelectFolder,'',tmpFolder) then
    begin
      tmpFile:=RemoveFileExtension(ExtractFileName(CurrentMakerTab.FileName));

      if tmpFile='' then
         tmpFile:='TeeMaker';

      tmpFolder:=tmpFolder+'\'+tmpFile;

      if {$IFDEF D15}SysUtils.{$ENDIF}DirectoryExists(tmpFolder) or
         TeeYesNo(Format(TeeMsg_CreateFolder,[tmpFolder])) then
      begin
        Screen.Cursor:=crHourGlass;
        try
          ForceDirectories(tmpFolder);

          {$IFDEF TEEBASS}
          CopyDll('Bass',tmpFolder);
          {$ENDIF}
          
          CopyDll('GLUT32',tmpFolder);

          {$IFOPT D+}
          CopyDll('FastMM_FullDebugMode',tmpFolder);
          {$ENDIF}

          CopyDll(Application.ExeName,tmpFolder);

          EmbeddPictures(CurrentMaker.Blocks);
          EmbeddLinks(CurrentMaker.Blocks,ExtractFilePath(CurrentMakerTab.FileName),tmpFolder);

          CurrentMakerTab.FileName:=tmpFolder+'\'+tmpFile;

          Save1Click(Self);

          TeeGotoURL(Handle,tmpFolder);
          PageExtrasChange(Self);
        finally
          Screen.Cursor:=crDefault;
        end;
      end;
    end;
  end;
end;

procedure TMakerEditor.BViewPicClick(Sender: TObject);
begin
  if TreePictures.ItemIndex<>-1 then
     TTextureSelector.ModalShow(Self,TreePictures.Items[TreePictures.ItemIndex]);
end;

procedure TMakerEditor.BViewObjectClick(Sender: TObject);
begin
  TBlockGallery.ModalPreview(Self,TCustomBlock(TreeLinks.Selected.Data));
end;

procedure TMakerEditor.CameraInertiaChange(Sender: TObject);
begin
  if not IModifying then
  begin
    CurrentMaker.Options.Navigate.RotateInertia:=CameraInertia.Position;
    MarkDirty;
  end;
end;

procedure TMakerEditor.CBClickToFocusClick(Sender: TObject);
begin
  CurrentMaker.Options.ClickToFocus:=CBClickToFocus.Checked;
end;

procedure TMakerEditor.PageMakersChange(Sender: TObject);
begin
  ActivateMaker(Makers[PageMakers.ActivePageIndex].Maker);
end;

procedure TMakerEditor.Close1Click(Sender: TObject);
var tmp : Integer;
    t   : Integer;
begin
  if DoSave(True) then
  begin
    TreeBlocks.Selected:=nil;

    tmp:=PageMakers.ActivePage.TabIndex;
    RemoveMaker(tmp);

    for t:=tmp+1 to Length(Makers)-1 do
        Makers[t-1]:=Makers[t];

    SetLength(Makers,Length(Makers)-1);

    PageMakers.ActivePage.Free;

    if tmp>0 then
       PageMakers.ActivePageIndex:=tmp-1;

    PageMakersChange(Self);
  end;
end;

procedure TMakerEditor.CloseAll1Click(Sender: TObject);
begin
  while PageMakers.PageCount>1 do
        Close1Click(Self);
end;

procedure TMakerEditor.Closetab1Click(Sender: TObject);
begin
  Close1Click(Self);
end;

procedure TMakerEditor.PopupTabsPopup(Sender: TObject);
begin
  Closetab1.Enabled:=(PageMakers.PageCount>1);
end;

procedure TMakerEditor.Newtab1Click(Sender: TObject);
begin
  New1Click(Self);
end;

procedure TMakerEditor.CBMouseWheelChange(Sender: TObject);
begin
  CurrentMaker.Options.Navigate.MouseWheel:=TWheelAction(CBMouseWheel.ItemIndex);
  MarkDirty;
end;

procedure TMakerEditor.ReplaceItemClick(Sender: TObject);
{var tmp : TCustomBlock;
    tmpNew : TCustomBlock;
    tmpSt  : String;
    tmpNode : TTreeNode;}
begin
  BChangeClick(Self);

  (*
  tmp:=Current;
  tmpNode:=TreeBlocks.Selected;
  tmpNew:=AddNewBlockGallery;

  if Assigned(tmpNew) then
  begin
    tmpSt:=tmp.Name;
    tmpNew.Parent:=tmp.Parent;

    //tmpNew.Assign(tmp);

    DeleteNodeAndBlock(tmpNode);
    tmpNew.Name:=tmpSt;
    MarkDirty;
  end;
  *)
end;

{ TMakerDropBackup }

Destructor TMakerDropBackup.Destroy;
begin
  Picture.Free;
  inherited;
end;

procedure TMakerDropBackup.Restore(Maker:TMaker);
begin
  if Active then
  begin
    if IsTexture then
    begin
      if Assigned(Texture) then
         Texture.PictureLink:=PictureLink
      else
      begin
        Maker.BackImage.Assign(Picture);
        FreeAndNil(Picture);
      end;
    end
    else
    begin
      if Assigned(Block) then
         Block.Format.Color:=Color
      else
      if IsMaker then
         if Maker.Gradient.Visible then
            Maker.Gradient.EndColor:=Color
        else
            Maker.Color:=Color;

      IsMaker:=False;

      if IsBlock then
      begin
        Block.Free;
        IsBlock:=False;
      end;

      Block:=nil;
    end;

    Active:=False;
  end;
end;

procedure TMakerEditor.SpeedButton1Click(Sender: TObject);
begin
  if TTextureSelector.ModalShow(Self,CurrentMaker.Blocks,CurrentMaker.Options.Floor.Format) then
     EDefFloorTexture.Text:=CurrentMaker.Options.Floor.Format.Texture.PictureLink;
end;

procedure TMakerEditor.X1Click(Sender: TObject);
begin
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.VerticalSync1Click(Sender: TObject);
var tmp : TGLCanvas;
begin
  VerticalSync1.Checked:=not VerticalSync1.Checked;

  tmp:=(CurrentMaker.Canvas as TGLCanvas);

  if VerticalSync1.Checked then
     tmp.ScreenSync:=ssYes
  else
     tmp.ScreenSync:=ssNo;

  if tmp.ScreenSync=ssNo then
     VerticalSync1.Checked:=False;
end;

procedure TMakerEditor.FullScreen1Click(Sender: TObject);
begin
  FullScreen1.Checked:=not FullScreen1.Checked;

  if FullScreen1.Checked then
  begin
    Menu:=nil;
    CurrentMaker.Parent:=Self;
  end
  else
  begin
    Menu:=MainMenu1;
    CurrentMaker.Parent:=CurrentMakerTab.Tab;
  end;
end;

procedure TMakerEditor.CBShadowSmoothClick(Sender: TObject);
begin
  CurrentMaker.Blocks.Shadows.Smooth:=CBShadowSmooth.Checked;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.SetCamera(const ACamera:TMakerCamera);
begin
  PanelCamera.Enabled:=Assigned(ACamera);

  CurrentMaker.Options.Cameras.Selected:=ACamera;

  if PanelCamera.Enabled then
  begin
    if not Assigned(ICameraEditor) then
    begin
      ICameraEditor:=TCameraEditor.Create(Self);
      ICameraEditor.Align:=alClient;
      TTeeVCL.AddFormTo(ICameraEditor,PanelCamera);
    end;

    ICameraEditor.RefreshCamera(ACamera);
  end;
end;

procedure TMakerEditor.LBCamerasClick(Sender: TObject);
begin
  PanelCamera.Visible:=LBCameras.ItemIndex<>-1;

  if PanelCamera.Visible then
     SetCamera(CurrentMaker.Options.Cameras[LBCameras.ItemIndex]);

  SBRemoveCamera.Enabled:=LBCameras.ItemIndex<>-1;
  BRenameCamera.Enabled:=SBRemoveCamera.Enabled;
end;

procedure TMakerEditor.CameraChanged(Sender: TObject);
begin
  with CurrentMaker.Options.Cameras do
  if Selected=Sender then
  begin
    Selected.SetToView(CurrentMaker.View3DOptions);
    UpdateCameraData(CurrentMaker);
  end;
end;

procedure TMakerEditor.SBAddCameraClick(Sender: TObject);

  function DefaultName:String;
  begin
    result:='Camera '+IntToStr(CurrentMaker.Options.Cameras.Count);
  end;

var tmpSt : String;
    tmp   : TMakerCamera;
begin
  tmpSt:=DefaultName;

  if InputQuery('New Camera','Camera Name?',tmpSt) then
  begin
    tmpSt:=Trim(tmpSt);

    if tmpSt='' then
       tmpSt:=DefaultName;

    tmp:=(CurrentMaker.Options.Cameras.Add as TMakerCamera);
    tmp.OnChange:=CameraChanged;
    tmp.Title:=tmpSt;

    tmp.SetFromView(CurrentMaker.View3DOptions);

    LBCameras.Items.Add(tmpSt);

    LBCameras.ItemIndex:=LBCameras.Items.Count-1;
    LBCamerasClick(Self);

    MarkDirty;
  end;
end;

procedure TMakerEditor.SBRemoveCameraClick(Sender: TObject);
begin
  if TeeYesNo('Remove Camera: '+LBCameras.Items[LBCameras.ItemIndex]+'?') then
  begin
    CurrentMaker.Options.Cameras.Delete(LBCameras.ItemIndex-1);
    MarkDirty;
  end;
end;

procedure TMakerEditor.SourceMaker1Click(Sender: TObject);
var tmpIn : TMemoryStream;
    Old   : TBlocks;
begin
  tmpIn:=TMemoryStream.Create;
  try
    Old:=IExtraBlocks.Parent;
    IExtraBlocks.Parent:=nil;

    SaveTeeToStream(CurrentMaker,tmpIn);

    IExtraBlocks.Parent:=Old;

    ViewSource(tmpIn);
  finally
    tmpIn.Free;
  end;
end;

procedure TMakerEditor.Rename2Click(Sender: TObject);
var tmp : String;
begin
  tmp:=CurrentMaker.Name;

  if InputQuery('New Maker name','Name?',tmp) then
  begin
    CurrentMaker.Name:=tmp;
    PageMakers.ActivePage.Caption:=tmp;
  end;
end;

procedure TMakerEditor.BRenameCameraClick(Sender: TObject);
var tmp : String;
begin
  tmp:=LBCameras.Items[LBCameras.ItemIndex];

  if InputQuery('New Camera Name','Camera name?',tmp) then
  if Trim(tmp)<>'' then
  begin
    LBCameras.Items[LBCameras.ItemIndex]:=tmp;
    CurrentMaker.Options.Cameras[LBCameras.ItemIndex].Title:=tmp;
    MarkDirty;
  end;
end;

procedure TMakerEditor.Replace1Click(Sender: TObject);

  procedure ReplaceBlocks(Blocks:TBlocks; NewBlock:TCustomBlock);
  var t : Integer;
      tmp : TCustomBlock;
  begin
    TreeBlocks.Items.BeginUpdate;
    try
      for t:=0 to Blocks.Count-1 do
      if (Blocks[t] is TCustomObjectBlock) and
         (TCustomObjectBlock(Blocks[t]).LinkFile='') then
           ReplaceBlocks(TCustomObjectBlock(Blocks[t]).Items,NewBlock)
      else
      begin
        tmp:=TBlockClass(NewBlock.ClassType).Create(Self);
        ReplaceBlock(Blocks[t],tmp);
      end;

    finally
      TreeBlocks.Items.EndUpdate;
    end;
  end;

begin
  with TBlockReplacer.Create(Self) do
  try
    if ShowModal=mrOk then
       ReplaceBlocks(CurrentMaker.Blocks,MakerNew.Blocks[0]);
  finally
    Free;
  end;
end;

//type
//  TPanelAccess=class(TCustomTeePanel);

procedure TMakerEditor.CBFPSClick(Sender: TObject);
begin
  CurrentMaker.RepaintMonitor:=CBFPS.Checked;
end;

procedure TMakerEditor.CBTransp3DClick(Sender: TObject);
begin
  TGLCanvasAccess(CurrentMaker.Canvas).ITransp3D:=CBTransp3D.Checked;
  CurrentMaker.Invalidate;
end;

procedure TMakerEditor.Button1Click(Sender: TObject);
begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_CURRENT_USER;

    if OpenKey(TeeMakerKey+'\BlockEditor\Back',True) then
    begin
      WriteInteger('GradientStart',CurrentMaker.Gradient.StartColor);
      WriteInteger('GradientMiddle',CurrentMaker.Gradient.MidColor);
      WriteInteger('GradientEnd',CurrentMaker.Gradient.EndColor);
      WriteBool('Gradient',CurrentMaker.Gradient.Visible);
      WriteInteger('Color',CurrentMaker.Color);

      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TMakerEditor.SBEditModeClick(Sender: TObject);
begin
  EditMode1.Checked:=SBEditMode.Down;
  SetEditMode(EditMode1.Checked);
end;

procedure TMakerEditor.EWalkSpeedChange(Sender: TObject);
begin
  if Showing then
     CurrentMaker.Options.Navigate.WalkSpeed:=UDWalkSpeed.Position;
end;

procedure TMakerEditor.Kinematics1Click(Sender: TObject);
begin
  Kinematics1.Checked:=not Kinematics1.Checked;

  if not Assigned(IKinematics) then
  begin
    IKinematics:=TKinematicsEditor.Create(Self);
    IKinematics.Align:=alBottom;
    TTeeVCL.AddFormTo(IKinematics,PanelBig);
  end;

  if Kinematics1.Checked then
  begin
    if not Assigned(Kinematics) then
    begin
      Kinematics:=TKinematics.Create(CurrentMaker);
      Kinematics.Movements.OnCollision:=BlockCollision;
    end;

    IKinematics.RefreshKinematics(Kinematics);
  end;

  IKinematics.Visible:=Kinematics1.Checked;

  SplitterAnim.Visible:=IKinematics.Visible or (Assigned(IAnimEditor) and IAnimEditor.Visible);
end;

procedure TMakerEditor.BlockCollision(Sender:TMovement; const ABlock:TCustomBlock;
                                              var ACollided:TCustomBlock;
                                              var APoint:TPoint3DFloat);
begin
  CurrentMaker.DoBlockAction(ABlock,BlockAction_KinematicsCollision);
end;

procedure TMakerEditor.TBAmbientLightChange(Sender: TObject);
begin
  with CurrentMaker.Render do
  begin
    AmbientLight:=TBAmbientLight.Position;
    LAmbientLight.Caption:=IntToStr(AmbientLight);
  end;
end;

procedure TMakerEditor.ESmoothSizeChange(Sender: TObject);
begin
  if Showing then
  begin
    CurrentMaker.Blocks.Shadows.SmoothSize:=UDSmoothSize.Position;
    CurrentMaker.Invalidate;
  end;
end;

{$IFNDEF CLX}
initialization
  NewStyleControls:=True;
{$ENDIF}
end.
