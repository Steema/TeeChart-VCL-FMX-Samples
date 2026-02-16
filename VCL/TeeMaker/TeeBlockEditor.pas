{********************************************}
{ TeeMaker Block Editor Dialog               }
{ Copyright (c) 2007-2026 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeBlockEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, TypInfo,

  {$IFDEF D17}
  System.UITypes,
  {$ENDIF}
 
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls, QCheckLst,
  QExtCtrls, QMenus, QButtons,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, ComCtrls, StdCtrls,
  Menus,
  {$ENDIF}
  TeeFilters, TeCanvas, TeeProcs, TeePenDlg, TeeBlocks, TeeEdiGrad,
  TeeMakerControl, TeeRoundRect, TeeBlockFormat, TeePointEditor,
  TeeSelectProperty, TeeAnimate;

type
  TBlockEditor = class(TVisualEditor)
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    PageCurrent: TPageControl;
    TabFormat: TTabSheet;
    TabRotation: TTabSheet;
    LRotation: TLabel;
    LElevation: TLabel;
    LTilt: TLabel;
    BlockRotation: TScrollBar;
    BlockElevation: TScrollBar;
    BlockTilt: TScrollBar;
    TabPosition: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    EditX: TEdit;
    BlockX: TUpDown;
    EditY: TEdit;
    BlockY: TUpDown;
    EditZ: TEdit;
    BlockZ: TUpDown;
    EditWidth: TEdit;
    BlockWidth: TUpDown;
    EditHeight: TEdit;
    BlockHeight: TUpDown;
    EditDepth: TEdit;
    BlockDepth: TUpDown;
    CBSpinBy: TCheckBox;
    ESpinBy: TEdit;
    UDSpinBy: TUpDown;
    TabTile: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    BlockTileX: TScrollBar;
    BlockTileY: TScrollBar;
    BlockTileZ: TScrollBar;
    GroupBox2: TGroupBox;
    Label9: TLabel;
    Label15: TLabel;
    Label19: TLabel;
    BlockTileOffX: TScrollBar;
    BlockTileOffY: TScrollBar;
    BlockTileOffZ: TScrollBar;
    TabEllipsoid: TTabSheet;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    ElipSides: TEdit;
    BlockElipSides: TUpDown;
    ElipStacks: TEdit;
    BlockElipStacks: TUpDown;
    BlockElipTotal: TScrollBar;
    TabGradient: TTabSheet;
    TabCylinder: TTabSheet;
    GroupBox3: TGroupBox;
    BlockElipCover: TButton;
    BlockElipCoverDef: TCheckBox;
    BlockElipCoverVisible: TCheckBox;
    TabText: TTabSheet;
    TabBevel: TTabSheet;
    Label29: TLabel;
    BlockBevelX: TScrollBar;
    LabelBevelX: TLabel;
    Label32: TLabel;
    BlockElipTotalAngle: TScrollBar;
    TabSphere: TTabSheet;
    TabLight: TTabSheet;
    TabRoundRect: TTabSheet;
    TabObject: TTabSheet;
    TabPyramid: TTabSheet;
    TabBridge: TTabSheet;
    Label44: TLabel;
    Label45: TLabel;
    BlockBridgeSize: TScrollBar;
    BlockBridgeHeight: TScrollBar;
    TabPie: TTabSheet;
    TabHole: TTabSheet;
    TabScale: TTabSheet;
    GroupBox1: TGroupBox;
    LScaleX: TLabel;
    LScaleY: TLabel;
    LScaleZ: TLabel;
    BlockScaleX: TScrollBar;
    BlockScaleY: TScrollBar;
    BlockScaleZ: TScrollBar;
    GroupBox4: TGroupBox;
    LCenterX: TLabel;
    LCenterY: TLabel;
    LCenterZ: TLabel;
    BlockCenterX: TScrollBar;
    BlockCenterY: TScrollBar;
    BlockCenterZ: TScrollBar;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    PageControl4: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label37: TLabel;
    ESpot: TEdit;
    BlockLightSpot: TUpDown;
    CBDefaultSpot: TCheckBox;
    Label38: TLabel;
    BlockLightSpotExp: TTrackBar;
    BlockLightInt: TTrackBar;
    BlockLightColor: TButtonColor;
    BlockLightFixed: TCheckBox;
    BlockLightLamp: TCheckBox;
    BlockLightUseDir: TCheckBox;
    Label51: TLabel;
    BlockLightDiffuse: TTrackBar;
    BlockLightSpecular: TTrackBar;
    Label52: TLabel;
    PanelButtons: TPanel;
    Button1: TButton;
    TabArrow: TTabSheet;
    Label53: TLabel;
    BlockArrowWidth: TScrollBar;
    TabPipe: TTabSheet;
    Label57: TLabel;
    BlockPipeXRadius: TScrollBar;
    GroupBox7: TGroupBox;
    Label8: TLabel;
    Label42: TLabel;
    BlockWedgeX1: TScrollBar;
    BlockWedgeY1: TScrollBar;
    GroupBox8: TGroupBox;
    Label65: TLabel;
    Label66: TLabel;
    BlockWedgeX2: TScrollBar;
    BlockWedgeY2: TScrollBar;
    PipeXRadius: TLabel;
    BlockBridgeRounded: TCheckBox;
    BlockArrowHeight: TScrollBar;
    Label70: TLabel;
    TabCross: TTabSheet;
    Label72: TLabel;
    Label73: TLabel;
    BlockCrossWidth: TScrollBar;
    BlockCrossHeight: TScrollBar;
    GroupBox9: TGroupBox;
    Label74: TLabel;
    Label75: TLabel;
    BlockCrossCenterX: TScrollBar;
    BlockCrossCenterY: TScrollBar;
    TabCone: TTabSheet;
    LabelCone: TLabel;
    BlockConeX: TScrollBar;
    Label76: TLabel;
    Label77: TLabel;
    BlockConeY: TScrollBar;
    LabelConeX: TLabel;
    LabelConeY: TLabel;
    PageControl5: TPageControl;
    TabHoleSize: TTabSheet;
    TabHoleCorners: TTabSheet;
    Label49: TLabel;
    Label50: TLabel;
    BlockHoleX: TScrollBar;
    BlockHoleY: TScrollBar;
    BlockHoleStyle: TComboFlat;
    TabPath: TTabSheet;
    Label67: TLabel;
    BlockArrowIndent: TScrollBar;
    Label80: TLabel;
    BlockSphereRadius: TScrollBar;
    TabExtruded: TTabSheet;
    TabBlock: TTabSheet;
    BlockVisible: TCheckBox;
    Label4: TLabel;
    BlockTitle: TEdit;
    Button2: TButton;
    BlockExtrudedFront: TButton;
    BlockExtrudedBack: TButton;
    CBBlockExtFront: TCheckBox;
    CBBlockExtBack: TCheckBox;
    TabRectangle: TTabSheet;
    TabCube: TTabSheet;
    BlockCubeSidePanel: TPanel;
    Panel2: TPanel;
    Label7: TLabel;
    BlockCubeSide: TListBox;
    Panel3: TPanel;
    BlockCubeSideDefault: TCheckBox;
    PageControl6: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet6: TTabSheet;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    BlockPieDonut: TScrollBar;
    BlockPieStart: TScrollBar;
    BlockPieAngle: TScrollBar;
    PageControl3: TPageControl;
    TabPieOuterTop: TTabSheet;
    Label31: TLabel;
    PageControl7: TPageControl;
    TabFont: TTabSheet;
    TabTextLines: TTabSheet;
    BlockTextLines: TMemo;
    TabPieOuterBottom: TTabSheet;
    TabPieInnerTop: TTabSheet;
    TabPieInnerBottom: TTabSheet;
    TabSides: TTabSheet;
    BlockSide1: TButton;
    BlockSide2: TButton;
    BlockSide1Def: TCheckBox;
    BlockSide2Def: TCheckBox;
    BlockSide1Visible: TCheckBox;
    BlockSide2Visible: TCheckBox;
    TabHoleFormat: TTabSheet;
    TabEllipse: TTabSheet;
    Label18: TLabel;
    BlockEllipseSlices: TScrollBar;
    LBlockEllipse: TLabel;
    TabTube: TTabSheet;
    Label28: TLabel;
    BlockBridgeRound: TEdit;
    UDBlockBridgeRound: TUpDown;
    LTileX: TLabel;
    LTileY: TLabel;
    LTileZ: TLabel;
    LPyramid1X: TLabel;
    LPyramid1Y: TLabel;
    LPyramid2X: TLabel;
    LPyramid2Y: TLabel;
    LPieDonut: TLabel;
    LPieStart: TLabel;
    LPieAngle: TLabel;
    TabRectPyramid: TTabSheet;
    Label84: TLabel;
    BlockRectLeft: TScrollBar;
    LRectLeft: TLabel;
    Label85: TLabel;
    BlockRectRight: TScrollBar;
    LRectRight: TLabel;
    BlockScaleXYZ: TScrollBar;
    Label90: TLabel;
    LSphereRadius: TLabel;
    Label91: TLabel;
    PipeYRadius: TLabel;
    BlockPipeYRadius: TScrollBar;
    Label92: TLabel;
    BlockPipeConnector: TComboFlat;
    TabStar: TTabSheet;
    Label93: TLabel;
    BlockStarInner: TScrollBar;
    Label94: TLabel;
    BlockStarSlant: TScrollBar;
    LStarInner: TLabel;
    LStarSlant: TLabel;
    PageControl8: TPageControl;
    TabRectCorners: TTabSheet;
    TabRectSides: TTabSheet;
    BlockRectLT: TButtonColor;
    BlockRectLB: TButtonColor;
    BlockRectRT: TButtonColor;
    BlockRectRB: TButtonColor;
    BlockRectDefLT: TCheckBox;
    BlockRectDefLB: TCheckBox;
    BlockRectDefRT: TCheckBox;
    BlockRectDefRB: TCheckBox;
    BlockRectL: TButtonColor;
    BlockRectR: TButtonColor;
    BlockRectT: TButtonColor;
    BlockRectB: TButtonColor;
    BlockRectDefL: TCheckBox;
    BlockRectDefR: TCheckBox;
    BlockRectDefT: TCheckBox;
    BlockRectDefB: TCheckBox;
    Label95: TLabel;
    LabelBevelY: TLabel;
    BlockBevelY: TScrollBar;
    Label96: TLabel;
    LabelBevelZ: TLabel;
    BlockBevelZ: TScrollBar;
    BlockBevelAll: TCheckBox;
    Label98: TLabel;
    BlockName: TEdit;
    TabActions: TTabSheet;
    LEllipTotal: TLabel;
    LEllipAngle: TLabel;
    BlockPieStacks: TScrollBar;
    LPieStacks: TLabel;
    TabSheet7: TTabSheet;
    Label97: TLabel;
    BlockPieInnerTop: TScrollBar;
    Label99: TLabel;
    BlockPieInnerBottom: TScrollBar;
    Label100: TLabel;
    BlockBevelCurvePoints: TScrollBar;
    LBevelCurvePoints: TLabel;
    CBPosition10: TCheckBox;
    BlockBevelRounded: TCheckBox;
    Label102: TLabel;
    BlockBevelStyle: TComboFlat;
    PageObject: TPageControl;
    TabObjectSource: TTabSheet;
    TabObjectAnimations: TTabSheet;
    GroupBox5: TGroupBox;
    BlockObjectAnimations: TComboFlat;
    BlockObjectPlay: TButton;
    Label13: TLabel;
    LObjectBlocks: TLabel;
    Label30: TLabel;
    BSelectLink: TButton;
    BClearLink: TButton;
    BEditLink: TButton;
    BlockLink: TEdit;
    BLoad: TButton;
    Label103: TLabel;
    Label104: TLabel;
    BlockFontEdit: TButton;
    ESize: TEdit;
    BlockFontSize: TUpDown;
    GroupBox12: TGroupBox;
    BlockFontItalic: TCheckBox;
    BlockFontBold: TCheckBox;
    BlockFontStrike: TCheckBox;
    BlockFontUnder: TCheckBox;
    BlockFontColor: TButtonColor;
    BlockFontName: TComboFlat;
    BlockTextStyle: TComboFlat;
    Label78: TLabel;
    ListActions: TListBox;
    BlockPipeVisible: TCheckBox;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet8: TTabSheet;
    BlockCylinderCover: TButton;
    BlockCylinderCoverDef: TCheckBox;
    BlockCylinderCoverVisible: TCheckBox;
    Label33: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    LabelCylinderAngle: TLabel;
    LabelSlices: TLabel;
    LabelStacks: TLabel;
    Label71: TLabel;
    LabelCylinderStart: TLabel;
    BlockCylinderAngle: TScrollBar;
    BlockCylinderSlices: TScrollBar;
    BlockCylinderStacks: TScrollBar;
    BlockCylinderStart: TScrollBar;
    TabCylinderEdge: TTabSheet;
    PageControl9: TPageControl;
    TabCylTopEdge: TTabSheet;
    TabCylBottomEdge: TTabSheet;
    TabTextLink: TTabSheet;
    LabelTextLink: TLabel;
    BTextLink: TButton;
    Label105: TLabel;
    BlockTextLinkFormat: TComboFlat;
    BlockRectCenter: TButtonColor;
    BlockRectDefCenter: TCheckBox;
    Panel1: TPanel;
    Label35: TLabel;
    BlockTextAlign: TComboFlat;
    PageControl10: TPageControl;
    TabPoints: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    Label61: TLabel;
    BlockPathPointer: TComboFlat;
    Button7: TButton;
    BlockPathColorEach: TCheckBox;
    PanelPoints: TPanel;
    SBPathAdd: TSpeedButton;
    SBPathRemove: TSpeedButton;
    BlockPathPoints: TEdit;
    UDPathPoints: TUpDown;
    TabTriangle: TTabSheet;
    PageControl11: TPageControl;
    TabPoint0: TTabSheet;
    TabPoint1: TTabSheet;
    TabPoint2: TTabSheet;
    TabCustom: TTabSheet;
    Panel6: TPanel;
    BAddAction: TButton;
    BDeleteAction: TButton;
    Panel8: TPanel;
    TabObjectProperties: TTabSheet;
    BlockObjectProperties: TListBox;
    Label58: TLabel;
    BlockObjectPropValue: TEdit;
    SBObjPropValue: TSpeedButton;
    BClearTextLink: TButton;
    TabStars: TTabSheet;
    Label60: TLabel;
    Label79: TLabel;
    LabelStarCount: TLabel;
    StarMapSize: TUpDown;
    EStartMapSize: TEdit;
    StarCount: TScrollBar;
    Label107: TLabel;
    CBCursor: TComboFlat;
    SpeedButton1: TSpeedButton;
    Label106: TLabel;
    BlockElipEccen: TScrollBar;
    BOpenExplorer: TButton;
    TabHoleCenter: TTabSheet;
    Label108: TLabel;
    Label109: TLabel;
    BlockHoleCenterX: TScrollBar;
    BlockHoleCenterY: TScrollBar;
    Label110: TLabel;
    BObjectEmbedd: TButton;
    Label112: TLabel;
    LabelPropValue: TLabel;
    LabelWrongValue: TLabel;
    PopupActions: TPopupMenu;
    Manualedit1: TMenuItem;
    LHoleX: TLabel;
    LHoleY: TLabel;
    SBActionUp: TSpeedButton;
    SBActionDown: TSpeedButton;
    TreeEvents: TTreeView;
    Splitter1: TSplitter;
    Change1: TMenuItem;
    SBAddCustomAction: TSpeedButton;
    SBRemoveCustomAction: TSpeedButton;
    BlockNoScaling: TButton;
    CBTextQuality: TCheckBox;
    Label36: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Button8: TButton;
    Button9: TButton;
    LLinkBlock: TLabel;
    CBFaceToViewer: TCheckBox;
    LArrowHeadW: TLabel;
    LArrowHeadH: TLabel;
    LArrowIndent: TLabel;
    procedure FormShow(Sender: TObject);
    procedure EditXChange(Sender: TObject);
    procedure EditYChange(Sender: TObject);
    procedure EditZChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure EditHeightChange(Sender: TObject);
    procedure EditDepthChange(Sender: TObject);
    procedure BlockRotationChange(Sender: TObject);
    procedure BlockElevationChange(Sender: TObject);
    procedure BlockTiltChange(Sender: TObject);
    procedure BlockTitleChange(Sender: TObject);
    procedure CBSpinByClick(Sender: TObject);
    procedure BlockTileXChange(Sender: TObject);
    procedure BlockTileYChange(Sender: TObject);
    procedure BSelectLinkClick(Sender: TObject);
    procedure BClearLinkClick(Sender: TObject);
    procedure UDSpinByClick(Sender: TObject; Button: TUDBtnType);
    procedure BlockTileZChange(Sender: TObject);
    procedure BlockVisibleClick(Sender: TObject);
    procedure BlockLinkChange(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure BlockScaleXChange(Sender: TObject);
    procedure BlockScaleYChange(Sender: TObject);
    procedure BlockScaleZChange(Sender: TObject);
    procedure BlockTileOffXChange(Sender: TObject);
    procedure BlockTileOffYChange(Sender: TObject);
    procedure BlockTileOffZChange(Sender: TObject);
    procedure ElipSidesChange(Sender: TObject);
    procedure ElipStacksChange(Sender: TObject);
    procedure BlockElipTotalChange(Sender: TObject);
    procedure BlockConeXChange(Sender: TObject);
    procedure BlockSide1Click(Sender: TObject);
    procedure BlockSide2Click(Sender: TObject);
    procedure BlockSide1DefClick(Sender: TObject);
    procedure BlockSide2DefClick(Sender: TObject);
    procedure BlockSide1VisibleClick(Sender: TObject);
    procedure BlockSide2VisibleClick(Sender: TObject);
    procedure BEditLinkClick(Sender: TObject);
    procedure BlockElipCoverClick(Sender: TObject);
    procedure BlockElipCoverDefClick(Sender: TObject);
    procedure BlockElipCoverVisibleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BlockBevelXChange(Sender: TObject);
    procedure BlockElipTotalAngleChange(Sender: TObject);
    procedure BlockCylinderAngleChange(Sender: TObject);
    procedure BlockSphereSlicesChange(Sender: TObject);
    procedure BlockSphereStacksChange(Sender: TObject);
    procedure BlockLightColorClick(Sender: TObject);
    procedure ESpotChange(Sender: TObject);
    procedure CBDefaultSpotClick(Sender: TObject);
    procedure BlockLightFixedClick(Sender: TObject);
    procedure BlockLightIntChange(Sender: TObject);
    procedure BlockColorClick(Sender: TObject);
    procedure BlockWedgeX1Change(Sender: TObject);
    procedure BlockWedgeY1Change(Sender: TObject);
    procedure BlockBridgeSizeChange(Sender: TObject);
    procedure BlockBridgeHeightChange(Sender: TObject);
    procedure BlockLightLampClick(Sender: TObject);
    procedure BlockPieDonutChange(Sender: TObject);
    procedure BlockPieStartChange(Sender: TObject);
    procedure BlockPieAngleChange(Sender: TObject);
    procedure BlockHoleXChange(Sender: TObject);
    procedure BlockHoleYChange(Sender: TObject);
    procedure BlockHoleStyleChange(Sender: TObject);
    procedure BlockCenterXChange(Sender: TObject);
    procedure BlockCenterYChange(Sender: TObject);
    procedure BlockCenterZChange(Sender: TObject);
    procedure BlockBorderClick(Sender: TObject);
    procedure BlockLightUseDirClick(Sender: TObject);
    procedure BlockLightDiffuseChange(Sender: TObject);
    procedure BlockLightSpecularChange(Sender: TObject);
    procedure BlockLightSpotExpChange(Sender: TObject);
    procedure BlockArrowWidthChange(Sender: TObject);
    procedure BlockObjectAnimationsChange(Sender: TObject);
    procedure BlockObjectPlayClick(Sender: TObject);
    procedure BlockPipeXRadiusChange(Sender: TObject);
    procedure BlockPathPointerChange(Sender: TObject);
    procedure BlockPathPointsChange(Sender: TObject);
    procedure SBPathAddClick(Sender: TObject);
    procedure SBPathRemoveClick(Sender: TObject);
    procedure BlockWedgeX2Change(Sender: TObject);
    procedure BlockWedgeY2Change(Sender: TObject);
    procedure BlockBridgeRoundedClick(Sender: TObject);
    procedure BlockArrowHeightChange(Sender: TObject);
    procedure BlockCylinderCoverClick(Sender: TObject);
    procedure BlockCylinderCoverDefClick(Sender: TObject);
    procedure BlockCylinderCoverVisibleClick(Sender: TObject);
    procedure BlockCylinderStartChange(Sender: TObject);
    procedure BlockCrossWidthChange(Sender: TObject);
    procedure BlockCrossHeightChange(Sender: TObject);
    procedure BlockCrossCenterXChange(Sender: TObject);
    procedure BlockCrossCenterYChange(Sender: TObject);
    procedure BlockConeYChange(Sender: TObject);
    procedure BlockTextStyleChange(Sender: TObject);
    procedure BlockArrowIndentChange(Sender: TObject);
    procedure BlockSphereRadiusChange(Sender: TObject);
    procedure CBBlockExtFrontClick(Sender: TObject);
    procedure CBBlockExtBackClick(Sender: TObject);
    procedure BlockExtrudedFrontClick(Sender: TObject);
    procedure BlockExtrudedBackClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BlockRectDefLTClick(Sender: TObject);
    procedure BlockRectDefLBClick(Sender: TObject);
    procedure BlockRectDefRTClick(Sender: TObject);
    procedure BlockRectDefRBClick(Sender: TObject);
    procedure BlockRectLTClick(Sender: TObject);
    procedure BlockRectLBClick(Sender: TObject);
    procedure BlockRectRTClick(Sender: TObject);
    procedure BlockRectRBClick(Sender: TObject);
    procedure BlockCubeSideClick(Sender: TObject);
    procedure BlockCubeSideDefaultClick(Sender: TObject);
    procedure BlockTextLinesChange(Sender: TObject);
    procedure BlockTextAlignChange(Sender: TObject);
    procedure BlockEllipseSlicesChange(Sender: TObject);
    procedure BlockBridgeRoundChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure BlockRectLeftChange(Sender: TObject);
    procedure BlockRectRightChange(Sender: TObject);
    procedure BlockScaleXYZChange(Sender: TObject);
    procedure BlockPipeYRadiusChange(Sender: TObject);
    procedure BlockPipeConnectorChange(Sender: TObject);
    procedure BlockStarInnerChange(Sender: TObject);
    procedure BlockStarSlantChange(Sender: TObject);
    procedure BlockRectLClick(Sender: TObject);
    procedure BlockRectRClick(Sender: TObject);
    procedure BlockRectTClick(Sender: TObject);
    procedure BlockRectBClick(Sender: TObject);
    procedure BlockRectDefLClick(Sender: TObject);
    procedure BlockRectDefRClick(Sender: TObject);
    procedure BlockRectDefTClick(Sender: TObject);
    procedure BlockRectDefBClick(Sender: TObject);
    procedure BlockBevelYChange(Sender: TObject);
    procedure BlockBevelZChange(Sender: TObject);
    procedure BlockNameChange(Sender: TObject);
    procedure ListActionsClick(Sender: TObject);
    procedure BDeleteActionClick(Sender: TObject);
    procedure PageCurrentChange(Sender: TObject);
    procedure BAddActionClick(Sender: TObject);
    procedure BlockPieStacksChange(Sender: TObject);
    procedure BlockPieInnerTopChange(Sender: TObject);
    procedure BlockPieInnerBottomChange(Sender: TObject);
    procedure BlockBevelCurvePointsChange(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CBPosition10Click(Sender: TObject);
    procedure BlockBevelRoundedClick(Sender: TObject);
    procedure BlockBevelStyleChange(Sender: TObject);
    procedure BlockFontItalicClick(Sender: TObject);
    procedure BlockFontBoldClick(Sender: TObject);
    procedure BlockFontStrikeClick(Sender: TObject);
    procedure BlockFontUnderClick(Sender: TObject);
    procedure BlockFontNameChange(Sender: TObject);
    procedure BlockFontEditClick(Sender: TObject);
    procedure ESizeChange(Sender: TObject);
    procedure BlockFontColorClick(Sender: TObject);
    procedure SBActionUpClick(Sender: TObject);
    procedure SBActionDownClick(Sender: TObject);
    procedure BlockPipeVisibleClick(Sender: TObject);
    procedure BTextLinkClick(Sender: TObject);
    procedure BlockTextLinkFormatChange(Sender: TObject);
    procedure BlockRectDefCenterClick(Sender: TObject);
    procedure BlockRectCenterClick(Sender: TObject);
    procedure BlockPathColorEachClick(Sender: TObject);
    procedure BlockObjectPropertiesClick(Sender: TObject);
    procedure BlockObjectPropValueChange(Sender: TObject);
    procedure SBObjPropValueClick(Sender: TObject);
    procedure BClearTextLinkClick(Sender: TObject);
    procedure EStartMapSizeChange(Sender: TObject);
    procedure StarCountChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBCursorChange(Sender: TObject);
    procedure BlockElipEccenChange(Sender: TObject);
    procedure BOpenExplorerClick(Sender: TObject);
    procedure BlockHoleCenterYChange(Sender: TObject);
    procedure BlockHoleCenterXChange(Sender: TObject);
    procedure BObjectEmbeddClick(Sender: TObject);
    procedure PopupActionsPopup(Sender: TObject);
    procedure Manualedit1Click(Sender: TObject);
    procedure TreeEventsChange(Sender: TObject; Node: TTreeNode);
    procedure Button5Click(Sender: TObject);
    procedure BlockCubeSideDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure Change1Click(Sender: TObject);
    procedure PageObjectChange(Sender: TObject);
    procedure ESpinByChange(Sender: TObject);
    procedure BlockNoScalingClick(Sender: TObject);
    procedure CBTextQualityClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CBFaceToViewerClick(Sender: TObject);
  private
    { Private declarations }
    FGradientEditor : TTeeGradientEditor;
    ICubeSideEditor : TBlockFormatEditor;
    ITubeEditor     : TBlockFormatEditor;

    procedure AddEditAction(const AAction:String='');
    procedure CheckBlockNoScaling;
    procedure CheckFontStyle(Check:TCheckBox; Value:TFontStyle);
    procedure CheckPathPoints(ACount:Integer);
    procedure CheckPointEditor(var AEditor:TPointEditor; ATab:TTabSheet;
                               APoint:TPointXYZColor; const AFactor:Double);
    function CurrentActions:TBlockActionItem;
    function CurrentCubeSide:TBlockFormat;
    function CurrentMaker:TMaker;
    function CurrentPropertyIndex:Integer;
    function CurrentPropertyName:String;
    procedure EnableShapeTabs;
    procedure FillActions;
    procedure FontRefreshBasicProps;
    function Position10:Integer;
    procedure SetLabelPropValue;
    procedure SetLinkBlockLabel;

    {$IFDEF CLX}
    procedure TreeEventsCustomDrawItem(Sender: TCustomViewControl;
    Node: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
    State: TCustomDrawState; Stage: TCustomDrawStage;
    var DefaultDraw: Boolean);
    {$ELSE}
    procedure TreeEventsCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    {$ENDIF}

  protected
    IFormatEditor    : TBlockFormatEditor;
    IHoleFormat      : TBlockFormatEditor;
    IHoleRoundEditor : TRoundRectEditor;
    IModifying       : Boolean;
    IPointEditor     : TPointEditor;
    IPoint0          : TPointEditor;
    IPoint1          : TPointEditor;
    IPoint2          : TPointEditor;
    IRoundEditor     : TRoundRectEditor;

  public
    { Public declarations }

    BlocksTreeView : TTreeView;
    Current        : TCustomBlock;

    class procedure AddBlocks(ACombo:TComboFlat; ASkip,ASelected:TCustomBlock; const Desc:String);
    procedure RefreshBlock(const ABlock:TVisualBlock); override;
    procedure RefreshLocation;
    procedure RefreshRotation;
    procedure RefreshSize;
    class function ModalShow(AOwner:TComponent; ABlock:TCustomBlock):Boolean;
  end;

  TMakerPropertySelector=class(TPropertySelector)
  private
    procedure AddBlocks(Node:TTreeNode; ABlocks:TBlocks; ExternalBlocks:Boolean=False);
    procedure SelectNode(AObject:TObject; AName:String);
  protected
    procedure AddProperties(ATree:TTreeView; AObject:TObject; AList:TList;
                            AFilter:TTypeKinds); override;
  public
    procedure AddSelection(ABlocks:TBlocks; ExternalBlocks:Boolean=False);

    class function ModalShow(AOwner:TComponent; ABlocks:TBlocks;
                             var AObject:TObject; var AName:String):Boolean; overload;
    class function ModalShow(AOwner:TComponent; ABlocks:TBlocks; Link:TPropertyLink):Boolean; overload;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeRain, TeeExtruded, TeeLoadBlock, TeePipe, TeeStairs,
  TeeEdgeStyle, TeeTerrain, TeeBrushDlg, TeeRevolution, TeeHelix,
  TeeActionGallery, TeeExtrudedEditor, TeeMouseCursor,
  TeeMakerConst, TeeBlockGallery,
  TeeEdiFont; // <-- (used by TeeNumericGauge.pas unit only)

procedure TBlockEditor.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage:=TabGeneral;
  PageCurrent.ActivePage:=TabBlock;

  {$IFNDEF CLX}
  TreeEvents.HotTrack:=True;
  {$ENDIF}

  PanelButtons.Visible:=not Assigned(Parent);

  if Tag<>0 then
     RefreshBlock(TCustomBlock(Tag));
end;

procedure TBlockEditor.ListActionsClick(Sender: TObject);
begin
  BDeleteAction.Enabled:=ListActions.ItemIndex<>-1;
  SBActionUp.Enabled:=ListActions.ItemIndex>0;
  SBActionDown.Enabled:=ListActions.ItemIndex<ListActions.Items.Count-1;
end;

function TBlockEditor.Position10:Integer;
begin
  if CBPosition10.Checked then
     result:=10
  else
     result:=1;
end;

procedure TBlockEditor.EditXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Location.X:=BlockX.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.EditYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Location.Y:=BlockY.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.EditZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Location.Z:=BlockZ.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.EditWidthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Size.X:=BlockWidth.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.EditHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Size.Y:=BlockHeight.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.EditDepthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    if Showing then
       Current.Size.Z:=BlockDepth.Position/Position10;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRotationChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.X:=BlockRotation.Position;
    LRotation.Caption:=TeeStr(BlockRotation.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockElevationChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.Y:=BlockElevation.Position;
    LElevation.Caption:=TeeStr(BlockElevation.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTiltChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.Z:=BlockTilt.Position;
    LTilt.Caption:=TeeStr(BlockTilt.Position);
    MarkDirty;
  end;
end;

type
  TBlocksAccess=class(TBlocks);
  TPathBlockAccess=class(TPathBlock);

class procedure TBlockEditor.AddBlocks(ACombo:TComboFlat; ASkip,ASelected:TCustomBlock; const Desc:String);

  procedure AddBlocks(ABlocks:TBlocks);

    function IsParentOf(AParent,ABlock:TCustomBlock):Boolean;
    var tmp : TCustomBlock;
    begin
      result:=False;

      if AParent is TCustomObjectBlock then
      repeat
        tmp:=TBlocksAccess(ABlock.Parent).IObject;

        if Assigned(tmp) then
        begin
          if tmp=AParent then
          begin
            result:=True;
            break;
          end
          else
            ABlock:=tmp;
        end;

      until not Assigned(tmp);
    end;

  var t : Integer;
  begin
    with ABlocks do
    for t:=0 to Count-1 do
    if Block[t]<>ASkip then
    begin
      if not IsParentOf(Block[t],ASkip) then
         ACombo.Items.AddObject(Block[t].Title,Block[t]);

      {if Block[t] is TCustomObjectBlock then
         if TCustomObjectBlock(Block[t]).HasContents then
            AddBlocks(TCustomObjectBlock(Block[t]).Items);}
    end;
  end;

begin
  ACombo.Clear;
  ACombo.Items.AddObject(Desc,nil);

  if Assigned(ASkip.Parent) then
     AddBlocks(ASkip.Parent.DrawBlocks);

  ACombo.ItemIndex:=ACombo.Items.IndexOfObject(ASelected);
end;

procedure TBlockEditor.BlockTitleChange(Sender: TObject);
begin
  if Showing and (not IModifying) then
  begin
    Current.Title:=BlockTitle.Text;

    if Assigned(BlocksTreeView) then
       BlocksTreeView.Selected.Text:=Current.Title;

    MarkDirty;
  end;
end;

type
  TPointEditorAccess=class(TPointEditor);

procedure TBlockEditor.CheckPointEditor(var AEditor:TPointEditor; ATab:TTabSheet;
                           APoint:TPointXYZColor; const AFactor:Double);
begin
  if not Assigned(AEditor) then
  begin
    AEditor:=TPointEditor.Create(Self);
    AEditor.Align:=alClient;
    TPointEditorAccess(AEditor).OnDirty:=Self.OnDirty;
    AEditor.Factor:=AFactor;
    TTeeVCL.AddFormTo(AEditor,ATab);
  end;

  AEditor.SelectPoint(APoint);
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TBlockEditor.SetLinkBlockLabel;
begin
  if Assigned(Current) and (Current is TCustomObjectBlock) and
     (TCustomObjectBlock(Current).LinkBlock<>nil) then
        LLinkBlock.Caption:=TBlockAccess(TCustomObjectBlock(Current).LinkBlock).TitleOrName
  else
     LLinkBlock.Caption:='';
end;

procedure TBlockEditor.EnableShapeTabs;

  procedure AddEdgeEditor(ATab:TTabSheet; AEdge:TBlockEdge);
  var tmp : TEdgeEditor;
  begin
    tmp:=TEdgeEditor.Create(Self);
    tmp.Edge:=AEdge;
    tmp.OnDirty:=OnDirty;

    TTeeVCL.AddFormTo(tmp,ATab);
  end;

  procedure AddButtonGradient;
  var b : TButtonColor;
  begin
    b:=TButtonColor.Create(FGradientEditor);
    b.Parent:=FGradientEditor.TabColors;
    b.Left:=FGradientEditor.BEnd.Left;
    b.Top:=FGradientEditor.BEnd.BoundsRect.Bottom+8;
    b.LinkProperty(TGradientBlock(Current),'Center');
    b.Caption:='&Center...';
    b.Width:=FGradientEditor.BEnd.Width;
  end;

  procedure TryAddCustomEditor;
  var tmpSt : String;
      tmpClass : TClass;
      tmp      : TCustomFormClass;
      f        : TCustomForm;
  begin
    TabCustom.TabVisible:=False;
    RemoveControls(TabCustom);

    tmpSt:=TBlockAccess(Current).GetEditor;

    if tmpSt<>'' then
    begin
      tmpClass:=TeeGetClass(tmpSt);

      if Assigned(tmpClass) then
      begin
        tmp:=TCustomFormClass(tmpClass);

        f:=tmp.Create(Application);
        f.Align:=alClient;

        TTeeVCL.AddFormTo(TForm(f),TabCustom,Current);
        TabCustom.TabVisible:=True;
      end;
    end;
  end;

  procedure AddAnimations(AObject:TCustomObjectBlock);
  var t : Integer;
  begin
    BlockObjectAnimations.Clear;

    with AObject do
    begin
      if Items.HasAnimations then
      with Items.Animates do
      for t:=0 to Count-1 do
          BlockObjectAnimations.Items.AddObject(Item[t].Description,Item[t].Animate);

      if BlockObjectAnimations.Items.Count>0 then
      begin
        BlockObjectAnimations.ItemIndex:=0;
        BlockObjectAnimationsChange(Self);
      end;
    end;
  end;

begin
  TryAddCustomEditor;

  TabObject.TabVisible:=Current is TCustomObjectBlock;

  if TabObject.TabVisible then
  with TCustomObjectBlock(Current) do
  begin
    BlockLink.Text:=LinkFile;

    BClearLink.Enabled:=LinkFile<>'';
    BOpenExplorer.Enabled:=BClearLink.Enabled;
    BObjectEmbedd.Enabled:=BClearLink.Enabled;

    SetLinkBlockLabel;
    
    AddAnimations(TCustomObjectBlock(Current));

    LObjectBlocks.Caption:=TeeStr(Items.Count);

    // Reset tabs
    while PageObject.PageCount>3 do
    if (PageObject.Pages[0]<>TabObjectSource) and
       (PageObject.Pages[0]<>TabObjectAnimations) then
          PageObject.Pages[0].Free;

    TabObjectSource.TabVisible:=True;
    TabObjectAnimations.TabVisible:=True;
    TabObjectProperties.TabVisible:=True;

    PageObject.ActivePage:=TabObjectSource;
  end;

  TabRectangle.TabVisible:=(Current is TRectangleBlock);

  if TabRectangle.TabVisible then
  with TRectangleBlock(Current) do
  begin
    BlockRectLT.LinkProperty(Corners,'LeftTop');
    BlockRectDefLT.Checked:=Corners.LeftTop=clDefault;

    BlockRectLB.LinkProperty(Corners,'LeftBottom');
    BlockRectDefLB.Checked:=Corners.LeftBottom=clDefault;

    BlockRectRT.LinkProperty(Corners,'RightTop');
    BlockRectDefRT.Checked:=Corners.RightTop=clDefault;

    BlockRectRB.LinkProperty(Corners,'RightBottom');
    BlockRectDefRB.Checked:=Corners.RightBottom=clDefault;

    BlockRectL.LinkProperty(Sides,'Left');
    BlockRectDefL.Checked:=Sides.Left=clDefault;

    BlockRectR.LinkProperty(Sides,'Right');
    BlockRectDefR.Checked:=Sides.Right=clDefault;

    BlockRectT.LinkProperty(Sides,'Top');
    BlockRectDefT.Checked:=Sides.Top=clDefault;

    BlockRectB.LinkProperty(Sides,'Bottom');
    BlockRectDefB.Checked:=Sides.Bottom=clDefault;

    BlockRectCenter.LinkProperty(Current,'Center');
    BlockRectDefCenter.Checked:=Center=clDefault;
  end;

  TabEllipse.TabVisible:=(Current is TEllipseBlock);

  if TabEllipse.TabVisible then
  with TEllipseBlock(Current) do
  begin
    BlockEllipseSlices.Position:=Slices;
    LBlockEllipse.Caption:=TeeStr(Slices);
  end;

  TabStars.TabVisible:=(Current is TStarsBlock);

  if TabStars.TabVisible then
  with TStarsBlock(Current) do
  begin
    StarCount.Position:=ElementCount;
    LabelStarCount.Caption:=TeeStr(StarCount.Position);
    StarMapSize.Position:=Size;
  end;

  TabEllipsoid.TabVisible:=(Current is TEllipsoidBlock);

  if TabEllipsoid.TabVisible then
  with TEllipsoidBlock(Current) do
  begin
    BlockElipSides.Position:=Round(Sides);
    BlockElipStacks.Position:=Round(Stacks);

    BlockElipTotal.Position:=Round(Total);
    LEllipTotal.Caption:=FormatFloat('0.##',Total);

    BlockElipTotalAngle.Position:=Round(TotalAngle);
    LEllipAngle.Caption:=FormatFloat('0.##',TotalAngle);

    BlockElipCoverDef.Checked:=not HasCover;
    BlockElipCoverVisible.Checked:=BlockElipCoverDef.Checked or Cover.Solid;

    BlockElipEccen.Position:=Round(100*Eccentricity);
  end;

  TabGradient.TabVisible:=(Current is TGradientBlock);

  if TabGradient.TabVisible then
  begin
    if not Assigned(FGradientEditor) then
    begin
      FGradientEditor:=TTeeGradientEditor.CreateCustom(Self,nil);
      TTeeVCL.AddFormTo(FGradientEditor,TabGradient);
      FGradientEditor.CBVisible.Hide;

      AddButtonGradient;
    end;

    FGradientEditor.RefreshGradient(TGradientBlock(Current).Gradient);
  end;

  TabCone.TabVisible:=(Current is TConeBlock);

  if TabCone.TabVisible then
  begin
    BlockConeX.Position:=Round(TConeBlock(Current).ConeSize.X);
    LabelConeX.Caption:=TeeStr(BlockConeX.Position);

    BlockConeY.Position:=Round(TConeBlock(Current).ConeSize.Y);
    LabelConeY.Caption:=TeeStr(BlockConeY.Position);
  end;

  TabTube.TabVisible:=(Current is TTubeBlock);

  if TabTube.TabVisible then
  begin
    if not Assigned(ITubeEditor) then
    begin
      ITubeEditor:=TBlockFormatEditor.Create(Self);
      ITubeEditor.Align:=alClient;
      ITubeEditor.OnDirty:=OnDirty;

      TTeeVCL.AddFormTo(ITubeEditor,TabTube);
    end;

    ITubeEditor.RefreshFormat(TTubeBlock(Current).Tube);
  end;

  TabSides.TabVisible:=(Current is TCustomCoverBlock);

  if TabSides.TabVisible then
  with TCustomCoverBlock(Current) do
  begin
    BlockSide1Def.Checked:=not HasBrush1;
    BlockSide1Visible.Checked:=BlockSide1Def.Checked or Brush1.Solid;

    BlockSide2Def.Checked:=not HasBrush2;
    BlockSide2Visible.Checked:=BlockSide2Def.Checked or Brush2.Solid;
  end;

  TabCylinder.TabVisible:=(Current is TCylinderBlock);

  if TabCylinder.TabVisible then
  with TCylinderBlock(Current) do
  begin
    BlockCylinderCoverDef.Checked:=not HasBrushCover;
    BlockCylinderCoverVisible.Checked:=BlockCylinderCoverDef.Checked or
                                       BrushCover.Solid;

    BlockCylinderAngle.Position:=Angle;
    LabelCylinderAngle.Caption:=TeeStr(BlockCylinderAngle.Position);

    BlockCylinderStart.Position:=StartAngle;
    LabelCylinderStart.Caption:=TeeStr(BlockCylinderStart.Position);

    BlockCylinderSlices.Position:=Slices;
    LabelSlices.Caption:=TeeStr(Slices);

    BlockCylinderStacks.Position:=Stacks;
    LabelStacks.Caption:=TeeStr(Stacks);

    AddEdgeEditor(TabCylTopEdge,TopEdge);
    AddEdgeEditor(TabCylBottomEdge,BottomEdge);
  end;

  TabCube.TabVisible:=(Current is TCubeBlock);

  if TabCube.TabVisible then
  begin
    BlockCubeSide.ItemIndex:=0;
    BlockCubeSideClick(Self);
  end;

  TabText.TabVisible:=(Current is TeeBlocks.TTeeTextBlock);

  if TabText.TabVisible then
  with TeeBlocks.TTeeTextBlock(Current) do
  begin
    BlockTextStyle.ItemIndex:=Ord(FontStyle);
    BlockTextLines.Lines:=Lines;
    BlockTextAlign.ItemIndex:=Ord(Alignment);
    CBTextQuality.Checked:=(Font.Quality=fqBest);

    // Link

    LabelTextLink.Caption:=TActionGallery.PropertyText(LinkText);

    with LinkText do
         BClearTextLink.Enabled:=Assigned(Instance) or (PropertyName<>'');

    BlockTextLinkFormat.Text:=LinkFormat;

    BlockFontColor.LinkProperty(Font,'Color');

    if BlockFontName.Items.Count=0 then
    begin
      BlockFontName.Items:=Screen.Fonts;

      if Assigned(TeeAddFontNames) then  // used by TeeNumericGauge unit
         TeeAddFontNames(BlockFontName.Items);
    end;

    // Font

    with Font do
    begin
      BlockFontSize.Position:=Size;

      BlockFontItalic.Checked:=fsItalic in Style;
      BlockFontBold.Checked:=fsBold in Style;
      BlockFontStrike.Checked:=fsStrikeOut in Style;
      BlockFontUnder.Checked:=fsUnderline in Style;

      BlockFontName.ItemIndex:=BlockFontName.Items.IndexOf(Name);
    end;
  end;

  TabBevel.TabVisible:=(Current is TBeveledCubeBlock);

  if TabBevel.TabVisible then
  with TBeveledCubeBlock(Current),BevelSize do
  begin
    BlockBevelX.Position:=Round(X);
    LabelBevelX.Caption:=TeeStr(BlockBevelX.Position);

    BlockBevelY.Position:=Round(Y);
    LabelBevelY.Caption:=TeeStr(BlockBevelY.Position);

    BlockBevelZ.Position:=Round(Z);
    LabelBevelZ.Caption:=TeeStr(BlockBevelZ.Position);

    BlockBevelCurvePoints.Position:=CurvePoints;
    LBevelCurvePoints.Caption:=TeeStr(CurvePoints);

    BlockBevelRounded.Checked:=CurveRound;
    BlockBevelStyle.ItemIndex:=Ord(Style);

    BlockBevelCurvePoints.Enabled:=(Style=bsRound);
    BlockBevelRounded.Enabled:=(Style=bsRound);
  end;

  TabRoundRect.TabVisible:=(Current is TRoundRectBlock);

  if TabRoundRect.TabVisible then
  begin
    if not Assigned(IRoundEditor) then
    begin
      IRoundEditor:=TRoundRectEditor.Create(Self);
      IRoundEditor.Corners:=TRoundRectBlock(Current).Corners;
      IRoundEditor.Align:=alClient;
      IRoundEditor.OnDirty:=OnDirty;

      TTeeVCL.AddFormTo(IRoundEditor,TabRoundRect);
    end;

    IRoundEditor.Corners:=TRoundRectBlock(Current).Corners;
  end;

  TabSphere.TabVisible:=(Current is TSphereBlock);

  if TabSphere.TabVisible then
  with TSphereBlock(Current) do
  begin
    BlockSphereRadius.Position:=Round(Radius);
    LSphereRadius.Caption:=TeeStr(BlockSphereRadius.Position);
  end;

  TabLight.TabVisible:=(Current is TLightBlock);

  if TabLight.TabVisible then
  with TLightBlock(Current) do
  begin
    BlockLightColor.LinkProperty(Current.Format,'Color');
    BlockLightColorClick(Self);

    CBDefaultSpot.Checked:=Spot=180;

    if Spot=180 then
       BlockLightSpot.Position:=90
    else
       BlockLightSpot.Position:=Spot;

    BlockLightSpotExp.Position:=SpotExponent;

    BlockLightFixed.Checked:=Fixed;
    BlockLightUseDir.Checked:=UseDirection;

    BlockLightDiffuse.Position:=GetRValue(Diffuse);
    BlockLightSpecular.Position:=GetRValue(Specular);

    BlockLightLamp.Checked:=ShowLamp;
  end
  else
    BlockLightColor.LinkProperty(nil,'');

  TabRectPyramid.TabVisible:=(Current is TRectPyramidBlock);

  if TabRectPyramid.TabVisible then
  with TRectPyramidBlock(Current) do
  begin
    BlockRectLeft.Position:=Round(LeftPercent);
    BlockRectRight.Position:=Round(RightPercent);

    LRectLeft.Caption:=TeeStr(BlockRectLeft.Position);
    LRectRight.Caption:=TeeStr(BlockRectRight.Position);
  end;

  TabPyramid.TabVisible:=(Current is TPyramidBlock);

  if TabPyramid.TabVisible then
  with TPyramidBlock(Current) do
  begin
    BlockWedgeX1.Position:=Round(Side1.X);
    BlockWedgeY1.Position:=Round(Side1.Y);

    BlockWedgeX2.Position:=Round(Side2.X);
    BlockWedgeY2.Position:=Round(Side2.Y);

    LPyramid1X.Caption:=TeeStr(BlockWedgeX1.Position)+' %';
    LPyramid1Y.Caption:=TeeStr(BlockWedgeY1.Position)+' %';

    LPyramid2X.Caption:=TeeStr(BlockWedgeX2.Position)+' %';
    LPyramid2Y.Caption:=TeeStr(BlockWedgeY2.Position)+' %';
  end;

  TabBridge.TabVisible:=(Current is TBridgeBlock);

  if TabBridge.TabVisible then
  with TBridgeBlock(Current) do
  begin
    BlockBridgeHeight.Position:=Round(ColumnSize.Y);
    BlockBridgeSize.Position:=Round(ColumnSize.X);
    BlockBridgeRounded.Checked:=Rounded;
    UDBlockBridgeRound.Position:=RoundSlices;
  end;

  TabPie.TabVisible:=(Current is TPieSliceBlock);

  if TabPie.TabVisible then
  with TPieSliceBlock(Current) do
  begin
    BlockPieDonut.Position:=Round(DonutPercent);
    BlockPieAngle.Position:=Round(Angle);
    BlockPieStart.Position:=Round(StartAngle);

    LPieDonut.Caption:=FormatFloat('#.##',DonutPercent);
    LPieStart.Caption:=FormatFloat('#.##',StartAngle);
    LPieAngle.Caption:=FormatFloat('#.##',Angle);

    AddEdgeEditor(TabPieOuterTop,Edges.OuterTop);
    AddEdgeEditor(TabPieOuterBottom,Edges.OuterBottom);

    AddEdgeEditor(TabPieInnerTop,Edges.InnerTop);
    AddEdgeEditor(TabPieInnerBottom,Edges.InnerBottom);

    BlockPieStacks.Position:=Round(Stacks*10);
    LPieStacks.Caption:=FormatFloat('0.##',Stacks);

    BlockPieInnerTop.Position:=Round(InnerSize.X);
    BlockPieInnerBottom.Position:=Round(InnerSize.Y);
  end;

  TabHole.TabVisible:=(Current is THoleBlock);

  if TabHole.TabVisible then
  with THoleBlock(Current) do
  begin
    BlockHoleX.Position:=Round(Hole.Size.X);
    LHoleX.Caption:=TeeStr(BlockHoleX.Position);

    BlockHoleY.Position:=Round(Hole.Size.Y);
    LHoleY.Caption:=TeeStr(BlockHoleY.Position);

    BlockHoleStyle.ItemIndex:=Ord(Hole.Style);

    BlockHoleCenterX.Position:=Round(Hole.Center.X);
    BlockHoleCenterY.Position:=Round(Hole.Center.Y);

    if not Assigned(IHoleRoundEditor) then
    begin
      IHoleRoundEditor:=TRoundRectEditor.Create(Self);
      IHoleRoundEditor.Corners:=THoleBlock(Current).Hole.Corners;
      IHoleRoundEditor.OnDirty:=OnDirty;

      TTeeVCL.AddFormTo(IHoleRoundEditor,TabHoleCorners);
    end;

    if not Assigned(IHoleFormat) then
    begin
      IHoleFormat:=TBlockFormatEditor.Create(Self);
      IHoleFormat.OnDirty:=OnDirty;
      IHoleFormat.Align:=alClient;

      TTeeVCL.AddFormTo(IHoleFormat,TabHoleFormat);
    end;

    IHoleFormat.RefreshFormat(Hole.Format);
  end;

  TabArrow.TabVisible:=(Current is TArrowBlock);

  if TabArrow.TabVisible then
  with TArrowBlock(Current) do
  begin
    BlockArrowWidth.Position:=Round(Head.X);
    BlockArrowHeight.Position:=Round(Head.Y);
    BlockArrowIndent.Position:=Round(Indent);

    LArrowHeadW.Caption:=IntToStr(BlockArrowWidth.Position);
    LArrowHeadH.Caption:=IntToStr(BlockArrowHeight.Position);
    LArrowIndent.Caption:=IntToStr(BlockArrowIndent.Position);
  end;

  TabCross.TabVisible:=(Current is TCrossBlock);

  if TabCross.TabVisible then
  with TCrossBlock(Current) do
  begin
    BlockCrossWidth.Position:=Round(CrossSize.X);
    BlockCrossHeight.Position:=Round(CrossSize.Y);

    BlockCrossCenterX.Position:=Round(CrossCenter.X);
    BlockCrossCenterY.Position:=Round(CrossCenter.Y);
  end;

  TabTriangle.TabVisible:=(Current is TTriangleBlock);

  if TabTriangle.TabVisible then
  with TTriangleBlock(Current) do
  begin
    CheckPointEditor(IPoint0,TabPoint0,Point0,0.001);
    CheckPointEditor(IPoint1,TabPoint1,Point1,0.001);
    CheckPointEditor(IPoint2,TabPoint2,Point2,0.001);
  end;

  TabPath.TabVisible:=(Current is TPathBlock);

  if TabPath.TabVisible then
  with TPathBlock(Current) do
  begin
    CheckPathPoints(Points.Count);
    AddBlocks(BlockPathPointer,Current,Pointer,'(none)');

    CheckPointEditor(IPointEditor,TabPoints,nil,1);

    with IPointEditor do
    begin
      LabelPathY.Visible:=TPathBlockAccess(Self.Current).HasYValues;
      BlockPathY.Visible:=LabelPathY.Visible;
      PathY.Visible:=LabelPathY.Visible;
    end;

    BlockPathColorEach.Checked:=ColorEach;
  end;

  TabExtruded.TabVisible:=(Current is TExtrudedBlock);

  if TabExtruded.TabVisible then
  with TExtrudedBlock(Current) do
  begin
    CBBlockExtFront.Checked:=not HasFront;
    BlockExtrudedFront.Enabled:=HasFront;

    CBBlockExtBack.Checked:=not HasBack;
    BlockExtrudedBack.Enabled:=HasBack;
  end;

  TabPipe.TabVisible:=(Current is TPipeBlock);

  if TabPipe.TabVisible then
  with TPipeBlock(Current) do
  begin
    BlockPipeXRadius.Position:=Round(Radius.X);
    PipeXRadius.Caption:=TeeStr(BlockPipeXRadius.Position);

    BlockPipeYRadius.Position:=Round(Radius.Y);
    PipeYRadius.Caption:=TeeStr(BlockPipeYRadius.Position);

    BlockPipeVisible.Checked:=ConnectorVisible;

    AddBlocks(BlockPipeConnector,Current,Connector,'(default)');

    with BlockPipeConnector do
         ItemIndex:=Items.IndexOfObject(Connector);
  end;

  TabStar.TabVisible:=(Current is TStarBlock);

  if TabStar.TabVisible then
  with TStarBlock(Current) do
  begin
    BlockStarSlant.Position:=Round(SlantAngle);
    LStarSlant.Caption:=FormatFloat('#.###',SlantAngle);

    BlockStarInner.Position:=Round(InnerSize*1000);
    LStarInner.Caption:=FormatFloat('#.###',InnerSize);
  end;
end;

procedure TBlockEditor.CBSpinByClick(Sender: TObject);
var tmp : Integer;
begin
  if CBSpinBy.Checked then
     tmp:=UDSpinBy.Position
  else
     tmp:=1;

  BlockX.Increment:=tmp;
  BlockY.Increment:=tmp;
  BlockZ.Increment:=tmp;
  BlockWidth.Increment:=tmp;
  BlockHeight.Increment:=tmp;
  BlockDepth.Increment:=tmp;
end;

procedure TBlockEditor.BlockTileXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.X:=BlockTileX.Position;
    LTileX.Caption:=TeeStr(BlockTileX.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTileYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.Y:=BlockTileY.Position;
    LTileY.Caption:=TeeStr(BlockTileY.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BSelectLinkClick(Sender: TObject);
var tmp : String;
begin
  tmp:=BlockLink.Text;

  if TLoadBlockDialog.ModalShow(Self,'*'+TeeMakerExtension,
                                TeeMakerExtension,tmp) then
     BlockLink.Text:=tmp;
end;

procedure TBlockEditor.BTextLinkClick(Sender: TObject);

  procedure CheckLinkFormat;
  var t        : Integer;
      tmpFound : Boolean;
  begin
    tmpFound:=False;

    with TeeBlocks.TTeeTextBlock(Current).Lines do
    begin
      for t:=0 to Count-1 do
      begin
        tmpFound:=Pos('%S',UpperCase(Strings[t]))>0;

        if tmpFound then
           break;
      end;

      if not tmpFound then
         Add('%s');
    end;
  end;

begin
  if TMakerPropertySelector.ModalShow(Self,CurrentMaker.Blocks,TeeBlocks.TTeeTextBlock(Current).LinkText) then
  begin
    BClearTextLink.Enabled:=True;

    LabelTextLink.Caption:=TActionGallery.PropertyText(TeeBlocks.TTeeTextBlock(Current).LinkText);
    CheckLinkFormat;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BClearLinkClick(Sender: TObject);
begin
  TCustomObjectBlock(Current).LinkFile:='';
  TCustomObjectBlock(Current).LinkBlock:=nil;

  BlockLink.Text:='';
  BOpenExplorer.Enabled:=False;
  BObjectEmbedd.Enabled:=False;
  LLinkBlock.Caption:='';
  
  MarkDirty;
end;

procedure TBlockEditor.UDSpinByClick(Sender: TObject; Button: TUDBtnType);
begin
  CBSpinByClick(Self);
end;

procedure TBlockEditor.BlockTileZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.Z:=BlockTileZ.Position;
    LTileZ.Caption:=TeeStr(BlockTileZ.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockVisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Visible:=BlockVisible.Checked;
    MarkDirty;
  end;
end;

procedure TBlockEditor.CheckBlockNoScaling;
begin
  if TBlockAccess(Current).HasScale then
  with Current.Scale do
    BlockNoScaling.Enabled:=(X<>1) or (Y<>1) or (Z<>1)
  else
    BlockNoScaling.Enabled:=True;
end;

procedure TBlockEditor.RefreshBlock(const ABlock:TVisualBlock);

  function Needs10(const P:TPoint3DFloat):Boolean;
  begin
    with P do
         result:=(Round(X)<>X) or (Round(Y)<>Y) or (Round(Z)<>Z);
  end;

begin
  IModifying:=True;

  Current:=ABlock as TCustomBlock;

  with Current do
  begin
    if not CBPosition10.Checked then
    begin
      CBPosition10.Checked:=Needs10(Location.Point) or Needs10(Size.Point);
      CBPosition10.Enabled:=not CBPosition10.Checked;
    end;

    RefreshLocation;
    RefreshSize;

    EnableControls(not (Current is TCustomObjectBlock),
                   [BlockWidth,BlockHeight,BlockDepth,EditWidth,EditHeight,EditDepth]);

    // General
    BlockTitle.Text:=Title;
    BlockVisible.Checked:=Visible;
    BlockName.Text:=Name;
    TeeFillCursors(CBCursor,Cursor);

    // Format
    if not Assigned(IFormatEditor) then
    begin
      IFormatEditor:=TBlockFormatEditor.Create(Self);
      IFormatEditor.OnDirty:=OnDirty;
      IFormatEditor.Align:=alClient;

      TTeeVCL.AddFormTo(IFormatEditor,TabFormat);
    end;

    IFormatEditor.RefreshFormat(Format);

    RefreshRotation;

    // Tile
    if TBlockAccess(Current).HasTile then
    begin
      BlockTileX.Position:=Round(Tile.X);
      BlockTileY.Position:=Round(Tile.Y);
      BlockTileZ.Position:=Round(Tile.Z);

      LTileX.Caption:=TeeStr(BlockTileX.Position);
      LTileY.Caption:=TeeStr(BlockTileY.Position);
      LTileZ.Caption:=TeeStr(BlockTileZ.Position);

      BlockTileOffX.Position:=Round(Tile.Offset.X);
      BlockTileOffY.Position:=Round(Tile.Offset.Y);
      BlockTileOffZ.Position:=Round(Tile.Offset.Z);
    end
    else
    begin
      BlockTileX.Position:=1;
      BlockTileY.Position:=1;
      BlockTileZ.Position:=1;

      LTileX.Caption:='1';
      LTileY.Caption:='1';
      LTileZ.Caption:='1';

      BlockTileOffX.Position:=0;
      BlockTileOffY.Position:=0;
      BlockTileOffZ.Position:=0;
    end;

    // Scale
    if TBlockAccess(Current).HasScale then
    begin
      BlockScaleX.Position:=TBlockFormatEditor.FromScaleValue(Scale.X);
      BlockScaleY.Position:=TBlockFormatEditor.FromScaleValue(Scale.Y);
      BlockScaleZ.Position:=TBlockFormatEditor.FromScaleValue(Scale.Z);

      with Scale do
      begin
        LScaleX.Caption:=FormatFloat('0.000',X);
        LScaleY.Caption:=FormatFloat('0.000',Y);
        LScaleZ.Caption:=FormatFloat('0.000',Z);
      end;

      CheckBlockNoScaling;
    end
    else
    begin
      BlockScaleX.Position:=TBlockFormatEditor.FromScaleValue(1);
      BlockScaleY.Position:=TBlockFormatEditor.FromScaleValue(1);
      BlockScaleZ.Position:=TBlockFormatEditor.FromScaleValue(1);

      LScaleX.Caption:='1';
      LScaleY.Caption:='1';
      LScaleZ.Caption:='1';
    end;

    // Actions
    TreeEvents.Items.Clear;
    ListActions.Clear;
    PageCurrentChange(Self);

    EnableShapeTabs;
  end;

  IModifying:=False;
end;

procedure TBlockEditor.RefreshRotation;

  function Adjust360(const Value:Integer):Integer;
  begin
    result:=Value mod 360;

    while result<0 do
          result:=360+result;
  end;

begin
  if TBlockAccess(Current).HasRotation then
  with Current.Rotation do
  begin
    BlockRotation.Position:=Adjust360(Round(X));
    BlockElevation.Position:=Adjust360(Round(Y));
    BlockTilt.Position:=Adjust360(Round(Z));

    // Rotation Center
    BlockCenterX.Position:=Round(Center.X);
    BlockCenterY.Position:=Round(Center.Y);
    BlockCenterZ.Position:=Round(Center.Z);

    CBFaceToViewer.Checked:=FaceToViewer;
  end
  else
  begin
    BlockRotation.Position:=0;
    BlockElevation.Position:=0;
    BlockTilt.Position:=0;

    BlockCenterX.Position:=0;
    BlockCenterY.Position:=0;
    BlockCenterZ.Position:=0;
  end;

  LRotation.Caption:=TeeStr(BlockRotation.Position);
  LElevation.Caption:=TeeStr(BlockElevation.Position);
  LTilt.Caption:=TeeStr(BlockTilt.Position);

  LCenterX.Caption:=TeeStr(BlockCenterX.Position);
  LCenterY.Caption:=TeeStr(BlockCenterY.Position);
  LCenterZ.Caption:=TeeStr(BlockCenterZ.Position);
end;

procedure TBlockEditor.RefreshLocation;
begin
  with Current.Location do
  begin
    BlockX.Position:=Round(X*Position10);
    BlockY.Position:=Round(Y*Position10);
    BlockZ.Position:=Round(Z*Position10);
  end;
end;

procedure TBlockEditor.RefreshSize;
begin
  with Current.Size do
  begin
    BlockWidth.Position:=Round(X*Position10);
    BlockHeight.Position:=Round(Y*Position10);
    BlockDepth.Position:=Round(Z*Position10);
  end;
end;

procedure TBlockEditor.BlockLinkChange(Sender: TObject);
begin
  if not IModifying then
  begin
    BLoad.Enabled:=BlockLink.Text<>'';
    BOpenExplorer.Enabled:=BLoad.Enabled;
    BObjectEmbedd.Enabled:=BLoad.Enabled;
  end;
end;

procedure TBlockEditor.BLoadClick(Sender: TObject);
begin
  TCustomObjectBlock(Current).LinkFile:=BlockLink.Text;
  TCustomObjectBlock(Current).Items;  // Force load

  BClearLink.Enabled:=TCustomObjectBlock(Current).LinkFile<>'';
  BLoad.Enabled:=False;

  SetLinkBlockLabel;
  
  MarkDirty;
end;

procedure TBlockEditor.BlockScaleXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Scale.X:=TBlockFormatEditor.ToScaleValue(BlockScaleX.Position);
    LScaleX.Caption:=FormatFloat('0.000',Current.Scale.X);

    CheckBlockNoScaling;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockScaleYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Scale.Y:=TBlockFormatEditor.ToScaleValue(BlockScaleY.Position);
    LScaleY.Caption:=FormatFloat('0.000',Current.Scale.Y);

    CheckBlockNoScaling;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockScaleZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Scale.Z:=TBlockFormatEditor.ToScaleValue(BlockScaleZ.Position);
    LScaleZ.Caption:=FormatFloat('0.000',Current.Scale.Z);

    CheckBlockNoScaling;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTileOffXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.Offset.X:=BlockTileOffX.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTileOffYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.Offset.Y:=BlockTileOffY.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTileOffZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Tile.Offset.Z:=BlockTileOffZ.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.ElipSidesChange(Sender: TObject);
begin
  if Showing then
  if not IModifying then
  begin
    TEllipsoidBlock(Current).Sides:=BlockElipSides.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.ElipStacksChange(Sender: TObject);
begin
  if Showing then
  if not IModifying then
  begin
    TEllipsoidBlock(Current).Stacks:=BlockElipStacks.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockElipTotalChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TEllipsoidBlock(Current).Total:=BlockElipTotal.Position;
    LEllipTotal.Caption:=FormatFloat('0.##',TEllipsoidBlock(Current).Total);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockConeXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TConeBlock(Current).ConeSize.X:=BlockConeX.Position;
    LabelConeX.Caption:=TeeStr(BlockConeX.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide1Click(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TCustomCoverBlock(Current).Brush1) then
  begin
    BlockSide1Def.Checked:=False;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide2Click(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TCustomCoverBlock(Current).Brush2) then
  begin
    BlockSide2Def.Checked:=False;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide1DefClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if BlockSide1Def.Checked then
       TCustomCoverBlock(Current).Brush1:=nil
    else
    begin
      TCustomCoverBlock(Current).Brush1; // force "get"
      Current.Repaint;
    end;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide2DefClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if BlockSide2Def.Checked then
       TCustomCoverBlock(Current).Brush2:=nil
    else
    begin
      TCustomCoverBlock(Current).Brush2; // force "get"
      Current.Repaint;
    end;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide1VisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TCustomCoverBlock(Current).Brush1.Solid:=BlockSide1Visible.Checked;

    BlockSide1Def.Checked:=False;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSide2VisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TCustomCoverBlock(Current).Brush2.Solid:=BlockSide2Visible.Checked;

    BlockSide2Def.Checked:=False;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BDeleteActionClick(Sender: TObject);
var tmpIndex : Integer;
begin
  if TeeYesNo(TeeMsg_SureToDeleteAction) then
  begin
    CurrentActions.Actions.Delete(ListActions.ItemIndex);

    tmpIndex:=ListActions.ItemIndex;
    ListActions.Items.Delete(tmpIndex);

    if tmpIndex<ListActions.Items.Count then
       ListActions.ItemIndex:=tmpIndex
    else
    if ListActions.Items.Count>0 then
       ListActions.ItemIndex:=tmpIndex-1;

    ListActionsClick(Self);

    Self.TreeEvents.Invalidate;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BEditLinkClick(Sender: TObject);
begin
  with Current.Editor(Self) as TCustomForm do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBlockEditor.BlockElipCoverClick(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TEllipsoidBlock(Current).Cover) then
  begin
    BlockElipCoverDef.Checked:=False;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockElipCoverDefClick(Sender: TObject);
begin
  if BlockElipCoverDef.Checked then
     TEllipsoidBlock(Current).Cover:=nil
  else
  begin
    TEllipsoidBlock(Current).Cover; // force "get"
    Current.Repaint;
  end;

  MarkDirty;
end;

procedure TBlockEditor.BlockElipCoverVisibleClick(Sender: TObject);
begin
  TEllipsoidBlock(Current).Cover.Solid:=BlockElipCoverVisible.Checked;

  BlockElipCoverDef.Checked:=False;

  MarkDirty;
end;

procedure TBlockEditor.FormCreate(Sender: TObject);
begin
  TTeeVCL.LoadArrowBitmaps(SBActionUp.Glyph,SBActionDown.Glyph);

  TreeEvents.OnCustomDrawItem:=TreeEventsCustomDrawItem;

  PageControl1.ActivePage:=TabGeneral;
  PageCurrent.ActivePage:=TabBlock;
end;

procedure TBlockEditor.BlockBevelXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBeveledCubeBlock(Current).BevelSize.X:=BlockBevelX.Position;
    LabelBevelX.Caption:=TeeStr(BlockBevelX.Position);
    MarkDirty;

    if BlockBevelAll.Checked then
    begin
      BlockBevelY.Position:=BlockBevelX.Position;
      BlockBevelZ.Position:=BlockBevelX.Position;
    end;
  end;
end;

procedure TBlockEditor.BlockElipTotalAngleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TEllipsoidBlock(Current).TotalAngle:=BlockElipTotalAngle.Position;
    LEllipAngle.Caption:=FormatFloat('0.##',TEllipsoidBlock(Current).TotalAngle);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCylinderAngleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCylinderBlock(Current).Angle:=BlockCylinderAngle.Position;
    LabelCylinderAngle.Caption:=TeeStr(BlockCylinderAngle.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSphereSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCylinderBlock(Current).Slices:=(Sender as TScrollBar).Position;
    LabelSlices.Caption:=TeeStr((Sender as TScrollBar).Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSphereStacksChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCylinderBlock(Current).Stacks:=(Sender as TScrollBar).Position;
    LabelStacks.Caption:=TeeStr((Sender as TScrollBar).Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockLightColorClick(Sender: TObject);
var h,l,s : Word;
begin
  ColorToHLS(Current.Format.Color,h,l,s);
  BlockLightInt.Position:=l;
end;

procedure TBlockEditor.ESpotChange(Sender: TObject);
begin
  if not IModifying then
  if not CBDefaultSpot.Checked then
  begin
    TLightBlock(Current).Spot:=BlockLightSpot.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.CBDefaultSpotClick(Sender: TObject);
begin
  EnableControls(not CBDefaultSpot.Checked,[ESpot,BlockLightSpot]);

  if CBDefaultSpot.Checked then
     TLightBlock(Current).Spot:=180
  else
     ESpotChange(Self);

  MarkDirty;
end;

procedure TBlockEditor.BlockLightFixedClick(Sender: TObject);
begin
  TLightBlock(Current).Fixed:=BlockLightFixed.Checked;
  MarkDirty;
end;

procedure TBlockEditor.BlockLightIntChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with BlockLightInt do
         Current.Format.Color:=RGB(Position,Position,Position);

    BlockLightColor.Repaint;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockColorClick(Sender: TObject);
begin
  MarkDirty;
end;

procedure TBlockEditor.BlockWedgeX1Change(Sender: TObject);
begin
  if not IModifying then
  begin
    TPyramidBlock(Current).Side1.X:=BlockWedgeX1.Position;
    LPyramid1X.Caption:=TeeStr(BlockWedgeX1.Position)+' %';
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockWedgeY1Change(Sender: TObject);
begin
  if not IModifying then
  begin
    TPyramidBlock(Current).Side1.Y:=BlockWedgeY1.Position;
    LPyramid1Y.Caption:=TeeStr(BlockWedgeY1.Position)+' %';
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBridgeSizeChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBridgeBlock(Current).ColumnSize.X:=BlockBridgeSize.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBridgeHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBridgeBlock(Current).ColumnSize.Y:=BlockBridgeHeight.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockLightLampClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TLightBlock(Current).ShowLamp:=BlockLightLamp.Checked;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieDonutChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).DonutPercent:=BlockPieDonut.Position;
    LPieDonut.Caption:=FormatFloat('#.##',TPieSliceBlock(Current).DonutPercent);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieStartChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).StartAngle:=BlockPieStart.Position;
    LPieStart.Caption:=FormatFloat('#.##',TPieSliceBlock(Current).StartAngle);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieAngleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).Angle:=BlockPieAngle.Position;
    LPieAngle.Caption:=FormatFloat('#.##',TPieSliceBlock(Current).Angle);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockHoleXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    THoleBlock(Current).Hole.Size.X:=BlockHoleX.Position;
    LHoleX.Caption:=TeeStr(BlockHoleX.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockHoleYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    THoleBlock(Current).Hole.Size.Y:=BlockHoleY.Position;
    LHoleY.Caption:=TeeStr(BlockHoleY.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockHoleStyleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    THoleBlock(Current).Hole.Style:=THoleStyle(BlockHoleStyle.ItemIndex);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCenterXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.Center.X:=BlockCenterX.Position;
    LCenterX.Caption:=TeeStr(BlockCenterX.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCenterYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.Center.Y:=BlockCenterY.Position;
    LCenterY.Caption:=TeeStr(BlockCenterY.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCenterZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Current.Rotation.Center.Z:=BlockCenterZ.Position;
    LCenterZ.Caption:=TeeStr(BlockCenterZ.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBorderClick(Sender: TObject);
begin
  MarkDirty;
end;

procedure TBlockEditor.BlockLightUseDirClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TLightBlock(Current).UseDirection:=BlockLightUseDir.Checked;

    if BlockLightUseDir.Checked then
    begin
      CBDefaultSpot.Checked:=False;
      CBDefaultSpotClick(Self);
    end;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockLightDiffuseChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with BlockLightDiffuse do
         TLightBlock(Current).Diffuse:=RGB(Position,Position,Position);

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockLightSpecularChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with BlockLightSpecular do
         TLightBlock(Current).Specular:=RGB(Position,Position,Position);

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockLightSpotExpChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TLightBlock(Current).SpotExponent:=BlockLightSpotExp.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockArrowWidthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TArrowBlock(Current).Head.X:=BlockArrowWidth.Position;
    LArrowHeadW.Caption:=IntToStr(BlockArrowWidth.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockObjectAnimationsChange(Sender: TObject);
begin
  BlockObjectPlay.Enabled:=True;
end;

procedure TBlockEditor.BlockObjectPlayClick(Sender: TObject);
var tmp : TTeeAnimate;
begin
  tmp:=TTeeAnimate(BlockObjectAnimations.Items.Objects[BlockObjectAnimations.ItemIndex]);
  tmp.Play;
end;

procedure TBlockEditor.BlockPipeXRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPipeBlock(Current).Radius.X:=BlockPipeXRadius.Position;
    PipeXRadius.Caption:=TeeStr(BlockPipeXRadius.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPathPointerChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPathBlock(Current).Pointer:=TCustomBlock(BlockPathPointer.Items.Objects[BlockPathPointer.ItemIndex]);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPathPointsChange(Sender: TObject);
var tmpOk : Boolean;
begin
  if (not IModifying) and Assigned(Current) then
  begin
    tmpOk:=TPathBlock(Current).Points.Count>UDPathPoints.Position;

    CheckPointEditor(IPointEditor,TabPoints,nil,1);

    with TPathBlock(Current) do
    if tmpOk then
    begin
      Selected:=UDPathPoints.Position;
      IPointEditor.SelectPoint(Points[Selected].Point);
    end
    else
    begin
      Selected:=-1;
      IPointEditor.SelectPoint(nil);
    end;

    EnableControls(tmpOk,[BlockPathPoints,UDPathPoints,SBPathRemove]);

    Current.Repaint;
  end;
end;

procedure TBlockEditor.SBPathAddClick(Sender: TObject);
begin
  UDPathPoints.Max:=TPathBlock(Current).Points.Count;
  TPathBlock(Current).Points.Add(0,0,0);
  UDPathPoints.Position:=UDPathPoints.Max;
  BlockPathPointsChange(Self);
  MarkDirty;
end;

procedure TBlockEditor.SBPathRemoveClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=UDPathPoints.Position;
  TPathBlock(Current).Points.Delete(tmp);

  with UDPathPoints do
  begin
    if Max>0 then
    begin
      Max:=Max-1;

      if tmp>=Max then
         Position:=Max
      else
         Position:=tmp;
    end
    else
       Enabled:=False;
  end;

  BlockPathPointsChange(Self);
  MarkDirty;
end;

procedure TBlockEditor.BlockWedgeX2Change(Sender: TObject);
begin
  if not IModifying then
  begin
    TPyramidBlock(Current).Side2.X:=BlockWedgeX2.Position;
    LPyramid2X.Caption:=TeeStr(BlockWedgeX2.Position)+' %';
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockWedgeY2Change(Sender: TObject);
begin
  if not IModifying then
  begin
    TPyramidBlock(Current).Side2.Y:=BlockWedgeY2.Position;
    LPyramid2Y.Caption:=TeeStr(BlockWedgeY2.Position)+' %';
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBridgeRoundedClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TBridgeBlock(Current).Rounded:=BlockBridgeRounded.Checked;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockArrowHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TArrowBlock(Current).Head.Y:=BlockArrowHeight.Position;
    LArrowHeadH.Caption:=IntToStr(BlockArrowHeight.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCylinderCoverClick(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TCylinderBlock(Current).BrushCover) then
  begin
    BlockCylinderCoverDef.Checked:=False;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCylinderCoverDefClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if BlockCylinderCoverDef.Checked then
       TCylinderBlock(Current).BrushCover:=nil
    else
    begin
      TCylinderBlock(Current).BrushCover; // force "get"
      Current.Repaint;
    end;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCylinderCoverVisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TCylinderBlock(Current).BrushCover.Solid:=BlockCylinderCoverVisible.Checked;

    BlockCylinderCoverDef.Checked:=False;

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCylinderStartChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCylinderBlock(Current).StartAngle:=BlockCylinderStart.Position;
    LabelCylinderStart.Caption:=TeeStr(BlockCylinderStart.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCrossWidthChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCrossBlock(Current).CrossSize.X:=BlockCrossWidth.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCrossHeightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCrossBlock(Current).CrossSize.Y:=BlockCrossHeight.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCrossCenterXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCrossBlock(Current).CrossCenter.X:=BlockCrossCenterX.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockCrossCenterYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TCrossBlock(Current).CrossCenter.Y:=BlockCrossCenterY.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockConeYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TConeBlock(Current).ConeSize.Y:=BlockConeY.Position;
    LabelConeY.Caption:=TeeStr(BlockConeY.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTextStyleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).FontStyle:=TTeeFontStyle(BlockTextStyle.ItemIndex);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockArrowIndentChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TArrowBlock(Current).Indent:=BlockArrowIndent.Position;
    LArrowIndent.Caption:=IntToStr(BlockArrowIndent.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockSphereRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TSphereBlock(Current).Radius:=BlockSphereRadius.Position;
    LSphereRadius.Caption:=TeeStr(BlockSphereRadius.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.CBBlockExtFrontClick(Sender: TObject);
begin
  BlockExtrudedFront.Enabled:=not CBBlockExtFront.Checked;

  if CBBlockExtFront.Checked then
     TExtrudedBlock(Current).Front:=nil;
end;

procedure TBlockEditor.CBBlockExtBackClick(Sender: TObject);
begin
  BlockExtrudedBack.Enabled:=not CBBlockExtBack.Checked;

  if CBBlockExtBack.Checked then
     TExtrudedBlock(Current).Back:=nil;
end;

procedure TBlockEditor.BlockExtrudedFrontClick(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TExtrudedBlock(Current).Front) then
     MarkDirty;
end;

procedure TBlockEditor.BlockExtrudedBackClick(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,TExtrudedBlock(Current).Back) then
     MarkDirty;
end;

procedure TBlockEditor.Button2Click(Sender: TObject);
begin
  with Current.Editor(Self) as TCustomForm do
  try
    if ShowModal=mrOk then
       MarkDirty;
  finally
    Free;
  end;
end;

procedure TBlockEditor.BlockRectDefLTClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Corners do
    if BlockRectDefLT.Checked then
       LeftTop:=clDefault
    else
    if LeftTop=clDefault then
       LeftTop:=Current.Format.Color;

    BlockRectLT.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefLBClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Corners do
    if BlockRectDefLB.Checked then
       LeftBottom:=clDefault
    else
    if LeftBottom=clDefault then
       LeftBottom:=Current.Format.Color;

    BlockRectLB.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefRTClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Corners do
    if BlockRectDefRT.Checked then
       RightTop:=clDefault
    else
    if RightTop=clDefault then
       RightTop:=Current.Format.Color;

    BlockRectRT.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefRBClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Corners do
    if BlockRectDefRB.Checked then
       RightBottom:=clDefault
    else
    if RightBottom=clDefault then
       RightBottom:=Current.Format.Color;

    BlockRectRB.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectLTClick(Sender: TObject);
begin
  BlockRectDefLT.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectLBClick(Sender: TObject);
begin
  BlockRectDefLB.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectRTClick(Sender: TObject);
begin
  BlockRectDefRT.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectRBClick(Sender: TObject);
begin
  BlockRectDefRB.Checked:=False;
  MarkDirty;
end;

function TBlockEditor.CurrentMaker:TMaker;
begin
  result:=Current.Parent.DrawBlocks.Parent as TMaker;
end;

class function TBlockEditor.ModalShow(AOwner: TComponent;
  ABlock: TCustomBlock): Boolean;
begin
  with TBlockEditor.Create(AOwner) do
  try
    //IBlocks:=Self.Maker1.Blocks;
    RefreshBlock(ABlock);

    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

type
  TCubeSidesAccess=class(TCubeSides);

function TBlockEditor.CurrentCubeSide:TBlockFormat;
begin
  result:=TCubeBlock(Current).Sides.SideOf(BlockCubeSide.ItemIndex);
end;

procedure TBlockEditor.BlockCubeSideClick(Sender: TObject);
begin
  BlockCubeSideDefault.Checked:=(CurrentCubeSide=nil);

  if BlockCubeSideDefault.Checked then
     BlockCubeSidePanel.Visible:=False
  else
  begin
    BlockCubeSidePanel.Visible:=True;

    if not Assigned(ICubeSideEditor) then
    begin
      ICubeSideEditor:=TBlockFormatEditor.Create(Self);
      ICubeSideEditor.Align:=alClient;
      ICubeSideEditor.OnDirty:=OnDirty;

      TTeeVCL.AddFormTo(ICubeSideEditor,BlockCubeSidePanel);
    end;

    ICubeSideEditor.RefreshFormat(CurrentCubeSide);
  end;
end;

procedure TBlockEditor.BlockCubeSideDefaultClick(Sender: TObject);
begin
  if BlockCubeSideDefault.Checked then
  begin
    if CurrentCubeSide<>nil then
    with TCubeSidesAccess(TCubeBlock(Current).Sides) do
    case BlockCubeSide.ItemIndex of
      0: Left:=nil;
      1: Top:=nil;
      2: Right:=nil;
      3: Bottom:=nil;
      4: Front:=nil;
    else
      Back:=nil;
    end;
  end
  else
  begin
    with TCubeSidesAccess(TCubeBlock(Current).Sides) do
    case BlockCubeSide.ItemIndex of
      0: Left;
      1: Top;
      2: Right;
      3: Bottom;
      4: Front;
    else
      Back;
    end;
  end;

  MarkDirty;

  BlockCubeSide.Invalidate;

  BlockCubeSideClick(Self);
end;

procedure TBlockEditor.BlockTextLinesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).Lines:=BlockTextLines.Lines;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTextLinkFormatChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).LinkFormat:=BlockTextLinkFormat.Text;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockTextAlignChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).Alignment:=TAlignment(BlockTextAlign.ItemIndex);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockEllipseSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TEllipseBlock(Current).Slices:=BlockEllipseSlices.Position;
    LBlockEllipse.Caption:=TeeStr(BlockEllipseSlices.Position);

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBridgeRoundChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBridgeBlock(Current).RoundSlices:=UDBlockBridgeRound.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.CheckPathPoints(ACount:Integer);
begin
  EnableControls(ACount>0,[BlockPathPoints,UDPathPoints,SBPathRemove]);

  if ACount>0 then
  begin
    UDPathPoints.Max:=ACount-1;
    UDPathPoints.Position:=0;
    BlockPathPointsChange(Self);
  end;
end;

procedure TBlockEditor.PageControl1Change(Sender: TObject);
var tmp : Integer;
begin
  if PageControl1.ActivePage=TabPath then
  begin
    tmp:=TPathBlock(Current).Points.Count;

    if (UDPathPoints.Max<>tmp-1) or ((not UDPathPoints.Enabled) and (tmp>0)) then
       CheckPathPoints(tmp);
  end;
end;

procedure TBlockEditor.FillActions;
var tmp   : TBlockActionItem;
    tmpSt : String;
begin
  ListActions.Clear;

  tmpSt:=TActionGallery.IndexToEvent(TreeEvents.Selected);

  if TBlockAccess(Current).HasActions(tmpSt) then
  begin
    tmp:=Current.Actions.OfEvent(tmpSt);

    if Assigned(tmp) then
       ListActions.Items.AddStrings(tmp.Actions);

    if ListActions.Items.Count>0 then
       ListActions.ItemIndex:=0;
  end;

  ListActionsClick(Self);
end;

procedure TBlockEditor.PageCurrentChange(Sender: TObject);
var t : Integer;
begin
  if PageCurrent.ActivePage=TabActions then
     if TreeEvents.Items.Count=0 then
     begin
       TActionGallery.AddBasicEvents(TreeEvents);

       if Current is TCustomObjectBlock then
          if TBlocksAccess(TCustomObjectBlock(Current).Items).HasEvents then
             with TCustomObjectBlock(Current).Items.Events do
             for t:=0 to Count-1 do
                 TActionGallery.AddTreeEvent(TreeEvents,Strings[t]);

       TreeEvents.Selected:=TreeEvents.Items[0];
       TreeEvents.Selected.Expanded:=True;
     end;
end;

procedure TBlockEditor.Button4Click(Sender: TObject);
begin
  with Current.Editor(Self) as TCustomForm do
  try
    if ShowModal=mrOk then
       MarkDirty;
  finally
    Free;
  end;
end;

procedure TBlockEditor.BAddActionClick(Sender: TObject);
begin
  AddEditAction;
end;

procedure TBlockEditor.AddEditAction(const AAction:String='');

  procedure UpdateListActions(const ANewAction,AEvent:String);
  var tmpAction : TBlockActionItem;
  begin
    if AAction='' then
    begin
      tmpAction:=Current.Actions.OfEvent(AEvent);

      if not Assigned(tmpAction) then
         tmpAction:=Current.Actions.Add(AEvent);

      tmpAction.Actions.Add(ANewAction);

      ListActions.Items.Add(ANewAction);

      ListActions.ItemIndex:=ListActions.Items.Count-1;
      ListActionsClick(Self);

      TreeEvents.Invalidate;
    end
    else
      ListActions.Items[ListActions.ItemIndex]:=ANewAction;

    MarkDirty;
  end;

var ActionText : String;
begin
  if TActionGallery.ModalShow(Self,Current,CurrentMaker,ActionText,AAction) then
     UpdateListActions(ActionText,TActionGallery.IndexToEvent(TreeEvents.Selected));
end;

procedure TBlockEditor.BlockRectLeftChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TRectPyramidBlock(Current).LeftPercent:=BlockRectLeft.Position;
    LRectLeft.Caption:=TeeStr(BlockRectLeft.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectRightChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TRectPyramidBlock(Current).RightPercent:=BlockRectRight.Position;
    LRectRight.Caption:=TeeStr(BlockRectRight.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockScaleXYZChange(Sender: TObject);
begin
  if not IModifying then
  with Current.Scale do
  begin
    X:=TBlockFormatEditor.ToScaleValue(BlockScaleXYZ.Position);
    Y:=X;
    Z:=Y;

    LScaleX.Caption:=FormatFloat('0.000',X);
    LScaleY.Caption:=FormatFloat('0.000',Y);
    LScaleZ.Caption:=FormatFloat('0.000',Z);

    CheckBlockNoScaling;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPipeYRadiusChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPipeBlock(Current).Radius.Y:=BlockPipeYRadius.Position;
    PipeYRadius.Caption:=TeeStr(BlockPipeYRadius.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPipeConnectorChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPipeBlock(Current).Connector:=TCustomBlock(BlockPipeConnector.Items.Objects[BlockPipeConnector.ItemIndex]);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockStarInnerChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TStarBlock(Current).InnerSize:=BlockStarInner.Position*0.001;
    LStarInner.Caption:=FormatFloat('0.###',TStarBlock(Current).InnerSize);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockStarSlantChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TStarBlock(Current).SlantAngle:=BlockStarSlant.Position;
    LStarSlant.Caption:=FormatFloat('0.###',TStarBlock(Current).SlantAngle);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectLClick(Sender: TObject);
begin
  BlockRectDefL.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectRClick(Sender: TObject);
begin
  BlockRectDefR.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectTClick(Sender: TObject);
begin
  BlockRectDefT.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectBClick(Sender: TObject);
begin
  BlockRectDefB.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockRectDefLClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Sides do
    if BlockRectDefL.Checked then
       Left:=clDefault
    else
    if Left=clDefault then
       Left:=Current.Format.Color;

    BlockRectL.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefRClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Sides do
    if BlockRectDefR.Checked then
       Right:=clDefault
    else
    if Right=clDefault then
       Right:=Current.Format.Color;

    BlockRectR.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefTClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Sides do
    if BlockRectDefT.Checked then
       Top:=clDefault
    else
    if Top=clDefault then
       Top:=Current.Format.Color;

    BlockRectT.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefBClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current).Sides do
    if BlockRectDefB.Checked then
       Bottom:=clDefault
    else
    if Bottom=clDefault then
       Bottom:=Current.Format.Color;

    BlockRectB.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBevelYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBeveledCubeBlock(Current).BevelSize.Y:=BlockBevelY.Position;
    LabelBevelY.Caption:=TeeStr(BlockBevelY.Position);
    MarkDirty;

    if BlockBevelAll.Checked then
    begin
      BlockBevelX.Position:=BlockBevelY.Position;
      BlockBevelZ.Position:=BlockBevelY.Position;
    end;
  end;
end;

procedure TBlockEditor.BlockBevelZChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBeveledCubeBlock(Current).BevelSize.Z:=BlockBevelZ.Position;
    LabelBevelZ.Caption:=TeeStr(BlockBevelZ.Position);
    MarkDirty;

    if BlockBevelAll.Checked then
    begin
      BlockBevelX.Position:=BlockBevelZ.Position;
      BlockBevelY.Position:=BlockBevelZ.Position;
    end;
  end;
end;

procedure TBlockEditor.BlockNameChange(Sender: TObject);
begin
  if Showing and (not IModifying) then
  begin
    Current.Name:=BlockName.Text;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieStacksChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).Stacks:=BlockPieStacks.Position*0.1;
    LPieStacks.Caption:=FormatFloat('0.##',TPieSliceBlock(Current).Stacks);
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieInnerTopChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).InnerSize.X:=BlockPieInnerTop.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockPieInnerBottomChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TPieSliceBlock(Current).InnerSize.Y:=BlockPieInnerBottom.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBevelCurvePointsChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TBeveledCubeBlock(Current).CurvePoints:=BlockBevelCurvePoints.Position;
    LBevelCurvePoints.Caption:=TeeStr(BlockBevelCurvePoints.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.Button7Click(Sender: TObject);
begin
  with TExtrudedEditor.Create(Self) do
  try
    Tag:=ObjectToTag(TPathBlock(Current));
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBlockEditor.CBPosition10Click(Sender: TObject);
begin
  if not IModifying then
     RefreshBlock(Current);
end;

procedure TBlockEditor.BlockBevelRoundedClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TBeveledCubeBlock(Current).CurveRound:=BlockBevelRounded.Checked;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockBevelStyleChange(Sender: TObject);
begin
  if not IModifying then
  with TBeveledCubeBlock(Current) do
  begin
    Style:=TBevelCubeStyle(BlockBevelStyle.ItemIndex);

    BlockBevelCurvePoints.Enabled:=(Style=bsRound);
    BlockBevelRounded.Enabled:=(Style=bsRound);

    MarkDirty;
  end;
end;

procedure TBlockEditor.CheckFontStyle(Check:TCheckBox; Value:TFontStyle);
begin
  with TeeBlocks.TTeeTextBlock(Current).Font do
  if Check.Checked then
     Style:=Style+[Value]
  else
     Style:=Style-[Value];

  MarkDirty;
end;

procedure TBlockEditor.BlockFontItalicClick(Sender: TObject);
begin
  CheckFontStyle(BlockFontItalic,fsItalic);
end;

procedure TBlockEditor.BlockFontBoldClick(Sender: TObject);
begin
  CheckFontStyle(BlockFontBold,fsBold);
end;

procedure TBlockEditor.BlockFontStrikeClick(Sender: TObject);
begin
  CheckFontStyle(BlockFontStrike,fsStrikeOut);
end;

procedure TBlockEditor.BlockFontUnderClick(Sender: TObject);
begin
  CheckFontStyle(BlockFontUnder,fsUnderline);
end;

procedure TBlockEditor.BlockFontNameChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).Font.Name:=BlockFontName.CurrentItem;
    MarkDirty;
  end;
end;

procedure TBlockEditor.FontRefreshBasicProps;
begin
  with TeeBlocks.TTeeTextBlock(Current).Font do
  begin
    BlockFontSize.Position:=Size;
    BlockFontItalic.Checked:=fsItalic in Style;
    BlockFontBold.Checked:=fsBold in Style;
    BlockFontStrike.Checked:=fsStrikeOut in Style;
    BlockFontUnder.Checked:=fsUnderline in Style;
    BlockFontName.ItemIndex:=BlockFontName.Items.IndexOf(Name);
  end;
end;

procedure TBlockEditor.BlockFontEditClick(Sender: TObject);
begin
  if TTeeFontEditor.Edit(Self,TeeBlocks.TTeeTextBlock(Current).Font) then
  begin
    BlockFontColor.Repaint;
    FontRefreshBasicProps;
    BlockFontColorClick(Self);
  end;
end;

procedure TBlockEditor.ESizeChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TeeBlocks.TTeeTextBlock(Current).Font.Size:=BlockFontSize.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockFontColorClick(Sender: TObject);
begin
  Current.Format.Color:=TeeBlocks.TTeeTextBlock (Current).Font.Color;
  MarkDirty;
end;

function TBlockEditor.CurrentActions:TBlockActionItem;
begin
  result:=Current.Actions.OfEvent(TActionGallery.IndexToEvent(TreeEvents.Selected));
end;

procedure TBlockEditor.SBActionUpClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=ListActions.ItemIndex;
  CurrentActions.Actions.Exchange(tmp,tmp-1);
  ListActions.Items.Exchange(tmp,tmp-1);
  MarkDirty;
end;

procedure TBlockEditor.SBActionDownClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=ListActions.ItemIndex;
  CurrentActions.Actions.Exchange(tmp,tmp+1);
  ListActions.Items.Exchange(tmp,tmp+1);
  MarkDirty;
end;

procedure TBlockEditor.BlockPipeVisibleClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TPipeBlock(Current).ConnectorVisible:=BlockPipeVisible.Checked;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectDefCenterClick(Sender: TObject);
begin
  if not IModifying then
  begin
    with TRectangleBlock(Current) do
    if BlockRectDefCenter.Checked then
       Center:=clDefault
    else
    if Center=clDefault then
       Center:=Current.Format.Color;

    BlockRectCenter.Invalidate;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockRectCenterClick(Sender: TObject);
begin
  BlockRectDefCenter.Checked:=False;
  MarkDirty;
end;

procedure TBlockEditor.BlockPathColorEachClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TPathBlock(Current).ColorEach:=BlockPathColorEach.Checked;
    MarkDirty;
  end;
end;

function TBlockEditor.CurrentPropertyName:String;
var tmp    : {$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF};
    tmpSt  : String;
    ALeft  : String;
    ARight : String;
begin
  tmp:=BlockObjectProperties.ItemIndex;

  if tmp=-1 then
     result:=''
  else
  begin
    tmp:={$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(BlockObjectProperties.Items.Objects[tmp]);

    tmpSt:=TCustomObjectBlock(Current).Items.Properties[tmp];
    SplitValue(tmpSt,ALeft,ARight);

    result:=UpperCase(ALeft);
  end;
end;

function TBlockEditor.CurrentPropertyIndex:Integer;
begin
  if BlockObjectProperties.ItemIndex=-1 then
     result:=-1
  else
     result:=TCustomObjectBlock(Current).Properties.IndexOfProperty(CurrentPropertyName);
end;

procedure TBlockEditor.BlockObjectPropertiesClick(Sender: TObject);
var tmp : Integer;
    ALeft  : String;
    ARight : String;
begin
  tmp:=CurrentPropertyIndex;

  IModifying:=True;
  try
    if tmp=-1 then
       BlockObjectPropValue.Text:=''
    else
    begin
      SplitValue(TCustomObjectBlock(Current).Properties[tmp],ALeft,ARight);
      BlockObjectPropValue.Text:=ARight;
    end;
  finally
    IModifying:=False;
  end;

  SetLabelPropValue;

  BlockObjectPropValue.Enabled:=True;
  SBObjPropValue.Enabled:=True;
end;

procedure TBlockEditor.SetLabelPropValue;
var tmp : String;
    tmpLeft : String;
    tmpIndex : Integer;
begin
  with TCustomObjectBlock(Current).Items.Properties do
  try
    tmp:=Value[CurrentPropertyName]
  except
    on Exception do
    begin
      tmpIndex:=IndexOfProperty(CurrentPropertyName);

      if tmpIndex=-1 then
         tmp:=''
      else
         SplitValue(Strings[tmpIndex],tmpLeft,tmp);
    end;
  end;

  LabelPropValue.Caption:=tmp;
end;

procedure TBlockEditor.BlockObjectPropValueChange(Sender: TObject);
var tmp      : Integer;
    tmpValue : String;
begin
  if not IModifying then
  begin
    tmp:=CurrentPropertyIndex;
    tmpValue:=BlockObjectPropValue.Text;

    with TCustomObjectBlock(Current).Properties do
    if tmp=-1 then
       Add(CurrentPropertyName+'='+tmpValue)
    else
       Strings[tmp]:=CurrentPropertyName+'='+tmpValue;

    try
      TCustomObjectBlock(Current).Items.Properties.Value[CurrentPropertyName]:=tmpValue;
      LabelWrongValue.Caption:='';
    except
      on E:Exception do
         LabelWrongValue.Caption:=E.Message;
    end;

    SetLabelPropValue;

    MarkDirty;
  end;
end;

procedure TBlockEditor.SBObjPropValueClick(Sender: TObject);
var AObject : TObject;
    AName   : String;
begin
  AObject:=nil;
  AName:='';

  if TMakerPropertySelector.ModalShow(Self,CurrentMaker.Blocks,AObject,AName) then
  begin
    AName:=(AObject as TComponent).Name+'.'+AName;

    BlockObjectPropValue.Text:=AName;
    BlockObjectPropValueChange(Self);
  end;
end;

procedure TBlockEditor.BClearTextLinkClick(Sender: TObject);
begin
  with TeeBlocks.TTeeTextBlock(Current).LinkText do
  begin
    Instance:=nil;
    PropertyName:='';
  end;
end;

procedure TBlockEditor.EStartMapSizeChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TStarsBlock(Current).Size:=StarMapSize.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.StarCountChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TStarsBlock(Current).ElementCount:=StarCount.Position;
    LabelStarCount.Caption:=TeeStr(StarCount.Position);
    MarkDirty;
  end;
end;

procedure TBlockEditor.SpeedButton1Click(Sender: TObject);
var Old : TCursor;
begin
  if not IModifying then
  begin
    Old:=Current.Cursor;
    Current.Cursor:=TMouseCursorEdit.Choose(Current,Current.Cursor);

    if Current.Cursor<>Old then
    begin
      TMouseCursorEdit.SetComboCursor(CBCursor,Current.Cursor);

      MarkDirty;
    end;
  end;
end;

procedure TBlockEditor.CBCursorChange(Sender: TObject);
begin
  if not IModifying then
  begin
    with Current do
         Cursor:=TeeSetCursor(Cursor,CBCursor.CurrentItem);

    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockElipEccenChange(Sender: TObject);
begin
  if not IModifying then
  begin
    TEllipsoidBlock(Current).Eccentricity:=BlockElipEccen.Position*0.01;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BOpenExplorerClick(Sender: TObject);
var tmp : String;
begin
  tmp:='explorer /select,"'+BlockLink.Text+'"';
  WinExec(PAnsiChar(AnsiString(tmp)),SW_SHOWNORMAL);
end;

procedure TBlockEditor.BlockHoleCenterYChange(Sender: TObject);
begin
  if not IModifying then
  begin
    THoleBlock(Current).Hole.Center.Y:=BlockHoleCenterY.Position;
    MarkDirty;
  end;
end;

procedure TBlockEditor.BlockHoleCenterXChange(Sender: TObject);
begin
  if not IModifying then
  begin
    THoleBlock(Current).Hole.Center.X:=BlockHoleCenterX.Position;
    MarkDirty;
  end;
end;

type
  TCustomObjectBlockAccess=class(TCustomObjectBlock);

procedure TBlockEditor.BObjectEmbeddClick(Sender: TObject);
begin
  if not IModifying then
  begin
    TCustomObjectBlockAccess(Current).FLink:='';
    BLoad.Enabled:=True;
    BOpenExplorer.Enabled:=False;
    BObjectEmbedd.Enabled:=False;

    MarkDirty;
  end;
end;

procedure TBlockEditor.PopupActionsPopup(Sender: TObject);
begin
  Manualedit1.Enabled:=ListActions.ItemIndex<>-1;
end;

procedure TBlockEditor.Manualedit1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=ListActions.Items[ListActions.ItemIndex];

  if InputQuery('Edit Action','Text:',tmp) then
  begin
    with ListActions do
         Items[ItemIndex]:=tmp;

    CurrentActions.Actions[ListActions.ItemIndex]:=tmp;

    MarkDirty;
  end;
end;

procedure TBlockEditor.TreeEventsChange(Sender: TObject; Node: TTreeNode);
begin
  EnableControls(Node.Count=0,[BAddAction,ListActions,BDeleteAction,SBActionUp,SBActionDown]);

  if Node.Count=0 then
     FillActions;
end;

{$IFDEF CLX}
procedure TBlockEditor.TreeEventsCustomDrawItem(Sender: TCustomViewControl;
    Node: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
    State: TCustomDrawState; Stage: TCustomDrawStage;
    var DefaultDraw: Boolean);
{$ELSE}
procedure TBlockEditor.TreeEventsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
{$ENDIF}

  function NodeHasActionsRecursive(ANode:TTreeNode):Boolean;
  var t : Integer;
      tmpID : String;
  begin
    tmpID:=TActionGallery.IndexToEvent(ANode);

    if (tmpID<>'') and TBlockAccess(Current).HasActions(tmpID) then
       result:=True
    else
    begin
      result:=False;

      for t:=0 to ANode.Count-1 do
      if NodeHasActionsRecursive(ANode.Item[t]) then
      begin
        result:=True;
        exit;
      end;
    end;
  end;

begin
  if NodeHasActionsRecursive({$IFDEF CLX}TTreeNode{$ENDIF}(Node)) then
     {$IFNDEF CLX}TreeEvents.{$ENDIF}Canvas.Font.Style:=[fsBold]
  else
     {$IFNDEF CLX}TreeEvents.{$ENDIF}Canvas.Font.Style:=[];
end;

procedure TBlockEditor.Button5Click(Sender: TObject);
var tmp : TGraphic;
begin
  tmp:=TTerrainBlock(Current).GetAsBitmap;

  if Assigned(tmp) then
  try
    TTeeVCL.SavePictureDialog(Self,tmp);
  finally
    tmp.Free;
  end;
end;

procedure TBlockEditor.BlockCubeSideDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  BlockCubeSide.Canvas.FillRect(Rect);

  if TCubeBlock(Current).Sides.SideOf(Index)=nil then
     BlockCubeSide.Canvas.Font.Style:=[]
  else
     BlockCubeSide.Canvas.Font.Style:=[fsBold];

  BlockCubeSide.Canvas.TextOut(Rect.Left+2,Rect.Top,BlockCubeSide.Items[Index]);
end;

procedure TBlockEditor.Change1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=ListActions.Items[ListActions.ItemIndex];
  AddEditAction(tmp);
end;

{ TMakerSelectProperty }
procedure TMakerPropertySelector.AddSelection(ABlocks:TBlocks; ExternalBlocks:Boolean=False);

  function Instances: TTreeNodes;
  begin
    result:=TreeObjects.Items;
  end;

var tmp : TMaker;
begin
  tmp:=ABlocks.DrawBlocks.Parent as TMaker;

  Instances.AddObject(nil,TeeMsg_MakerWorld,tmp);
  Instances.AddObject(nil,TeeMsg_MakerRender,tmp.Render);

  if Assigned(tmp.Selected) then
     Instances.AddObject(nil,TeeMsg_SelfBlock,tmp.Selected);

  AddBlocks(Instances.AddObject(nil,TeeMsg_MakerBlocks,ABlocks),ABlocks,ExternalBlocks);

  Instances.AddObject(nil,TeeMsg_SystemObject,MakerSystem);
end;

class function TMakerPropertySelector.ModalShow(AOwner:TComponent; ABlocks:TBlocks;
                             var AObject:TObject; var AName:String):Boolean;
begin
  with TMakerPropertySelector.Create(AOwner) do
  try
    AddSelection(ABlocks,True);
    SelectNode(AObject,AName);

    result:=ShowModal=mrOk;

    if result then
    begin
      AObject:=SelectedProperty(AName) as TComponent;

      if AObject=MakerSystem then
         AName:='MakerSystem.'+AName;
    end;

  finally
    Free;
  end;
end;

class function TMakerPropertySelector.ModalShow(AOwner:TComponent; ABlocks:TBlocks;
                                  Link:TPropertyLink):Boolean;
var tmpObj : TObject;
    tmpProp : String;
begin
  tmpObj:=Link.Instance;
  tmpProp:=Link.PropertyName;

  result:=ModalShow(AOwner,ABlocks,tmpObj,tmpProp);

  if result then
  begin
    Link.Instance:=tmpObj as TComponent;
    Link.PropertyName:=tmpProp;
  end;
end;

procedure TMakerPropertySelector.AddBlocks(Node:TTreeNode; ABlocks:TBlocks; ExternalBlocks:Boolean=False);

  function TitleOrName(ABlock:TCustomBlock):String;
  begin
    result:=ABlock.Title;
    if result='' then result:=ABlock.Name;
  end;

var t : Integer;
    tmp : TTreeNode;
    tmpSt : String;
begin
  for t:=0 to ABlocks.Count-1 do
  begin
    tmpSt:=TitleOrName(ABlocks[t]);

    if tmpSt='' then
       tmpSt:=IntToStr(t);

    tmp:=Node.Owner.AddChildObject(Node,tmpSt,ABlocks[t]);

    if ABlocks[t] is TCustomObjectBlock then
    with TCustomObjectBlock(ABlocks[t]) do
         if ExternalBlocks or (LinkFile='') then
            AddBlocks(tmp,Items);
  end;
end;

procedure TMakerPropertySelector.SelectNode(AObject:TObject; AName:String);

  procedure FindProp(ANode:TTreeNode; S:String);

     function FindNode(ANode:TTreeNode; S:String):TTreeNode;
     begin
       if Assigned(ANode) then
          result:=ANode.getFirstChild
       else
          result:=TreeProps.Items.GetFirstNode;

       while Assigned(result) do
       begin
         if result.Text=S then
            Exit;

         result:=result.getNextSibling
       end;

       result:=nil;
     end;

  var i : Integer;
  begin
    repeat
      i:=Pos('.',S);
      if i>0 then
      begin
        ANode:=FindNode(ANode,Copy(S,1,i-1));
        Delete(S,1,i);
      end
      else
        ANode:=FindNode(ANode,S);
    until i=0;

    if Assigned(ANode) then
       ANode.Selected:=True;
  end;

var t : Integer;
begin
  with TreeObjects do
  for t:=0 to Items.Count-1 do
  begin
    if Items[t].Data=AObject then
    begin
      Selected:=Items[t];
      FindProp(nil,AName);
      break;
    end;
  end;
end;

procedure TMakerPropertySelector.AddProperties(ATree:TTreeView; AObject:TObject;
                                     AList:TList; AFilter:TTypeKinds);

  procedure AddObjectProperties(AProperties:TObjectProperties);
  var tmpParent : TTreeNode;
      t         : Integer;
      ALeft     : String;
      ARight    : String;
  begin
    tmpParent:=nil;

    with ATree.Items do
    for t:=0 to Count-1 do
    if Item[t].Data=AProperties then
    begin
      tmpParent:=Item[t];
      break;
    end;

    with AProperties do
    for t:=0 to Count-1 do
    begin
      SplitValue(Strings[t],ALeft,ARight);

      if ALeft<>'' then
         ATree.Items.AddChildObject(tmpParent,ALeft,AObject);
    end;
  end;

begin
  inherited;

  // Add custom external properties, not found by TypInfo:
  if AObject is TCustomObjectBlock then
  begin
    AddObjectProperties(TCustomObjectBlock(AObject).Properties);
    AddObjectProperties(TCustomObjectBlock(AObject).Items.Properties);
  end
  else
  if AObject is TBlocks then
     AddObjectProperties(TBlocks(AObject).Properties);
end;

procedure TBlockEditor.PageObjectChange(Sender: TObject);

  procedure AddProperties(AObject:TCustomObjectBlock);
  var t      : Integer;
      ALeft  : String;
      ARight : String;
  begin
    LabelPropValue.Caption:='';

    BlockObjectProperties.ItemIndex:=-1;
    BlockObjectPropValue.Text:='';
    BlockObjectPropValue.Enabled:=False;
    SBObjPropValue.Enabled:=False;

    BlockObjectProperties.Sorted:=False;
    BlockObjectProperties.Clear;

    with AObject do
    begin
      if Items.HasProperties then
      with Items.Properties do
      for t:=0 to Count-1 do
      begin
        SplitValue(Strings[t],ALeft,ARight);

        if ALeft<>'' then
           BlockObjectProperties.Items.AddObject(ALeft,TObject(t));
      end;

      BlockObjectProperties.Sorted:=True;

      if BlockObjectProperties.Items.Count>0 then
      begin
        BlockObjectProperties.ItemIndex:=0;
        BlockObjectPropertiesClick(Self);
      end;
    end;
  end;

begin
  if PageObject.ActivePage=TabObjectProperties then
     if BlockObjectProperties.Items.Count=0 then
        AddProperties(TCustomObjectBlock(Current));
end;

procedure TBlockEditor.ESpinByChange(Sender: TObject);
begin
  CBSpinByClick(Self);
end;

procedure TBlockEditor.BlockNoScalingClick(Sender: TObject);
begin
  BlockScaleXYZ.Position:=0;

  BlockScaleX.Position:=0;
  BlockScaleY.Position:=0;
  BlockScaleZ.Position:=0;
end;

procedure TBlockEditor.CBTextQualityClick(Sender: TObject);
begin
  if not IModifying then
  begin
    if CBTextQuality.Checked then
       TeeBlocks.TTeeTextBlock(Current).Font.Quality:=fqBest
    else
       TeeBlocks.TTeeTextBlock(Current).Font.Quality:=fqNormal;

    MarkDirty;
  end;
end;

procedure TBlockEditor.Button8Click(Sender: TObject);
begin
  BlockLink.Text:=TBlockGallery.ChooseObject(Self);
end;

function EditBlock(AOwner:TComponent; AVisual:TVisualBlock):Boolean;
begin
  result:=TBlockEditor.ModalShow(AOwner,AVisual as TCustomBlock);
end;

procedure TBlockEditor.Button9Click(Sender: TObject);
var tmp : TCustomBlock;
begin
  tmp:=TBlockGallery.ChooseBlock(Self,Current.Parent.DrawBlocks,Current);

  if Assigned(tmp) then
     TCustomObjectBlock(Current).LinkBlock:=tmp;

  SetLinkBlockLabel;
end;

procedure TBlockEditor.CBFaceToViewerClick(Sender: TObject);
begin
  Current.Rotation.FaceToViewer:=CBFaceToViewer.Checked;
end;

initialization
  TeeOnEditVisual:=EditBlock;
  TeeVisualEditorClass:=TBlockEditor;

  RegisterClass(TBlockEditor);
finalization
  TeeOnEditVisual:=nil;
end.
