unit Interfaces;

interface

uses
   Types;

type
   // 3D
   TPoint3DFloat=packed record
   {$IFDEF CLR}
   public
   {$ENDIF}
    X : Double;
    Y : Double;
    Z : Double;
   end;

   TTeeCanvasSurfaceStyle=(tcsSolid,tcsWire,tcsDot);

   TTeeFontStyle=(fsNormal, fsOutline, fsBitmap);

   {TGLFontCache=packed record
    Offset    : Integer;
    Name      : String;
    Weight    : Integer;
    Style     : TFontStyles;
    Extrusion : Double;
    GLStyle   : TTeeFontStyle;
   end; }

   IGraphicsGL = Interface(IInterface)
    function getDrawStyle() : TTeeCanvasSurfaceStyle;
    procedure setDrawStyle(style : TTeeCanvasSurfaceStyle);

    function getFont() : TObject;
    procedure setFont(fontColor : Integer; fontName : string; fontSize : Integer;
    isBold : Boolean; isItalic : Boolean; isUnderLine : Boolean);

    function iMeasureString(text : string) : TSize;

    function getFontStyle() : TTeeFontStyle;
    procedure setFontStyle(fontStyle : TTeeFontStyle);

    function getFontExtrusion() : Double;
    procedure setFontExtrusion(fontExtrusion : Double);

    function iFindFont() : Integer;

    function getFontCacheOffset(index : Integer) : Integer;
    procedure setFontCacheOffset(index : Integer; offset : Integer);

    function getFontCacheExtrusion(index : Integer) : Double;
    procedure setFontCacheExtrusion(index : Integer; extrusion : Double);

    function iTextWidth(text : string) : Single;

    procedure iCalcMinMax(points : TObject; Var aMin : TPoint3DFloat; Var aMax : TPoint3DFloat);

    function getShininess() : Double;
    procedure setShininess(shininess : Double);

    procedure iActivateBlend(value : Boolean);

    function iFindTexture(aBitmap : TObject) : Cardinal;

    procedure iCube(left : Double; right : Double; top : Double; bottom : Double; z0 : Double; z1 : Double; darkSides : Boolean);

    function getPenColor() : Integer;
    procedure setPenColor(color : Integer);

    function getBrushTransparency() : Integer;
    procedure setBrushTransparency(transparency : Integer);

    procedure iDoProjection(doPick : Boolean; x : Integer; y : Integer);
    procedure iSetModelView();

    procedure iDisableRotation();
    procedure iEnableRotation();
  end;

  {Point3DDouble=packed record
       X : Double;
       Y : Double;
       Z : Double;
  end;

  TVertexCoord=packed record
    Point : Point3DDouble;
    Coord : Point3DDouble;
  end; }

implementation



end.
