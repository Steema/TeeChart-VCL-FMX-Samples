unit TeeRevolution;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF D6}
  Types,
  {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, Menus,
  ComCtrls,
  TeeBlocks, TeeProcs, TeCanvas,
  {$IFDEF BLOCKS}
  Interfaces,
  {$ENDIF}
  TeeExtruded;

type
  TRevolutionBlock=class(TCustomCoverBlock)
  private
    FCover       : TBlockFormat;
    FCurvePoints : Integer;
    FOuter       : TPointCollection;
    FSlices      : Integer;
    FTotalAngle  : Double;

    IList        : Integer;
    IListCover   : Integer;

    function GetCover:TBlockFormat;
    function IsPointsStored:Boolean;
    procedure SetCover(const Value: TBlockFormat);
    procedure SetCurvePoints(const Value: Integer);
    procedure SetPoints(const Value:TPointCollection);
    procedure SetSlices(const Value: Integer);
    procedure SetTotalAngle(const Value: Double);
  protected
    procedure DeleteLists; override;
    function DesignHandles(AOwner:TComponent):TCustomBlock; override;
    function GetEditor:String; override;
    procedure PrepareForGallery; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    procedure DataChanged;
    procedure Draw; override;
    function HasCover:Boolean;
  published
    property Cover:TBlockFormat read GetCover write SetCover;
    property CurvePoints:Integer read FCurvePoints write SetCurvePoints default 16;
    property OuterPoints:TPointCollection read FOuter write SetPoints stored IsPointsStored;
    property Slices:Integer read FSlices write SetSlices default 32;
    property TotalAngle:Double read FTotalAngle write SetTotalAngle;
  end;

  TRevolutionEditor = class(TVisualEditor)
    Label82: TLabel;
    LRevoSlices: TLabel;
    Label83: TLabel;
    LRevolutionCurve: TLabel;
    Label59: TLabel;
    LRevolutionAngle: TLabel;
    BlockRevolutionSlices: TScrollBar;
    Button4: TButton;
    BlockRevolutionCurve: TScrollBar;
    BlockRevolutionAngle: TScrollBar;
    GroupBox10: TGroupBox;
    Button6: TButton;
    BlockRevolutionCoverDef: TCheckBox;
    BlockRevolutionCoverVisible: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure BlockRevolutionSlicesChange(Sender: TObject);
    procedure BlockRevolutionCurveChange(Sender: TObject);
    procedure BlockRevolutionAngleChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure BlockRevolutionCoverDefClick(Sender: TObject);
    procedure BlockRevolutionCoverVisibleClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    Revolution : TRevolutionBlock;
    IModifying : Boolean;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math,
  {$IFDEF D20}
  System.Math.Vectors,
  {$ENDIF}
  TeeGLCanvas, OpenGL2, TeeBlockFormat, TeeRevolutionEditor;

{ TRevolutionBlock }

Constructor TRevolutionBlock.Create(AOwner:TComponent);
begin
  inherited;
  FCurvePoints:=16;
  FSlices:=32;

  FOuter:=TPointCollection.Create(Self,TPointItem);
end;

Destructor TRevolutionBlock.Destroy;
begin
  FCover.Free;
  FOuter.Free;
  inherited;
end;

procedure TRevolutionBlock.DataChanged;
begin
  DeleteLists;
end;

procedure TRevolutionBlock.DeleteLists;
begin
  inherited;

  DeleteList(IList);
  DeleteList(IListCover);
end;

procedure TRevolutionBlock.PrepareForGallery;
begin
  inherited;

  // Sample points:
  FOuter.Clear;
  FOuter.Add(-1,0.9);
  FOuter.Add(0.6,0.3);
  FOuter.Add(0.9,0.8);
end;

type
  TBlockFormatAccess=class(TBlockFormat);

function TRevolutionBlock.DesignHandles(AOwner: TComponent): TCustomBlock;
begin
  result:=inherited DesignHandles(AOwner);

  with TObjectBlockHandle(result) do
  begin
    AddHandle(Point3D(0.5,1,0),'','Cover.Visible=not Visible').Format.Color:=clYellow;
    AddHandle(Point3D(1,1,0.5),'TotalAngle,MinMax:0;360').Format.Color:=clAqua;
  end;
end;

{ TRevolutionBlock }

procedure TRevolutionBlock.Assign(Source: TPersistent);
begin
  if Source is TRevolutionBlock then
  with TRevolutionBlock(Source) do
  begin
    Self.SetCover(FCover);
    Self.FCurvePoints:=FCurvePoints;
    Self.FSlices:=FSlices;
    Self.FTotalAngle:=FTotalAngle;
    Self.OuterPoints:=FOuter;
  end;

  inherited;
end;

procedure TRevolutionBlock.Draw;
var
  tmpPi : Single;
  OuterCount : Integer;
  tmpTotalAngle : Single;

  procedure CalcData(AStartIndex:Integer);
  var tmpXCount : Single;
      tmpTextX,
      tmpTextY  : Single;
      tmpSinCos : Array of TPointSinCos;

  const
    BezierPoints=3;

    Procedure Bezier(const NumPoints,AFirst:Integer);
    var
      t1 : Single;
      t2 : Single;
      tmpT  : Single;
      px,py,
      px1,py1 : Single;

      procedure AddPoint(const APoint:Integer);
      begin
        with tmpSinCos[APoint] do
        begin
          glNormal3f(0,X,Y);

          glTexCoord2f(t1,tmpT);
          glVertex3f(px,py*Y,py*X);

          glTexCoord2f(t2,tmpT);
          glVertex3f(px1,py1*Y,py1*X);
        end;
      end;

    var P  : TPointFloatArray;
        P1 : TPointFloat;
        P2 : TPointFloat;
        P3 : TPointFloat;
        P4 : TPointFloat;
        t  : Integer;
        tt : Integer;
    begin
      with FOuter do
      begin
        if NumPoints=4 then
        begin
          with Point[AFirst-3].Point.Point do
          begin
            P1.X:=X;
            P1.Y:=Z;
          end;

          with Point[AFirst-2].Point.Point do
          begin
            P2.X:=X;
            P2.Y:=Z;
          end;

          with Point[AFirst-1].Point.Point do
          begin
            P3.X:=X;
            P3.Y:=Z;
          end;

          with Point[AFirst].Point.Point do
          begin
            P4.X:=X;
            P4.Y:=Z;
          end;
        end
        else
        begin
          with Point[AFirst-2].Point.Point do
          begin
            P1.X:=X;
            P1.Y:=Z;
          end;

          with Point[AFirst-1].Point.Point do
          begin
            P2.X:=X;
            P2.Y:=Z;
          end;

          with Point[AFirst].Point.Point do
          begin
            P3.X:=X;
            P3.Y:=Z;
          end;
        end;
      end;

      CalcBezier(FCurvePoints,p1,p2,p3,p4,p,NumPoints);

      t1:=((AFirst/(BezierPoints-1))-1)*tmpTextX;

      for t:=0 to FCurvePoints-1 do
      begin
        t2:=t1+tmpXCount;

        px:=p[t].X;
        py:=p[t].Y;

        px1:=p[t+1].X;
        py1:=p[t+1].Y;

        glBegin(GL_QUAD_STRIP);

        for tt:=AStartIndex downto 0 do
        begin
          tmpT:=1-tt*tmpTextY;
          AddPoint(tt);
        end;

        if (tmpTotalAngle=360) or (AStartIndex=0) then
        begin
          tmpT:=(Slices-1)*tmpTextY;
          AddPoint(Slices-1);
        end;

        glEnd;

        t1:=t2;
      end;
    end;

  var t : Integer;
  begin
    tmpTextX:=(BezierPoints-1)/(OuterCount-1);
    tmpTextY:=1/(Slices+1);

    if tmpSinCos=nil then
    begin
      SetLength(tmpSinCos,Slices);

      for t:=0 to Slices-1 do
      with tmpSinCos[t] do
           SinCos(t*tmpPi,X,Y);
    end;

    tmpXCount:=tmpTextX/CurvePoints;

    t:=BezierPoints-1;

    if t<OuterCount then
    repeat
      Bezier(BezierPoints,t);
      Inc(t,BezierPoints-1);
    until t>OuterCount-1;

    tmpSinCos:=nil;
  end;

  procedure DoDraw;
  begin
    if IList=0 then
    begin
      IList:=CreateNewList;
      CalcData(Slices-1);
      glEndList;
    end
    else
      glCallList(IList);

    if tmpTotalAngle<>360 then
    if (not Assigned(FCover)) or FCover.Solid then
    begin
      if Assigned(FCover) then
      begin
        TBlockFormatAccess(Format).Finish;
        TBlockFormatAccess(FCover).Start;
      end;

      if IListCover=0 then
      begin
        IListCover:=CreateNewList;
        CalcData(0);
        glEndList;
      end
      else
        glCallList(IListCover);

      if Assigned(FCover) then
      begin
        TBlockFormatAccess(FCover).Finish;
        TBlockFormatAccess(Format).Start;
      end;
    end;
  end;

  procedure DrawSide(AFormat:TBlockFormat; PointIndex:Integer; Forw:Boolean);
  var XPos : Single;
      v    : Single;

    procedure DrawSlice(t:Integer);
    var tmpSin,
        tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
    begin
      SinCos(t*tmpPi,tmpSin,tmpCos);

      glTexCoord2d(tmpCos,tmpSin);
      glVertex3f(XPos,v*tmpCos,v*tmpSin);
    end;

  var t : Integer;
  begin
    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(Format).Finish;
      TBlockFormatAccess(AFormat).Start;
    end;

    XPos:=FOuter[PointIndex].Point.X;

    v:=FOuter[PointIndex].Point.Z;

    glBegin(GL_TRIANGLE_FAN);

    glNormal3i(0,0,-1);

    if not Assigned(AFormat) then
       AFormat:=Format;

    glTexCoord3f(0,0,0.5);
    glVertex2d(XPos,0);

    if Forw then
       for t:=0 to Slices do
           DrawSlice(t)
    else
       for t:=Slices downto 0 do
           DrawSlice(t);

    glEnd;

    if TBlockFormatAccess(AFormat).PreparePen then
    begin
      glBegin(GL_LINE_LOOP);

      for t:=0 to Slices do
          DrawSlice(t);

      glEnd;
    end;

    if Assigned(AFormat) then
    begin
      TBlockFormatAccess(AFormat).Finish;
      TBlockFormatAccess(Format).Start;
    end;
  end;

var
  OldStyle : TTeeCanvasSurfaceStyle;
  HasSide1 : Boolean;
  HasSide2 : Boolean;
  CanCull  : Boolean;
begin
  OuterCount:=FOuter.Count;
  if OuterCount=0 then
     Exit;

  if FTotalAngle=0 then
     tmpTotalAngle:=360
  else
     tmpTotalAngle:=FTotalAngle;

  tmpPi:=(Pi/Slices)*tmpTotalAngle/180;

  if Format.Solid then
  begin
    HasSide1:=(not Assigned(FBrush1)) or FBrush1.Solid;
    HasSide2:=(not Assigned(FBrush2)) or FBrush2.Solid;

    CanCull:=(not ShouldDrawInterior) and HasSide1 and HasSide2 and (tmpTotalAngle=360);

    if CanCull then
       glEnable(GL_CULL_FACE);

    DoDraw;

    if HasSide1 then
       DrawSide(FBrush1,0,False);

    if HasSide2 then
       DrawSide(FBrush2,OuterCount-1,True);

    if CanCull then
       glDisable(GL_CULL_FACE);
  end;

  if TBlockFormatAccess(Format).PreparePen then
  begin
    {$IFDEF BLOCKS}
    OldStyle:=ICanvas.getDrawStyle;
    ICanvas.setDrawStyle(tcsWire);
    {$ELSE}
    OldStyle:=ICanvas.DrawStyle;
    ICanvas.DrawStyle:=tcsWire;
    {$ENDIF}

    DoDraw;

    {$IFDEF BLOCKS}
    ICanvas.setDrawStyle(OldStyle);
    {$ELSE}
    ICanvas.DrawStyle:=OldStyle;
    {$ENDIF}
    TBlockFormatAccess(Format).FinishPen;
  end;
end;

procedure TRevolutionBlock.SetSlices(const Value: Integer);
begin
  FSlices:=Value;
  DeleteLists;
end;

function TRevolutionBlock.GetEditor: String;
begin
  result:='TRevolutionEditor';
end;

function TRevolutionBlock.HasCover: Boolean;
begin
  result:=Assigned(FCover);
end;

function TRevolutionBlock.GetCover:TBlockFormat;
begin
  if not Assigned(FCover) then
     FCover:=TBlockFormat.Create(Self);

  result:=FCover;
end;

function TRevolutionBlock.IsPointsStored: Boolean;
begin
  result:=FOuter.Count>0;
end;

procedure TRevolutionBlock.SetPoints(const Value: TPointCollection);
begin
  FOuter.Assign(Value);
  DeleteLists;
end;

procedure TRevolutionBlock.SetCurvePoints(const Value: Integer);
begin
  FCurvePoints := Value;
  DeleteLists;
end;

procedure TRevolutionBlock.SetTotalAngle(const Value: Double);
begin
  FTotalAngle := Value;
  DeleteLists;
end;

procedure TRevolutionBlock.SetCover(const Value: TBlockFormat);
begin
  if Assigned(Value) then
     Cover.Assign(Value)
  else
     FreeAndNil(FCover);

  Repaint;
end;

procedure TRevolutionEditor.FormShow(Sender: TObject);
begin
  Revolution:=TRevolutionBlock(Tag);

  if Assigned(Revolution) then
  with Revolution do
  begin
    IModifying:=True;

    BlockRevolutionSlices.Position:=Slices;
    LRevoSlices.Caption:=TeeStr(Slices);

    BlockRevolutionCurve.Position:=CurvePoints;
    LRevolutionCurve.Caption:=TeeStr(CurvePoints);

    BlockRevolutionAngle.Position:=Round(TotalAngle);
    LRevolutionAngle.Caption:=FormatFloat('0.##',TotalAngle);

    BlockRevolutionCoverDef.Checked:=not HasCover;
    BlockRevolutionCoverVisible.Checked:=BlockRevolutionCoverDef.Checked or Cover.Solid;

    IModifying:=False;
  end;
end;

procedure TRevolutionEditor.BlockRevolutionSlicesChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Revolution.Slices:=BlockRevolutionSlices.Position;
    LRevoSlices.Caption:=TeeStr(BlockRevolutionSlices.Position);

    MarkDirty;
  end;
end;

procedure TRevolutionEditor.BlockRevolutionCurveChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Revolution.CurvePoints:=BlockRevolutionCurve.Position;
    LRevolutionCurve.Caption:=TeeStr(BlockRevolutionCurve.Position);
    MarkDirty;
  end;
end;

procedure TRevolutionEditor.BlockRevolutionAngleChange(Sender: TObject);
begin
  if not IModifying then
  begin
    Revolution.TotalAngle:=BlockRevolutionAngle.Position;
    LRevolutionAngle.Caption:=FormatFloat('0.##',Revolution.TotalAngle);
    MarkDirty;
  end;
end;

procedure TRevolutionEditor.Button6Click(Sender: TObject);
begin
  if TBlockFormatEditor.ModalShow(Self,Revolution.Cover) then
  begin
    BlockRevolutionCoverDef.Checked:=False;
    MarkDirty;
  end;
end;

procedure TRevolutionEditor.BlockRevolutionCoverDefClick(Sender: TObject);
begin
  if BlockRevolutionCoverDef.Checked then
     Revolution.Cover:=nil
  else
  begin
    Revolution.Cover; // force "get"
    Revolution.Repaint;
  end;

  MarkDirty;
end;

procedure TRevolutionEditor.BlockRevolutionCoverVisibleClick(
  Sender: TObject);
begin
  Revolution.Cover.Solid:=BlockRevolutionCoverVisible.Checked;

  BlockRevolutionCoverDef.Checked:=False;

  MarkDirty;
end;

procedure TRevolutionEditor.Button4Click(Sender: TObject);
begin
  with TRevolutionPointsEditor.Create(Self) do
  try
    Tag:={$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(Self.Revolution);
    ShowModal;
  finally
    Free;
  end;
end;

initialization
  RegisterBlock(TRevolutionBlock);
  RegisterClass(TRevolutionEditor);
end.
