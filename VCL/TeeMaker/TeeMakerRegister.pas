unit TeeMakerRegister;
{$I TeeDefs.inc}

interface

uses
  {$IFDEF D6DESIGNER}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  {$IFDEF LCL}
  PropEdits, ComponentEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  {$ENDIF}

  TeeMakerControl, TeeMakerEditor;

procedure Register;

implementation

{$R TeeMakerControl.res}

uses
   Classes, SysUtils, Menus,
   TeeConst, TeeBlocks, TeePrevi, TeExport, TeeAbout, TeeChart3D,
   Chart, EditChar, TeeChartBlock, OpenGL2, TeeBlockEditor,
   TeeAnimate,
   TeeGLUT,
   TeeWater, TeeProperties;

type
  TMakerControlEditor=class(TComponentEditor)
  protected
    Function Maker:TMaker;
  public
    procedure Edit; override;
    procedure ExecuteVerb( Index : Integer ); override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
  end;

  TTeeClassProperty=class(TClassProperty)
  {$IFDEF LCL}
  private
    function Designer:TTeeClassProperty;
  {$ENDIF}
  protected
    function GetObject:Integer;
  public
    function GetValue: string; override;
  end;

  TBlocksProperty=class(TTeeClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

  TChart3DProperty=class(TTeeClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

  TChart3DControlEditor=class(TMakerControlEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb( Index : Integer ); override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
  end;

  TCustomBlockEditor=class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb( Index : Integer ); override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
  end;

  TPropertiesProperty=class(TTeeClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

type
  TCustomBlockChartAccess=class(TCustomBlockChart);

{ TMakerControlEditor }

Function TMakerControlEditor.Maker:TMaker;
begin
  result:=TMaker(Component);
end;

procedure TMakerControlEditor.Edit;
begin
  TMakerEditor.ModalShow(nil,Maker);
  Designer.Modified;
  Maker.Invalidate;
end;

procedure TMakerControlEditor.ExecuteVerb( Index : Integer );
begin
  case Index of
    0..3: TeeShowAboutBox('','',PChar(glGetString(GL_RENDERER)));
    4: Edit;
  end;
end;

function TMakerControlEditor.GetVerbCount : Integer;
begin
  result:=inherited GetVerbCount+7;
end;

function TMakerControlEditor.GetVerb( Index : Integer ) : string;
begin
  result:='';

  case Index of
    0: begin
         result:=TeeMsg_Version;
         if TeeIsTrial then result:=result+' TRIAL'; // Do not localize
       end;

    1: result:=TeeMsg_Copyright;
    2: result:=cLineCaption;
    3: result:='About...';
    4: result:='Edit...';
    5: result:='Print...';
    6: result:='Export...';
  end;
end;

{ TChart3DControlEditor }

procedure TChart3DControlEditor.Edit;
begin
  EditChart(nil,TChart3D(Component).ChartBlock.Chart);
  TChartBlock(TChart3D(Component).ChartBlock).CreateItems;
  Designer.Modified;
end;

function TChart3DControlEditor.GetVerbCount : Integer;
begin
  result:=inherited GetVerbCount+1;
end;

procedure TChart3DControlEditor.ExecuteVerb( Index : Integer );
begin
  if Index=7 then
     inherited Edit
  else
     inherited;
end;

function TChart3DControlEditor.GetVerb( Index : Integer ) : string;
begin
  if Index=4 then
     result:=TeeMsg_EditChart
  else
  if Index=7 then
     result:='Edit 3D...'
  else
     result:=inherited GetVerb(Index);
end;

{ TTeeClassProperty }

{$IFDEF LCL}
function TTeeClassProperty.Designer:TTeeClassProperty;
begin
  result:=Self;
end;
{$ENDIF}

function TTeeClassProperty.GetObject:Integer;
begin
  result:=GetOrdValue;
end;

function TTeeClassProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]); // Do not localize
end;

{ TBlocksProperty }

function TBlocksProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TBlocksProperty.Edit;
begin
  if TMakerEditor.ModalShow(nil,TMaker(TBlocks(GetObject).Parent)) then
     Designer.Modified;
end;

{ TChart3DProperty }

procedure TChart3DProperty.Edit;
var tmp : TCustomBlockChart;
begin
  tmp:=TCustomBlockChart(GetObject);
  EditChart(nil,tmp);

  TChartBlock(TCustomBlockChartAccess(tmp).IBlock).CreateItems;

  Designer.Modified;
end;

function TChart3DProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TChart3DProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TCustomBlockEditor }

procedure TCustomBlockEditor.Edit;
begin
  if TBlockEditor.ModalShow(nil,TCustomBlock(Component)) then
     Designer.Modified;
end;

function TCustomBlockEditor.GetVerbCount : Integer;
begin
  result:=inherited GetVerbCount+1;
end;

procedure TCustomBlockEditor.ExecuteVerb( Index : Integer );
begin
  if Index=0 then
     Edit
  else
     inherited;
end;

function TCustomBlockEditor.GetVerb( Index : Integer ) : string;
begin
  if Index=0 then
     result:='Edit Block...'
  else
     result:=inherited GetVerb(Index);
end;

{ TPropertiesProperty }

function TPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPropertiesProperty.Edit;
begin
  if TPropertiesEditor.ModalShow(nil,TObjectProperties(GetObject)) then
     Designer.Modified;
end;

procedure Register;
begin
  RegisterNoIcon([ TCustomBlock, TTeeAnimation ]);

  RegisterComponents( 'TeeMaker', [TChart3D] );

  RegisterComponentEditor(TMaker,TMakerControlEditor);
  RegisterComponentEditor(TChart3D,TChart3DControlEditor);
  RegisterComponentEditor(TCustomBlock,TCustomBlockEditor);

  RegisterPropertyEditor(TypeInfo(TBlocks), nil, '', TBlocksProperty);
  RegisterPropertyEditor(TypeInfo(TChart),TChart3D,'',TChart3DProperty);
  RegisterPropertyEditor(TypeInfo(TChart),TChartBlock,'',TChart3DProperty);
  RegisterPropertyEditor(TypeInfo(TObjectProperties), nil, '', TPropertiesProperty);
end;

end.
