unit TeeVisualsEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}

  SysUtils, Classes,

  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,

  {$IFDEF D6}
  Variants,
  {$ENDIF}

  TeCanvas, TeEngine, TeePenDlg, TeeBlocks;

type
  TVisualsList=class(TList)
  public
    Series : TChartSeries;
    Template : TVisualBlock;
  end;

type
  TVisualsEditor = class(TForm)
    Panel1: TPanel;
    Button3: TButton;
    BClearVisual: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BClearVisualClick(Sender: TObject);
  private
    { Private declarations }
    Visuals : TVisualsList;
    IEditor : TVisualEditor;
    FOnChangedVisual : TNotifyEvent;

    procedure EnableClear;
    procedure TemplateChanged(Sender:TObject);
  public
    { Public declarations }
    class function ModalShow(AOwner:TComponent; AVisuals:TVisualsList):Boolean;

    property OnChangedVisual:TNotifyEvent read FOnChangedVisual write FOnChangedVisual;
  end;

implementation

{$IFNDEF LCL}
{$R *.DFM}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TVisualsEditor.FormShow(Sender: TObject);
begin
  Visuals:=TVisualsList(Tag);
  EnableClear;
end;

procedure TVisualsEditor.TemplateChanged(Sender:TObject);
begin
  Visuals.Clear;
end;

procedure TVisualsEditor.EnableClear;
begin
  BClearVisual.Enabled:=Assigned(Visuals) and Assigned(Visuals.Template);

  if BClearVisual.Enabled then
  begin
    if not Assigned(IEditor) then
    begin
      IEditor:=TeeVisualEditorClass.Create(Self);
      IEditor.Align:=alClient;
      TTeeVCL.AddFormTo(IEditor,Self,Visuals.Template);

      IEditor.OnDirty:=TemplateChanged;
    end
    else
      IEditor.RefreshBlock(Visuals.Template);
  end;
end;

procedure TVisualsEditor.Button3Click(Sender: TObject);
var tmpVisual : TVisualBlock;
    tmpNew    : TVisualBlock;
begin
  if Assigned(TeeOnChangeVisual) then
  begin
    tmpVisual:=Visuals.Template;

    tmpNew:=TeeOnChangeVisual(Self,Visuals.Series,tmpVisual);

    if Assigned(tmpNew) then
    begin
      tmpVisual.Free;

      Visuals.Template:=tmpNew;

      if Assigned(FOnChangedVisual) then
         FOnChangedVisual(Self);

      EnableClear;
    end;
  end;
end;

procedure TVisualsEditor.BClearVisualClick(Sender: TObject);
begin
  Visuals.Template:=nil;

  if Assigned(FOnChangedVisual) then
     FOnChangedVisual(Self);

  BClearVisual.Enabled:=False;

  FreeAndNil(IEditor);
end;

class function TVisualsEditor.ModalShow(AOwner: TComponent;
  AVisuals: TVisualsList): Boolean;
begin
  with TVisualsEditor.Create(AOwner) do
  try
    Tag:=ObjectToTag(AVisuals);
    result:=ShowModal=mrOk;
  finally
    Free;
  end;
end;

end.
