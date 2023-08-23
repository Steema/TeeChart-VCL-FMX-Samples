unit TeeProperties;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, 
  SysUtils, 
  {$IFDEF D6}
  Variants, 
  {$ENDIF}
  Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QButtons, QStdCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls,
  {$ENDIF}
  TeeBlocks;

type
  TPropertiesEditor = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    BRemove: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Button3: TButton;
    ListProperties: TListBox;
    Panel4: TPanel;
    Label1: TLabel;
    EName: TEdit;
    Label2: TLabel;
    EValue: TEdit;
    SpeedButton1: TSpeedButton;
    Label3: TLabel;
    LValue: TLabel;
    BChange: TButton;
    procedure BRemoveClick(Sender: TObject);
    procedure ListPropertiesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BChangeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure EValueChange(Sender: TObject);
  private
    { Private declarations }

    procedure VerifyName(const AName:String; AIndex:Integer);
  public
    { Public declarations }
    Properties : TObjectProperties;

    class function ModalShow(AOwner:TComponent; AProperties:TObjectProperties):Boolean;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeProcs, TeePenDlg, TeeMakerConst, TeeBlockEditor, TeeMakerControl;

class function TPropertiesEditor.ModalShow(AOwner:TComponent; AProperties:TObjectProperties):Boolean;
begin
  with TPropertiesEditor.Create(AOwner) do
  try
    Properties:=AProperties;
    ShowModal;

    result:=True;
  finally
    Free;
  end;
end;

procedure TPropertiesEditor.BRemoveClick(Sender: TObject);
var tmp : Integer;
begin
  if TeeYesNo(Format(TeeMsg_SureToDeleteProperty,[EName.Text])) then
  begin
    tmp:=ListProperties.ItemIndex;
    ListProperties.Items.Delete(tmp);
    Properties.Delete(tmp);

    if tmp>=Properties.Count-1 then
       Dec(tmp);

    ListProperties.ItemIndex:=tmp;
    ListPropertiesClick(Self);
  end;
end;

procedure TPropertiesEditor.ListPropertiesClick(Sender: TObject);
var ALeft  : String;
    ARight : String;
begin
  if ListProperties.ItemIndex=-1 then
  begin
    EName.Text:='';
    EValue.Text:='';
    LValue.Caption:='';
    BChange.Enabled:=False;
  end
  else
  begin
    SplitValue(Properties[ListProperties.ItemIndex],ALeft,ARight);
    EName.Text:=ALeft;
    EValue.Text:=ARight;
    LValue.Caption:=Properties.Value[ALeft];
    BChange.Enabled:=True;
  end;

  EnableControls(ListProperties.ItemIndex<>-1,[EName,EValue,SpeedButton1,BRemove]);
end;

procedure TPropertiesEditor.VerifyName(const AName:String; AIndex:Integer);
var tmp : Integer;
begin
  if AName='' then
     Raise Exception.Create(TeeMsg_PropertyCannotBeEmpty)
  else
  begin
    tmp:=Properties.IndexOfProperty(AName);

    if (tmp<>-1) and (tmp<>AIndex) then
       Raise Exception.Create(TeeMsg_PropertyAlreadyExists);
  end;
end;

procedure TPropertiesEditor.Button1Click(Sender: TObject);
var tmp : String;
begin
  tmp:='';

  if InputQuery(TeeMsg_NewProperty,TeeMsg_Property,tmp) then
  begin
    tmp:=Trim(tmp);

    VerifyName(tmp,-1);

    Properties.Add(tmp);
    ListProperties.Items.Add(tmp);

    ListProperties.ItemIndex:=ListProperties.Items.Count-1;
    ListPropertiesClick(Self);
  end;
end;

procedure TPropertiesEditor.BChangeClick(Sender: TObject);
var ALeft  : String;
    ARight : String;
    tmp    : Integer;
begin
  tmp:=ListProperties.ItemIndex;
  SplitValue(Properties[tmp],ALeft,ARight);

  if InputQuery(TeeMsg_ChangeProperty,TeeMsg_Property,ALeft) then
  begin
    ALeft:=Trim(ALeft);

    VerifyName(ALeft,tmp);

    Properties[tmp]:=ALeft+'='+ARight;
    ListProperties.Items[tmp]:=ALeft;
  end;
end;

procedure TPropertiesEditor.FormShow(Sender: TObject);
var t : Integer;
    ALeft  : String;
    ARight : String;
begin
  ListProperties.Items.Clear;

  t:=0;

  if Assigned(Properties) then 
  while t<Properties.Count do
  begin
    SplitValue(Properties[t],ALeft,ARight);

    ALeft:=Trim(ALeft);

    if ALeft='' then
       Properties.Delete(t)
    else
    begin
      ListProperties.Items.Add(ALeft);

      Inc(t);
    end;
  end;

  if ListProperties.Items.Count>0 then
  begin
    ListProperties.ItemIndex:=0;
    ListPropertiesClick(Self);
  end;
end;

procedure TPropertiesEditor.SpeedButton1Click(Sender: TObject);
var AObject : TObject;
    AName   : String;
begin
  AName:='';
  AObject:=nil;

  if TMakerPropertySelector.ModalShow(Self,Properties.Blocks,AObject,AName) then
  begin
    if AObject is TComponent then
       AName:=TComponent(AObject).Name+'.'+AName;

    EValue.Text:=AName;
  end;
end;

procedure TPropertiesEditor.EValueChange(Sender: TObject);
var tmp     : Integer;
    ALeft   : String;
    ARight  : String;
begin
  tmp:=ListProperties.ItemIndex;
  SplitValue(Properties[tmp],ALeft,ARight);
  Properties[tmp]:=ALeft+'='+EValue.Text;

  LValue.Caption:=Properties.Value[ALeft];
end;

end.
