{********************************************}
{    TeeChart Pro Block Classes              }
{ Copyright (c) 2007-2024 by Steema Software }
{       All Rights Reserved                  }
{********************************************}
unit TeeBlockClasses;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,

  {$IFDEF D17}
  System.UITypes, System.Types,
  {$ENDIF}
 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  TeeProcs, TeeMakerControl, TeeAnimate, TeeBlocks, TeCanvas;

type
  TBlockChooser = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BOK: TButton;
    BCancel: TButton;
    LabelSelected: TLabel;
    PageControl1: TPageControl;
    TabBlocks: TTabSheet;
    Preferences: TTabSheet;
    Maker1: TMaker;
    RandomColors: TCheckBox;
    BlockColor: TButtonColor;
    Label1: TLabel;
    ComboTextures: TComboFlat;
    SpeedButton1: TSpeedButton;
    BlockBorders: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Maker1DblClick(Sender: TObject);
    procedure Maker1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1DblClick(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    Animation : TNumberAnimation;

    procedure AddBlocks(IsRandom,Borders:Boolean; AColor:TColor; const ATexture:String);
    procedure ApplyConfig;
    function SelectBlock(X,Y:Integer; DeselectBlock:Boolean=False):TCustomBlock;
  public
    { Public declarations }
    class function Choose:TBlockClass;
  end;

implementation

{$R *.dfm}

uses
  TeeMakerEditor, Registry, TeeTextureSelector, TeeViewBlock, TeeWater;

var
  GlobalChooser : TBlockChooser=nil;

type
  TBlockAccess=class(TCustomBlock);

class function TBlockChooser.Choose: TBlockClass;
begin
  if not Assigned(GlobalChooser) then
     GlobalChooser:=TBlockChooser.Create(nil);

  if GlobalChooser.ShowModal=mrOk then
     result:=TBlockClass(GlobalChooser.Maker1.Selected.ClassType)
  else
     result:=nil;
end;

procedure TBlockChooser.AddBlocks(IsRandom,Borders:Boolean; AColor:TColor; const ATexture:String);
var t,
    tmpIndex : Integer;
    tmp,
    tmpCols : Integer;
    tmpBlock : TCustomBlock;
begin
  Maker1.Blocks.Clear;

  with BlockClasses.Sorted do
  try
    tmp:=Round(Sqrt(Count));
    tmpCols:=1+(Count div tmp);

    for t:=0 to Count-1 do
    begin
      tmpIndex:=Integer(Objects[t]);

      tmpBlock:=BlockClasses[tmpIndex].Create(Self);
      Maker1.Blocks.Add(tmpBlock);
      TBlockAccess(tmpBlock).PrepareForGallery;

      with tmpBlock.Location.Point do
      begin
        X:=-400+(140*(t mod tmpCols));
        Z:=(140*(t div tmpCols));
      end;

      tmpBlock.Title:=Strings[t];

      if IsRandom then
         tmpBlock.Format.Color:=RGB(Random(255),Random(255),Random(255))
      else
         tmpBlock.Format.Color:=AColor;

      if ATexture<>'' then
         if not (tmpBlock is TCameraViewBlock) then
            tmpBlock.Format.Texture.PictureLink:=ATexture;

      tmpBlock.Format.Border.Visible:=Borders;

      if tmpBlock is TWaterBlock then
         TWaterBlock(tmpBlock).AutoPlay:=False;
    end;
  finally
    Free;
  end;

  Animation:=TNumberAnimation.Create(Self);

  with Animation do
  begin
    StartValue:=0;
    EndValue:=360;
    KeepEndValue:=True;
    PropertyName:='Rotation.X';
    Duration:=360;
  end;

  {
  with Maker1.Blocks.Animates.Add do
  begin
    Animations.Add(Animation);
    Loop:=True;
  end;
  }
end;

const
  ConfigKey=TeeMakerKey+'\BlockGallery';

procedure TBlockChooser.FormShow(Sender: TObject);

  procedure LoadConfig;
  begin
    with TRegistry.Create do
    try
      if OpenKeyReadOnly(ConfigKey) then
      begin
        if ValueExists('RandomColors') then
           RandomColors.Checked:=ReadBool('RandomColors');

        if ValueExists('Borders') then
           BlockBorders.Checked:=ReadBool('Borders');

        if ValueExists('BlockColor') then
           BlockColor.SymbolColor:=ReadInteger('BlockColor');

        if ValueExists('Texture') then
           ComboTextures.Text:=ReadString('Texture');
      end;
    finally
      Free;
    end;
  end;

begin
  if Maker1.Blocks.Count=0 then
  begin
    Maker1.AutoRepaint:=False;

    LoadConfig;

    Maker1.Options.BoundingBox:=True;
    Maker1.Options.ShowBoundPositions:=False;

    AddBlocks(RandomColors.Checked,BlockBorders.Checked,BlockColor.SymbolColor,ComboTextures.Text);

    Maker1.AutoRepaint:=True;
  end;

  if Maker1.CanFocus then
     Maker1.SetFocus;
end;

procedure TBlockChooser.FormCreate(Sender: TObject);
begin
  Maker1.View3DOptions.ZoomFloat:=30;
  Maker1.View3DOptions.VertOffsetFloat:=350;
  Maker1.Render.Light.Color:=clWhite;

  BlockColor.SymbolColor:=clWhite;
  ComboTextures.Items.Add(TeeMakerLibraryTag+'Pavement2.jpg');
end;

procedure TBlockChooser.Maker1DblClick(Sender: TObject);
begin
  with Maker1.GetCursorPos do
       Maker1.Selected:=Maker1.Blocks.ClickedBlock(X,Y,False,False);

  if Assigned(Maker1.Selected) then
     ModalResult:=mrOk;
end;

function TBlockChooser.SelectBlock(X,Y:Integer; DeselectBlock:Boolean=False):TCustomBlock;
begin
  result:=Maker1.Blocks.ClickedBlock(X,Y,False,False);

  if Assigned(result) then
  begin
    LabelSelected.Caption:=result.Title;
    Animation.Instance:=result;
    Animation.Value:=result.Rotation.X;

    if Assigned(Animation.Animate) then
       if not Animation.Animate.Playing then
          Animation.Animate.Play;
  end
  else
  if DeselectBlock then
  begin
    LabelSelected.Caption:='';
    Animation.Instance:=nil;

    if Assigned(Animation.Animate) then
       if Animation.Animate.Playing then
          Animation.Animate.Stop;
  end;
end;

procedure TBlockChooser.Maker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(SelectBlock(X,Y,True)) then
     Maker1.Cursor:=crHandPoint
  else
     Maker1.Cursor:=crDefault;

  Maker1.OriginalCursor:=Maker1.Cursor;
end;

procedure TBlockChooser.Maker1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    with Maker1.GetCursorPos do
         Maker1.Selected:=SelectBlock(X,Y);

    BOK.Enabled:=Assigned(Maker1.Selected);
  end;
end;

procedure TBlockChooser.Panel1DblClick(Sender: TObject);
begin
  TMakerEditor.ModalShow(Self,Maker1);
end;

procedure TBlockChooser.BOKClick(Sender: TObject);
begin
  if PageControl1.ActivePage=TabBlocks then
     ModalResult:=mrOk
  else
  begin
    ApplyConfig;
    PageControl1.ActivePage:=TabBlocks;
    Maker1.SetFocus;
  end;
end;

procedure TBlockChooser.BCancelClick(Sender: TObject);
begin
  if PageControl1.ActivePage=TabBlocks then
     ModalResult:=mrCancel
  else
  begin
    PageControl1.ActivePage:=TabBlocks;
    Maker1.SetFocus;
  end;
end;

procedure TBlockChooser.ApplyConfig;

  procedure SaveConfig;
  begin
    with TRegistry.Create do
    try
      if OpenKey(ConfigKey,True) then
      begin
        WriteBool('RandomColors',RandomColors.Checked);
        WriteBool('Borders',BlockBorders.Checked);
        WriteInteger('BlockColor',BlockColor.SymbolColor);
        WriteString('Texture',ComboTextures.Text);
      end;
    finally
      Free;
    end;
  end;

begin
  SaveConfig;
  AddBlocks(RandomColors.Checked,BlockBorders.Checked,BlockColor.SymbolColor,ComboTextures.Text);
end;

procedure TBlockChooser.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage=Preferences then
     BOK.Enabled:=True
  else
  begin
    ApplyConfig;
    BOK.Enabled:=Assigned(Maker1.Selected);
  end;
end;

procedure TBlockChooser.SpeedButton1Click(Sender: TObject);
var tmp : TBlockFormat;
    tmpSt : String;
begin
  tmp:=TBlockFormat.Create(nil);
  try
    tmp.Texture.PictureLink:=ComboTextures.Text;

    if TTextureSelector.ModalShow(Self,nil,tmp) then
    with ComboTextures do
    begin
      tmpSt:=tmp.Texture.PictureLink;

      if Items.IndexOf(tmpSt)=-1 then
         Items.Add(tmpSt);

      Text:=tmpSt;
    end;
  finally
    tmp.Free;
  end;
end;

initialization
finalization
  GlobalChooser.Free;
end.
