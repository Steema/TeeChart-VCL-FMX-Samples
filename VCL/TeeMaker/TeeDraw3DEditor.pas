{**********************************************}
{   TeeChart Pro Component Library             }
{   TDraw3D editor dialog                      }
{                                              }
{   Copyright (c) 2001-2026 by Steema Software }
{   All Rights Reserved                        }
{**********************************************}
unit TeeDraw3DEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs;

type
  TDraw3DEditor = class(TForm)
    PageControl1: TPageControl;
    Tab3D: TTabSheet;
    TabPanel: TTabSheet;
    TabExport: TTabSheet;
    TabPrint: TTabSheet;
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    Panel : TCustomTeePanelExtended;
    procedure BroadcastOnShow(ATab:TTabSheet);
  public
    { Public declarations }
    class procedure Edit(AOwner:TComponent; APanel:TCustomTeePanelExtended);
  end;

implementation

{$R *.dfm}

uses
  TeeEdiPane, TeeEdi3D, TeeExport, TeePrevi, TeePenDlg, TeeEditCha;

type
  TCustomFormAccess=class(TCustomForm);

class procedure TDraw3DEditor.Edit(AOwner:TComponent; APanel:TCustomTeePanelExtended);

  procedure AddFormToPage(tmpForm:TForm; APage:TTabSheet);
  begin
    with TCustomFormAccess(tmpForm) do
    begin
      {$IFNDEF CLX}
      {$IFDEF D7}
      ParentBackground:=True;
      {$ENDIF}
      {$ENDIF}

      ParentColor:=True;
    end;

    { show the form... }
    With tmpForm do
    begin
      BorderIcons:=[];
      BorderStyle:=TeeFormBorderStyle;
      Align:=alClient;

      Parent:=APage;

      {$IFDEF CLX}
      TeeFixParentedForm(tmpForm);
      {$ENDIF}

      TTeeVCL.ScaleForm(tmpForm);

      Show;
      Realign;  // Fixes Axis tab align bug in VCL
    end;

    TeeTranslateControl(tmpForm);
  end;

var tmp : TDraw3DEditor;
    tmpExport : TTeeExportFormBase;
    tmpPreview : TChartPreview;
    tmp3D : TFormTee3D;
begin
  tmp:=TDraw3DEditor.Create(AOwner);

  with tmp do
  try
    Panel:=APanel;

    AddFormToPage(TFormTeePanel.CreatePanel(tmp,Panel),TabPanel);

    tmp3D:=TFormTee3D.Create(tmp);
    tmp3D.Tag:=ObjectToTag(Panel);

    AddFormToPage(tmp3D,Tab3D);
    tmp3D.CheckControls;

    tmpExport:=TTeeExportFormBase.Create(tmp);
    TTeeVCL.AddFormTo(tmpExport,TabExport,Panel);

    with tmpExport do
    begin
      Align:=alClient;
      BClose.Visible:=False;
    end;

    tmpPreview:=TChartPreview.Create(tmp);
    with tmpPreview do
    begin
      HideCloseButton;
      Align:=alClient;
    end;
    TTeeVCL.AddFormTo(tmpPreview,TabPrint,Panel);

    BroadCastOnShow(nil);

    ShowModal;
  finally
    Free;
  end;
end;

procedure TDraw3DEditor.BroadcastOnShow(ATab:TTabSheet);
var t : Integer;
begin
  if Assigned(TeeOnShowEditor) then
     for t:=0 to TeeOnShowEditor.Count-1 do
         TTeeOnCreateEditor(TeeOnShowEditor[t])(Self,Panel,ATab,PageControl1);
end;

procedure TDraw3DEditor.PageControl1Change(Sender: TObject);
begin
  BroadcastOnShow(PageControl1.ActivePage);
end;

end.
