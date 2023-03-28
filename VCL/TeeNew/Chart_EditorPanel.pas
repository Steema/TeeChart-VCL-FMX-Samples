unit Chart_EditorPanel;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeEdit, TeeProcs, TeEngine, Chart, Series, TeeTools;

type
  TChartEditorPanelForm = class(TBaseForm)
    Series1: THorizLineSeries;
    Splitter1: TSplitter;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    ChartTool1: TMarksTipTool;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    ChartEditorPanel1: TChartEditorPanel;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TChartEditorPanelForm.FormCreate(Sender: TObject);
begin
  inherited;

  ChartEditorPanel1:=TChartEditorPanel.Create(Self);
  ChartEditorPanel1.Parent:=Self;
  ChartEditorPanel1.Align:=alClient;
  ChartEditorPanel1.Chart:=Chart1;

  // Hide the editor Tree view so the tabs can be aligned to bottom, etc:
  ChartEditorPanel1.Editor.Tree.Hide;
  
  Series1.FillSampleValues(10);
end;

procedure TChartEditorPanelForm.ComboBox1Change(Sender: TObject);
begin
  { change the tabs position }
  With ChartEditorPanel1.Editor.MainPage do
  Case ComboBox1.ItemIndex of
    0: TabPosition:=tpTop;
    1: begin
         Style:=tsTabs;
         TabPosition:=tpBottom;

         ComboBox2.ItemIndex:=0;
       end;
    2: begin
         Style:=tsTabs;
         TabPosition:=tpLeft;

         ComboBox2.ItemIndex:=0;
       end;
    3: begin
         Style:=tsTabs;
         TabPosition:=tpRight;

         ComboBox2.ItemIndex:=0;
       end;
  end;
end;

procedure TChartEditorPanelForm.ComboBox2Change(Sender: TObject);
begin
  { change the tabs style }
  With ChartEditorPanel1.Editor.MainPage do
  Case ComboBox2.ItemIndex of
    0: Style:=tsTabs;
    1: begin
         TabPosition:=tpTop;
         Style:=tsButtons;

         ComboBox1.ItemIndex:=0;  // Only allowed at Top
       end;
    2: begin
         TabPosition:=tpTop;
         Style:=tsFlatButtons;

         ComboBox1.ItemIndex:=0;  // Only allowed at Top
       end;
  end;
end;

procedure TChartEditorPanelForm.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  { On mouse down, show the appropiate tab in the editor... }
  ChartEditorPanel1.SelectUnderMouse;
end;

procedure TChartEditorPanelForm.FormShow(Sender: TObject);
begin
  inherited;
  ComboBox1.ItemIndex:=0;
  ComboBox2.ItemIndex:=0;
end;

initialization
  RegisterClass(TChartEditorPanelForm);
end.

