unit Unit_Pro;

{
  This unit detects if we are using the "Pro" TeeChart versior or not.
}

interface

uses
  VCLTee.TeeConst, VCLTee.TeeProcs;

procedure TryAdding_ChartEditors(const AChart:TCustomTeePanel; const AParent:TObject);

implementation

{$IFDEF FPC}
{$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
{$ELSE}
{$IF TeeMsg_TeeChartPalette='TeeChart'}
{$DEFINE TEEPRO} // <-- TeeChart Lite or Pro ?
{$ENDIF}
{$ENDIF}

{$IFDEF TEEPRO}
uses
  VCLTee.TeeComma, VCLTee.TeePNG, VCL.Controls;

procedure TryAdding_ChartEditors(const AChart:TCustomTeePanel; const AParent:TObject);
var c : TTeeCommander;
    tmp : TWinControl;
begin
  tmp:=AParent as TWinControl;

  c:=TTeeCommander.Create(tmp);
  c.Parent:=tmp;
  c.Panel:=AChart;
  c.Align:=alTop;
end;
{$ELSE}

procedure TryAdding_ChartEditors;
begin
end;

{$ENDIF}

end.
