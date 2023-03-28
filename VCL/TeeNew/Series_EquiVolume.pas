unit Series_EquiVolume;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls;

type
  TEquiVolumeSeriesForm = class(TBaseForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  RegisterClass(TEquiVolumeSeriesForm);
end.
