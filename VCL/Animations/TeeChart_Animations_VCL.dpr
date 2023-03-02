program TeeChart_Animations_VCL;

uses
  Forms,
  Unit_Test_Animations in 'Unit_Test_Animations.pas' {AnimationTests};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TeeChart Pro VCL Test ALL';
  Application.CreateForm(TAnimationTests, AnimationTests);
  Application.Run;
end.
