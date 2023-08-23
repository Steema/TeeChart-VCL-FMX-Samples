program TeeMaker;
{$I TeeDefs.inc}

(*
  Note: After adding or removing units or forms to this project,
        you should manually verify the first used units are the ones
        listed below in that order and with that IFDEF conditionals:

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
*)

uses
  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}
  Forms,
  TeeMakerEditor in 'TeeMakerEditor.pas' {MakerEditor},
  TeeBlocks in 'TeeBlocks.pas',
  TeeDraw3DEditor in 'TeeDraw3DEditor.pas' {Draw3DEditor},
  TeeTextureSelector in 'TeeTextureSelector.pas' {TextureSelector},
  TeeBlockEditor in 'TeeBlockEditor.pas' {BlockEditor},
  TeeRain in 'TeeRain.pas',
  TeeMakerControl in 'TeeMakerControl.pas',
  TeeStairs in 'TeeStairs.pas' {StairsEditor},
  TeeRevolution in 'TeeRevolution.pas' {RevolutionEditor},
  TeeRevolutionEditor in 'TeeRevolutionEditor.pas' {RevolutionPointsEditor},
  TeeChartBlock in 'TeeChartBlock.pas',
  TeeExtrudedEditor in 'TeeExtrudedEditor.pas' {ExtrudedEditor},
  TeeLoadBlock in 'TeeLoadBlock.pas' {LoadBlockDialog},
  TeeExtruded in 'TeeExtruded.pas',
  TeePlayMP3 in 'TeePlayMP3.pas',
  TeePipe in 'TeePipe.pas',
  TeeLoadError in 'TeeLoadError.pas' {AskLoadError},
  TeeRoundRect in 'TeeRoundRect.pas' {RoundRectEditor},
  TeeBlockGallery in 'TeeBlockGallery.pas' {BlockGallery},
  TeeBlockFormat in 'TeeBlockFormat.pas' {BlockFormatEditor},
  TeeEdgeStyle in 'TeeEdgeStyle.pas' {EdgeEditor},
  TeeTerrain in 'TeeTerrain.pas' {TerrainEditor},
  TeeHelix in 'TeeHelix.pas' {HelixEditor},
  TeeMakerLibrary in 'TeeMakerLibrary.pas' {MakerLibrary},
  TeeWater in 'TeeWater.pas' {WaterEditor},
  TeeActionGallery in 'TeeActionGallery.pas' {ActionGallery},
  TeeSelectProperty in 'TeeSelectProperty.pas' {PropertySelector},
  TeeMesh in 'TeeMesh.pas' {MeshEditor},
  TeeMakerMain in 'TeeMakerMain.pas' {MakerEditor1},
  TeeMakerConst in 'TeeMakerConst.pas',
  TeeProperties in 'TeeProperties.pas' {PropertiesEditor},
  TeeColorPalette in 'TeeColorPalette.pas' {ColorPalette},
  TeeBlockReplacer in 'TeeBlockReplacer.pas' {BlockReplacer},
  TeeNumberAnimation in 'TeeNumberAnimation.pas' {NumberAnimationEditor},
  TeeBlockAnimations in 'TeeBlockAnimations.pas',
  TeeObjFormat in 'TeeObjFormat.pas' {ObjBlockEditor},
  TeeKinematics in 'TeeKinematics.pas' {KinematicsEditor},
  TeeViewBlock in 'TeeViewBlock.pas' {ViewBlockEditor},
  TeeCamera in 'TeeCamera.pas' {CameraEditor},
  TeeFacesBlock in 'TeeFacesBlock.pas' {FacesBlockEditor},
  TeeClipBlock in 'TeeClipBlock.pas' {ClipBlockEditor},
  TeeTorusEditor in 'TeeTorusEditor.pas' {TorusEditor},
  TeeTapeEditor in 'TeeTapeEditor.pas' {TapeBlockEditor},
  Tee3DSFormat in 'Tee3DSFormat.pas' {Block3DSEditor},
  TeeGLSLShaders in 'TeeGLSLShaders.pas' {Form1},
  TeeSubdivideMesh in 'TeeSubdivideMesh.pas' {SubDivideEditor};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TeeMaker';
  {$IFDEF D11}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TMakerEditor1, MakerEditor1);
  TrimWorkingSet;
  Application.Run;
end.
