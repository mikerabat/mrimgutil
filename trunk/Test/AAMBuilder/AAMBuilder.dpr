program AAMBuilder;

uses
  Forms,
  ufrmBuildAAM in 'ufrmBuildAAM.pas' {frmAAMBuilder},
  AAMAppearanceBuilder in '..\..\AAMAppearanceBuilder.pas',
  AAMConst in '..\..\AAMConst.pas',
  AAMLinearWarp in '..\..\AAMLinearWarp.pas',
  AAMMatrixExt in '..\..\AAMMatrixExt.pas',
  AAMModel in '..\..\AAMModel.pas',
  AAMModelBuilder in '..\..\AAMModelBuilder.pas',
  AAMPyramidModel in '..\..\AAMPyramidModel.pas',
  AAMPyramidModelBuilder in '..\..\AAMPyramidModelBuilder.pas',
  AAMShapeBuilder in '..\..\AAMShapeBuilder.pas',
  AAMTPSWarp in '..\..\AAMTPSWarp.pas',
  AAMVisuals in '..\..\AAMVisuals.pas',
  AAMWarp in '..\..\AAMWarp.pas',
  BaseAAMModel in '..\..\BaseAAMModel.pas',
  CombinedPCA in '..\..\CombinedPCA.pas',
  ShapePCA in '..\..\ShapePCA.pas',
  TexturePCA in '..\..\TexturePCA.pas',
  ufrmAAMParams in 'ufrmAAMParams.pas' {frmModelParams},
  ufrmAAMOptimize in 'ufrmAAMOptimize.pas' {frmAAMOptimize};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAAMBuilder, frmAAMBuilder);
  Application.CreateForm(TfrmModelParams, frmModelParams);
  Application.CreateForm(TfrmAAMOptimize, frmAAMOptimize);
  Application.Run;
end.
