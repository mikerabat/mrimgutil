program ImgUtilsTests;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners fügen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardmäßig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestProcrustes in 'TestProcrustes.pas',
  Registration in '..\Registration.pas',
  RigidTransformation in '..\RigidTransformation.pas',
  PtsDefinitions in '..\PtsDefinitions.pas',
  Procrustes in '..\Procrustes.pas',
  BaseTransformationTest in 'BaseTransformationTest.pas',
  TestThinPlateSplines in 'TestThinPlateSplines.pas',
  DelaunyTriangulation in '..\DelaunyTriangulation.pas',
  TestDelaunyTriangulation in 'TestDelaunyTriangulation.pas',
  Triangulation in '..\Triangulation.pas',
  LinearTriangulationTransformation in '..\LinearTriangulationTransformation.pas',
  DelaunyMapping in '..\DelaunyMapping.pas',
  ImageMatrixConv in '..\ImageMatrixConv.pas',
  ThinPlateSplines in '..\ThinPlateSplines.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

