// ###################################################################
// #### This file is part of the mrimageutils project, depends on
// #### the mathematics library project and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2012, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

program ImgUtilsTests;

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
  ThinPlateSplines in '..\ThinPlateSplines.pas',
  ImageFilter in '..\ImageFilter.pas',
  ImageResize in '..\ImageResize.pas',
  TestImageFilter in 'TestImageFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

