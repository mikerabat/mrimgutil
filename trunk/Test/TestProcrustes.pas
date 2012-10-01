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

unit TestProcrustes;

interface

uses
  TestFramework, Registration, Procrustes, RigidTransformation, Matrix,
  BaseTransformationTest;

// ##################################################
// #### Test the procrustres method
type
 TestTProcrustes = class(TBaseTransformationCase)
 published
   procedure TestCreatePointMapping;
 end;

implementation

uses SysUtils, Classes, PtsDefinitions;

procedure TestTProcrustes.TestCreatePointMapping;
var toPts: TPtsMappingObj;
    fromPts: TPtsMappingObj;
    pts : TDoubleMatrix;
    procrustes : TProcrustes;
    mappedPts : TPtsMappingObj;
    i : integer;
begin
     pts := CreatePts(1, 0, 0, 0);

     with TStringList.Create do
     try
        decimalseparator := '.';
        for i := 0 to pts.Height - 1 do
            Add(Format('%.3f,%.3f', [pts[0,i], pts[1,i]]));

        SaveToFile('D:\pts1.txt');//,TEncoding.ASCII);
     finally
            Free;
     end;
     toPts := TPtsMappingObj.Create(pts);
     pts.Free;
     pts := CreatePts(2.5, 0.3, 10, 10);
     fromPts := TPtsMappingObj.Create(pts);


     with TStringList.Create do
     try
        decimalseparator := '.';
        for i := 0 to pts.Height - 1 do
            Add(Format('%.3f,%.3f', [pts[0,i], pts[1,i]]));

        SaveToFile('D:\pts2.txt');//,TEncoding.ASCII);
     finally
            Free;
     end;
     pts.Free;



     mappedPts := nil;
     procrustes := TProcrustes.Create;
     try
        procrustes.CreatePointMapping(fromPts, toPts);
        mappedPts := procrustes.MapPoints(fromPts);

        Check(ComparePts(toPts, mappedPts), 'Procrustes failed.');
     finally
            procrustes.Free;
            mappedPts.Free;
            toPts.Free;
            fromPts.Free;
     end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTProcrustes.Suite);
end.

