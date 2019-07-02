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

unit BaseTransformationTest;

interface

uses TestFramework, Registration, Procrustes, RigidTransformation, Matrix;

// ##################################################
// #### Common function for transformation testing
type
 TBaseTransformationCase = class(TTestCase)
 protected
   // creates a hexagon according to the params
   function CreatePts(const scale, phi, dx, dy : double) : TDoubleMatrix;

   function ComparePts(pts1, pts2 : TPtsMappingObj) : boolean;
 end;

implementation

uses SysUtils, Math;

{ TestTProcrustes }

function TBaseTransformationCase.ComparePts(pts1,
  pts2: TPtsMappingObj): boolean;
var p1, p2 : TDoubleMatrix;
    x, y : integer;
begin
     p1 := pts1.PtsAsMtx;
     p2 := pts2.PtsAsMtx;

     try
        Result := (p1.Width = p2.Width) and (p1.Height = p2.Height);

        if Result then
        begin
             for y := 0 to p1.Height - 1 do
             begin
                  for x := 0 to p1.Width - 1 do
                      Result := Result and (CompareValue(p1[x, y], p2[x, y], 1e-8) = 0);
             end;
        end;
     finally
            p1.Free;
            p2.Free;
     end;
end;

function TBaseTransformationCase.CreatePts(const scale, phi, dx, dy: double): TDoubleMatrix;
var y : integer;
    angle : double;
begin
     Result := TDoubleMatrix.Create(2, 6);

     for y := 0 to Result.Height - 1 do
     begin
          angle := 2*pi/Result.height*y + phi;

          Result[0, y] := dx + scale*sin(angle);
          Result[1, y] := dy + scale*cos(angle);
     end;
end;

end.
