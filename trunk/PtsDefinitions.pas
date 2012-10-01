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

unit PtsDefinitions;

// ##########################################################
// #### Base definitions to simplify the use of coordinates
// ##########################################################

interface

uses SysUtils, Types, Matrix;

{$Align 8}

type
  EDimensionError = class(Exception);

// base points definition
type
  TPointf1D = double;
  TDynPointf1dArray = Array of TPointf1D;
  TDynDynPointf1dArray = Array of TDynPointf1dArray;
  TPointf2D = record
    x : double;
    y : double;
  end;
  TPointf2Ds = record
     x : single;
     y : single;
  end;
  PPointf2Ds = ^TPointf2Ds;
  TDynPointf2DArray = Array of TPointf2D;
  TDynPointf2DArrays = Array of TPointf2Ds;

  TDynDynPointf2DArray = Array of TDynPointf2DArray;
  TPointf3D = record
    z : double;
    case boolean of
      True: ( pt : TPointf2D;
            );
      False: ( x : double;
               y : double;
             );
  end;
  TDynPointf3dArray = Array of TPointf3D;
  TDynDynPointf3dArray = Array of TDynPointf3dArray;

// homogen coordinates defnitions
type
  TPointf2Dh = record
    x : double;
    y : double;
    h : double;   // normally 1 
  end;

type
  TTriangleDef = Array[0..2] of integer;   // indices of vertices (TPointf2D)
  PTriangleDef = ^TTriangleDef;
  TTriangles = Array of TTriangleDef;      // Triangulation
  TTriangleNeighbour = TTriangleDef;
  TVertexToTriangles = Array of Array of integer; // Triangles attached to a vertex
  TTriNeighbours = Array of TTriangleNeighbour;
  TTrianglePts = Array[0..2] of TPointf2D;

function PointsToMatrix(const pts : TDynPointf1dArray) : TDoubleMatrix; overload;
function PointsToMatrix(const pts : TDynPointf2dArray) : TDoubleMatrix; overload;
function PointsToMatrix(const pts : TDynPointf3dArray) : TDoubleMatrix; overload;
function PointsToMatrix(const pts : TDynPointf2dArrays) : TDoubleMatrix; overload;
function MatrixToPoints(matrix : TDoubleMatrix) : TDynPointf2DArray;
function MatrixToPointsS(matrix : TDoubleMatrix) : TDynPointf2DArrayS;

implementation

function PointsToMatrix(const pts : TDynPointf1dArray) : TDoubleMatrix;
var i: Integer;
begin
     Result := nil;
     if Length(pts) > 0 then
     begin
          Result := TDoubleMatrix.Create(1, Length(pts));
          for i := 0 to Result.Height - 1 do
              Result[0, i] := pts[i];
     end;
end;

function PointsToMatrix(const pts : TDynPointf2dArray) : TDoubleMatrix;
var i: Integer;
begin
     Result := nil;
     if Length(pts) > 0 then
     begin
          Result := TDoubleMatrix.Create(2, Length(pts));
          for i := 0 to Result.Height - 1 do
          begin
               Result[0, i] := pts[i].x;
               Result[1, i] := pts[i].y;
          end;
     end;
end;

function PointsToMatrix(const pts : TDynPointf2dArrays) : TDoubleMatrix; overload;
var i: Integer;
begin
     Result := nil;
     if Length(pts) > 0 then
     begin
          Result := TDoubleMatrix.Create(2, Length(pts));
          for i := 0 to Result.Height - 1 do
          begin
               Result[0, i] := pts[i].x;
               Result[1, i] := pts[i].y;
          end;
     end;
end;

function MatrixToPoints(matrix : TDoubleMatrix) : TDynPointf2DArray;
var i: Integer;
begin
     assert(matrix.Width = 2, 'Error only 2 dimensions are allowed in this function');

     SetLength(Result, matrix.Height);

     for i := 0 to matrix.Height - 1 do
     begin
          Result[i].x := matrix[0, i];
          Result[i].y := matrix[1, i];
     end;
end;

function MatrixToPointsS(matrix : TDoubleMatrix) : TDynPointf2DArrayS;
var i: Integer;
begin
     assert(matrix.Width = 2, 'Error only 2 dimensions are allowed in this function');

     SetLength(Result, matrix.Height);

     for i := 0 to matrix.Height - 1 do
     begin
          Result[i].x := matrix[0, i];
          Result[i].y := matrix[1, i];
     end;
end;

function PointsToMatrix(const pts : TDynPointf3dArray) : TDoubleMatrix;
var i: Integer;
begin
     Result := nil;
     if Length(pts) > 0 then
     begin
          Result := TDoubleMatrix.Create(3, Length(pts));
          for i := 0 to Result.Height - 1 do
          begin
               Result[0, i] := pts[i].x;
               Result[1, i] := pts[i].y;
               Result[2, i] := pts[i].z;
          end;
     end;
end;

end.
