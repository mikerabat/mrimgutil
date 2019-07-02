// ###################################################################
// #### This file is part of the mrimageutils project, depends on 
// #### the mathematics library project and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2014, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit AAMMatrixExt;

// ##########################################################
// #### extensions for the TDoubleMatrix class
// #### with usefull functions for Active Appearance Models
// ##########################################################

interface

uses SysUtils, Matrix;

// ##########################################################
// #### Matrix extension class:
type
  TAAMMatrix = class(TDoubleMatrix)
  public
    procedure SubVectorInPlace(vec : TDoubleMatrix; rowWise : boolean);
  end;
  TAAMMatrixDynArr = Array of TAAMMatrix;


procedure ZeroMeanUnitLength(texture : TDoubleMatrix; meanOnly : boolean = False);

implementation

uses Types;

procedure ZeroMeanUnitLength(texture : TDoubleMatrix; meanOnly : boolean = False);
var actTexture : TDoubleMatrix;
    help : TDoubleMatrix;
    y : integer;
    meanVal : double;
begin
     actTexture := TDoubleMatrix.Create;
     try
        actTexture.Assign(texture, True);

        // zero mean and unit length
        help := actTexture.Mean(False);
        try
           meanVal := help[0, 0];
           for y := 0 to actTexture.Height - 1 do
               actTexture[0, y] := actTexture[0, y] - meanVal;
        finally
               help.Free;
        end;
        if not meanOnly then
           actTexture.Normalize(False);
        texture.SetColumn(0, actTexture);
     finally
            actTexture.Free;
     end;
end;

{ TAAMMatrix }

type
  THackMatrix = class(TDoubleMatrix);

procedure TAAMMatrix.SubVectorInPlace(vec: TDoubleMatrix; rowWise: boolean);
var cnt, i : integer;
    pLine : PDouble;
    pVal : PDouble;
    pVec : PDouble;
    pVecVal : PDouble;
    vecLineWidth : integer;
    w, h : integer;
begin
     if rowWise then
     begin
          assert(vec.Width = Width, 'Dimension error');
          w := vec.Width - 1;

          // substract the given vector from each row
          pVec := THackMatrix(vec).StartElement;
          pLine := StartElement;
          for cnt := 0 to Height - 1 do
          begin
               pVecVal := pVec;
               pVal := pLine;

               for i := 0 to w do
               begin
                    pVal^ := pVal^ - pVecVal^;
                    inc(pVal);
                    inc(pVecVal);
               end;

               // next row
               inc(PByte(pLine), LineWidth);
          end;
     end
     else
     begin
          assert(vec.Height = Height, 'Dimension error');
          vecLineWidth := THackMatrix(vec).LineWidth;
          h := vec.Height - 1;

          // substract the given vector from each column
          pVec := THackMatrix(vec).StartElement;
          pLine := StartElement;
          for cnt := 0 to Width do
          begin
               pVecVal := pVec;
               pVal := pLine;

               for i := 0 to h do
               begin
                    pVal^ := pVal^ - pVecVal^;
                    inc(PByte(pVal), LineWidth);
                    inc(PByte(pVecVal), vecLineWidth);
               end;

               // next column
               inc(pLine);
          end;
     end;
end;

end.
