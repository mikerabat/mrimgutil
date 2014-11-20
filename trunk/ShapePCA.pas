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

unit ShapePCA;

// ###############################################################
// #### Implementations of the Shape Params object
// ###############################################################

interface

uses SysUtils, PCA, AAMShapeBuilder, Matrix;

// ###############################################################
// #### Implementation using a standard pca
type
  TPCAAAMShapeParams = class(TCustomAAMShapeBuilderParams)
  public
    procedure PCAOnShapes(const Shapes : TDoubleMatrix; var PCA : TMatrixPCA); override;
  end;

implementation

{ TPCAAAMShapeParams }

procedure TPCAAAMShapeParams.PCAOnShapes(const Shapes: TDoubleMatrix;
  var PCA: TMatrixPCA);
begin
     PCA := TMatrixPCA.Create([pcaEigVals]);
     PCA.OnProgress := PCAProgress;
     PCA.PCA(Shapes, ModelEnergy, True);
end;

end.
