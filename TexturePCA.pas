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

unit TexturePCA;

// ################################################################
// #### Texture component analysis
// ################################################################

interface

uses SysUtils, PCA, AAMAppearanceBuilder, Matrix, IncrementalPCA;

// ###############################################################
// #### Implementation using a standard pca
type
  TPCAAAMTextureParams = class(TCustomAAMAppearanceBuilderParams)
  private
    fPCA : TIncrementalPCA;
  public
    procedure PCAOnTexture(Textures : TDoubleMatrix; var PCA : TMatrixPCA); override;

    procedure InitIncrementalPCA(var PCA : TMatrixPCA); override;
    procedure ApplyIncrementalPCAStep(const Texture : TDoubleMatrix; const Shape : TDoubleMatrix); override;
  end;

implementation

{ TPCAAAMShapeParams }

procedure TPCAAAMTextureParams.ApplyIncrementalPCAStep(const Texture,
  Shape: TDoubleMatrix);
begin
     fPCA.UpdateEigenspace(Texture);
end;

procedure TPCAAAMTextureParams.InitIncrementalPCA(var PCA: TMatrixPCA);
begin
     fPCA := TIncrementalPCA.Create([pcaEigVals]);
     fPCA.OnProgress := PCAProgress;
     PCA := fPCA;
end;

procedure TPCAAAMTextureParams.PCAOnTexture(Textures : TDoubleMatrix;
  var PCA: TMatrixPCA);
begin
     PCA := TMatrixPCA.Create([pcaEigVals]);
     PCA.OnProgress := PCAProgress;
     PCA.PCA(Textures, ModelEnergy, True);
end;

end.
