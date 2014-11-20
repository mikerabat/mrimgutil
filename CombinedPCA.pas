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

unit CombinedPCA;

// ################################################################
// #### Combined model pca analysis
// ################################################################

interface

uses SysUtils, PCA, AAMModelBuilder, Matrix, AAMPyramidModelBuilder;

// ###############################################################
// #### Implementation using a standard pca
type
  TPCAAAMCombinedParams = class(TCustomAAMModelParams)
  public
    procedure PCAOnCombinedModel(Model : TDoubleMatrix; var PCA : TMatrixPCA); override;
  end;

type
  TPCAAAMPyramidCombinedParams = class(TCustomPyramidAAMParams)
  public
    procedure PCAOnCombinedModel(Model : TDoubleMatrix; var PCA : TMatrixPCA); override;
  end;

implementation

{ TPCAAAMShapeParams }

procedure TPCAAAMCombinedParams.PCAOnCombinedModel(Model : TDoubleMatrix; var PCA : TMatrixPCA);
begin
     PCA := TMatrixPCA.Create([pcaEigVals]);
     PCA.OnProgress := LearnParams.PcaProgress;
     PCA.PCA(Model, LearnParams.CombinedModelEnergy, True);
end;

{ TPCAAAMPyramidCombinedParams }

procedure TPCAAAMPyramidCombinedParams.PCAOnCombinedModel(Model: TDoubleMatrix;
  var PCA: TMatrixPCA);
begin
     PCA := TMatrixPCA.Create([pcaEigVals]);
     PCA.OnProgress := LearnParams.PcaProgress;
     PCA.PCA(Model, LearnParams.CombinedModelEnergy, True);
end;

end.
