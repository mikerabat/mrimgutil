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

unit AAMConst;

// ###############################################################
// #### Base definitions for the AAM Modeling
// ###############################################################

interface

uses SysUtils, Matrix;

type
  EAAMException = class(Exception);
type
  TPreprocessingFunc = procedure(Sender : TObject; Texture : TDoubleMatrix; NumColorPlanes : integer) of object;
  TAAMProgress = procedure(Sender : TObject; progress : integer) of object;

// model learning standard parameters
const cDefNumStdDeviations = 0.5;  // see Statistical Models of Appearance page 45ff
      cNumPoseParams = 6;          // pose parameter length: sx, sy, tx, ty, u1, u2 (u1, u2 are contrast and brightness)
      cNumAffinePoseParams = 4;    // Inverse compositional only comes with sx, sy, tx, ty
      cNumIterSteps = 10;          // number of iterations used in the leraning step
      cScaleDisturbance = 0.1;     // +- 10% disturbance of the scale parameter
      cTextureDisturbance = 0.1;
      cTextureNumColorPlanes = 1;

      cDefShapeModelEnergy = 0.85; // keep 85% of the eigenvalue "energy"
      cDefTextureModelEnergy = 0.9;
      cDefCombinedModelEnergy = 0.95; 

      cDefNumLevels = 4;
      cDefChangeScaleDisturbancePerLevel = True;

      cDefNumAAMSearchIter = 20;

type
  TAAMProgressObj = class(TObject)
  private
    fOnProgress: TAAMProgress;
  protected
    procedure DoProgress(Progress : integer);
  public
    property OnProgress : TAAMProgress read fOnProgress write fOnProgress;
  end;

implementation

{ TProgressObj }

procedure TAAMProgressObj.DoProgress(Progress: integer);
begin
     if Assigned(fOnProgress) then
        fOnProgress(self, Progress);
end;

end.
