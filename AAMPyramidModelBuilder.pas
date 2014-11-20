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

unit AAMPyramidModelBuilder;

// #############################################################
// ##### Pyramid aam levels
// #############################################################

interface

uses AAMModelBuilder, Matrix, BaseAAMModel, AAMPyramidModel;

type
  TCustomPyramidAAMParams = class(TCustomAAMModelParams)
  private
    fNumLevels: integer;
    fChangeScaleDisturbancePerLevel : boolean;
  public
    property NumLevels : integer read fNumLevels write fNumLevels;
    property ChangeScaleDisturbancePerLevel : boolean read fChangeScaleDisturbancePerLevel write fChangeScaleDisturbancePerLevel;

    constructor Create;
    destructor Destroy; override;
  end;

// create a pyramid of aam models
type
  TPyramidAAMBuilder = class(TCustomAAMModelBuilder)
  private
    fActStep : integer;
    fRefClass : TCustomAAMModelBuilderClass;
    fModelClass : TCustomPyramidAAMModelClass;
    function GetParams: TCustomPyramidAAMParams;
    procedure AAMModelProgress(Sender : TObject; overall, stepProgress : integer; step : TAAMStep);
  public
    procedure SetParams(newParams : TCustomAAMModelParams); override;
    property Params : TCustomPyramidAAMParams read GetParams;
    
    function BuildAAModel(const Shapes : TDoubleMatrixDynArr; const TextureFileDir : String) : TCustomAAMModel; override;

    constructor Create(RefModelClass : TCustomAAMModelBuilderClass; modelClass : TCustomPyramidAAMModelClass);
  end;

implementation

uses SysUtils, AAMConst;

const cLevelFactor = 1.5; 

{ TPyramidAAMBuilder }

procedure TPyramidAAMBuilder.AAMModelProgress(Sender : TObject; overall, stepProgress: integer;
  step: TAAMStep);
begin
     DoProgress((fActStep*100 + overall) div Params.NumLevels, overall, step);
end;

function TPyramidAAMBuilder.BuildAAModel(const Shapes: TDoubleMatrixDynArr;
  const TextureFileDir: String): TCustomAAMModel;
var models : TCustomAMMModelArr;
    i : Integer;
    ParamsOrigScale : double;
    modelBuilder : TCustomAAMModelBuilder;
begin
     assert(Assigned(fParams), 'Error - Assign the params first');

     SetLength(models, Params.NumLevels);
     ParamsOrigScale := fParams.AppearanceParams.AppearanceScale;

     // ##########################################################
     // #### build the models pyramids
     try
        fActStep := 0;
        for i := 0 to Params.NumLevels - 1 do
        begin
             modelBuilder := fRefClass.Create;
             try
                modelBuilder.Progress := AAMModelProgress;
                modelBuilder.SetParams(fParams);

                models[i] := modelBuilder.BuildAAModel(Shapes, TextureFileDir);

                fParams.AppearanceParams.AppearanceScale := Round(fParams.AppearanceParams.AppearanceScale/cLevelFactor);
                fParams.LearnParams.ScaleDisturbancePercentage := fParams.LearnParams.ScaleDisturbancePercentage*2;
             finally
                    modelBuilder.Free;
             end;

             inc(fActStep);
        end;
     finally
            fParams.AppearanceParams.AppearanceScale := ParamsOrigScale;
     end;

     // ############################################################
     // #### Create model builder
     Result := fModelClass.Create(Models);
end;

constructor TPyramidAAMBuilder.Create(RefModelClass: TCustomAAMModelBuilderClass;
  modelClass : TCustomPyramidAAMModelClass);
begin
     fRefClass := RefModelClass;
     fModelClass := modelClass;

     inherited Create;
end;

function TPyramidAAMBuilder.GetParams: TCustomPyramidAAMParams;
begin
     Result := fParams as TCustomPyramidAAMParams;
end;

procedure TPyramidAAMBuilder.SetParams(newParams: TCustomAAMModelParams);
begin
     if not (newParams is TCustomPyramidAAMParams) then
        raise Exception.Create('Error - params must be from type TCustomAAMModelParams');
        
     inherited;
end;

{ TCustomPyramidAAMParams }

constructor TCustomPyramidAAMParams.Create;
begin
     fNumLevels := cDefNumLevels;
     fChangeScaleDisturbancePerLevel := cDefChangeScaleDisturbancePerLevel;

     inherited Create;
end;

destructor TCustomPyramidAAMParams.Destroy;
begin
     inherited;
end;

end.
