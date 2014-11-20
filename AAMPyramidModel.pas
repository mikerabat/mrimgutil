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

unit AAMPyramidModel;

// #########################################################
// #### Pyramid model alignment
// #########################################################

interface

uses SysUtils, BaseAAMModel, Matrix, BaseMathPersistence;

// #########################################################
// #### Pyramid level implementation
type
  TCustomAMMModelArr = Array of TCustomAAMModel;
  TCustomPyramidAAMModel = class(TCustomAAMModel)
  private
    fActIdx : integer;
    fNumLevels : integer;
    fModels : TCustomAMMModelArr;
    function GetModel(index: integer): TCustomAAMModel;
  protected
    procedure DefineProps; override;
    procedure OnLoadBeginList(const Name : String; count : integer); override;
    function OnLoadObject(Obj : TBaseMathPersistence) : boolean; override;
    procedure OnLoadEndList; override;
  public
    property NumLevels : integer read fNumLevels;
    property Models[index : integer] : TCustomAAMModel read GetModel;
    
    constructor Create(Models : TCustomAMMModelArr);
    destructor Destroy; override;
  end;
  TCustomPyramidAAMModelClass = class of TCustomPyramidAAMModel;

type
  TPyramidAAMModel = class(TCustomPyramidAAMModel)
  private
    fNumIter : integer;

    function MapParams(actStep: integer; nextStep : integer; params: TDoubleMatrix): TDoubleMatrix;
    procedure Iteration(Sender : TObject; step : integer; P : TDoubleMatrix; StepImg : TDoubleMatrix);
  protected
    function GetLearnCombinedEnergy: double; override;
    function GetLearnImgScale: double; override;
    function GetLearnScaleDisturbPerc: double; override;
    function GetLearnShapeEnergy: double; override;
    function GetLearnStdDev: double; override;
    function GetLearnTextureDisturbPerc: double; override;
    function GetLearnTextureEnergy: double; override;

    function GetBrightness: double; override;
    function GetContrast: double; override;
    function GetEigVal(index: integer): double;  override;
    function GetMeanShape: TDoubleMatrix;  override;
    function GetMeanTexture: TDoubleMatrix; override;
    function GetNumEigVals: integer;  override;

    class function ClassIdentifier : String; override;
  public
    // params are taken from the largest model!
    property TextureContrast : double read GetContrast;
    property TextureBrightness : double read GetBrightness;
    property MeanTexture : TDoubleMatrix read GetMeanTexture;
    property MeanShape : TDoubleMatrix read GetMeanShape;
    property EigVals[index : integer] : double read GetEigVal;
    property NumEigVals : integer read GetNumEigVals;

    // optimization assumes that the initial params are taken from the largest model
    function Optimize(Img : TDoubleMatrix; InitParams : TDoubleMatrix) : TDoubleMatrix; override;

    // learning properties
    property LearnInitScale : double read GetLearnImgScale;
    property LearnScaleDisturb : double read GetLearnScaleDisturbPerc;
    property LearnTextureDisturb : double read GetLearnTextureDisturbPerc;
    property LearnParamStdDev : double read GetLearnStdDev;
    property LearnTextureEnergy : double read GetLearnTextureEnergy;
    property LearnShapeEnergy : double read GetLearnShapeEnergy;
    property LearnCombinedEnergy : double read GetLearnCombinedEnergy;

    function TextureFromP(const p : TDoubleMatrix) : TDoubleMatrix; override;
    procedure TextureFromPInImg(img : TDoubleMatrix; const p : TDoubleMatrix); override;
    function RawTextureFromP(const p : TDoubleMatrix; AddMean : boolean) : TDoubleMatrix; override;
    function ShapeFromP(const P: TDoubleMatrix): TDoubleMatrix; override;
  end;

implementation

uses ImageResize, AAMConst, ImageMatrixConv;

{ TPyramidAAMModel }

const cPyramidAAMModels = 'PAAModels';



constructor TCustomPyramidAAMModel.Create(Models: TCustomAMMModelArr);
begin
     fModels := Models;
     fNumLevels := Length(Models);
     fNumColorPlanes := Models[0].NumColorPlanes;
     TextureWidth := Models[0].TextureWidth;
     TextureHeight := Models[0].TextureHeight;

     inherited Create;
end;

procedure TCustomPyramidAAMModel.DefineProps;
var i: Integer;
begin
     inherited;

     BeginList(cPyramidAAMModels, fNumLevels);
     for i := 0 to fNumLevels - 1 do
         AddObject(fModels[i]);
     EndList;
end;

destructor TCustomPyramidAAMModel.Destroy;
var i : Integer;
begin
     for i := 0 to fNumLevels - 1 do
         fModels[i].Free;
         
     inherited;
end;

function TCustomPyramidAAMModel.GetModel(index: integer): TCustomAAMModel;
begin
     assert((index >= 0) and (index < Length(fModels)), 'Index out of bounds');
     Result := fModels[index];
end;

procedure TCustomPyramidAAMModel.OnLoadBeginList(const Name: String;
  count: integer);
begin
     fActIdx := -1;

     if CompareText(Name, cPyramidAAMModels) = 0 then
     begin
          SetLength(fModels, count);
          fNumLevels := count;
          fActIdx := 0;
     end
     else
         inherited;
end;

procedure TCustomPyramidAAMModel.OnLoadEndList;
begin
     fActIdx := -1;

     inherited;
end;

function TCustomPyramidAAMModel.OnLoadObject(
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if fActIdx >= 0 then
     begin
          fModels[fActIdx] := obj as TCustomAAMModel;
          inc(fActIdx);
     end
     else
         Result := inherited OnLoadObject(Obj);
end;

class function TPyramidAAMModel.ClassIdentifier: String;
begin
     Result := 'PyramidAAM';
end;

function TPyramidAAMModel.GetBrightness: double;
begin
     Result := Models[0].TextureBrightness;
end;

function TPyramidAAMModel.GetContrast: double;
begin
     Result := Models[0].TextureContrast;
end;

function TPyramidAAMModel.GetEigVal(index: integer): double;
begin
     Result := Models[0].EigVals[index];
end;

function TPyramidAAMModel.GetLearnCombinedEnergy: double;
begin
     Result := Models[0].LearnCombinedEnergy;
end;

function TPyramidAAMModel.GetLearnImgScale: double;
begin
     Result := Models[0].LearnInitScale;
end;

function TPyramidAAMModel.GetLearnScaleDisturbPerc: double;
begin
     Result := Models[0].LearnScaleDisturb;
end;

function TPyramidAAMModel.GetLearnShapeEnergy: double;
begin
     Result := Models[0].LearnShapeEnergy;
end;

function TPyramidAAMModel.GetLearnStdDev: double;
begin
     Result := Models[0].LearnParamStdDev;
end;

function TPyramidAAMModel.GetLearnTextureDisturbPerc: double;
begin
     Result := Models[0].LearnTextureDisturb;
end;

function TPyramidAAMModel.GetLearnTextureEnergy: double;
begin
     Result := Models[0].LearnTextureEnergy;
end;

function TPyramidAAMModel.GetMeanShape: TDoubleMatrix;
begin
     Result := Models[0].MeanShape;
end;

function TPyramidAAMModel.GetMeanTexture: TDoubleMatrix;
begin
     Result := Models[0].MeanTexture;
end;

function TPyramidAAMModel.GetNumEigVals: integer;
begin
     Result := Models[0].NumEigVals;
end;

procedure TPyramidAAMModel.Iteration(Sender: TObject; step: integer; P,
  StepImg: TDoubleMatrix);
begin
     if Assigned(OnIterationStep) then
        OnIterationStep(Self, fNumIter, p, StepImg);
        
     inc(fNumIter);
end;

function TPyramidAAMModel.MapParams(actStep : integer; nextStep : integer; params : TDoubleMatrix) : TDoubleMatrix;
var img : TDoubleMatrix;
    resizedImg : TDoubleMatrix;
    p : TDoubleMatrix;
    y : Integer;
begin
     assert(actStep > 0, 'Index error');

     // ###################################################################
     // #### Get texture-> map to next level -> map back.
     // note: since there is no relationship between the param in different levels
     // we need this procedure.
     img := Models[actStep].RawTextureFromP(params, False);
     try
        resizedImg := ImgResize(TBilinearImageResize, img, Models[nextStep].NumColorPlanes, Models[nextStep].TextureWidth, Models[nextStep].TextureHeight);

        try
           resizedImg.ReshapeInPlace(1, resizedImg.Width*resizedImg.Height);
           p := Models[nextStep].PFromRawTexture(resizedImg, False);
        finally
               resizedImg.Free;
        end;

        try
           // and copy the pose params:
           Result := TDoubleMatrix.Create(1, p.Height + cNumPoseParams);
           Result.SetSubMatrix(0, 0, 1, p.Height);
           Result.SetColumn(0, p);
           params.UseFullMatrix;
           Result.UseFullMatrix;
           for y := 1 to cNumPoseParams do
               Result[0, Result.Height - y] := params[0, params.Height - y];
        finally
               p.Free;
        end;
     finally
            img.Free;
     end;
end;

function TPyramidAAMModel.Optimize(Img,
  InitParams: TDoubleMatrix): TDoubleMatrix;
var modelCnt: Integer;
    actParams : TDoubleMatrix;
    help : TDoubleMatrix;
    isZero : boolean;
    counter : integer;
    y : integer;
begin
     fNumIter := 0;
     fNumIterations := 0;
     for modelCnt := 0 to fNumLevels - 1 do
     begin
          if Assigned(OnIterationStep)
          then
              Models[modelCnt].OnIterationStep := Iteration
          else
              Models[modelCnt].OnIterationStep := nil;
     end;
     // ########################################################
     // #### base aam pyramid optimization
     // map the params from the highest to the lowest level first!

     // check if the params are not all set to zero -> we can then short cut
     // the mapping!
     isZero := True;
     for counter := 0 to InitParams.Height - cNumPoseParams - 1 do
         isZero := isZero and (InitParams[0, counter] = 0);

     if not isZero
     then
         actParams := MapParams(0, fNumLevels - 1, InitParams)
     else
     begin
          actParams := TDoubleMatrix.Create(1, Models[fNumLevels - 1].NumEigVals + cNumPoseParams);
          for y := 0 to cNumPoseParams - 1 do
              actParams[0, actParams.Height - 1 - y] := InitParams[0, InitParams.Height - 1 - y];
     end;

     // ########################################################
     // #### Go through all models an optimize them
     for modelCnt := fNumLevels - 1 downto 0 do
     begin
          help := Models[modelCnt].Optimize(Img, actParams);
          try
             inc(fNumIterations, Models[modelCnt].NumOptimizationIters);
             actParams.Free;
             if modelCnt > 0
             then
                 actParams := MapParams(modelCnt, modelCnt - 1, help)
             else
             begin
                  actParams := help;
                  help := nil;
             end;
          finally
                 help.Free;
          end;
     end;

     Result := actParams;
end;

function TPyramidAAMModel.RawTextureFromP(
  const p: TDoubleMatrix; AddMean : boolean): TDoubleMatrix;
begin
     assert(fNumLevels > 0, 'Error no models assigned');

     Result := Models[0].RawTextureFromP(p, AddMean);
end;

function TPyramidAAMModel.TextureFromP(const p: TDoubleMatrix): TDoubleMatrix;
begin
     Result := Models[0].TextureFromP(p);
end;

function TPyramidAAMModel.ShapeFromP(const P: TDoubleMatrix): TDoubleMatrix;
begin
     Result := Models[0].ShapeFromP(p);
end;

procedure TPyramidAAMModel.TextureFromPInImg(img: TDoubleMatrix;
  const p: TDoubleMatrix);
begin
     Models[0].TextureFromPInImg(img, p);
end;

initialization
  RegisterMathIO(TPyramidAAMModel);

end.
