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

unit AAMModelBuilder;

// ############################################################
// #### Combined Shape/Appearance Model
// ############################################################

interface

uses SysUtils, Classes, AAMShapeBuilder, AAMAppearanceBuilder, Matrix, PCA, AAMWarp,
     Types, BaseAAMModel, AAMModel;

type
  TLearnParams = class(TObject)
    NumStdDeviations : double;
    NumIterSteps : integer;
    ScaleDisturbancePercentage : double;
    TextureDisturbancePercentage : double;
    CombinedModelEnergy : double;
    PcaProgress : TPCAProgress;
    debugPath : string;

    constructor Create;
  end;
type
  TCustomAAMModelParams = class(TObject)
  protected
    fShapeParams: TCustomAAMShapeBuilderParams;
    fAppearanceParams: TCustomAAMAppearanceBuilderParams;
    fLearnParams: TLearnParams;
    procedure SetAppearanceParams(const Value: TCustomAAMAppearanceBuilderParams);
  public
    property ShapeParams : TCustomAAMShapeBuilderParams read fShapeParams write fShapeParams;
    property LearnParams : TLearnParams read fLearnParams write fLearnParams;
    property AppearanceParams : TCustomAAMAppearanceBuilderParams read fAppearanceParams write SetAppearanceParams;

    procedure PCAOnCombinedModel(Model : TDoubleMatrix; var PCA : TMatrixPCA); virtual; 

    constructor Create;
    destructor Destroy; override;
  end;

// #############################################################
// #### Building a combined model such that textures and
// shapes can be built from one set of params
type
  TAAMStep = (asBuildShape, asBuildTexture, asBuildCombined, asLearn, asFinalize);
  TAAMBuildProgress = procedure(Sender : TObject; overallProgress : integer; stepProgress : integer; step : TAAMStep) of object;
type
  TCustomAAMModelBuilder = class(TObject)
  protected
    fParams : TCustomAAMModelParams;
    fProgress : TAAMBuildProgress;

    procedure DoProgress(overall, stepProgress : integer; step : TAAMStep);
  public
    procedure SetParams(newParams : TCustomAAMModelParams); virtual;

    function BuildAAModel(const Shapes : TDoubleMatrixDynArr; const TextureFileDir : String) : TCustomAAMModel; virtual; abstract;

    property Progress : TAAMBuildProgress read fProgress write fProgress;
  end;
  TCustomAAMModelBuilderClass = class of TCustomAAMModelBuilder;
type
  TAAMModelBuilder = class(TCustomAAMModelBuilder)
  private
    fShape : TAAMShapeBuilder;
    fTexture : TAAMAppearanceBuilder;

    fPCA : TMatrixPCA;
    fShapeCoords : TDoubleMatrixDynArr;
    fTexturePath : string;
    fModel : TAAMModel;

    fMeanShape : TDoubleMatrix;
    fR : TDoubleMatrix;
    fMask : TBooleanDynArray;
    fQgInv : TDoubleMatrix;

    function CreateWeights : TDoubleDynArray;
    procedure OnImgLearnLoad(Sender : TObject; mtx : TDoubleMatrix; actNum, NumImages : integer; const FileName : string);
    function ShapeModelScalingFunction : double;
    procedure InternalBuildCombinedModel;
    procedure LearningModelParams;
    procedure SaveNormalizedImg(const fileName : string; Img: TDoubleMatrix; brightness: Double; contrast: Double);
    function GetParams: TCustomAAMModelParams;

    procedure ShapeProgress(Sender : TObject; progress : integer);
    procedure TextureProgress(Sender : TObject; progress : integer);
    procedure CombinedProgess(Sender : TObject; progress : integer);
    procedure RInvertProgress(progress : integer);
  public
    property Params : TCustomAAMModelParams read GetParams;

    function BuildAAModel(const Shapes : TDoubleMatrixDynArr; const TextureFileDir : String) : TCustomAAMModel; override;

    destructor Destroy; override;
  end;

implementation

uses MatrixImageLists, Procrustes, Registration, LinearAlgebraicEquations, AAMConst,
     AAMMatrixExt, ImageMatrixConv, graphics, MatrixConst;

{ TAAMModelBuilder }

function TAAMModelBuilder.BuildAAModel(const Shapes: TDoubleMatrixDynArr; const TextureFileDir: String) : TCustomAAMModel;
var learnParams : TLearnParams;
begin
     fShapeCoords := Shapes;
     fTexturePath := TextureFileDir;

     // ##########################################################
     // #### Build separate models - shape and appearance
     fShape := TAAMShapeBuilder.Create;
     fShape.OnProgress := ShapeProgress;
     fShape.BuildShapeModel(fParams.ShapeParams, Shapes);

     fTexture := TAAMAppearanceBuilder.Create;
     fTexture.OnProgress := TextureProgress;
     fTexture.BuildAppearanceModel(fParams.AppearanceParams, fShape, TextureFileDir);
     fMask := fTexture.NormTextureMask;

     fModel := TAAMModel.Create(fParams.AppearanceParams.PreprocessingStep);

     // ##########################################################
     // ##### Build combined model
     learnParams := fParams.LearnParams;
     learnParams.PcaProgress := CombinedProgess;
     fParams.LearnParams := learnParams;
     InternalBuildCombinedModel;
     learnParams.PcaProgress := nil;
     fParams.LearnParams := learnParams;

     fModel.SetLearnParams(fParams.LearnParams.NumStdDeviations, fParams.LearnParams.ScaleDisturbancePercentage,
                           fParams.LearnParams.TextureDisturbancePercentage,
                           fParams.AppearanceParams.ModelEnergy, fParams.ShapeParams.ModelEnergy,
                           fParams.LearnParams.CombinedModelEnergy, fParams.AppearanceParams.AppearanceScale);

     Result := fModel;
     fModel := nil;

     DoProgress(100, 100, asFinalize);
end;

procedure TAAMModelBuilder.CombinedProgess(Sender: TObject; progress: integer);
begin
     DoProgress(40 + progress div 10, progress, asBuildCombined);
end;

function TAAMModelBuilder.CreateWeights: TDoubleDynArray;
var counter : integer;
    sum : double;
begin
     SetLength(Result, fParams.LearnParams.NumIterSteps*2 + 1);

     sum := 0;
     for counter := 0 to Length(Result) - 1 do
     begin
          Result[counter] := Exp( -0.5 * 1.5 * sqr( (counter - fParams.LearnParams.NumIterSteps)/(fParams.LearnParams.NumIterSteps/2) ));
          sum := sum + Result[counter];
     end;

     for counter := 0 to Length(Result) - 1 do
         Result[counter] := Result[counter]/sum;

     //for counter := 0 to Length(Result) - 1 do
//         Result[counter] := 1; //1/Length(Result);
end;

destructor TAAMModelBuilder.Destroy;
begin
     fModel.Free;
     fShape.Free;
     fTexture.Free;

     inherited;
end;

function TAAMModelBuilder.GetParams: TCustomAAMModelParams;
begin
     Result := fParams as TCustomAAMModelParams;
end;

procedure TAAMModelBuilder.SaveNormalizedImg(const fileName : string; Img: TDoubleMatrix; brightness: Double; contrast: Double);
var x: TDoubleMatrix;
begin
     x := TDoubleMatrix.Create;
     try
        x.Assign(Img);
        x.AddAndScaleInPlace(0, contrast);
        x.AddAndScaleInPlace(brightness, 1);
        x.ReshapeInPlace(fTexture.NormTextureWidth, fTexture.NormTextureHeight);
        with TMatrixImageConverter.ConvertImage(x, ctGrayScale) do
        try
           SaveToFile(fileName);
        finally
               free;
        end;
     finally
            x.Free;
     end;
end;

procedure TAAMModelBuilder.InternalBuildCombinedModel;
var shapeScale : double;
    combinedMatrix : TDoubleMatrix;
    i : Integer;
    p : TDoubleMatrix;
    Qs : TDoubleMatrix;
    Qg : TDoubleMatrix;
    help : TDoubleMatrix;
    texture : TDoubleMatrix;
begin
     // ##########################################################
     // #### combined model from shape + texture subspaces
     shapeScale := ShapeModelScalingFunction;

     // map all examples to the feature space -> build the combined features matrix
     combinedMatrix := TDoubleMatrix.Create(Length(fShapeCoords), fShape.NumModes + fTexture.NumModes);
     try
        // map shapes
        combinedMatrix.SetSubMatrix(0, 0, combinedMatrix.Width, fShape.NumModes);
        for i := 0 to Length(fShapeCoords) - 1 do
        begin
             p := fShape.PFromShape(fShape.AlignedShapes[i]);
             try
                p.ScaleInPlace(shapeScale);

                combinedMatrix.SetColumn(i, p);
             finally
                    p.Free;
             end;
        end;

        // map textures
        combinedMatrix.SetSubMatrix(0, fShape.NumModes, combinedMatrix.Width, fTexture.NumModes);
        for i := 0 to Length(fShapeCoords) - 1 do
        begin
             p := fTexture.PFromNormTexture(i);
             try
                combinedMatrix.SetColumn(i, p);
             finally
                    p.Free;
             end;
        end;

        // apply pca on the combined data
        combinedMatrix.UseFullMatrix;
        fParams.PCAOnCombinedModel(combinedMatrix, fPCA);
     finally
            combinedMatrix.Free;
     end;
     // ###########################################################
     // #### Build shape and appearance matrices and store other internals

     // combined matrix = (Pcs; Pcg);

     // qs = Ps*Ws^-1*Pcs
     fPCA.EigVecs.SetSubMatrix(0, 0, fPCA.EigVecs.Width, fShape.NumModes);

     Qs := fShape.Model.EigVecs.Scale(1/shapeScale);
     Qs.MultInPlace(fPCA.EigVecs);

     // Qg = Pg*Pcg
     fPCA.EigVecs.UseFullMatrix;
     fPCA.EigVecs.SetSubMatrix(0, fShape.NumModes, fPCA.EigVecs.Width, fPCA.EigVecs.Height - fShape.NumModes);
     Qg := fTexture.Model.EigVecs.Mult(fPCA.EigVecs);

     assert(Qs.Width = Qg.Width, 'Dimension error');

     // fill model object with the base properties
     fModel.SetShapeParams(Qs, fShape.MeanShape, True);

     help := TDoubleMatrix.Create;
     help.Assign(fTexture.NormTextureMeanShape);
     texture := TDoubleMatrix.Create;
     texture.Assign(fTexture.Model.Mean);
     fModel.SetTextureParams(Qg, fParams.AppearanceParams.WarpClass, help, texture,
                             fTexture.NormTextureWidth, fTexture.NormTextureHeight,
                             fTexture.MeanContrast, fTexture.MeanBrightness,
                             fTexture.NormTextureMask, True, fTexture.NumColorPlanes);
     fModel.SetEigVals(fPCA.EigVals);

     FreeAndNil(fPCA);

     // ###########################################################
     // #### Learn model disturbances -> num images x num model params
     LearningModelParams;
end;

procedure TAAMModelBuilder.LearningModelParams;
var imgList : TIncrementalImageList;
begin
     fR := nil;
     imgList := TIncrementalImageList.Create;
     try
        imgList.OnImageStep := OnImgLearnLoad;

        fR := TDoubleMatrix.Create(fModel.NumEigVals + cNumPoseParams, fModel.Qg.Height);
        // note: we can't invert by transposition - Qg seems not to be orthogonal
        if fModel.Qg.PseudoInversion(fQgInv) <> srOk then
           raise EBaseMatrixException.Create('Error could not invert Qg');
        fMeanShape := fShape.MeanShape;
        try
           // load images -> and perform the learning steps
           imgList.ReadListFromDirectoryRaw(fTexturePath, fParams.AppearanceParams.ImgLoadType);
        finally
               fQgInv.Free;
               fMeanShape.Free;
        end;

        // ###############################################################
        // #### Last step - inversion of R (pseudoinvert)
        fR.LineEQProgress := RInvertProgress;
        if fR.PseudoInversionInPlace <> srOk then
           raise EBaseMatrixException.Create('Error - could not learn model disturbance. Inversion failed');
        fR.ScaleInPlace(-1);

        fModel.SetDisturbanceMatrix(fR, True);
        fR := nil;
     finally
            fR.Free;
            imgList.Free;
     end;
end;

procedure TAAMModelBuilder.OnImgLearnLoad(Sender: TObject; mtx: TDoubleMatrix;
  actNum, NumImages: integer; const FileName : string);
var
    normMtx : TDoubleMatrix;
    brightness : double;
    contrast : double;
    optParams : TDoubleMatrix;
    p : TDoubleMatrix;
    mapping : TDoubleMatrix;
    paramCnt : integer;
    disturbCnt : integer;
    weights : TDoubleDynArray;
    curParam : TDoubleMatrix;
    optDifference : TDoubleMatrix;
    disturbImg : TDoubleMatrix;
    row : TDoubleMatrix;
    initShape : TDoubleMatrix;
begin
     optParams := nil;
     try
        // apply a preprocessing step -> change it's features (e.g. highpass -> harris corener ...)
        if Assigned(fParams.AppearanceParams.PreprocessingStep) then
           fParams.AppearanceParams.PreprocessingStep(Self, mtx, fTexture.NumColorPlanes);

        // ###################################################################
        // #### first get the optimal model param for the current example

        // warp image -> put it into the normalized frame
        initShape := fShape.OrigShapesMtx[actNum];
        normMtx := fModel.NormalizedImg(mtx, initShape, False);
        try
           normMtx.ReshapeInPlace(fTexture.NormTextureWidth, fTexture.NormTextureHeight*fTexture.NumColorPlanes);
           normMtx.ReshapeInPlace(1, normMtx.Width*normMtx.Height);

           brightness := fTexture.Brightness[actNum];
           contrast := fTexture.Contrast[actNum];
           normMtx.AddAndScaleInPlace(-brightness, 1/contrast);
           normMtx.SubInPlace(fModel.MeanTexture);

           // calculate optimal model params (from the texture!)
           p := fQgInv.Mult(normMtx);
           try
              // add the shape transformation params
              optParams := TDoubleMatrix.Create(1, p.Height + cNumPoseParams);
              optParams.SetSubMatrix(0, 0, 1, p.Height);
              optParams.SetColumn(0, p);

              optParams.SetSubMatrix(0, p.Height,1, cNumPoseParams);
              optParams[0, cNumPoseParams - 2] := brightness;
              optParams[0, cNumPoseParams - 1] := contrast - 1;
           finally
                  p.Free;
           end;
        finally
               normMtx.Free;
        end;

        // translation and scale params:
        mapping := TProcrustes.CreateMappingMatrix(fMeanShape, fShape.OrigShapesMtx[actNum]);
        try
           optParams[0, 0] := 1 + mapping[0, 0];
           optParams[0, 1] := mapping[0, 1];
           optParams[0, 2] := Mapping[2, 0];
           optParams[0, 3] := Mapping[2, 1];
        finally
               mapping.Free;
        end;
        optParams.UseFullMatrix;

        // ###############################################################
        // #### Create mapping from the optimal params -> used as reference
        optDifference := fModel.CreateDiffImgFromParams(mtx, optParams);
        try
           // ###############################################################
           // #### disturb params and caculate delta(p)
           // disturb model params
           weights := CreateWeights;
           for paramCnt := 0 to optParams.Height - 1 do
           begin
                row := TDoubleMatrix.Create(1, optDifference.Height);
                try
                   // disturb the model params
                   for disturbCnt := -fParams.LearnParams.NumIterSteps to fParams.LearnParams.NumIterSteps do
                   begin
                        if disturbCnt = 0 then
                           continue;
                           
                        curParam := TDoubleMatrix.Create(1, optParams.Height);
                        try
                           // different handling for pose and model params
                           if paramCnt < optParams.Height - cNumPoseParams
                           then
                               curParam[0, paramCnt] := disturbCnt/fParams.LearnParams.NumIterSteps*
                                                        fParams.LearnParams.NumStdDeviations*
                                                        sqrt(fModel.EigVals[paramCnt])
                           else if paramCnt <= optParams.Height - 2
                           then
                               curParam[0, paramCnt] := disturbCnt/fParams.LearnParams.NumIterSteps*
                                                        fParams.LearnParams.ScaleDisturbancePercentage*
                                                        optParams[0, paramCnt]
                           else
                               curParam[0, paramCnt] := disturbCnt/fParams.LearnParams.NumIterSteps*
                                                        fParams.LearnParams.TextureDisturbancePercentage*
                                                        optParams[0, paramCnt];

                           curParam.AddInplace(optParams);

                           // now create the image (in the normalized frame)
                           disturbImg := fModel.CreateDiffImgFromParams(mtx, curParam);
                           try
                              if fParams.LearnParams.debugPath <> '' then
                                 SaveNormalizedImg(IncludeTrailingPathDelimiter(fParams.LearnParams.debugPath) + 'diff_' + IntToStr(actNum) +
                                                   '_' + IntToStr(paramCnt) + '_' + IntToStr(disturbCnt) + '.bmp',
                                                   disturbImg, brightness, contrast);

                              disturbImg.SubInPlace(optDifference);
                              disturbImg.ScaleInPlace(weights[disturbCnt + fParams.LearnParams.NumIterSteps]/(curParam[0, paramCnt] - optParams[0, paramCnt]));

                              // add to the current row
                              row.AddInplace(disturbImg);
                           finally
                                  disturbImg.Free;
                           end;
                        finally
                               curParam.Free;
                        end;
                   end;

                   // final step: add the row to the R matrix
                   row.ScaleInPlace(1/NumImages);
                   fR.SetSubMatrix(paramCnt, 0, 1, fR.Height);
                   fR.AddInplace(row);
                finally
                       row.Free;
                end;
           end;
        finally
               optDifference.Free;
        end;

        fR.UseFullMatrix;
     finally
            optParams.Free;
     end;

     DoProgress(50 + 40*actNum div numImages, 100*actNum div numImages, asLearn);
end;

procedure TAAMModelBuilder.RInvertProgress(progress: integer);
begin
     DoProgress(90 + 9*progress div 100, progress, asFinalize);
end;

function TAAMModelBuilder.ShapeModelScalingFunction: double;
begin
     // Cootes et. al.: r^2 = ratio of total intensity variation to total shape variation
     Result := sqrt(fTexture.EigValSum/fShape.EigValSum);
end;

procedure TAAMModelBuilder.ShapeProgress(Sender: TObject; progress: integer);
begin
     DoProgress(progress div 10, progress, asBuildShape);
end;

procedure TAAMModelBuilder.TextureProgress(Sender: TObject; progress: integer);
begin
     DoProgress(10 + 30*progress div 100, progress, asBuildTexture);
end;

{ TCustomAAMModelParams }

constructor TCustomAAMModelParams.Create;
begin
end;

destructor TCustomAAMModelParams.Destroy;
begin
     ShapeParams.Free;
     AppearanceParams.Free;
     
     inherited;
end;

procedure TCustomAAMModelParams.PCAOnCombinedModel(Model: TDoubleMatrix;
  var PCA: TMatrixPCA);
begin
     // do nothing here.
end;

procedure TCustomAAMModelParams.SetAppearanceParams(
  const Value: TCustomAAMAppearanceBuilderParams);
begin
     fAppearanceParams := Value;

     fAppearanceParams.debugPath := LearnParams.debugPath;
end;

{ TCustomAAMModelBuilder }

procedure TCustomAAMModelBuilder.DoProgress(overall, stepProgress: integer;
  step: TAAMStep);
begin
     if Assigned(fProgress) then
        fProgress(self, overall, stepProgress, step);
end;

procedure TCustomAAMModelBuilder.SetParams(newParams: TCustomAAMModelParams);
begin
     fParams := newParams;
end;

{ TLearnParams }

constructor TLearnParams.Create;
begin
     NumStdDeviations := cDefNumStdDeviations;
     NumIterSteps := cNumIterSteps;
     ScaleDisturbancePercentage := cScaleDisturbance;
     TextureDisturbancePercentage := cTextureDisturbance;
     CombinedModelEnergy := cDefCombinedModelEnergy;

end;

end.
