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

unit AAMAppearanceBuilder;

// ################################################################
// #### Implements the building blocks of an appearance model
// ################################################################

interface

uses SysUtils, Classes, AAMShapeBuilder, PtsDefinitions, Matrix, AAMWarp, PCA,
     ImageMatrixConv, Types, AAMConst;

type
  TCustomAAMAppearanceBuilderParams = class(TObject)
  public
    ShapeModel : TAAMShapeBuilder;
    AppearanceScale : Double;
    ImgLoadType : TImageConvType;
    WarpClass : TCustomAAMWarperClass;
    ModelEnergy : double;

    debugPath : string;

    // #########################################################
    // #### Preprocessing function (e.g. apply a high pass filter on the picture)
    PreprocessingStep : TPreprocessingFunc;
    PCAProgress : TPCAProgress;

    // #########################################################
    // #### PCA procedures - Incremental and batch
    procedure PCAOnTexture(Textures : TDoubleMatrix; var PCA : TMatrixPCA); virtual; abstract;

    procedure InitIncrementalPCA(var PCA : TMatrixPCA); virtual; abstract;
    procedure ApplyIncrementalPCAStep(const Texture : TDoubleMatrix; const Shape : TDoubleMatrix); virtual; abstract;

    constructor Create;
  end;

type
  TAAMAppearanceBuilder = class(TAAMProgressObj)
  private
    fParams : TCustomAAMAppearanceBuilderParams;
    fTextureImages : TDoubleMatrix;
    fShapeModel : TAAMShapeBuilder;
    fImgWarp : TCustomAAMWarper;
    fMeanShape : TDoubleMatrix;

    fMask : TBooleanDynArray;
    fAppearanceModel : TMatrixPCA;
    fAppearanceWidth,
    fAppearanceHeight : integer;
    fContrast: TDoubleDynArray;
    fBrightness: TDoubleDynArray;
    fMeanContrast: Double;
    fMeanBrightness: double;
    fNumColorPlanes: integer;

    procedure ImgLoadEvent(Sender : TObject; mtx : TDoubleMatrix; actNum, NumImages : integer; const FileName : string);
    procedure BuildModel;
    procedure NormalizeTextures;
    function GetNumModes: integer;
    function GetMaskArray: TBooleanDynArray;
    procedure CreateMask;
    procedure AlignTexturesToMeanTexture(TextureImages,
      meanTexture: TDoubleMatrix; var brightness, contrast: TDoubleDynArray);
    procedure UndoAlignment(Textures: TDoubleMatrix; const brightness, contrast: TDoubleDynArray);
    procedure SaveReconstructedImgs;

    procedure pcaProgress(Sender : TObject; progress : integer);
  public
    // ########################################################
    // #### Training data access
    property NumColorPlanes : integer read fNumColorPlanes;
    property NormalizedTextures : TDoubleMatrix read fTextureImages;
    property NormTextureWidth : integer read fAppearanceWidth;
    property NormTextureHeight : integer read fAppearanceHeight;
    property NormTextureMeanShape : TDoubleMatrix read fMeanShape;
    property NormTextureMask : TBooleanDynArray read GetMaskArray;
    property Contrast : TDoubleDynArray read fContrast;
    property Brightness : TDoubleDynArray read fBrightness;
    property MeanContrast : Double read fMeanContrast;
    property MeanBrightness : double read fMeanBrightness;

    function PFromNormTexture(i : integer) : TDoubleMatrix;

    // ########################################################
    // #### Shape constructing methods
    procedure BuildAppearanceModel(Params : TCustomAAMAppearanceBuilderParams; ShapeModel : TAAMShapeBuilder; const TextureFileDir : String); overload;
    procedure BuildAppearanceModel(Params : TCustomAAMAppearanceBuilderParams; ShapeModel : TAAMShapeBuilder; Textures : Array of TDoubleMatrix); overload;

    // ########################################################
    // #### creator functions
    property NumModes : integer read GetNumModes;

    function TextureFromP(P : TDoubleMatrix) : TDoubleMatrix;

    // helper function
    function EigValSum: double;
    function Model : TMatrixPCA;

    destructor Destroy; override;
  end;

implementation

uses MatrixImageLists, Math, Graphics, AAMMatrixExt, BinaryReaderWriter, OptimizedFuncs;

{ TAAMAppearance }

procedure TAAMAppearanceBuilder.BuildAppearanceModel(Params: TCustomAAMAppearanceBuilderParams;
  ShapeModel: TAAMShapeBuilder; const TextureFileDir: String);
var imgList : TIncrementalImageList;
begin
     DoProgress(0);

     fShapeModel := ShapeModel;
     fParams := Params;

     case fParams.ImgLoadType of
       ctGrayScale: fNumColorPlanes := 1;
       ctRGB: fNumColorPlanes := 3;
       ctHUV: fNumColorPlanes := 3;
       ctGrayScaleNorm: fNumColorPlanes := 1;
       ctRGBNorm: fNumColorPlanes := 3;
     end;

     // ########################################################
     // #### Load images incrementaly
     imgList := TIncrementalImageList.Create;
     try
        imgList.OnImageStep := ImgLoadEvent;
        imgList.ReadListFromDirectoryRaw(TextureFileDir, Params.ImgLoadType);
     finally
            imgList.Free;
     end;

     // ########################################################
     // #### Building step
     BuildModel;

     DoProgress(100);
end;

function TAAMAppearanceBuilder.EigValSum: double;
var i : integer;
begin
     Result := 0;
     for i := 0 to fAppearanceModel.EigVals.Height - 1 do
         Result := Result + fAppearanceModel.EigVals[0, i];
end;

function TAAMAppearanceBuilder.GetMaskArray: TBooleanDynArray;
begin
     Result := Copy(fMask, 0, Length(fMask));
end;

function TAAMAppearanceBuilder.GetNumModes: integer;
begin
     Result := fAppearanceModel.NumModes;
end;

procedure TAAMAppearanceBuilder.BuildAppearanceModel(Params: TCustomAAMAppearanceBuilderParams;
  ShapeModel: TAAMShapeBuilder; Textures: array of TDoubleMatrix);
var i: Integer;
begin
     assert(ShapeModel.OrigShapesCount = Length(Textures));

     DoProgress(0);

     fParams := Params;
     fShapeModel := ShapeModel;
     // the textures already hold the complete list -> build a new aligned one
     for i := 0 to Length(Textures) - 1 do
         ImgLoadEvent(Self, Textures[i], i, ShapeModel.OrigShapesCount, '');

     // ########################################################
     // #### Building step
     BuildModel;

     DoProgress(100);
end;

procedure TAAMAppearanceBuilder.BuildModel;
begin
     // normalize texures to remove dependency on global lightning
     NormalizeTextures;

     DoProgress(10);

     // build the model
     fParams.PCAProgress := pcaProgress;
     fParams.PCAOnTexture(fTextureImages, fAppearanceModel);
     fParams.PCAProgress := nil;

     if (fParams.debugPath <> '') and DirectoryExists(fParams.debugPath) then
        SaveReconstructedImgs;

     CreateMask;
end;

procedure TAAMAppearanceBuilder.CreateMask;
var img : TDoubleMatrix;
    x, y : integer;
    mask : TDoubleMatrix;
    yOffset : integer;
    colCnt : integer;
begin
     SetLength(fMask, fAppearanceWidth*fAppearanceHeight*NumColorPlanes);

     img := TDoubleMatrix.Create(fAppearanceWidth, fAppearanceHeight*NumColorPlanes);
     try
        yOffset := 0;
        for colCnt := 0 to NumColorPlanes - 1 do
        begin
             for y := 0 to fAppearanceHeight - 1 do
                 for x := 0 to fAppearanceWidth - 1 do
                     img[x, yOffset + y] := 1;

             inc(yOffset, fAppearanceHeight);
        end;

        // build the binary mask
        mask := fImgWarp.MapTexture(fMeanShape, img);
        try
           mask.ReshapeInPlace(1, fAppearanceWidth*fAppearanceHeight*NumColorPlanes);
           for y := 0 to mask.Height - 1 do
               fMask[y] := mask[0, y] > 0.5;
        finally
               mask.Free;
        end;
     finally
            img.Free;
     end;
end;

destructor TAAMAppearanceBuilder.Destroy;
begin
     fAppearanceModel.Free;
     fImgWarp.Free;
     fMeanShape.Free;
     fTextureImages.Free;

     inherited;
end;

procedure TAAMAppearanceBuilder.SaveReconstructedImgs;
var img : TDoubleMatrix;
    i : Integer;
    p : TDoubleMatrix;
    imgConv : TImageConvType;
begin
     for i := 0 to fTextureImages.Width - 1 do
     begin
          p := PFromNormTexture(i);
          try
             img := fAppearanceModel.Reconstruct(p);
             try
                img.ScaleAndAddInPlace(fBrightness[i], fContrast[i]);
                img.ReshapeInPlace(fAppearanceWidth, fAppearanceHeight*fNumColorPlanes);
                if fNumColorPlanes = 1
                then
                    imgConv := ctGrayScale
                else
                    imgConv := ctRGB;
                with TMatrixImageConverter.ConvertImage(img, imgConv) do
                try
                   SaveToFile(fParams.debugPath + '\Recons_' + inttostr(i) + '.bmp');
                finally
                       Free;
                end;
             finally
                    img.Free;
             end;
          finally
                 p.Free;
          end;
     end;
end;

procedure TAAMAppearanceBuilder.ImgLoadEvent(Sender: TObject; mtx: TDoubleMatrix;
  actNum, NumImages: integer; const FileName : string);
var normTexture : TDoubleMatrix;
    minX, minY : double;
    counter : Integer;
begin
     // init objects
     if actNum = 0 then
     begin
          FreeAndNil(fMeanShape);
          FreeAndNil(fImgWarp);
          FreeAndNil(fTextureImages);

          fMeanShape := fShapeModel.MeanShape;
          // update mean shape (which is mean normalized and |meanShape| = 1
          // such that a real visible texture is available
          // note that the max(width, height) is 2 times the scale factor
          minX := fMeanShape[0, 0];
          minY := fMeanShape[1, 0];

          for counter := 1 to fMeanShape.Height - 1 do
          begin
               minX := min(minX, fMeanShape[0, counter]);
               minY := min(minY, fMeanShape[1, counter]);
          end;
          for counter := 0 to fMeanShape.Height - 1 do
          begin
               fMeanShape[0, counter] := fMeanShape[0, counter] - minX;
               fMeanShape[1, counter] := fMeanShape[1, counter] - minY;
          end;
          fMeanShape.ScaleInPlace(fParams.AppearanceScale);

          fAppearanceWidth := Ceil(fMeanShape[0, 0]);
          fAppearanceHeight := Ceil(fMeanShape[1, 0]);
          for counter := 1 to fMeanShape.Height - 1 do
          begin
               fAppearanceWidth := max(fAppearanceWidth, Ceil(fMeanShape[0, counter]));
               fAppearanceHeight := max(fAppearanceHeight, Ceil(fMeanShape[1, counter]));
          end;

          // ensure that the width is compatible with an "aligned" matrix for fast reshaping!
          fAppearanceWidth := fAppearanceWidth + fAppearanceWidth and $00000001;
          
          fImgWarp := fParams.WarpClass.Create(fMeanShape, fAppearanceWidth, fAppearanceHeight, fNumColorPlanes);
          fTextureImages := TDoubleMatrix.Create(NumImages, fAppearanceWidth*fAppearanceHeight*fNumColorPlanes);
     end;

     // warp the shape to the normalized frame
     normTexture := fImgWarp.MapTexture(fShapeModel.OrigShapesMtx[actNum], mtx);
     try
        if Assigned(fParams.PreprocessingStep) then
           fParams.PreprocessingStep(Self, normTexture, fNumColorPlanes);

        normTexture.ReshapeInPlace(1, fAppearanceWidth*fAppearanceHeight*fNumColorPlanes);
        fTextureImages.SetColumn(actNum, normTexture);
     finally
            normTexture.Free;
     end;

     DoProgress(actNum*15 div NumImages);
end;

function TAAMAppearanceBuilder.Model: TMatrixPCA;
begin
     Result := fAppearanceModel;
end;

procedure TAAMAppearanceBuilder.AlignTexturesToMeanTexture(TextureImages, meanTexture : TDoubleMatrix;
  var brightness, contrast : TDoubleDynArray);
var imgCnt: Integer;
    img : TDoubleMatrix;
begin
     img := TDoubleMatrix.Create;
     try
        for imgCnt := 0 to TextureImages.Width - 1 do
        begin
             TextureImages.SetSubMatrix(imgCnt, 0, 1, TextureImages.Height);

             img.Assign(TextureImages, True);
             img.TransposeInPlace;
             img.MultInPlace(meanTexture);
             contrast[imgCnt]:= img[0, 0];

             img.Assign(TextureImages, True);
             img.MeanInPlace(False);
             brightness[imgCnt] := img[0, 0];

             TextureImages.AddAndScaleInPlace(-brightness[imgcnt], 1/contrast[imgCnt]);
        end;
     finally
            img.Free;
     end;

     TextureImages.UseFullMatrix;
end;

procedure TAAMAppearanceBuilder.UndoAlignment(Textures : TDoubleMatrix; const brightness, contrast : TDoubleDynArray);
var imgCnt, y : integer;
begin
     for imgCnt := 0 to Textures.Width - 1 do
     begin
          for y := 0 to Textures.Height - 1 do
              Textures[imgCnt, y] := (Textures[imgCnt, y]*contrast[imgCnt]) + brightness[imgCnt];
     end;
end;

procedure TAAMAppearanceBuilder.NormalizeTextures;
var diff : double;
    iter : integer;
    meanTexture : TDoubleMatrix;
    lastMeanTexture : TDoubleMatrix;
    i : Integer;
begin
     if (fTextureImages.Width = 0) or (fTextureImages.Height = 0) then
        exit;

     // todo: normalization step is not built for iterative model building

     diff := MaxDouble;
     iter := 0;

     meanTexture := nil;
     lastMeanTexture := nil;
     try
        // elect the first texture as mean texture
        lastMeanTexture := TDoubleMatrix.Create;
        MeanTexture := TDoubleMatrix.Create;
        fTextureImages.SetSubMatrix(0, 0, 1, fTextureImages.Height);
        MeanTexture.Assign(fTextureImages, True);

        ZeroMeanUnitLength(MeanTexture);

        fTextureImages.UseFullMatrix;
        SetLength(fBrightness, fTextureImages.Width);
        SetLength(fContrast, fTextureImages.Width);
        for i := 0 to Length(fContrast) - 1 do
            fContrast[i] := 1;

        // iterate until the mean texture does not change
        while (diff > 1e-5) and (iter < 10) do
        begin
             // save last state
             lastMeanTexture.Assign(meanTexture);

             UndoAlignment(fTextureImages, fBrightness, fContrast);
             AlignTexturesToMeanTexture(fTextureImages, meanTexture, fBrightness, fContrast);
             fTextureImages.UseFullMatrix;

             // reestimate mean
             meanTexture.Free;
             meanTexture := fTextureImages.Mean(True);

             // zero mean and unit length on mean texture:
             ZeroMeanUnitLength(meanTexture);

             // test if mean has changed
             lastMeanTexture.SubInPlace(meanTexture);
             diff := lastMeanTexture.ElementwiseNorm2;

             inc(iter);

             DoProgress(15 + iter);
        end;
     finally
            meanTexture.Free;
            lastMeanTexture.Free;
     end;

     // calculate the mean brightness and contrast values -> used in img generation
     fMeanContrast := 0;
     fMeanBrightness := 0;
     for i := 0 to Length(fContrast) - 1 do
     begin
          fMeanContrast := fMeanContrast + fContrast[i];
          fMeanBrightness := fMeanBrightness + fBrightness[i];
     end;
     fMeanContrast := fMeanContrast/Length(fContrast);
     fMeanBrightness := fMeanBrightness/Length(fBrightness);
end;

procedure TAAMAppearanceBuilder.pcaProgress(Sender: TObject; progress: integer);
begin
     DoProgress(25 + progress div 74);
end;

function TAAMAppearanceBuilder.PFromNormTexture(i: integer): TDoubleMatrix;
begin
     fTextureImages.SetSubMatrix(i, 0, 1, fTextureImages.Height);
     Result := fAppearanceModel.ProjectToFeatureSpace(fTextureImages);
     fTextureImages.UseFullMatrix;
end;

function TAAMAppearanceBuilder.TextureFromP(P: TDoubleMatrix): TDoubleMatrix;
var cnt : integer;
begin
     Result := fAppearanceModel.Reconstruct(P);

     // since the model has an overall mean of zero -> add 128 to be more bitmap compatible
     // also mask out the elements
     for cnt := 0 to Result.Height - 1 do
     begin
          if fMask[cnt]
          then
              Result[0, cnt] := Min(255, Max(0, Round(fMeanContrast*Result[0, cnt] + fMeanBrightness)))
          else
              Result[0, cnt] := 0;
     end;

     Result.ReshapeInPlace(fAppearanceWidth, fAppearanceHeight);
end;

{ TCustomAAMAppearanceBuilderParams }

constructor TCustomAAMAppearanceBuilderParams.Create;
begin
     ModelEnergy := cDefTextureModelEnergy;
end;

end.
