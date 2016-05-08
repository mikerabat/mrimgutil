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

unit AAMModel;

// #############################################################
// #### AAM (combined model) representation
// #############################################################

interface

uses SysUtils, Classes, Types, Matrix, AAMWarp, AAMConst, BaseAAMModel, BaseMathPersistence;

// #############################################################
// #### Model object - holds basically all necessary data
type
  TAAMModel = class(TCustomAAMModel)
  private
    fQs, fQg, fQgInv : TDoubleMatrix;
    fwarp : TCustomAAMWarper;
    fMask : TBooleanDynArray;
    fR : TDoubleMatrix;
    fMeanShape : TDoubleMatrix;
    fNumDimensions : integer;
    fNumShapeCoords : integer;
    fTextureMeanShape : TDoubleMatrix;
    fMeanTexture : TDoubleMatrix;
    fPreprocessingStep : TPreprocessingFunc;
    fEigVals : TDoubleDynArray;
    fBrightness: double;
    fContrast: double;
    fContrastInv : double;
    fMaxDx : double;
    fMaxDy : double;
    fMaxBrightnessChange : double;
    fMaxContrastChange : double;

    // learning options:
    fLearnStdDev : double;
    fLearnScaleDisturbPerc : double;
    fLearnTextureDisturbPerc : double;
    fLearnShapeEnergy : double;
    fLearnTextureEnergy : double;
    fLearnCombinedEnergy : double;
    fLearnImgScale : double;

    function CreateTextureFromParams(const p : TDoubleMatrix) : TDoubleMatrix;
    function CreateShapeFromParams(const p: TDoubleMatrix): TDoubleMatrix;

    procedure DoIterationStep(step : integer; const img, p: TDoubleMatrix);
  protected
    function GetEigVal(index: integer): double; override;
    function GetNumEigVals: integer; override;
    function GetContrast : double; override;
    function GetBrightness : double; override;
    function GetMeanTexture : TDoubleMatrix; override;
    function GetMeanShape : TDoubleMatrix; override;

    function GetLearnCombinedEnergy: double; override;
    function GetLearnImgScale: double; override;
    function GetLearnScaleDisturbPerc: double; override;
    function GetLearnShapeEnergy: double; override;
    function GetLearnStdDev: double; override;
    function GetLearnTextureDisturbPerc: double; override;
    function GetLearnTextureEnergy: double; override;

    procedure OnLoadDoubleProperty(const Name : String; const Value : double); override;
    procedure OnLoadBinaryProperty(const Name : String; const Value; size : integer); override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
    procedure DefineProps; override;
    function PropTypeOfName(const Name: string): TPropType; override;

    class function ClassIdentifier : String; override;
  public
    procedure SetShapeParams(Qs : TDoubleMatrix; MeanShape : TDoubleMatrix; ownsMatrix : boolean);
    procedure SetTextureParams(Qg : TDoubleMatrix; warp : TCustomAAMWarperClass; TextureShape : TDoubleMatrix; MeanTexture : TDoubleMatrix;
                               aTextureWidth, aTextureHeight : integer; const aContrast, aBrightness : double;
                               const Mask : TBooleanDynArray; ownsMatrix : boolean; NumColorPlanes : integer);
    procedure SetEigVals(mtx : TDoubleMatrix);
    procedure SetDisturbanceMatrix(R : TDoubleMatrix; ownsMatrix : boolean);
    procedure SetLearnParams(LearnSTDDev, scaleDisturb, textureDisturb, shapeEnergy, textureEnergy, combinedEnergy, initScale : double);

    procedure SetGlobalConstraints(const MaxDx, MaxDy, BrightnessChange, ContrastChange : double);

    property Qg : TDoubleMatrix read fQg;
    property Qs : TDoubleMatrix read fQs;
    property Warp : TCustomAAMWarper read fWarp;

    // creator functions
    function NormalizedImg(Img : TDoubleMatrix; Shape : TDoubleMatrix; doNormalize : boolean = True) : TDoubleMatrix;
    function CreateDiffImgFromParams(img : TDoubleMatrix; p : TDoubleMatrix) : TDoubleMatrix;

    function Optimize(Img : TDoubleMatrix; InitParams : TDoubleMatrix) : TDoubleMatrix; override;

    function TextureFromP(const p : TDoubleMatrix) : TDoubleMatrix; override;
    procedure TextureFromPInImg(img : TDoubleMatrix; const p : TDoubleMatrix); override;

    function PFromRawTexture(const Texture : TDoubleMatrix; SubMean : boolean) : TDoubleMatrix; override;
    function RawTextureFromP(const p : TDoubleMatrix; AddMean : boolean) : TDoubleMatrix; override;

    function ShapeFromP(const P : TDoubleMatrix) : TDoubleMatrix; override;

    constructor Create(PreprocessingStep : TPreprocessingFunc; NumDimensions : integer = 2);
    destructor Destroy; override;
  end;

implementation

uses AAMMatrixExt, Math;

{ TAAMModel }

const cAAMQs = 'AAMQs';
      cAAMQg = 'AAMQg';
      cAAMQgInv = 'AAMQgInv';
      cAAMWarp = 'AAMWarper';
      cAAMR = 'AAMR';
      cAAMMeanShape = 'AAMMeanShape';
      cAAMNumDim = 'AAMDims';
      cAAMTextMeanShape = 'AAMTextureMeanShape';
      cAAMMeanTexture = 'AAMMeanTexture';
      cAAMEigVals = 'AAMEigVals';
      cAAMBrighness = 'AAMBrightness';
      cAAMContrast = 'AAMContrast';
      cAAMMaxDx = 'AAMMaxDx';
      cAAMMaxDy = 'AAMMaxDy';
      cAAMMaxBrightnessChange = 'AAMMaxBrightChange';
      cAAMMaxContrastChange = 'AAMMaxContrastChange';
      cAAMMask = 'AAMMask';
      cAAMMeanTextureShape = 'AAMMeanTextureShape';
      cAAMLearnLearnInitScale = 'AAMLearnLearnInitScale';
      cAAMLearnScaleDisturb = 'AAMLearnScaleDisturb';
      cAAMLearnTextureDisturb = 'AAMLearnTextureDisturb';
      cAAMLearnParamStdDev = 'AAMLearnParamStdDev';
      cAAMLearnTextureEnergy = 'AAMLearnTextureEnergy';
      cAAMLearnShapeEnergy = 'AAMLearnShapeEnergy';
      cAAMLearnCombinedEnergy = 'AAMLearnCombinedEnergy';

class function TAAMModel.ClassIdentifier: String;
begin
     Result := 'ActiveAppearanceModel';
end;

constructor TAAMModel.Create(PreprocessingStep : TPreprocessingFunc; NumDimensions: integer);
begin
     inherited Create;

     fNumIterations := -1;
     fMaxDx := -1;
     fMaxDy := -1;
     fMaxBrightnessChange := -1;
     fMaxContrastChange := -1;

     fPreprocessingStep := PreprocessingStep;
     fNumDimensions := NumDimensions;
end;

function TAAMModel.CreateDiffImgFromParams(img,
  p: TDoubleMatrix): TDoubleMatrix;
var r1 : TDoubleMatrix;
    r2 : TDoubleMatrix;
    shape : TDoubleMatrix;
    brightness : double;
    contrast : double;
begin
     r1 := nil;
     try
        // warp texture from given params
        shape := CreateShapeFromParams(p);
        try
           // create image from params
           r2 := CreateTextureFromParams(p);
           try
              r2.ReshapeInPlace(1, r2.Width*r2.Height);

              r1 := NormalizedImg(img, shape, False);
              r1.ReshapeInPlace(1, r1.Height*r1.Width);
              contrast := 1/(1 + p[0, p.Height - 1]);
              brightness := p[0, p.Height - 2];

              r1.AddAndScaleInPlace(-brightness, contrast);

              // create difference gs - gm
              r1.SubInPlace(r2);
           finally
                  r2.Free;
           end;
        finally
               shape.Free;
        end;

        // apply mask
        r1.MaskedSetValue(fMask, 0);

        Result := r1;
     except
           r1.Free;
           raise;
     end;
end;


function TAAMModel.CreateShapeFromParams(const p: TDoubleMatrix): TDoubleMatrix;
var mtx : TDoubleMatrix;
    origWidth, origHeight : integer;
    scale1, scale2 : double;
    transX, transY : double;
    y : integer;
    valX, valY : double;
begin
     mtx := TDoubleMatrix.Create;
     try
        origWidth := P.Width;
        origHeight := P.Height;

        P.SetSubMatrix(0, 0, 1, fQs.Width);
        mtx.Assign(P, True);

        P.SetSubMatrix(0, 0, origWidth, origHeight);
        scale1 := P[0, fQs.Width] - 1;
        scale2 := P[0, fQs.Width + 1];
        transX := P[0, fQs.Width + 2];
        transY := P[0, fQs.Width + 3];

        Result := fQs.Mult(mtx);
        Result.AddInplace(fMeanShape);
        Result.ReshapeInPlace(fNumDimensions, fNumShapeCoords);

        // ############################################################
        // #### apply transformation
        for y := 0 to fNumShapeCoords - 1 do
        begin
             valX := Result[0, y];
             valY := Result[1, y];

             Result[0, y] := valx*scale1 - valy*scale2 + transX;
             Result[1, y] := valx*scale2 + valy*scale1 + transY;
        end;
     finally
            mtx.Free;
     end;
end;

function TAAMModel.CreateTextureFromParams(const p: TDoubleMatrix): TDoubleMatrix;
begin
     p.SetSubMatrix(0, 0, 1, fQg.Width);
     Result := fQg.Mult(p);
     p.UseFullMatrix;
     Result.AddInPlace(fMeanTexture);
     Result.ReshapeInPlace(TextureWidth, TextureHeight*NumColorPlanes);
end;

procedure TAAMModel.DefineProps;
begin
     // define properties in the base class
     inherited;

     AddIntProperty(cAAMNumDim, fNumDimensions);
     AddDoubleProperty(cAAMBrighness, fBrightness);
     AddDoubleProperty(cAAMContrast, fContrast);
     AddDoubleProperty(cAAMMaxDx, fMaxDx);
     AddDoubleProperty(cAAMMaxDy, fMaxDy);
     AddDoubleProperty(cAAMMaxBrightnessChange, fMaxBrightnessChange);
     AddDoubleProperty(cAAMMaxContrastChange, fMaxContrastChange);

     // learned params
     AddDoubleProperty(cAAMLearnLearnInitScale, fLearnImgScale);
     AddDoubleProperty(cAAMLearnScaleDisturb, fLearnScaleDisturbPerc);
     AddDoubleProperty(cAAMLearnTextureDisturb, fLearnTextureDisturbPerc);
     AddDoubleProperty(cAAMLearnParamStdDev, fLearnStdDev);
     AddDoubleProperty(cAAMLearnTextureEnergy, fLearnTextureEnergy);
     AddDoubleProperty(cAAMLearnShapeEnergy, fLearnShapeEnergy);
     AddDoubleProperty(cAAMLearnCombinedEnergy, fLearnCombinedEnergy);

     if Assigned(fQs) then
        AddObject(cAAMQs, fQs);
     if Assigned(fQg) then
        AddObject(cAAMQg, fQg);
     if Assigned(fQgInv) then
        AddObject(cAAMQgInv, fQgInv);
     if Assigned(fMeanTexture) then
        AddObject(cAAMMeanTexture, fMeanTexture);
     if Assigned(fTextureMeanShape) then
        AddObject(cAAMMeanTextureShape, fTextureMeanShape);
     if Assigned(fwarp) then
        AddObject(cAAMWarp, fwarp);
     if Assigned(fR) then
        AddObject(cAAMR, fR);
     if Assigned(fMeanShape) then
        AddObject(cAAMMeanShape, fMeanShape);
     if Length(fMask) > 0 then
        AddBinaryProperty(cAAMMask, fMask[0], Length(fMask)*sizeof(boolean));
     if Length(fEigVals) > 0 then
        AddDoubleArr(cAAMEigVals, fEigVals);
end;

function TAAMModel.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, cAAMNumDim) = 0
     then
         Result := ptInteger
     else if (CompareText(Name, cAAMBrighness) = 0) or (CompareText(Name, cAAMContrast) = 0) or
             (CompareText(Name, cAAMMaxDx) = 0) or (CompareText(Name, cAAMMaxDy) = 0) or
             (CompareText(Name, cAAMMaxBrightnessChange) = 0) or (CompareText(Name, cAAMMaxContrastChange) = 0) or
             (CompareText(Name, cAAMLearnLearnInitScale) = 0) or (CompareText(Name, cAAMLearnScaleDisturb) = 0) or
             (CompareText(Name, cAAMLearnTextureDisturb) = 0) or (CompareText(Name, cAAMLearnParamStdDev) = 0) or
             (CompareText(Name, cAAMLearnTextureEnergy) = 0) or (CompareText(Name, cAAMLearnShapeEnergy) = 0) or
             (CompareText(Name, cAAMLearnCombinedEnergy) = 0) or (CompareText(Name, cAAMEigVals) = 0)
     then
         Result := ptDouble
     else if (CompareText(Name, cAAMQs) = 0) or (CompareText(Name, cAAMQg) = 0) or
             (CompareText(Name, cAAMQgInv) = 0) or (CompareText(Name, cAAMMeanTexture) = 0) or
             (CompareText(Name, cAAMMeanTextureShape) = 0) or
             (CompareText(Name, cAAMWarp) = 0) or (CompareText(Name, cAAMR) = 0) or
             (CompareText(Name, cAAMMeanShape) = 0)
     then
         Result := ptObject
     else if CompareText(Name, cAAMMask) = 0
     then
         Result := ptBinary
     else
         Result := inherited PropTypeOfName(Name);
end;


destructor TAAMModel.Destroy;
begin
     fQs.Free;
     fQg.Free;
     fMeanShape.Free;
     fwarp.Free;
     fR.Free;
     fTextureMeanShape.Free;
     fMeanTexture.Free;
     fQgInv.Free;

     inherited;
end;

procedure TAAMModel.DoIterationStep(step : integer; const img, p: TDoubleMatrix);
var outImg : TDoubleMatrix;
begin
     if Assigned(OnIterationStep) then
     begin
          outImg := TDoubleMatrix.Create;
          try
             outImg.Assign(img);
             TextureFromPInImg(outImg, p);

             OnIterationStep(Self, step, p, outImg);
          finally
                 outImg.Free;
          end;
     end;
end;

function TAAMModel.GetBrightness: double;
begin
     Result := fBrightness;
end;

function TAAMModel.GetContrast: double;
begin
     Result := fContrast;
end;

function TAAMModel.GetEigVal(index: integer): double;
begin
     assert((index >= 0) and (index < Length(fEigVals)), 'index out of bounds');
     Result := fEigVals[index];
end;

function TAAMModel.GetLearnCombinedEnergy: double;
begin
     Result := fLearnCombinedEnergy;
end;

function TAAMModel.GetLearnImgScale: double;
begin
     Result := fLearnImgScale;
end;

function TAAMModel.GetLearnScaleDisturbPerc: double;
begin
     Result := fLearnScaleDisturbPerc;
end;

function TAAMModel.GetLearnShapeEnergy: double;
begin
     Result := fLearnShapeEnergy;
end;

function TAAMModel.GetLearnStdDev: double;
begin
     Result := fLearnStdDev;
end;

function TAAMModel.GetLearnTextureDisturbPerc: double;
begin
     Result := fLearnTextureDisturbPerc;
end;

function TAAMModel.GetLearnTextureEnergy: double;
begin
     Result := fLearnTextureEnergy;
end;

function TAAMModel.GetMeanShape: TDoubleMatrix;
begin
     Result := fMeanShape;
end;

function TAAMModel.GetMeanTexture: TDoubleMatrix;
begin
     Result := fMeanTexture;
end;

function TAAMModel.GetNumEigVals: integer;
begin
     Result := Length(fEigVals);
end;

function TAAMModel.NormalizedImg(Img, Shape: TDoubleMatrix;
  doNormalize: boolean): TDoubleMatrix;
begin
     Result := warp.MapTexture(Shape, Img);
     Result.ReshapeInPlace(1, Result.Width*Result.Height);

     if doNormalize then
        Result.AddAndScaleInPlace(-fBrightness, fContrastInv);
end;

procedure TAAMModel.OnLoadBinaryProperty(const Name: String; const Value;
  size: integer);
begin
     if CompareText(Name, cAAMMask) = 0 then
     begin
          assert(size > 0, 'Size must be greater than zero');
          SetLength(fMask, size);
          Move(Value, fMask[0], size);
     end
     else
         inherited;
end;

procedure TAAMModel.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     if CompareText(Name, cAAMEigVals) = 0
     then
         fEigVals := Value
     else
         inherited;
end;

procedure TAAMModel.OnLoadDoubleProperty(const Name: String;
  const Value: double);
begin
     if CompareText(Name, cAAMBrighness) = 0
     then
         fBrightness := Value
     else if CompareText(Name, cAAMContrast) = 0 then
     begin
          fContrast := Value;
          fContrastInv := 1/fContrast;
     end
     else if CompareText(Name, cAAMMaxDx) = 0
     then
         fMaxDx := Value
     else if CompareText(Name, cAAMMaxDy) = 0
     then
         fMaxDy := Value
     else if CompareText(Name, cAAMMaxBrightnessChange) = 0
     then
         fMaxBrightnessChange := Value
     else if CompareText(Name, cAAMMaxContrastChange) = 0
     then
         fMaxContrastChange := Value
     else if CompareText(Name, cAAMLearnLearnInitScale) = 0
     then
         fLearnImgScale := Value
     else if CompareText(Name, cAAMLearnScaleDisturb) = 0
     then
         fLearnScaleDisturbPerc := Value
     else if CompareText(Name, cAAMLearnTextureDisturb) = 0
     then
         fLearnTextureDisturbPerc := Value
     else if CompareText(Name, cAAMLearnParamStdDev) = 0
     then
         fLearnStdDev := Value
     else if CompareText(Name, cAAMLearnTextureEnergy) = 0
     then
         fLearnTextureEnergy := Value
     else if CompareText(Name, cAAMLearnShapeEnergy) = 0
     then
         fLearnShapeEnergy := Value
     else if CompareText(Name, cAAMLearnCombinedEnergy) = 0
     then
         fLearnCombinedEnergy := Value
     else
         inherited;
end;

procedure TAAMModel.OnLoadIntProperty(const Name: String; Value: integer);
begin
     if CompareText(Name, cAAMNumDim) = 0
     then
         fNumDimensions := Value
     else
         inherited;
end;

function TAAMModel.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;

     if CompareText(name, cAAMQs) = 0
     then
         fQs := obj as TDoubleMatrix
     else if CompareText(name, cAAMQg) = 0
     then
         fQg := obj as TDoubleMatrix
     else if CompareText(name, cAAMQgInv) = 0
     then
         fQgInv := obj as TDoubleMatrix
     else if CompareText(name, cAAMWarp) = 0
     then
         fwarp := obj as TCustomAAMWarper
     else if CompareText(name, cAAMR) = 0
     then
         fR := obj as TDoubleMatrix
     else if CompareText(name, cAAMMeanShape) = 0 then
     begin
          fMeanShape := obj as TDoubleMatrix;
          fNumShapeCoords := fMeanShape.Height div fNumDimensions;
     end
     else if CompareText(name, cAAMMeanTextureShape) = 0
     then
         fTextureMeanShape := obj as TDoubleMatrix     
     else if CompareText(name, cAAMTextMeanShape) = 0
     then
         fTextureMeanShape := obj as TDoubleMatrix
     else if CompareText(name, cAAMMeanTexture) = 0
     then
         fMeanTexture := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, obj);
end;

// optimization process form: Statistical Models of Appearance p 48ff  T. Cootes, J. Tylor
function TAAMModel.Optimize(Img, InitParams: TDoubleMatrix): TDoubleMatrix;
var E0 : double;
    E : double;
    diffImg : TDoubleMatrix;
    dC : TDoubleMatrix;
    optFact : double;
    dC1 : TDoubleMatrix;
    converged : boolean;
const cOptStepsFact : Array[0..5] of double = (1, 1.5, 0.5, 0.25, 2, 0.125);  // the last 2 are not in the paper
      cMaxNumIter = 20;
begin
     Result := TDoubleMatrix.Create;
     Result.Assign(InitParams);

     DoIterationStep(0, img, Result);
     fNumIterations := 0;

     // ############################################################
     // #### Basic, unconstrainted AAM search (without aam pyramids!)
     diffImg := CreateDiffImgFromParams(Img, Result);
     try
        // initial differential value from the current params:
        E0 := diffImg.ElementwiseNorm2;
        repeat
              converged := True;

              // #############################################################
              // #### main optimization loop
              dC := fR.Mult(diffImg);
              try
                 // constrain the global changes:
                 if (fMaxContrastChange > 0) and (fMaxContrastChange < Abs(dC[0, dc.Height - 1])) then
                    dC[0, dc.Height - 1] := sign(dC[0, dc.Height - 1])*fMaxContrastChange;
                 if (fMaxBrightnessChange > 0) and (fMaxBrightnessChange < Abs(dC[0, dc.Height - 2])) then
                    dC[0, dc.Height - 2] := sign(dC[0, dc.Height - 2])*fMaxBrightnessChange;
                 if (fMaxDy > 0) and (fMaxDy < Abs(dc[0, dc.Height - 3])) then
                    dC[0, dc.Height - 3] := sign(dC[0, dc.Height - 3])*fMaxDy;
                 if (fMaxDx > 0) and (fMaxDx < Abs(dc[0, dc.Height - 4])) then
                    dC[0, dc.Height - 4] := sign(dC[0, dc.Height - 4])*fMaxDx;

                 // find optimal displacement value which results in a better optimization than E0
                 for optFact in cOptStepsFact do
                 begin
                      dC1 := dC.Scale(optFact);
                      try
                         dC1.AddInplace(Result);

                         diffImg.Free;
                         diffImg := CreateDiffImgFromParams(Img, dC1);
                         E := diffImg.ElementwiseNorm2;

                         if E < 0.99*E0 then
                         begin
                              // we found a better solution -> store and iterate
                              Result.Assign(dC1);
                              E0 := E;

                              DoIterationStep(fnumIterations, img, Result);

                              converged := False;
                              break;
                         end;
                      finally
                             dC1.Free;
                      end;
                 end;
              finally
                     dC.Free;
              end;

              inc(fnumIterations);
        until converged or (fnumIterations > cMaxNumIter);
     finally
            diffImg.Free;
     end;
end;

function TAAMModel.PFromRawTexture(const Texture: TDoubleMatrix;
  SubMean: boolean): TDoubleMatrix;
var resImg : TDoubleMatrix;
begin
     assert((Texture.Width = 1) and (Texture.Width*Texture.Height = fMeanTexture.Height), 'Dimension error');

     resImg := Texture.Reshape(1, fMeanTexture.Height);
     try
        if SubMean then
           resImg.SubInPlace(fMeanTexture);

        Result := fQgInv.Mult(resImg);
     finally
            resImg.Free;
     end;
end;

function TAAMModel.RawTextureFromP(const p: TDoubleMatrix;
  AddMean: boolean): TDoubleMatrix;
begin
     p.SetSubMatrix(0, 0, 1, fQg.Width);
     Result := fQg.Mult(p);
     p.UseFullMatrix;

     if AddMean then
        Result.AddInplace(fMeanTexture);

     Result.ReshapeInPlace(TextureWidth, TextureHeight*NumColorPlanes);
end;

procedure TAAMModel.SetDisturbanceMatrix(R: TDoubleMatrix; ownsMatrix : boolean);
begin
     if ownsMatrix
     then
         fR := R
     else
     begin
          fR := TDoubleMatrix.Create;
          fR.Assign(R);
     end;
end;

procedure TAAMModel.SetEigVals(mtx: TDoubleMatrix);
begin
     fEigVals := mtx.SubMatrix;
end;

procedure TAAMModel.SetGlobalConstraints(const MaxDx, MaxDy, BrightnessChange,
  ContrastChange: double);
begin
     fMaxDx := MaxDX;
     fMaxDY := MaxDy;
     fMaxBrightnessChange := BrightnessChange;
     fMaxContrastChange := ContrastChange;
end;

procedure TAAMModel.SetLearnParams(LearnSTDDev, scaleDisturb, textureDisturb,
  shapeEnergy, textureEnergy, combinedEnergy, initScale : double);
begin
     fLearnStdDev := LearnSTDDev;
     fLearnScaleDisturbPerc := scaleDisturb;
     fLearnTextureDisturbPerc := textureDisturb;
     fLearnShapeEnergy := shapeEnergy;
     fLearnTextureEnergy := textureEnergy;
     fLearnCombinedEnergy := combinedEnergy;
     fLearnImgScale := initScale;
end;

procedure TAAMModel.SetShapeParams(Qs, MeanShape: TDoubleMatrix; ownsMatrix : boolean);
begin
     if ownsMatrix then
     begin
          fQs := Qs;
          fMeanShape := MeanShape;
     end
     else
     begin
          fQs := TDoubleMatrix.Create;
          fQs.Assign(Qs);
          fMeanShape := TDoubleMatrix.Create;
          fMeanShape.Assign(MeanShape);
     end;

     // ensure that the mean shape is only one dimensional
     fMeanShape.ReshapeInPlace(1, fMeanShape.Width*fMeanShape.Height);
     fNumShapeCoords := fMeanShape.Height div fNumDimensions;
end;

procedure TAAMModel.SetTextureParams(Qg: TDoubleMatrix; warp: TCustomAAMWarperClass;
  TextureShape: TDoubleMatrix; MeanTexture : TDoubleMatrix; aTextureWidth, aTextureHeight: integer;
  const aContrast, aBrightness : double; const Mask : TBooleanDynArray; ownsMatrix: boolean;
  NumColorPlanes : integer);
begin
     fNumColorPlanes := NumColorPlanes;
     fwarp := warp.Create(TextureShape, aTextureWidth, aTextureHeight, NumColorPlanes);

     TextureWidth := aTextureWidth;
     TextureHeight := aTextureHeight;
     fMask := Copy(Mask, 0, Length(Mask));
     fContrast := aContrast;
     fBrightness := aBrightness;
     fContrastInv := 1/fContrast;
     
     if ownsMatrix then
     begin
          fMeanTexture := MeanTexture;
          fTextureMeanShape := TextureShape;
          fQg := Qg;
     end
     else
     begin
          fMeanTexture := TDoubleMatrix.Create;
          fMeanTexture.Assign(MeanTexture);
          fTextureMeanShape := TDoubleMatrix.Create;
          fTextureMeanShape.Assign(TextureShape);
          fQg := TDoubleMatrix.Create;
          fQg.Assign(Qg);
     end;

     fQg.PseudoInversion(fQgInv);

     // we need the texture as a vector -> ensure that
     fMeanTexture.ReshapeInPlace(1, fMeanTexture.Width*fMeanTexture.Height);
end;

function TAAMModel.ShapeFromP(const P: TDoubleMatrix): TDoubleMatrix;
begin
     Result := CreateShapeFromParams(p);
end;

function TAAMModel.TextureFromP(const p: TDoubleMatrix): TDoubleMatrix;
var shape, text : TDoubleMatrix;
    minx, maxx, miny, maxy : double;
    counter : integer;
begin
     // shape from p:
     shape := nil;
     text := nil;
     try
        shape := CreateShapeFromParams(p);
        text := CreateTextureFromParams(p);
     
        // apply texture transformations in the normalized frame:
        p.UseFullMatrix;
        text.ScaleAndAddInPlace(p[0, p.Height - 2] - 1, p[0, p.Height - 1] - 1);

        // align shape to 0,0:
        minx := shape[0, 0];
        maxx := minx;
        miny := shape[1, 0];
        maxy := miny;

        for counter := 1 to shape.Height - 1 do
        begin
             minx := min(minx, shape[0, counter]);
             maxx := max(maxx, shape[0, counter]);
             miny := min(miny, shape[1, counter]);
             maxy := max(maxy, shape[1, counter]);
        end;

        for counter := 0 to shape.Height - 1 do
        begin
             shape[0, counter] := shape[0, counter] - minx;
             shape[1, counter] := shape[1, counter] - miny;
        end;

        // map texture to the shape
        with TCustomAAMWarperClass(fwarp.ClassType).Create(shape, Ceil(maxx - minx), Ceil(maxy - miny), fNumColorPlanes) do
        try
           Result := MapTexture(fTextureMeanShape, text);
        finally
               Free;
        end;
     finally
            shape.Free;
            text.Free;
     end;
end;

procedure TAAMModel.TextureFromPInImg(img: TDoubleMatrix;
  const p: TDoubleMatrix);
var shape, text : TDoubleMatrix;
    x, y : integer;
    newImg : TDoubleMatrix;
begin
     shape := nil;
     text := nil;
     try
        // shape from p:
        shape := CreateShapeFromParams(p);
        text := CreateTextureFromParams(p);

        // apply texture transformations in the normalized frame:
        p.UseFullMatrix;
        text.AddAndScaleInPlace(0, p[0, p.Height - 1] - 1);
        text.AddAndScaleInPlace(p[0, p.Height - 2], 1);

        // map texture to the shape
        with TCustomAAMWarperClass(fwarp.ClassType).Create(shape, img.Width, img.Height div fNumColorPlanes, fNumColorPlanes) do
        try
           newImg := MapTexture(fTextureMeanShape, text);
           try
              for y := 0 to newImg.Height - 1 do
              begin
                   for x := 0 to newImg.Width - 1 do
                   begin
                        if newImg[x, y] <> 0 then
                           img[x, y] := newImg[x, y];
                   end;
              end;
           finally
                  newImg.Free;
           end;
        finally
               Free;
        end;
     finally
            shape.Free;
            text.Free;
     end;
end;

initialization
   RegisterMathIO(TAAMModel);

end.
