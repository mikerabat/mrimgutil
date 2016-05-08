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

unit BaseAAMModel;

// ########################################################
// #### Base Class definition for different aam types
// ########################################################

interface

uses Matrix, BaseMathPersistence;

type
  TOnAAMOptimizeIteration = procedure(Sender : TObject; step : integer; P : TDoubleMatrix; StepImg : TDoubleMatrix) of Object;
// ########################################################
// #### common properties for active appearance models
type
  TCustomAAMModel = class(TBaseMathPersistence)
  private
    fOnIterationStep: TOnAAMOptimizeIteration;
    fTextureWidth, fTextureHeight : integer;
    procedure SetTextureHeight(const Value: integer);
    procedure SetTextureWidth(const Value: integer);
  protected
    fNumIterations : integer;
    fNumColorPlanes : integer;

    function GetContrast : double; virtual; abstract;
    function GetBrightness : double; virtual; abstract;
    function GetMeanTexture : TDoubleMatrix; virtual; abstract;
    function GetMeanShape : TDoubleMatrix; virtual; abstract;
    function GetEigVal(index : integer) : double; virtual; abstract;
    function GetNumEigVals : integer; virtual; abstract;

    // learn params
    function GetLearnCombinedEnergy: double; virtual; abstract;
    function GetLearnImgScale: double; virtual; abstract;
    function GetLearnScaleDisturbPerc: double; virtual; abstract;
    function GetLearnShapeEnergy: double; virtual; abstract;
    function GetLearnStdDev: double; virtual; abstract;
    function GetLearnTextureDisturbPerc: double; virtual; abstract;
    function GetLearnTextureEnergy: double; virtual; abstract;

    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    procedure DefineProps; override;
    function PropTypeOfName(const Name: string): TPropType; override;

  public
    property OnIterationStep : TOnAAMOptimizeIteration read fOnIterationStep write fOnIterationStep;

    property TextureWidth : integer read fTextureWidth write SetTextureWidth;
    property TextureHeight : integer read fTextureHeight write SetTextureHeight;
    property NumColorPlanes : integer read fNumColorPlanes write fNumColorPlanes;

    property TextureContrast : double read GetContrast;
    property TextureBrightness : double read GetBrightness;
    property MeanTexture : TDoubleMatrix read GetMeanTexture;
    property MeanShape : TDoubleMatrix read GetMeanShape;
    property EigVals[index : integer] : double read GetEigVal;
    property NumEigVals : integer read GetNumEigVals;

    property LearnInitScale : double read GetLearnImgScale;
    property LearnScaleDisturb : double read GetLearnScaleDisturbPerc;
    property LearnTextureDisturb : double read GetLearnTextureDisturbPerc;
    property LearnParamStdDev : double read GetLearnStdDev;
    property LearnTextureEnergy : double read GetLearnTextureEnergy;
    property LearnShapeEnergy : double read GetLearnShapeEnergy;
    property LearnCombinedEnergy : double read GetLearnCombinedEnergy;

    property NumOptimizationIters : integer read fNumIterations;

    function RawTextureFromP(const p : TDoubleMatrix; AddMean : boolean) : TDoubleMatrix; virtual; abstract;
    function PFromRawTexture(const Texture : TDoubleMatrix; SubMean : boolean) : TDoubleMatrix; virtual; abstract;
    function TextureFromP(const p : TDoubleMatrix) : TDoubleMatrix; virtual; abstract;
    procedure TextureFromPInImg(img : TDoubleMatrix; const p : TDoubleMatrix); virtual; abstract;

    function ShapeFromP(const P : TDoubleMatrix) : TDoubleMatrix; virtual; abstract;

    function Optimize(Img : TDoubleMatrix; InitParams : TDoubleMatrix) : TDoubleMatrix; virtual; abstract; 
  end;

implementation

uses SysUtils;

{ TCustomAAMModel }

const cAAMTextureWidth = 'TextureWidth';
      cAAMTextureHeight = 'TextureHeight';
      cAAMNumColorPlanes = 'AAMNumColorPlanes';

procedure TCustomAAMModel.DefineProps;
begin
     AddIntProperty(cAAMTextureWidth, fTextureWidth);
     AddIntProperty(cAAMTextureHeight, fTextureHeight);
     AddIntProperty(cAAMNumColorPlanes, fNumColorPlanes);
end;

function TCustomAAMModel.PropTypeOfName(const Name: string): TPropType;
begin
     if (CompareText(Name, cAAMTextureWidth) = 0) or (CompareText(Name, cAAMTextureHeight) = 0) or
        (CompareText(Name, cAAMNumColorPlanes) = 0)
     then
         Result := ptInteger
     else
         Result := inherited PropTypeOfName(Name);
end;

procedure TCustomAAMModel.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, cAAMTextureWidth) = 0
     then
         fTextureWidth := Value
     else if CompareText(Name, cAAMTextureHeight) = 0
     then
         fTextureHeight := Value
     else if CompareText(Name, cAAMNumColorPlanes) = 0
     then
         fNumColorPlanes := Value
     else
         inherited;
end;

procedure TCustomAAMModel.SetTextureHeight(const Value: integer);
begin
     assert(Value > 0, 'Error Texture height must be higher than 0');

     fTextureHeight := Value;
end;

procedure TCustomAAMModel.SetTextureWidth(const Value: integer);
begin
     assert(Value > 0, 'Error Texture width must be higher than 0');

     fTextureWidth := Value;
end;

end.
