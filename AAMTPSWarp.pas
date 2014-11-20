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

unit AAMTPSWarp;

// #############################################################
// #### Texture warping using Thin Plate Splines
// #############################################################

interface

uses SysUtils, AAMWarp, Matrix, PtsDefinitions, Registration, BaseMathPersistence;

// #############################################################
// #### Thin plate splines warping with enhanced masking
{$Align 8}
type
  TAAMTPSWarp = class(TCustomAAMWarper)
  private
    type
      TXYCoord = record
        x : integer;
        y : integer;
      end;
      TXYCoordArray = Array of TXYCoord;
  private
    fMask : Array of TXYCoord;
    fMaskObj : TPtsMappingObj;
    fToPtsObj : TPtsMappingObj;

    procedure InitMask;
    function CreateTexture(img, texturePts : TDoubleMatrix; width, Height : integer) : TDoubleMatrix;
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
    procedure OnLoadBinaryProperty(const Name : String; const Value; size : integer); override;
  public
    function MapTexture(FromPts : TDoubleMatrix; img : TDoubleMatrix) : TDoubleMatrix; override;

    constructor Create(ToPts : TDoubleMatrix; DestImgWidth, DestImgHeight : integer; NumColorPlanes : integer); override;
    destructor Destroy; override;
  end;

implementation

uses ThinPlateSplines, Math, LinearTriangulationTransformation, DelaunyMapping;

const cAAMTPSMask = 'AAMTpsMask';
      cAAMTpsMaskObj = 'AAMTpsMaskObj';
      cAAMTPSToPts = 'AAMTpsToPts';

{ TAAMTPSWarp }

function TAAMTPSWarp.MapTexture(FromPts, img: TDoubleMatrix): TDoubleMatrix;
var mapping : TTPSRegistration;
    FromPtsObj : TPtsMappingObj;
    texturePts : TPtsMappingObj;
begin
     // create mapping object
     mapping := TTPSRegistration.Create;
     try
        FromPtsObj := TPtsMappingObj.Create(FromPts);
        try
           mapping.CreatePointMapping(fToPtsObj, FromPtsObj);

           texturePts := mapping.MapPoints(fMaskObj);

           Result := CreateTexture(img, texturePts.PtsAsMtxRef, TextureWidth, TextureHeight);
        finally
               FromPtsObj.Free;
        end;
     finally
            mapping.Free;
     end;
end;

procedure TAAMTPSWarp.OnLoadBinaryProperty(const Name: String; const Value;
  size: integer);
begin
     if CompareText(Name, cAAMTPSMask) = 0 then
     begin
          assert((size mod sizeof(TXYCoord)) = 0, 'Error unknown size');
          SetLength(fMask, size div sizeof(TXYCoord));
          Move(Value, fMask[0], size);
     end
     else
         inherited;
end;

function TAAMTPSWarp.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, cAAMTpsMaskObj) = 0
     then
         fMaskObj := Obj as TPtsMappingObj
     else if CompareText(Name, cAAMTPSToPts) = 0
     then
         fToPtsObj := Obj as TPtsMappingObj
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

class function TAAMTPSWarp.ClassIdentifier: String;
begin
     Result := 'AAMTPSWarp';
end;

constructor TAAMTPSWarp.Create(ToPts: TDoubleMatrix; DestImgWidth,
  DestImgHeight: integer; NumColorPlanes : integer);
begin
     inherited Create(ToPts, DestImgWidth, DestImgHeight, NumColorPlanes);

     fToPtsObj := TPtsMappingObj.Create(ToPts, False);

     // ####################################################
     // #### Initialize the mask
     InitMask;
end;

function TAAMTPSWarp.CreateTexture(img,
  texturePts: TDoubleMatrix; width, Height : integer): TDoubleMatrix;
var x, y : integer;
    imgx, imgy : integer;
    w, h : integer;
    factx : double;
    facty : double;
    wm1, hm1 : integer;
    ptsIdx : integer;
    yOffset, yOffset1 : integer;
    colPlanes: Integer;
begin
     assert(Length(fMask) = texturePts.Height, 'Dimension error');

     Result := TDoubleMatrix.Create(width, Height*NumColorPlanes);
     w := img.Width;
     h := img.Height div NumColorPlanes;

     wm1 := w - 1;
     hm1 := h - 1;
     yOffset := 0;
     yOffset1 := 0;

     for colPlanes := 0 to NumColorPlanes - 1 do
     begin
          // warp only the pixels within the mask
          for ptsIdx := 0 to texturePts.Height - 1 do
          begin
               y := fMask[ptsIdx].y;
               x := fMask[ptsIdx].x;

               // billinear mapping from one image to the next (it's just nicer)
               imgx := Floor(texturePts[0, ptsIdx]);
               imgy := Floor(texturePts[1, ptsIdx]);
               factx := Frac(texturePts[0, ptsIdx]);
               facty := Frac(texturePts[1, ptsIdx]);

               if (imgx >= 0) and (imgy >= 0) then
               begin
                    if (imgx < w) and (imgy < h) then
                    begin
                         if (imgx < w - 1) and (imgy < h - 1)
                         then
                             Result[x, yOffset + y] := (1 - factx)*(1 - facty)*img[imgx, yOffset1 + imgy] +
                                             factx*(1 - facty)*img[imgx + 1, yOffset1 + imgy] +
                                             facty*(1 - factx)*img[imgx, yOffset1 + imgy + 1] +
                                             factx*facty*img[imgx + 1, yOffset1 + imgy + 1]
                         else
                             Result[x, yOffset + y] := img[imgx, yOffset1 + imgy];
                    end
                    else
                        Result[x, yOffset + y] := img[Max(0, Min(wm1, imgx)), Max(0, Min(hm1, yOffset1 + imgy))];
               end
               else
                   Result[x, yOffset + y] := img[Max(0, Min(wm1, imgx)), Max(0, Min(hm1, yOffset1 + imgy))];
          end;

          inc(yOffset, Height);
          inc(yOffset1, h);
     end;
end;

procedure TAAMTPSWarp.DefineProps;
begin
     // define base properties
     inherited;

     if Length(fMask) > 0 then
        AddBinaryProperty(cAAMTPSMask, fMask[0], Length(fMask)*sizeof(fMask[0]));
     if Assigned(fToPtsObj) then
        AddObject(cAAMTPSToPts, fToPtsObj);
     if Assigned(fMaskObj) then
        AddObject(cAAMTpsMaskObj, fMaskObj);
end;

destructor TAAMTPSWarp.Destroy;
begin
     fToPtsObj.Free;
     fMaskObj.Free;
     
     inherited;
end;

procedure TAAMTPSWarp.InitMask;
var x, y : integer;
    pt : TXYCoord;
    numInPoints : integer;
    maskPts : TDynPointf2DArray;
    maskImg : TDoubleMatrix;
    ones : TDoubleMatrix;
begin
     // create a linear mapping object and map a complete white image ->
     // everything which is zero is masked!
     with TDelaunyTriangulationMapping.Create do
     try
        InitTriangulation(fToPtsObj, TextureWidth, TextureHeight);

        maskImg := nil;
        ones := nil;
        try
           ones := TDoubleMatrix.Create(TextureWidth, TextureHeight);
           for x := 0 to TextureWidth - 1 do
               for y := 0 to TextureHeight - 1 do
                   ones[x, y] := 1;

           InitPtsForMapping(fToPtsObj);
           maskImg := MapImage(ones);

           // fill coordinate buffer
           SetLength(fMask, TextureWidth*TextureHeight);
           SetLengtH(maskPts, TextureWidth*TextureHeight);
           numInPoints := 0;

           for y := 0 to TextureHeight - 1 do
           begin
                pt.y := y;
                for x := 0 to TextureWidth - 1 do
                begin
                     pt.x := x;

                     if maskImg[x, y] > 0.5 then
                     begin
                          fMask[numInPoints] := pt;
                          maskPts[numInPoints].x := pt.x;
                          maskPts[numInPoints].y := pt.y;

                          inc(numInPoints);
                     end;
                end;
           end;

           // reduce memory footprint
           SetLength(fMask, numInPoints);
           SetLength(maskPts, numInPoints);
           fMaskObj := TPtsMappingObj.Create(maskPts);
        finally
               maskImg.Free;
               ones.Free;
        end;
     finally
            Free;
     end;
end;

initialization
   RegisterMathIO(TAAMTPSWarp);

end.
