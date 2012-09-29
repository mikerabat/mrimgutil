// ###################################################################
// #### This file is part of the mrimageutils project, depends on 
// #### the mathematics library project and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2012, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################

unit ImageMatrixConv;

// ##########################################################
// #### Image to matrix and backwards conversion routines
// ##########################################################

interface

uses Windows, SysUtils, Classes, Matrix, Graphics;

type
  TImageConvType = (ctGrayScale, ctRGB, ctHUV, ctGrayScaleNorm, ctRGBNorm);

// HSV Format is normalized in intervals of 0 to 1, otherwise it's 0 to 255!
type
  TOnImgResizeEvent = procedure(Sender : TObject; var Img : TBitmap; expectedWidth : integer; expectedHeight : integer) of Object;

// ##########################################################
// #### converter from a 2dim bmp to a 1 dim matrix and vice versa
type
  TMatrixImageConverter = class(TObject)
  private
    fConvType : TImageConvType;
    fWidth : integer;
    fHeight : integer;
    fColumnsFirst : boolean;
    fWeightRGB : boolean;
    fKeepSize : boolean;
    function GrayScaleNormMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
    procedure BmpToGrayScale(imgMatrix : TDoubleMatrix; img : TBitmap; normalize : boolean);
    function GetMatrixHeight: integer;
    function GrayScaleMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
    procedure BmpToHSVImg(imgMatrix : TDoubleMatrix; img : TBitmap);
    function HSVScaleMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
    procedure BmpToRGBImg(imgMatrix: TDoubleMatrix; img: TBitmap; normalize: boolean);
    function RGBMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
    function RGBNormMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
  public
    property ColumnsFirst : boolean read fColumnsFirst;    // used to debug in matlab -> matlab uses columns first approach
    property Width : integer read fWidth;
    property Height : integer read fHeight;
    property MatrixHeight : integer read GetMatrixHeight;  // depends on the conversion type!!
    property WeightRGB : boolean read fWeightRGB write fWeightRGB;

    procedure ImageToMatrix(imgMatrix : TDoubleMatrix; img : TBitmap); overload;
    function ImageToMatrix(img : TBitmap) : TDoubleMatrix; overload;
    function MatrixToImage(mtx : TDoubleMatrix) : TBitmap;

    class function MatrixHeightFromBmp(img : TBitmap; convType : TImageConvType) : integer;
    class function MatrixHeightFromDim(w, h : integer; convType : TImageConvType) : integer;

    class function ConvertImage(img : TBitmap; convType : TImageConvType) : TDoubleMatrix; overload;
    class function ConvertImage(const FileName : string; convType : TImageConvType) : TDoubleMatrix; overload;
    class function ConvertImage(mtx : TDoubleMatrix; convType : TImageConvType) : TBitmap; overload;

    constructor Create(ConvType : TImageConvType; columnsFirst : boolean; KeepSize : boolean; Width, Height : integer);
  end;

implementation

uses Math;

{ TMatrixImageConverter }

type
  TConvArr = Array[0..2] of double;

procedure TMatrixImageConverter.BmpToGrayScale(imgMatrix: TDoubleMatrix;
  img: TBitmap; normalize: boolean);
var x, y : integer;
    pPixel : PRGBTriple;
    pixVal : double;
    divisor : double;
    conv : TConvArr;
    idx : integer;
// conversion factors (from matlab) for RGB
const cGrayConfMtx : TConvArr = (0.2989, 0.5870, 0.1140);
      cSimpleGrayConvMtx : TConvArr = (1/3, 1/3, 1/3);
begin
     assert((not fKeepSize and (imgMatrix.Height >= img.width*img.Height)) or
            (fkeepSize and (imgMatrix.Height = img.Height) and (imgMatrix.Width = img.Width)), 'Error Matrix too small');
     divisor := ifthen(normalize, 256, 1);
     if fWeightRGB
     then
         conv := cGrayConfMtx
     else
         conv := cSimpleGrayConvMtx;

     idx := 0;
     if fColumnsFirst then
     begin
          for x := 0 to img.Width - 1 do
          begin
               for y := 0 to img.Height - 1 do
               begin
                    pPixel := img.ScanLine[y];
                    inc(pPixel, x);

                    pixVal := (pPixel^.rgbtBlue*conv[2] + pPixel^.rgbtGreen*conv[1] + pPixel^.rgbtRed*conv[0])/divisor;

                    if fKeepSize
                    then
                        imgMatrix[x, y] := pixVal
                    else
                        imgMatrix[0, idx] := pixVal;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          for y := 0 to img.Height - 1 do
          begin
               pPixel := img.ScanLine[y];

               for x := 0 to img.Width - 1 do
               begin
                    pixVal := (pPixel^.rgbtBlue*conv[2] + pPixel^.rgbtGreen*conv[1] + pPixel^.rgbtRed*conv[0])/divisor;

                    if fKeepSize
                    then
                        imgMatrix[x, y] := pixVal
                    else
                        imgMatrix[0, idx] := pixVal;
                    inc(pPixel);
                    inc(idx);
               end;
          end;
     end;
end;

procedure TMatrixImageConverter.BmpToHSVImg(imgMatrix: TDoubleMatrix;
  img: TBitmap);
var x, y : integer;
    pPixel : PRGBTriple;
    divisor : double;
    yOffset : integer;
    h, s, v : double;
    idx : integer;

procedure rgbToHSV(rgb : PRGBTriple; var h, s, v : double);
var maxVal, minVal : integer;
begin
     maxVal := max(pPixel^.rgbtBlue, max(pPixel^.rgbtGreen, pPixel^.rgbtRed));
     minVal := min(pPixel^.rgbtBlue, min(pPixel^.rgbtGreen, pPixel^.rgbtRed));

     if maxVal = minVal then
        h := 0;
     if maxval = pPixel^.rgbtRed
     then
         h := 1/6*(0 + (Integer(pPixel^.rgbtGreen) - Integer(pPixel^.rgbtBlue))/(maxVal - minVal))
     else if maxval = pPixel^.rgbtGreen
     then
         h := 1/6*(2 + (Integer(pPixel^.rgbtBlue) - Integer(pPixel^.rgbtRed))/(maxVal - minVal))
     else
         h := 1/6*(4 + (Integer(pPixel^.rgbtRed) - Integer(pPixel^.rgbtGreen))/(maxVal - minVal));
     if h < 0 then
        h := 255 + h;

     if maxVal = 0
     then
         s := 0
     else
         s := (maxVal - minVal)/maxVal;

     // normalize
     h := h*divisor;
     s := s*divisor;
     v := v*divisor;
end;
begin
     assert((not fKeepSize and (imgMatrix.Height >= 3*img.width*img.Height)) or
            (fkeepSize and (imgMatrix.Height = 3*img.Height) and (imgMatrix.Width = img.Width)), 'Error Matrix too small');
     divisor := 1/256;

     yOffset := img.Width*img.Height;

     if fKeepSize then
        imgMatrix.ReshapeInPlace(1, img.Width*img.Height*3);

     idx := 0;
     if fColumnsFirst then
     begin
          // the different RGB values are stored as "channels" each behind the other one
          for x := 0 to img.Width - 1 do
          begin
               for y := 0 to img.Height - 1 do
               begin
                    pPixel := img.ScanLine[y];
                    inc(pPixel, x);

                    rgbToHSV(pPixel, h, s, v);

                    imgMatrix[0, idx + 0*yOffset] := h;
                    imgMatrix[0, idx + 1*yOffset] := s;
                    imgMatrix[0, idx + 2*yOffset] := v;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          // the different RGB values are stored as "channels" each behind the other one
          for y := 0 to img.Height - 1 do
          begin
               pPixel := img.ScanLine[y];

               for x := 0 to img.Width - 1 do
               begin
                    rgbToHSV(pPixel, h, s, v);

                    imgMatrix[0, idx + 0*yOffset] := h;
                    imgMatrix[0, idx + 1*yOffset] := s;
                    imgMatrix[0, idx + 2*yOffset] := v;

                    inc(idx);
                    inc(pPixel);
               end;
          end;
     end;

     if fKeepSize then
        imgMatrix.ReshapeInPlace(img.Width, img.Height*3);
end;

class function TMatrixImageConverter.ConvertImage(mtx: TDoubleMatrix;
  convType: TImageConvType): TBitmap;
var imgHeight : integer;
begin
     imgHeight := mtx.Height;
     if not (convType in [ctGrayScale, ctGrayScaleNorm]) then
        imgHeight := imgHeight div 3;

     with TMatrixImageConverter.Create(convType, False, True, mtx.Width, imgHeight) do
     try
        Result := MatrixToImage(mtx);
     finally
            Free;
     end;
end;

constructor TMatrixImageConverter.Create(ConvType: TImageConvType;
  columnsFirst: boolean; KeepSize : boolean; Width, Height: integer);
begin
     inherited Create;

     fWeightRGB := True;
     fConvType := ConvType;
     fColumnsFirst := columnsFirst;
     fWidth := Width;
     fHeight := Height;
     fKeepSize := KeepSize;
end;

function TMatrixImageConverter.GetMatrixHeight: integer;
begin
     Result := fWidth*fHeight;

     if not (fconvType in [ctGrayScale, ctGrayScaleNorm]) then
        Result := Result*3;
end;

function TMatrixImageConverter.GrayScaleMtxToBmp(mtx: TDoubleMatrix): TBitmap;
var x, y : integer;
    pRGB : PRGBTriple;
    idx : integer;
    val : byte;
begin
     assert((fwidth*fHeight = mtx.Height) or ((fWidth = mtx.Width) and (fHeight = mtx.Height)), 'Dimension error');

     Result := TBitmap.Create;
     Result.PixelFormat := pf24bit;
     Result.SetSize(fWidth, fHeight);

     idx := 0;
     if fColumnsFirst then
     begin
          for x := 0 to fWidth - 1 do
          begin
               for y := 0 to fHeight - 1 do
               begin
                    pRGB := Result.ScanLine[y];
                    inc(pRGB, x);

                    if fKeepSize
                    then
                        val := Byte(Min(255, Max(0, Round(mtx[x, y]))))
                    else
                        val := Byte(Min(255, Max(0, Round(mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    pRGB^.rgbtGreen := val;
                    pRGB^.rgbtRed := val;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          for y := 0 to fHeight - 1 do
          begin
               pRGB := Result.ScanLine[y];

               for x := 0 to fWidth - 1 do
               begin
                    if fKeepSize
                    then
                        val := Byte(Min(255, Max(0, Round(mtx[x, y]))))
                    else
                        val := Byte(Min(255, Max(0, Round(mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    pRGB^.rgbtGreen := val;
                    pRGB^.rgbtRed := val;
                    inc(idx);
                    inc(pRGB);
               end;
          end;
     end;
end;

procedure TMatrixImageConverter.BmpToRGBImg(imgMatrix : TDoubleMatrix; img : TBitmap; normalize : boolean);
var x, y : integer;
    pPixel : PRGBTriple;
    pixVal : double;
    divisor : double;
    counter : integer;
    pRGBVal : PByte;
    idx : integer;
begin
     assert((not fKeepSize and (imgMatrix.Height >= 3*img.width*img.Height)) or
            (fkeepSize and (imgMatrix.Height = 3*img.Height) and (imgMatrix.Width = img.Width)), 'Error Matrix too small');
     divisor := ifthen(normalize, 1/256, 1);

     if fKeepSize then
        imgMatrix.ReshapeInPlace(1, img.Width*img.Height*3);

     if fColumnsFirst then
     begin
          // the different RGB values are stored as "channels" each behind the other one
          idx := 0;
          for counter := 0 to 2 do
          begin
               for x := 0 to img.Width - 1 do
               begin
                    for y := 0 to img.Height - 1 do
                    begin
                         pPixel := img.ScanLine[y];
                         inc(pPixel, x);

                         pRGBVal := PByte(pPixel);
                         inc(pRGBVal, counter);

                         pixVal := pRGBVal^;

                         imgMatrix[0, idx] := pixVal*divisor;
                         inc(idx);
                    end;
               end;
          end;
     end
     else
     begin
          // the different RGB values are stored as "channels" each behind the other one
          idx := 0;
          for counter := 0 to 2 do
          begin
               for y := 0 to img.Height - 1 do
               begin
                    pPixel := img.ScanLine[y];

                    for x := 0 to img.Width - 1 do
                    begin
                         pRGBVal := PByte(pPixel);
                         inc(pRGBVal, counter);

                         pixVal := pRGBVal^;

                         imgMatrix[0, idx] := pixVal*divisor;
                         inc(pPixel);
                         inc(idx);
                    end;
               end;
          end;
     end;

     if fKeepSize then
        imgMatrix.ReshapeInPlace(img.Width, img.Height*3);
end;

function TMatrixImageConverter.ImageToMatrix(img: TBitmap): TDoubleMatrix;
begin
     if fKeepSize
     then
         Result := TDoubleMatrix.Create(img.Width, MatrixHeight div img.Width)
     else
         Result := TDoubleMatrix.Create(1, MatrixHeight);

     ImageToMatrix(Result, img);
end;

procedure TMatrixImageConverter.ImageToMatrix(imgMatrix: TDoubleMatrix;
  img: TBitmap);
begin
     if img.PixelFormat <> pf24bit then
        img.PixelFormat := pf24bit;

     case fconvType of
       ctGrayScale:     BmpToGrayScale(imgMatrix, img, False);
       ctRGB:           BmpToRGBImg(imgMatrix, img, False);
       ctHUV:           BmpToHSVImg(imgMatrix, img);
       ctGrayScaleNorm: BmpToGrayScale(imgMatrix, img, True);
       ctRGBNorm:       BmpToRGBImg(imgMatrix, img, True);
     end;
end;

class function TMatrixImageConverter.ConvertImage(img: TBitmap;
  convType: TImageConvType): TDoubleMatrix;
begin
     with TMatrixImageConverter.Create(convType, False, True, img.Width, img.Height) do
     try
        Result := ImageToMatrix(img);
     finally
            Free;
     end;
end;

class function TMatrixImageConverter.ConvertImage(const FileName: string;
  convType: TImageConvType): TDoubleMatrix;
var bmp : TBitmap;
    img : TPicture;
begin
     img := TPicture.Create;
     try
        img.LoadFromFile(FileName);
        if img.graphic is TBitmap
        then
            Result := ConvertImage(img.graphic as TBitmap, convType)
        else
        begin
             bmp := TBitmap.Create;
             try
                bmp.SetSize(img.graphic.Width, img.graphic.Height);
                bmp.Canvas.Draw(0, 0, img.graphic);

                Result := ConvertImage(bmp, convType);
             finally
                    bmp.Free;
             end;
        end;
     finally
            img.Free;
     end;
end;

class function TMatrixImageConverter.MatrixHeightFromBmp(img: TBitmap;
  convType: TImageConvType): integer;
begin
     Result := img.Width*img.Height;

     if not (convType in [ctGrayScale, ctGrayScaleNorm]) then
        Result := Result*3;
end;

class function TMatrixImageConverter.MatrixHeightFromDim(w, h: integer;
  convType: TImageConvType): integer;
begin
     Result := w*h;

     if not (convType in [ctGrayScale, ctGrayScaleNorm]) then
        Result := Result*3;
end;

function TMatrixImageConverter.MatrixToImage(mtx: TDoubleMatrix): TBitmap;
begin
     case fConvType of
      ctGrayScale:     Result := GrayScaleMtxToBmp(mtx);
      ctRGB:           Result := RGBMtxToBmp(mtx);
      ctHUV:           Result := HSVScaleMtxToBmp(mtx);
      ctGrayScaleNorm: Result := GrayScaleNormMtxToBmp(mtx);
      ctRGBNorm:       Result := RGBNormMtxToBmp(mtx);
     else
         Result := nil;
     end;
end;

function TMatrixImageConverter.RGBMtxToBmp(mtx: TDoubleMatrix): TBitmap;
var x, y : integer;
    pRGB : PRGBTriple;
    idx : integer;
    val : byte;
    colorHeight : integer;
begin
     assert(3*fWidth*fHEight = mtx.Height*mtx.Width, 'Dimension error');

     Result := TBitmap.Create;
     Result.PixelFormat := pf24bit;
     Result.SetSize(fWidth, fHeight);
     colorHeight := fWidth*fHeight;

     if fKeepSize then
        mtx.ReshapeInPlace(1, mtx.Width*mtx.Height);

     idx := 0;
     if fColumnsFirst then
     begin
          for x := 0 to fWidth - 1 do
          begin
               for y := 0 to fHeight - 1 do
               begin
                    pRGB := Result.ScanLine[y];
                    inc(pRGB, x);

                    val := Byte(Min(255, Max(0, Round(mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    val := Byte(Min(255, Max(0, Round(mtx[0, idx + colorHeight]))));
                    pRGB^.rgbtGreen := val;
                    val := Byte(Min(255, Max(0, Round(mtx[0, idx + 2*colorHeight]))));
                    pRGB^.rgbtRed := val;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          for y := 0 to fHeight - 1 do
          begin
               pRGB := Result.ScanLine[y];

               for x := 0 to fWidth - 1 do
               begin
                    val := Byte(Min(255, Max(0, Round(mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    val := Byte(Min(255, Max(0, Round(mtx[0, idx + colorHeight]))));
                    pRGB^.rgbtGreen := val;
                    val := Byte(Min(255, Max(0, Round(mtx[0, idx + 2*colorHeight]))));
                    pRGB^.rgbtRed := val;
                    inc(idx);
                    inc(pRGB);
               end;
          end;
     end;

     if fKeepSize then
        mtx.ReshapeInPlace(fWidth, fHeight*3);
end;

function TMatrixImageConverter.GrayScaleNormMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
var x, y : integer;
    pRGB : PRGBTriple;
    idx : integer;
    val : byte;
begin
     assert((not fKeepSize and (fWidth*fHeight = mtx.Height)) or
            (fKeepSize and (fWidth = mtx.Width) and (fHeight = mtx.Height)), 'Dimension error');

     Result := TBitmap.Create;
     Result.PixelFormat := pf24bit;
     Result.SetSize(fWidth, fHeight);

     idx := 0;
     if fColumnsFirst then
     begin
          for x := 0 to fWidth - 1 do
          begin
               for y := 0 to fHeight - 1 do
               begin
                    pRGB := Result.ScanLine[y];
                    inc(pRGB, x);

                    if fKeepSize
                    then
                        val := Byte(Min(255, Max(0, Round(255*mtx[x, y]))))
                    else
                        val := Byte(Min(255, Max(0, Round(255*mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    pRGB^.rgbtGreen := val;
                    pRGB^.rgbtRed := val;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          for y := 0 to fHeight - 1 do
          begin
               pRGB := Result.ScanLine[y];

               for x := 0 to fWidth - 1 do
               begin
                    if fKeepSize
                    then
                        val := Byte(Min(255, Max(0, Round(255*mtx[x, y]))))
                    else
                        val := Byte(Min(255, Max(0, Round(255*mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    pRGB^.rgbtGreen := val;
                    pRGB^.rgbtRed := val;
                    inc(idx);
                    inc(pRGB);
               end;
          end;
     end;
end;

function TMatrixImageConverter.HSVScaleMtxToBmp(mtx: TDoubleMatrix): TBitmap;
begin
     raise EAbstractError.Create('Not yet implemented');
end;

function TMatrixImageConverter.RGBNormMtxToBmp(mtx : TDoubleMatrix) : TBitmap;
var x, y : integer;
    pRGB : PRGBTriple;
    idx : integer;
    val : byte;
    colorHeight : integer;
begin
     assert(3*fWidth*fHEight = mtx.Height*mtx.Width, 'Dimension error');

     Result := TBitmap.Create;
     Result.PixelFormat := pf24bit;
     Result.SetSize(fWidth, fHeight);
     colorHeight := fWidth*fHeight;

     if fKeepSize then
        mtx.ReshapeInPlace(1, mtx.Width*mtx.Height);

     idx := 0;
     if fColumnsFirst then
     begin
          for x := 0 to fWidth - 1 do
          begin
               for y := 0 to fHeight - 1 do
               begin
                    pRGB := Result.ScanLine[y];
                    inc(pRGB, x);

                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx + colorHeight]))));
                    pRGB^.rgbtGreen := val;
                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx + 2*colorHeight]))));
                    pRGB^.rgbtRed := val;
                    inc(idx);
               end;
          end;
     end
     else
     begin
          for y := 0 to fHeight - 1 do
          begin
               pRGB := Result.ScanLine[y];

               for x := 0 to fWidth - 1 do
               begin
                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx]))));
                    pRGB^.rgbtBlue := val;
                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx + colorHeight]))));
                    pRGB^.rgbtGreen := val;
                    val := Byte(Min(255, Max(0, Round(255*mtx[0, idx + 2*colorHeight]))));
                    pRGB^.rgbtRed := val;
                    inc(idx);
                    inc(pRGB);
               end;
          end;
     end;

     if fKeepSize then
        mtx.ReshapeInPlace(fWidth, fHeight*3);
end;

end.
