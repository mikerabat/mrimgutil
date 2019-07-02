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

unit ImageResize;

// #############################################################
// #### Image resize routines
// #############################################################

interface

uses SysUtils, Matrix, Types;

// #############################################################
// #### Base Class, basically implements the interface
type
  TCustomImageResize = class(TObject)
  private
    fNumColorPlanes: integer;
  protected
    property NumColorPlanes : integer read fNumColorPlanes;

    procedure ApplyKernel(var img : TDoubleDynArray; imgWidth, imgHeight : integer; const kernel : Array of double; kernelSize : integer);
  public
    function Resize(img : TDoubleMatrix; newWidth, newHeight : integer) : TDoubleMatrix; virtual; abstract;

    constructor Create(NumColorPlanes : integer = 1);
  end;
  TCustomImageResizeClass = class of TCustomImageResize;

type
  TBilinearImageResize = class(TCustomImageResize)
  private
    procedure BilinearMapping1Color(const imgVals: TDoubleDynArray; origWidth,
      origHeight: integer; var resImg: TDoubleDynArray; newWidth,
      newHeight: integer);
    procedure BilinearMappingNColor(const imgVals: TDoubleDynArray; origWidth,
      origHeight: integer; var resImg: TDoubleDynArray; newWidth,
      newHeight: integer);
  public
    function Resize(img : TDoubleMatrix; newWidth, newHeight : integer) : TDoubleMatrix; override;
  end;

function GrayImgResize(cl : TCustomImageResizeClass; img : TDoubleMatrix; newWidth, newHeight : integer) : TDoubleMatrix;
function ImgResize(cl : TCustomImageResizeClass; img : TDoubleMatrix; numColorPlanes : integer; newWidth, newHeight : integer) : TDoubleMatrix;

implementation

uses Math;

function GrayImgResize(cl : TCustomImageResizeClass; img : TDoubleMatrix; newWidth, newHeight : integer) : TDoubleMatrix;
begin
     with cl.Create(1) do
     try
        Result := Resize(img, newWidth, newHeight);
     finally
            Free;
     end;
end;

function ImgResize(cl : TCustomImageResizeClass; img : TDoubleMatrix; numColorPlanes : integer; newWidth, newHeight : integer) : TDoubleMatrix;
begin
     with cl.Create(numColorPlanes) do
     try
        Result := Resize(img, newWidth, newHeight);
     finally
            Free;
     end;
end;

{ TCustomImageResize }

procedure TCustomImageResize.ApplyKernel(var img: TDoubleDynArray;
  imgWidth, imgHeight : integer; const kernel: array of double; kernelSize : integer);
var offset : integer;
    y : Integer;
    x : integer;
    kernelX, kernelY : integer;
    res : TDoubleDynArray;
    actIdx : integer;
    kernelIdx : integer;
    colorIdx: Integer;
    colOffset : integer;
    imgx, imgy : integer;
begin
     SetLength(res, imgWidth*imgHeight*NumColorPlanes);

     offset := kernelSize div 2;

     // todo: optimize this routine...
     for colorIdx := 0 to fNumColorPlanes - 1 do
     begin
          colOffset := colorIdx*imgWidth*imgHeight;
          actIdx := offset*imgWidth*NumColorPlanes + colOffset;

          for y := 0 to imgHeight - 1 do
          begin
               for x := 0 to imgWidth - 1 do
               begin
                    kernelIdx := 0;

                    for kernelY := 0 to kernelSize - 1 do
                    begin
                         for kernelX := 0 to kernelSize - 1 do
                         begin
                              imgX := x - offset + kernelX;
                              if imgX < 0 then
                                 imgX := -imgX;
                              imgY := y - offset + kernelY;
                              if imgY < 0 then
                                 imgY := -imgY;
                              if imgX >= imgWidth then
                                 imgX := 2*imgWidth - imgX;
                              if imgY >= imgHeight then
                                 imgy := 2*imgHeight - imgY;

                              res[actIdx] := res[actIdx] +
                                             kernel[kernelIdx]*img[colOffset + imgx + imgY*imgWidth];
                              inc(kernelIdx);
                         end;
                    end;

                    inc(actIdx);
               end;
          end;
     end;

     // return new image:
     img := res;
end;

constructor TCustomImageResize.Create(NumColorPlanes: integer);
begin
     fNumColorPlanes := NumColorPlanes;

     inherited Create;
end;

{ TBilinearImageResize }

procedure TBilinearImageResize.BilinearMapping1Color(const imgVals : TDoubleDynArray;
 origWidth, origHeight : integer; var resImg : TDoubleDynArray; newWidth, newHeight : integer);
var scaleX, scaleY : single;
    x, y : integer;
    iX, iY : integer;
    factX, factY : single;
begin
     scaleX := origWidth/newWidth;
     scaleY := origHeight/newHeight;

     for y := 0 to newHeight - 1 do
     begin
          for x := 0 to newWidth - 1 do
          begin
               iX := Trunc(x*scaleX);
               iY := Trunc(y*scaleY);

               factX := Frac(x*scaleX);
               factY := Frac(y*scaleY);

               if (iX < origWidth - 1) and (iY < origHeight - 1) and ((factX <> 0) or (factY <> 0))
               then
                    resImg[x + y*newWidth] := (1 - factx)*(1 - facty)*imgVals[iX + iY*origWidth] +
                                         factx*(1 - facty)*imgVals[iX + 1 + iY*origWidth] +
                                         facty*(1 - factx)*imgVals[iX +(iY + 1)*origWidth] +
                                         factx*facty*imgVals[iX + 1 + (iY + 1)*origWidth]
               else
                   resImg[x + y*newWidth] := imgVals[iX + iY*origWidth];
          end;
     end;
end;

procedure TBilinearImageResize.BilinearMappingNColor(const imgVals : TDoubleDynArray;
 origWidth, origHeight : integer; var resImg : TDoubleDynArray; newWidth, newHeight : integer);
var scaleX, scaleY : single;
    x, y : integer;
    iX, iY : integer;
    factX, factY : single;
    colIdx : integer;
    yOffset, yOffset2 : integer;
    inc1, inc2 : Integer;
begin
     scaleX := origWidth/newWidth;
     scaleY := origHeight/newHeight;
     inc1 := newWidth*newHeight;
     inc2 := origWidth*origHeight;

     for y := 0 to newHeight - 1 do
     begin
          for x := 0 to newWidth - 1 do
          begin
               iX := Trunc(x*scaleX);
               iY := Trunc(y*scaleY);

               factX := Frac(x*scaleX);
               factY := Frac(y*scaleY);

               if (iX < origWidth - 1) and (iY < origHeight - 1) and ((factX <> 0) or (factY <> 0)) then
               begin
                    yOffset := 0;
                    yOffset2 := 0;
                    for colIdx := 0 to NumColorPlanes - 1 do
                    begin
                         resImg[yOffset + x + y*newWidth] := (1 - factx)*(1 - facty)*imgVals[yOffset2 + (iX + iY*origWidth)] +
                                                              factx*(1 - facty)*imgVals[yOffset2 + (iX + 1 + iY*origWidth)] +
                                                              facty*(1 - factx)*imgVals[yOffset2 + (iX +(iY + 1)*origWidth)] +
                                                              factx*facty*imgVals[yOffset2 + (iX + 1 + (iY + 1)*origWidth)];
                         inc(yOffset, inc1);
                         inc(yOffset2, inc2);
                    end;
               end
               else
               begin
                    yOffset := 0;
                    yOffset2 := 0;
                    for colIdx := 0 to NumColorPlanes - 1 do
                    begin
                         resImg[(x + y*newWidth) + yOffset] := imgVals[(iX + iY*origWidth) + yOffset2];
                         inc(yOffset, inc1);
                         inc(yOffset2, inc2);
                    end;
               end;
          end;
     end;
end;

function TBilinearImageResize.Resize(img: TDoubleMatrix; newWidth,
  newHeight: integer): TDoubleMatrix;
var resImg : TDoubleDynArray;
    kernel : Array of double;
    imgVals : TDoubleDynArray;
    scaleX, scaleY : single;
    kernelSize : integer;
    origWidth, origHeight : integer;
    x : integer;
begin
     origWidth := img.Width;
     origHeight := img.Height div fNumColorPlanes;
     scaleX := origWidth/newWidth;
     scaleY := origHeight/newHeight;

     imgVals := img.SubMatrix;
     SetLength(resImg, newWidth*newHeight*NumColorPlanes);

     kernelSize := Round(Max(scaleX, scaleY));

     if kernelSize >= 2 then
     begin
          if kernelSize mod 2 = 0 then
             inc(kernelSize);

          // #############################################################
          // #### Apply mean kernel:  // todo: the moving average kernel is far from optimal!
          // use a gausian like or a triangle kernel
          SetLength(kernel, kernelSize*kernelSize);
          for x := 0 to Length(kernel) - 1 do
              kernel[x] := 1/Length(kernel);

          ApplyKernel(imgVals, origWidth, origHeight, kernel, kernelSize);
     end;

     // ############################################################
     // #### bilinear mapping
     if NumColorPlanes = 1
     then
         BilinearMapping1Color(imgVals, origWidth, origHeight, resImg, newWidth, newHeight)
     else
         BilinearMappingNColor(imgVals, origWidth, origHeight, resImg, newWidth, newHeight);

     // ##############################################################
     // #### Build result
     Result := TDoubleMatrix.Create;
     Result.Assign(resImg, newWidth, newHeight*NumColorPlanes);
end;

end.
