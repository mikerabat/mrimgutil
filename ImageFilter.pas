unit ImageFilter;

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

interface

uses Windows, SysUtils, Classes, Matrix, Types, MatrixConst;

type
  TImageFilter = class(TObject)
  private
    fNumColorPlanes: integer;
    fKernel : TDoubleDynArray;
    fKernelW, fKernelH : integer;

    // temp vars
    fnumLineElem : integer;
    fImg : PConstDoubleArr;
    fOfsX, fOfsY : integer; // temp vars for ApplyKernel
    
    procedure ApplyKernel1Color(img : PConstDoubleArr; imgWidth, imgHeight : integer; LineWidth : integer; 
                               dest : PConstDoubleArr; destLineWidth : integer);
    function ApplyKernel(x, y: integer): double; inline; 
  public
    function Filter(img : TDoubleMatrix; NumColorPlanes : integer) : TDoubleMatrix;
  
    constructor Create(kernel : TDoubleMatrix); overload;
    constructor Create(const kernel : Array of Double; width, height : integer); overload;
  end;


// ###########################################
// #### Some predefined filters
type
  TMeanFilter = class(TImageFilter)
  public
    constructor Create(kernelSize : integer);
  end;

type
  TSobelFilter1 = class(TImageFilter)
  public
    constructor Create(vertFilt : boolean);
  end;

implementation

uses Math;

{ TImageFilter }

function TImageFilter.ApplyKernel(x, y : integer) : double; 
var kernelX, kernelY : integer;
    idx : integer;
    imgIdx : integer;
begin
     idx := 0;
     Result := 0;
     for kernelY := 0 to fKernelH - 1 do
     begin
          imgIdx := (y + fofsY + kernely)*fnumLineElem + x + fofsX;
          for kernelx := 0 to fKernelW - 1 do
          begin
               Result := Result + fimg^[imgIdx]*fKernel[idx];
               inc(idx);
               inc(imgIdx);
          end;
     end;
end;

procedure TImageFilter.ApplyKernel1Color(img: PConstDoubleArr; imgWidth, imgHeight,
  LineWidth: integer; dest : PConstDoubleArr; destLineWidth : integer);
var x, y : integer;
    numDestLine : integer;
    
function BorderIdx(dx, dy : integer) : integer;
var aX, aY : integer;
begin
     aX := Abs(dx + x);
     aY := Abs(dy + y);

     if aX >= imgWidth then
        aX := x - dx;
     if aY >= imgHeight then
        aY := y - dy;

     Result := aY*fnumLineElem + aX;
end;

function ApplyKernelBorder(x, y : integer) : double;
var kernelX, kernelY : integer;
    idx : integer;
begin
     idx := 0;
     Result := 0;
     for kernelY := 0 to fKernelH - 1 do
     begin
          for kernelx := 0 to fKernelW - 1 do
          begin
               Result := Result + img^[BorderIdx(fofsX + kernelx, fofsY + kernely)]*fKernel[idx];
               inc(idx);
          end;
     end;
end;

begin
     fnumLineElem := LineWidth div sizeof(double);
     numDestLine := destLineWidth div sizeof(double);
     
     fImg := img;
     fofsX := -fKernelW div 2;
     fofsY := -fKernelH div 2;
     
     // ###########################################
     // #### border handling -> Mirror the Image
     for y := 0 to fKernelH - 1 do
         for x := 0 to imgWidth - 1 do
             dest[y*numDestLine + x] := ApplyKernelBorder(x, y);

     for y := Max(fKernelH, imgHeight - 1 - fKernelH) to imgHeight - 1 do
         for x := 0 to imgwidth - 1 do
             dest[y*numDestLine + x] := ApplyKernelBorder(x, y);

     for y := fKernelH to imgHeight - 1 - fKernelH - 1 do
         for x := 0 to fkernelW - 1 do
             dest[y*numDestLine + x] := ApplyKernelBorder(x, y);

     for y := fKernelH to imgHeight - 1 - fKernelH - 1 do
         for x := Max(fKernelW, imgWidth - 1 - fKernelW) to imgWidth - 1 do
             dest[y*numDestLine + x] := ApplyKernelBorder(x, y);

     // ###########################################
     // #### Main Loop
     for y := fKernelH to imgHeight - fKernelH - 1 do
         for x := fKernelW to imgWidth - fKernelW - 1 do
             dest[y*numDestLine + x] := ApplyKernel(x, y);
end;

constructor TImageFilter.Create(const kernel: array of Double; width, height : integer);
begin
     SetLength(fkernel, Length(kernel));
     if Length(kernel) > 0 then
        Move(kernel[0], fKernel[0], Length(fKernel)*sizeof(double));
     fKernelW := width;
     fKernelH := Height;

     inherited Create;
end;

constructor TImageFilter.Create(kernel: TDoubleMatrix);
begin
     fKernel := kernel.SubMatrix;
     fKernelW := kernel.Width;
     fKernelH := kernel.Height;

     inherited Create;
end;

function TImageFilter.Filter(img: TDoubleMatrix;
  NumColorPlanes: integer): TDoubleMatrix;
var mem : PConstDoubleArr;
    lineW : integer;
    inImg : PByte;
    outImg : PByte;
    plane: Integer;
begin
     // filtering can only be applied if the the image is at least as big as the kernel
     if (img.Width < fKernelW) or (img.Height < fKernelH) then
     begin
          Result := TDoubleMatrix.Create;
          Result.Assign(img);    
     
          exit;
     end;
        
     fNumColorPlanes := NumColorPlanes;

     mem := GetMemory((img.Width + img.Width and 1)*img.Height*sizeof(double));
     lineW := (img.Width + img.Width and 1)*sizeof(double);
     
     inImg := PByte(img.StartElement);
     outImg := PByte(mem);
          
     for plane := 0 to fNumColorPlanes - 1 do
     begin
          ApplyKernel1Color(PConstDoubleArr(inImg), img.Width, img.Height div NumColorPlanes, img.LineWidth, 
                            PConstDoubleArr(outImg), lineW);
          inc(inImg, img.LineWidth*(img.Height div NumColorPlanes));
          inc(outImg, lineW*(img.Height div NumColorPlanes));
     end;

     // note: resulting matrix owns memory!
     Result := TDoubleMatrix.Create(PDouble(mem), lineW, img.Width, img.Height);
end;

{ TMeanFilter }

constructor TMeanFilter.Create(kernelSize: integer);
var aKernel : TDoubleDynArray;
    value : double;
    x: Integer;
begin
     SetLength(aKernel, kernelSize*kernelSize);
     
     value := 1/Length(aKernel);
     for x := 0 to Length(aKernel) - 1 do
         aKernel[x] := value;

     inherited Create(aKernel, kernelSize, kernelSize);
end;

{ TSobelFilter1 }

constructor TSobelFilter1.Create(vertFilt: boolean);
const cVertSobel : Array[0..8] of double = (1, 0, -1, 2, 0, -2, 1, 0, -1);
      cHorzSobel : Array[0..8] of double = (1, 2, 1, 0, 0, 0, -1, -2, -1);
begin
     if vertFilt 
     then
         inherited Create(cVertSobel, 3, 3)
     else
         inherited Create(cHorzSobel, 3, 3);
end;

end.
