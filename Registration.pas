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

unit Registration;

// #######################################################
// #### Base classes for rigid and non rigid registration problems
// #######################################################

interface

uses SysUtils, Classes, PtsDefinitions, Graphics, Matrix, BaseMathPersistence;

// #######################################################
// #### Mapping learning data object
type
  TPtsMappingObj = class(TBaseMathPersistence)
  private
    fPts : TDoubleMatrix;
    function GetHmgMatrix: TDoubleMatrix;
    function GetMatrix: TDoubleMatrix;
    function GetDim: integer;
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
  public
    property PtsAsMtxRef : TDoubleMatrix read fPts;
    property PtsAsMtx : TDoubleMatrix read GetMatrix;
    property HmgPtsAsMtx : TDoubleMatrix read GetHmgMatrix;
    property Dim : integer read GetDim;

    constructor Create(pts : TDoubleMatrix; ownsObj : boolean = False); overload;
    constructor Create(pts : TDynPointf1dArray); overload;
    constructor Create(pts : TDynPointf2dArray); overload;
    constructor Create(pts : TDynPointf3dArray); overload;
    constructor Create(const pts : Array of TPointf1D); overload;
    constructor Create(const pts : Array of TPointf2D); overload;
    constructor Create(const pts : Array of TPointf3D); overload;

    destructor Destroy; override;
  end;

type
  TRegistrationHeadMagic = Array[0..10] of AnsiChar;
  TRegistrationHeader = packed record
    magic : TRegistrationHeadMagic;
    ver : integer;
  end;
// #######################################################
// #### base class with common registration properties
type
  ERegistrationError = class(Exception);
  TBasePtsRegistration = class(TBaseMathPersistence)
  protected
    procedure DefineProps; override;
  public
    function MapPoints(const pts : TPtsMappingObj) : TPtsMappingObj; virtual; abstract;

    // special 2D warps
    function MapImage(img : TDoubleMatrix) : TDoubleMatrix; overload;
    function MapImage(img : TDoubleMatrix; NumColorPlanes : integer) : TDoubleMatrix; overload; virtual;
    function MapImage(img : TBitmap) : TBitmap; overload; virtual;

    constructor Create;
  end;
  TBasePtsFromToMapping = class(TBasePtsRegistration)
  public
    procedure CreatePointMapping(fromPts, toPts : TPtsMappingObj); overload; virtual; abstract;
  end;
  TBasePtsRegistrationClass = class of TBasePtsRegistration;

implementation

uses Math;

const cPtsMappingObjDescr = 'PtsMappingObj';
      cPtsObj = 'PtsObj';

{ TBasePtsRegistration }

constructor TBasePtsRegistration.Create;
begin
     inherited;
end;

procedure TBasePtsRegistration.DefineProps;
begin
     // do nothing in the base class
end;

function TBasePtsRegistration.MapImage(img: TDoubleMatrix): TDoubleMatrix;
begin
     Result := MapImage(img, 1);
end;

function TBasePtsRegistration.MapImage(img: TBitmap): TBitmap;
var x, y : integer;
    pts : TPtsMappingObj;
    actPts : TDynPointf2DArray;
    newPts : TPtsMappingObj;
    ptsmtx : TDoubleMatrix;
    imgx, imgy : integer;
    w, h : integer;
    factx : single;
    facty : single;
    numIter : integer;
    iterIdx : integer;
    pixCnt : integer;
    inScanLines : Array of PByte;
    scanLines : Array of PByte;
    inActScan : PByte;
    inActScanP1 : PByte;
    inActScanPY1 : PByte;
    outScan : PByte;
    wm1, hm1 : integer;
begin
     assert(img.PixelFormat in [pf8Bit, pf16Bit, pf24Bit, pf32Bit], 'Unsupported pixel format');

     case img.PixelFormat of
       pf8bit: iterIdx := 0;
       pf16bit: iterIdx := 1;
       pf24bit: iterIdx := 2;
       pf32bit: iterIdx := 3;
     else
         raise Exception.Create('Unsupported pixel format');
     end;
     numIter := iterIdx + 1;

     // ################################################
     // ##### Init scanlines
     Result := TBitmap.Create;
     Result.PixelFormat := img.PixelFormat;
     Result.SetSize(img.Width, img.Height);

     SetLength(scanLines, img.Height);
     SetLength(inScanLines, img.Height);
     for y := 0 to img.Height - 1 do
     begin
          scanLines[y] := Result.ScanLine[y];
          inScanLines[y] := img.ScanLine[y];
     end;

     w := img.Width;
     h := img.Height;
     wm1 := w - 1;
     hm1 := h - 1;

     SetLength(actPts, img.Height);
     for x := 0 to img.Width - 1 do
     begin
          for y := 0 to img.Height - 1 do
          begin
               actPts[y].x := x;
               actPts[y].y := y;
          end;

          pts := TPtsMappingObj.Create(actPts);
          try
             // note: the mapping must be defined from the target image to the source image (vice versa as expected)
             // - this is needed since the mapping would generate holes instead
             newPts := MapPoints(pts);
             try
                ptsmtx := newPts.fPts;  // hack (shortcut)

                // billinear mapping from one image to the next (it's just nicer)
                for y := 0 to ptsmtx.Height - 1 do
                begin
                     imgx := Floor(ptsmtx[0, y]);
                     imgy := Floor(ptsmtx[1, y]);
                     factx := Frac(ptsmtx[0, y]);
                     facty := Frac(ptsmtx[1, y]);

                     outScan := scanLines[y];
                     inc(outScan, numIter*x);

                     if (imgx >= 0) and (imgy >= 0) then
                     begin
                          if (imgx < w) and (imgy < h) then
                          begin
                               inActScan := inScanLines[imgy];
                               inc(inActScan, imgx*numIter);
                               if (imgx < w - 1) and (imgy < h - 1) then
                               begin
                                    inActScanP1 := inActScan;
                                    inc(inActScanP1, numIter);
                                    inActScanPY1 := inScanLines[imgy + 1];
                                    inc(inActScanPY1, imgx*numIter);
                                    for pixCnt := 0 to iterIdx do
                                    begin
                                         outScan^ := Floor((1 - factx)*(1 - facty)*inActScan^ +
                                                           factx*(1 - facty)*inActScanP1^ +
                                                           facty*(1 - factx)*inActScanPY1^ +
                                                           factx*facty*PByte(Cardinal(inActScanPY1) + Cardinal(numIter))^
                                                           );
                                         inc(outScan);
                                         inc(inActScan);
                                         inc(inActScanP1);
                                         inc(inActScanPY1);
                                    end;
                               end
                               else
                                   Move(inActScan^, outScan^, numIter);
                          end
                          else
                          begin
                               imgx := Max(0, Min(wm1, imgx));
                               imgy := Max(0, Min(hm1, imgy));
                               inActScan := inScanLines[imgy];
                               inc(inActScan, imgx*numIter);
                               Move(inActScan^, outScan^, numIter);
                          end;
                     end
                     else
                     begin
                          imgx := Max(0, Min(wm1, imgx));
                          imgy := Max(0, Min(hm1, imgy));
                          inActScan := inScanLines[imgy];
                          inc(inActScan, imgx*numIter);
                          Move(inActScan^, outScan^, numIter);
                     end;
                end;
             finally
                    newPts.Free;
             end;
          finally
                 pts.Free;
          end;
     end;
end;

function TBasePtsRegistration.MapImage(img: TDoubleMatrix; NumColorPlanes : integer): TDoubleMatrix;
var x, y : integer;
    pts : TPtsMappingObj;
    actPts : TDynPointf2DArray;
    newPts : TPtsMappingObj;
    ptsmtx : TDoubleMatrix;
    imgx, imgy : integer;
    w, h : integer;
    factx : double;
    facty : double;
    wm1, hm1 : integer;
    yOffset : integer;
    colorChannel: Integer;
begin
     assert(NumColorPlanes > 0);
     Result := TDoubleMatrix.Create(img.Width, img.Height);
     w := img.Width;
     h := img.Height div NumColorPlanes;

     wm1 := w - 1;
     hm1 := h - 1;

     SetLength(actPts, h);
     for x := 0 to img.Width - 1 do
     begin
          for y := 0 to h - 1 do
          begin
               actPts[y].x := x;
               actPts[y].y := y;
          end;

          pts := TPtsMappingObj.Create(actPts);
          try
             // note: the mapping must be defined from the target image to the source image (vice versa as expected)
             // - this is needed since the mapping would generate holes instead
             newPts := MapPoints(pts);
             try
                ptsmtx := newPts.fPts;  // hack (shortcut)

                // billinear mapping from one image to the next (it's just nicer)
                for y := 0 to ptsmtx.Height - 1 do
                begin
                     imgx := Floor(ptsmtx[0, y]);
                     imgy := Floor(ptsmtx[1, y]);
                     factx := Frac(ptsmtx[0, y]);
                     facty := Frac(ptsmtx[1, y]);

                     if (imgx >= 0) and (imgy >= 0) then
                     begin
                          if (imgx < w) and (imgy < h) then
                          begin
                               if (imgx < w - 1) and (imgy < h - 1) then
                               begin
                                    yOffset := 0;
                                    for colorChannel := 0 to NumColorPlanes - 1 do
                                    begin
                                         Result[x, y + yOffset] := (1 - factx)*(1 - facty)*img[imgx, imgy + yOffset] +
                                                                    factx*(1 - facty)*img[imgx + 1, imgy + yOffset] +
                                                                    facty*(1 - factx)*img[imgx, imgy + 1 + yOffset] +
                                                                    factx*facty*img[imgx + 1, imgy + 1 + yOffset];
                                         inc(yOffset, h);
                                    end;
                               end
                               else
                               begin
                                    yOffset := 0;
                                    for colorChannel := 0 to NumColorPlanes - 1 do
                                    begin
                                         Result[x, y + yOffset] := img[imgx, imgy + yOffset];
                                         inc(yOffset, h);
                                    end;
                               end;
                          end
                          else
                          begin
                               yOffset := 0;
                               for colorChannel := 0 to NumColorPlanes - 1 do
                               begin
                                    Result[x, y + yOffset] := img[Max(0, Min(wm1, imgx)), yOffset + Max(0, Min(hm1, imgy))];
                                    inc(yOffset, h);
                               end;
                          end;
                     end
                     else
                     begin
                          yOffset := 0;
                          for colorChannel := 0 to NumColorPlanes - 1 do
                          begin
                               Result[x, y + yOffset] := img[Max(0, Min(wm1, imgx)), yOffset + Max(0, Min(hm1, imgy))];
                               inc(yOffset, h);
                          end;
                     end;
                end;
             finally
                    newPts.Free;
             end;
          finally
                 pts.Free;
          end;
     end;
end;

{ TPtsMappingObj }

constructor TPtsMappingObj.Create(pts: TDynPointf2dArray);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(2, Length(pts));
     for y := 0 to fPts.Height - 1 do
     begin
          fPts[0, y] := pts[y].x;
          fPts[1, y] := pts[y].y;
     end;
end;

constructor TPtsMappingObj.Create(pts: TDynPointf1dArray);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(1, Length(pts));
     for y := 0 to fPts.Height - 1 do
         fPts[0, y] := pts[y];
end;

constructor TPtsMappingObj.Create(pts: TDoubleMatrix; ownsObj : boolean);
begin
     inherited Create;

     if ownsObj
     then
         fPts := pts
     else
     begin
          fPts := TDoubleMatrix.Create;
          fPts.Assign(pts);
     end;
end;

constructor TPtsMappingObj.Create(pts: TDynPointf3dArray);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(3, Length(pts));
     for y := 0 to fPts.Height - 1 do
     begin
          fPts[0, y] := pts[y].x;
          fPts[1, y] := pts[y].y;
          fPts[2, y] := pts[y].z;
     end;
end;

procedure TPtsMappingObj.DefineProps;
begin
     AddObject(cPtsObj, fPts);
end;

destructor TPtsMappingObj.Destroy;
begin
     fPts.Free;

     inherited;
end;

function TPtsMappingObj.GetDim: integer;
begin
     Result := fPts.Width;
end;

function TPtsMappingObj.GetHmgMatrix: TDoubleMatrix;
var y : integer;
    hmgX : integer;
    x : integer;
begin
     Result := TDoubleMatrix.Create(fPts.Width + 1, fPts.Height);
     hmgX := fPts.Width;
     for y := 0 to fPts.Height - 1 do
     begin
          for x := 0 to fPts.Width - 1 do
              Result[x, y] := fPts[x, y];
          Result[hmgX, y] := 1;
     end;
end;

function TPtsMappingObj.GetMatrix: TDoubleMatrix;
begin
     Result := TDoubleMatrix.Create;
     Result.Assign(fPts, True);
end;

function TPtsMappingObj.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(cPtsObj, Name) = 0
     then
         fPts := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

constructor TPtsMappingObj.Create(const pts: array of TPointf1D);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(1, Length(pts));
     for y := 0 to fPts.Height - 1 do
         fPts[0, y] := pts[y];
end;

constructor TPtsMappingObj.Create(const pts: array of TPointf2D);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(2, Length(pts));
     for y := 0 to fPts.Height - 1 do
     begin
          fPts[0, y] := pts[y].x;
          fPts[1, y] := pts[y].y;
     end;
end;

class function TPtsMappingObj.ClassIdentifier: String;
begin
     Result := cPtsMappingObjDescr;
end;

constructor TPtsMappingObj.Create(const pts: array of TPointf3D);
var y : integer;
begin
     inherited Create;

     fPts := TDoubleMatrix.Create(3, Length(pts));
     for y := 0 to fPts.Height - 1 do
     begin
          fPts[0, y] := pts[y].x;
          fPts[1, y] := pts[y].y;
          fPts[2, y] := pts[y].z;
     end;
end;

initialization
   RegisterMathIO(TPtsMappingObj);

end.


