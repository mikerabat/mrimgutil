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

unit LinearTriangulationTransformation;

// ###############################################################
// #### Linear - Triangle to Triangle - warping/mapping
// ###############################################################

interface

uses SysUtils, Registration, Types, Triangulation, Matrix, Graphics, PtsDefinitions, BaseMathPersistence, MatrixConst;

// implements a lineare triangle mapping
type
  TLinearTriangulationMapping = class(TBasePtsRegistration)
  private
    type
      TTriangleMapRec = record
         alpha : single;
         beta : single;
         gamma : single;
         SSEDummy : single;
      end;
      PTriangleMapRec = ^TTriangleMapRec;
      TTriangleMapRecArr = Array[0..MaxInt div sizeof(TTriangleMapRec) - 1] of TTriangleMapRec;
      PTriangleMapRecArr = ^TTriangleMapRecArr;
      TSmallSingleArr = Array[0..3] of single;
      PSmallSingleArr = ^TSmallSingleArr;

    type
      TMapFunction = procedure(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
                               actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
                               Width, Height : integer) of Object; stdcall;
  private
    fPts : TDynPointf2DArray;
    fFromPts : TDynPointf2DArrayS;
    fToPts : TDynPointf2DArray;
    fTriangulation : TBaseTriangulation;
    fTriangleFacts : TSingleDynArray;
    fTriangleFactsInv : TSingleDynArray;
    fPtToTriangleMap : PTriangleMapRecArr;
    fPtToTriMem : Pointer;
    fPtToMapNumElem : integer;
    fTriangles : Array of smallInt;
    fImgWidth, fImgHeight : integer;
    fPrevTriangle : integer;
    fMapImage1ColorPlane : TMapFunction;
    fMapImage3ColorPlane : TMapFunction;


    fSSEMem : Pointer;
    fSSEPts1, fSSEPts2 : PSmallSingleArr;

    procedure MapImagePascal(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
                             actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
                             Width, Height : integer); stdcall;
    procedure MapImagePascal3(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
                          actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
                          Width, Height : integer); stdcall;
    procedure MapImageSSE(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
                          actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
                          Width, Height : integer); stdcall;

    procedure MapImageSSE3(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
                          actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
                          Width, Height : integer); stdcall;
    procedure InitTriangleFactors;
    function IsInTriangle(const pt : TPointf2D; triNum : integer; var alpha, beta, gamma : single) : boolean;
    function IsInTriangles(const pt : TPointf2D; var triNum : integer; var alpha, beta, gamma : single) : boolean;
    procedure InitPtToTriangleMap(imgWidth, imgHeight : integer);
    function GetNumTriangles: integer;
  protected
    // persitence functions
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;
    procedure OnLoadBinaryProperty(const Name : String; const Value; size : integer); override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;

    // must be overridden by ancestor classes
    function GetTriangulatorClass : TBaseTriangulationClass; virtual; abstract;
  public
    property NumTriangles : integer read GetNumTriangles;
    property Triangulation : TBaseTriangulation read fTriangulation;

    procedure AfterConstruction; override;
    procedure InitTriangulation(pts : TPtsMappingObj; destImgWidth : integer = -1; destImgHeight : integer = -1);

    function CalcFactorsForPoints(const pt: TPointf2D; var ptIdx : TTriangleDef; var alpha, beta, gamma: single): boolean;
    function MapPoints(const pts : TPtsMappingObj) : TPtsMappingObj; override;

    // note the points must be compatible with the triangulation - e.g.
    // point[0] must be used as if it is point fPts[0] from which the triangulation has been
    // created.
    procedure InitPtsForMapping(FromPts : TPtsMappingObj); overload;
    procedure InitPtsForMapping(FromPts : TDoubleMatrix); overload;
    procedure InitPtsForMapping(FromPts, ToPts : TPtsMappingObj); overload;

    // special 2D warps
    function MapImage(img : TDoubleMatrix; NumColorPlanes : integer) : TDoubleMatrix; overload; override;
    function MapImage(img : TBitmap) : TBitmap; overload; override;

    procedure InitMappingFuncs(useSSE : boolean);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Math, CPUFeatures, ImageMatrixConv;

type
  THackDoublMtx = class(TDoubleMatrix);

{$IFNDEF CPUX64}
type
  NativeUInt = Cardinal;
{$ENDIF}

const cMapLinear = 'LinearMapping';
      cMapLinPts = 'LinMapPts';
      cMapLinFromPts = 'LinMapFromPts';
      cMapLinToPts = 'LinMapToPts';
      cMapLinTriangulation = 'LinMapTriangulation';
      cMapLinWidth = 'LinMapWidth';
      cMapLinHeight = 'LinMapHeight';
      cMapLinNumChannels = 'LinMapChannels';

const cOnes : Array[0..1] of single = (1, 1);

{ TLinearTriangulationMapping }

procedure TLinearTriangulationMapping.InitPtsForMapping(FromPts: TPtsMappingObj);
begin
     fToPts := fPts;
     fFromPts := MatrixToPointsS(FromPts.PtsAsMtxRef);

     // prepare triangle map -> fast association of pt to triangle
     if not Assigned(fPtToTriMem) and (fImgWidth > 0) and (fImgHeight > 0) then
        InitPtToTriangleMap(fImgWidth, fImgHeight);
end;

procedure TLinearTriangulationMapping.InitPtsForMapping(FromPts: TDoubleMatrix);
begin
     fToPts := fPts;
     fFromPts := MatrixToPointsS(FromPts);

     // prepare triangle map -> fast association of pt to triangle
     if not Assigned(fPtToTriMem) and (fImgWidth > 0) and (fImgHeight > 0) then
        InitPtToTriangleMap(fImgWidth, fImgHeight);
end;

function TLinearTriangulationMapping.CalcFactorsForPoints(const pt: TPointf2D; var ptIdx : TTriangleDef;
  var alpha, beta, gamma: single): boolean;
var triNum : integer;
begin
     assert(Assigned(fToPts) and Assigned(fFromPts), 'Error points not initialized!');
     
     triNum := fPrevTriangle;
     Result := IsInTriangles(pt, triNum, alpha, beta, gamma);
     fPrevTriangle := triNum;

     if Result then
     begin
          ptIdx[0] := fTriangulation.Triangles[triNum][0];
          ptIdx[1] := fTriangulation.Triangles[triNum][1];
          ptIdx[2] := fTriangulation.Triangles[triNum][2];
     end;
end;

class function TLinearTriangulationMapping.ClassIdentifier: String;
begin
     Result := cMapLinear;
end;

procedure TLinearTriangulationMapping.AfterConstruction;
begin
     inherited;

     fImgWidth := -1;
     fImgHeight := -1;
     fPrevTriangle := -1;

     InitMappingFuncs(True);

     fPtToMapNumElem := 0;
     fPtToTriangleMap := nil;
     fPtToTriMem := nil;
     fSSEMem := AllocMem(16 + 2*sizeof(TSmallSingleArr));
     fSSEPts1 := PSmallSingleArr(NativeUInt(fSSEMem) + 16 - (NativeUInt(fSSEMem) and $0000000F));
     fSSEPts2 := fSSEPts1;
     inc(fSSEPts2);
end;

constructor TLinearTriangulationMapping.Create;
begin
     inherited Create;
end;

procedure TLinearTriangulationMapping.DefineProps;
begin
     AddIntProperty(cMapLinWidth, fImgWidth);
     AddIntProperty(cMapLinHeight, fImgHeight);

     if Assigned(fTriangulation) then
        AddObject(cMapLinTriangulation, fTriangulation);
     if Length(fPts) > 0 then
        AddBinaryProperty(cMapLinPts, fPts[0], Length(fPts)*sizeof(fPts[0]));
     if Length(fFromPts) > 0 then
        AddBinaryProperty(cMapLinFromPts, fFromPts[0], Length(fFromPts)*sizeof(fFromPts[0]));
     if Length(fToPts) > 0 then
        AddBinaryProperty(cMapLinToPts, fToPts[0], Length(fToPts)*sizeof(fToPts[0]));
end;

destructor TLinearTriangulationMapping.Destroy;
begin
     fTriangulation.Free;
     if Assigned(fSSEMem) then
        FreeMem(fSSEMem);
     if Assigned(fPtToTriMem) then
        FreeMem(fPtToTriMem);
     
     inherited;
end;

function TLinearTriangulationMapping.GetNumTriangles: integer;
begin
     Result := Length(fTriangles);
end;

procedure TLinearTriangulationMapping.InitMappingFuncs(useSSE: boolean);
begin
     if useSSE and IsSSE3Present then
     begin
          fMapImage1ColorPlane := MapImageSSE;
          fMapImage3ColorPlane := MapImageSSE3;
     end
     else
     begin
          fMapImage1ColorPlane := MapImagePascal;
          fMapImage3ColorPlane := MapImagePascal3;
     end;
end;

procedure TLinearTriangulationMapping.InitPtsForMapping(FromPts,
  ToPts: TPtsMappingObj);
begin
     fFromPts := MatrixToPointsS(FromPts.PtsAsMtxRef);
     fToPts := MatrixToPoints(ToPts.PtsAsMtxRef);

     fPrevTriangle := -1;
     if Assigned(fPtToTriMem) then
        FreeMem(fPtToTriMem);
     fPtToTriMem := nil;
     fTriangles := nil;
     fPtToTriangleMap := nil;
     fTriangleFacts := nil;

     // prepare triangle map -> fast association of pt to triangle
     if (fImgWidth > 0) and (fImgHeight > 0) then
        InitPtToTriangleMap(fImgWidth, fImgHeight);
end;

procedure TLinearTriangulationMapping.InitPtToTriangleMap(imgWidth,
  imgHeight: integer);
var i: Integer;
    x, y : integer;
    pt : TPointf2D;
    triNum : integer;
    alpha, beta, gamma : single;
begin
     fPtToMapNumElem := imgWidth*imgHeight;
     GetMem(fPtToTriMem, 16 + fPtToMapNumElem*sizeof(TTriangleMapRec));
     fPtToTriangleMap := PTriangleMapRecArr(NativeUint(fPtToTriMem) + 16 - (NativeUint(fPtToTriMem) and $0000000F));
     SetLength(fTriangles, fPtToMapNumElem);

     for i := 0 to fPtToMapNumElem - 1 do
         fTriangles[i] := -1;

     fPrevTriangle := -1;

     // prepare the map -> all points within the convex hull get warped thus
     // get an index greater than 0
     for y := 0 to imgHeight - 1 do
     begin
          pt.y := y;
          for x := 0 to imgWidth - 1 do
          begin
               pt.x := x;
               if IsInTriangles(pt, triNum, alpha, beta, gamma) then
               begin
                    fTriangles[x + y*imgWidth] := triNum;
                    fPtToTriangleMap[x + y*imgWidth].alpha := alpha;
                    fPtToTriangleMap[x + y*imgWidth].beta := beta;
                    fPtToTriangleMap[x + y*imgWidth].gamma := gamma;
                    fPtToTriangleMap[x + y*imgWidth].SSEDummy := 0;

                    fPrevTriangle := triNum;
               end;
          end;
     end;

     fTriangleFacts := nil;
end;

procedure TLinearTriangulationMapping.InitTriangleFactors;
var i: Integer;
    x1, x2, x3, y1, y2, y3 : double;
begin
     assert(Assigned(fPts));

     SetLength(fTriangleFacts, Length(fTriangulation.Triangles));
     SetLength(fTriangleFactsInv, Length(fTriangulation.Triangles));

     for i := 0 to Length(fTriangleFacts) - 1 do
     begin
          x1 := fToPts[fTriangulation.Triangles[i][0]].x;
          x2 := fToPts[fTriangulation.Triangles[i][1]].x;
          x3 := fToPts[fTriangulation.Triangles[i][2]].x;
          y1 := fToPts[fTriangulation.Triangles[i][0]].y;
          y2 := fToPts[fTriangulation.Triangles[i][1]].y;
          y3 := fToPts[fTriangulation.Triangles[i][2]].y;

          fTriangleFacts[i] := -x2*y3 + x2*y1 + x1*y3 + x3*y2 - x3*y1 - x1*y2;
          if Abs(fTriangleFacts[i]) > 1e-10
          then
              fTriangleFactsInv[i] := 1/fTriangleFacts[i]
          else
              fTriangleFactsInv[i] := 0;
     end;
end;

procedure TLinearTriangulationMapping.InitTriangulation(pts: TPtsMappingObj; destImgWidth, destImgHeight : integer);
begin
     assert(pts.Dim = 2, 'Error only 2 dimensions are allowed');

     FreeAndNil(fTriangulation);
     fPts := MatrixToPoints(pts.PtsAsMtxRef);
     fTriangulation := GetTriangulatorClass.Create;
     fTriangulation.CreateTriangulation(fpts);

     if Assigned(fPtToTriMem) then
        FreeMem(fPtToTriMem);
     fPtToTriMem := nil;
     fPtToTriangleMap := nil;
     
     // preparations to warp the image to a different image size:
     fImgWidth := destImgWidth;
     fImgHeight := destImgHeight;
end;

function TLinearTriangulationMapping.IsInTriangle(const pt: TPointf2D;
  triNum: integer; var alpha, beta, gamma: single): boolean;
var x, y, x1, x2, x3, y1, y2, y3 : double;
    triFact : double;
begin
     Result := False;

	   x := pt.x;
	   y := pt.y;

     x1 := fToPts[fTriangulation.Triangles[triNum][0]].x;
     x2 := fToPts[fTriangulation.Triangles[triNum][1]].x;
     x3 := fToPts[fTriangulation.Triangles[triNum][2]].x;

     // base bounding test on x
     if (x < Min(x1, Min(x2, x3))) or (x > Max(x1, Max(x2, x3))) then
        exit;

     y1 := fToPts[fTriangulation.Triangles[triNum][0]].y;
     y2 := fToPts[fTriangulation.Triangles[triNum][1]].y;
     y3 := fToPts[fTriangulation.Triangles[triNum][2]].y;

     if (y < Min(y1, Min(y2, y3))) or (y > Max(y1, Max(y2, y3))) then
        exit;

	    alpha := -y*x3 + y3*x - x*y2 + x2*y - x2*y3 + x3*y2;
	    beta := y*x3 - x1*y - x3*y1 - y3*x + x1*y3 + x*y1;
	    gamma := x*y2 - x*y1 - x1*y2 -x2 *y + x2*y1 + x1*y;

     triFact := fTriangleFacts[triNum];

     if triFact >= 0
     then
         Result := (alpha >= 0) and (alpha <= triFact) and
                   (beta >= 0) and (beta <= triFact) and
                   (gamma >= 0) and (gamma <= triFact)
     else
         Result := (alpha <= 0) and (alpha >= triFact) and
                   (beta <= 0) and (beta >= triFact) and
                   (gamma <= 0) and (gamma >= triFact);

     if Result then
     begin
		        alpha := alpha*fTriangleFactsInv[triNum];
		        beta := beta*fTriangleFactsInv[triNum];
		        gamma := 1 - alpha - beta;
     end;
end;

function TLinearTriangulationMapping.IsInTriangles(const pt: TPointf2D; var triNum: integer;
  var alpha, beta, gamma : single): boolean;
var idx : integer;
begin
     if not Assigned(fTriangleFacts) then
        InitTriangleFactors;

     Result := True;
     // look in the previous triangle
     if (fPrevTriangle >= 0) and (IsInTriangle(pt, triNum, alpha, beta, gamma)) then
        exit;

     for idx := 0 to Length(fTriangulation.Triangles) - 1 do
     begin
          if IsInTriangle(pt, idx, alpha, beta, gamma) then
          begin
               triNum := idx;
               exit;
          end;
     end;

     Result := False;
end;

function TLinearTriangulationMapping.MapImage(img: TDoubleMatrix; NumColorPlanes : integer): TDoubleMatrix;
var res : PDouble;
    pInit : Array of PConstDoubleArr;
    pStart : PDouble;
    y : integer;
begin
     if (fImgWidth <= 0) or (fImgHeight <= 0) then
     begin
          fImgWidth := img.Width;
          fImgHeight := img.Height div NumColorPlanes;

          InitPtToTriangleMap(fImgWidth, fImgHeight);
     end;

     if not Assigned(fPts) or not Assigned(fToPts) then
        raise ERegistrationError.Create('Error InitPtsForMapping must be called before!');
     if not Assigned(fTriangulation) then
        raise ERegistrationError.Create('Error no Triangulation assigned');

     GetMem(res, NumColorPlanes*fImgWidth*fImgHeight*sizeof(double));
     try
        pStart := THackDoublMtx(img).StartElement;
        SetLength(pInit, img.Height);
        for y := 0 to img.Height - 1 do
        begin
             pInit[y] := PConstDoubleArr(pStart);
             inc(PByte(pStart), THackDoublMtx(img).LineWidth);
        end;

        if (fImgWidth > 0) and (fImgHeight > 0) then
        begin
             if NumColorPlanes = 1
             then
                 fMapImage1ColorPlane(pInit, img.Width, img.Height div NumColorPlanes, Res, PTriangleMapRec(fPtToTriangleMap),
                                      @fTriangles[0], @(fTriangulation.Triangles[0][0]), @fFromPts[0],
                                      fSSEPts1, fImgWidth, fImgHeight)
             else
                 fMapImage3ColorPlane(pInit, img.Width, img.Height div NumColorPlanes, Res, PTriangleMapRec(fPtToTriangleMap),
                                      @fTriangles[0], @(fTriangulation.Triangles[0][0]), @fFromPts[0],
                                      fSSEPts1, fImgWidth, fImgHeight);
        end;
     except
           FreeMem(res);
           raise;
     end;

     // the matrix object "owns" the data now:
     Result := TDoubleMatrix.Create(res, fImgWidth*sizeof(double), fImgWidth, fImgHeight*NumColorPlanes);
end;

function TLinearTriangulationMapping.MapImage(img: TBitmap): TBitmap;
var x, y : integer;
    imgx, imgy : integer;
    w, h : integer;
    factx : double;
    facty : double;
    wm1, hm1 : integer;
    actMap : PTriangleMapRec;
    inScanLines : Array of PByte;
    scanLines : Array of PByte;
    srcX : single;
    srcY : single;
    inActScan : PByte;
    inActScanP1 : PByte;
    inActScanPY1 : PByte;
    outScan : PByte;
    numIter : integer;
    pixCnt : integer;
    iterIdx : integer;
    actTri : PSmallInt;
begin
     if (fImgWidth <= 0) or (fImgHeight <= 0) then
     begin
          fImgWidth := img.Width;
          fImgHeight := img.Height;

          InitPtToTriangleMap(fImgWidth, fImgHeight);
     end;

     if not Assigned(fPts) or not Assigned(fToPts) then
        raise ERegistrationError.Create('Error InitPtsForMapping must be called before!');
     if not Assigned(fTriangulation) then
        raise ERegistrationError.Create('Error no Triangulation assigned');
     if fPtToMapNumElem <> img.Width*img.Height  then
        raise ERegistrationError.Create('Error map not yet initialized');

     // note: this function is overridden since consecutive calls to it can be speed up
     // by creating some kind of displacement factors map.
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

     w := img.Width;
     h := img.Height;

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

     wm1 := w - 1;
     hm1 := h - 1;
     actMap := @fPtToTriangleMap[0];
     actTri := @fTriangles[0];
     for y := 0 to h - 1 do
     begin
          for x := 0 to w - 1 do
          begin
               if actTri^ < 0 then
               begin
                    outScan := scanLines[y];
                    inc(outScan, numIter*x);

                    FillChar(outScan^, numIter, 0);
               end
               else
               begin
                    // calculate source coordinate
                    srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].x +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].x +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].x;
                    srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].y +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].y +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].y;

                    // billinear mapping from one image to the next (it's just nicer)
                    imgx := Floor(srcX);
                    imgy := Floor(srcY);
                    factx := Frac(srcX);
                    facty := Frac(srcY);

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
                                                          factx*facty*PByte(NativeUInt(inActScanPY1) + NativeUInt(numIter))^
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

               inc(actMap);
               inc(actTri);
          end;
     end;
end;

procedure TLinearTriangulationMapping.MapImagePascal(const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
  actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
  Width, Height : integer);
var x, y : integer;
    srcX, srcY : single;
    imgX, imgY : integer;
    pSrc, pSrc2 : PConstDoubleArr;
    factx, facty : single;
begin
     actTri := @fTriangles[0];
     actMap := @fPtToTriangleMap[0];
     dec(w);
     dec(h);
     dec(Height);
     dec(Width);

     // go through all pixels
     for y := 0 to Height do
     begin
          for x := 0 to Width do
          begin
               if actTri^ < 0
               then
                   pRes^ := 0 // img[x, y]
               else
               begin
                    (*fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
                    fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
                    fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
                    fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
                    fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
                    fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;
                    *)

                    // calculate source coordinate
                    srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].x +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].x +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].x;
                    srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].y +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].y +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].y;

                    imgx := Trunc(srcX);
                    imgy := Trunc(srcy);

                    // bilinear interpolation:
                    if (imgX < w) and (imgY < h) and (imgX >= 0) and (imgY >= 0) then
                    begin
                         pSrc := pInit[imgy];
                         pSrc2 := pInit[imgy + 1];

                         factx := Frac(srcX);
                         facty := Frac(srcY);

                         pRes^ := (1 - factx)*(1 - facty)*pSrc^[imgx] +
                                   factx*(1 - facty)*pSrc^[imgx + 1] +
                                   facty*(1 - factx)*pSrc2^[imgx] +
                                   factx*facty*pSrc2^[imgx + 1];
                    end
                    else
                    begin
                         pSrc := pInit[Max(0, Min(h, imgY))];
                         pRes^ := pSrc^[Max(0, Min(h, imgX))];
                    end;
               end;

               inc(actMap);
               inc(pRes);
               inc(actTri);
          end;
     end;
end;

procedure TLinearTriangulationMapping.MapImagePascal3(
  const pInit: array of PConstDoubleArr; w, h: integer; pRes: PDouble;
  actMap: PTriangleMapRec; actTri: PSmallInt; pTriangulation: PTriangleDef;
  pPts: PPointf2Ds; pSSEPt: PSmallSingleArr; Width, Height: integer);
var x, y : integer;
    srcX, srcY : single;
    imgX, imgY : integer;
    pSrc, pSrc2 : PConstDoubleArr;
    factx, facty : single;
    multFacts : array[0..3] of single;
    widthHeight : integer;
    colPlaneHeight : integer;
    pRes1 : PDouble;
    pRes2 : PDouble;
begin
     actTri := @fTriangles[0];
     actMap := @fPtToTriangleMap[0];
     widthHeight := (width)*(Height); // width / height use indices!
     colPlaneHeight := h;

     pRes1 := pRes;
     inc(pRes1, widthHeight);
     pRes2 := pRes;
     inc(pRes2, 2*widthHeight);

     dec(width);
     dec(height);

     dec(w);
     dec(h);

     // go through all pixels
     for y := 0 to Height do
     begin
          for x := 0 to Width do
          begin
               if actTri^ < 0 then
               begin
                    pRes^ := 0; // img[x, y]
                    pRes1^ := 0;
                    pRes2^ := 0;
               end
               else
               begin
                    (*
                    fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
                    fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
                    fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
                    fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
                    fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
                    fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;
                    *)

                    // calculate source coordinate
                    srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].x +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].x +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].x;
                    srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actTri^][0]].y +
                            actMap^.beta*fFromPts[fTriangulation.Triangles[actTri^][1]].y +
                            actMap^.gamma*fFromPts[fTriangulation.Triangles[actTri^][2]].y;

                    imgx := Trunc(srcX);
                    imgy := Trunc(srcy);

                    // bilinear interpolation:
                    if (imgX < w) and (imgY < h) and (imgX >= 0) and (imgY >= 0) then
                    begin
                         pSrc := pInit[imgy];
                         pSrc2 := pInit[imgy + 1];

                         factx := Frac(srcX);
                         facty := Frac(srcY);

                         multFacts[0] := (1 - factx)*(1 - facty);
                         multFacts[1] := factx*(1 - facty);
                         multFacts[2] := facty*(1 - factx);
                         multFacts[3] := factx*facty;

                         // blend 3 items (clor planes):
                         pRes^ := multFacts[0]*pSrc^[imgx] +
                                  multFacts[1]*pSrc^[imgx + 1] +
                                  multFacts[2]*pSrc2^[imgx] +
                                  multFacts[3]*pSrc2^[imgx + 1];

                         pSrc := pInit[imgy + colPlaneHeight];
                         pSrc2 := pInit[imgy + colPlaneHeight + 1];
                         pRes1^ := multFacts[0]*pSrc^[imgx] +
                                   multFacts[1]*pSrc^[imgx + 1] +
                                   multFacts[2]*pSrc2^[imgx] +
                                   multFacts[3]*pSrc2^[imgx + 1];

                         pSrc := pInit[imgy + 2*colPlaneHeight];
                         pSrc2 := pInit[imgy + 2*colPlaneHeight + 1];
                         pRes2^ := multFacts[0]*pSrc^[imgx] +
                                   multFacts[1]*pSrc^[imgx + 1] +
                                   multFacts[2]*pSrc2^[imgx] +
                                   multFacts[3]*pSrc2^[imgx + 1];
                    end
                    else
                    begin
                         pSrc := pInit[Max(0, Min(h, imgY))];
                         pRes^ := pSrc^[Max(0, Min(h, imgX))];

                         pSrc := pinit[Max(0, colPlaneHeight + Min(h, imgY))];
                         pRes1^ := pSrc^[Max(0, Min(h, imgX))];

                         pSrc := pinit[Max(0, 2*colPlaneHeight + Min(h, imgY))];
                         pRes2^ := pSrc^[Max(0, Min(h, imgX))];
                    end;
               end;

               inc(actMap);
               inc(pRes);
               inc(pRes1);
               inc(pRes2);
               inc(actTri);
          end;
     end;
end;

// ####################################################
// #### Assembler functions
// ####################################################

{$IFNDEF CPUX64}

procedure TLinearTriangulationMapping.MapImageSSE(
  const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
  actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
  Width, Height : integer);
var iter : integer;
    imgX : integer;
    pSrc, pSrc2 : PConstDoubleArr;
asm
   // #################################################
   // #### Prologue: Init some variables
   push ebx;
   push esi;
   push edi;

   xorpd xmm3, xmm3;
   movq xmm2, cOnes;

   dec w;
   dec h;

   // #################################################
   // #### Go through all pixels
   mov edx, Height;
   imul edx, Width;
   jz @exit;

   mov iter, edx;
   // for iter := 0 to Width*Height - 1 do
   @@forylabel:

        //if actTri^ < 0
        mov eax, actTri;
        movsx eax, smallint ptr [eax];
        cmp eax, 0;
        jl @assignZero;

        // standard triangle procedure
        // calculate source coordinate
        //  fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
//                    fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
//                    fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
//                    fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
//                    fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
//                    fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;
        // init pointers
        mov esi, pPts;
        mov edi, pTriangulation;
        mov ebx, pSSEPt;

        mov eax, actTri;
        movsx eax, smallint ptr [eax];
        lea eax, [eax + 2*eax];         // 3*eax

        mov edx, [edi + 4*eax];         // 4*(3*eax) = index of 3*sizeof(integer)
        mov ecx, [esi + 8*edx];
        mov [ebx], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 16], ecx;

        mov edx, [edi + 4*eax + 4];
        mov ecx, [esi + 8*edx];
        mov [ebx + 4], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 20], ecx;

        mov edx, [edi + 4*eax + 8];
        mov ecx, [esi + 8*edx];
        mov [ebx + 8], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 24], ecx;

        //srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].x +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].x +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].x;
//                  srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].y +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].y +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].y;

        // note this works since the points are int the same range...
        mov ecx, actMap;
        movaps xmm1, [ecx];

        //mov eax, pSSEPt;   // already loaded in ebx
        movaps xmm0, [ebx];
        movaps xmm5, [ebx + 16];

        // multiply 4 values at once - x and y
        mulps xmm0, xmm1;
        mulps xmm5, xmm1;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;

        // store back result
        //movss srcX, xmm0;
        // truncate
        cvttss2si ecx, xmm0;
        mov imgx, ecx;
        //facts[0] := Frac(srcX);   = float(srcX) - Int(src) 
        cvtsi2ss xmm1, ecx;
        subss xmm0, xmm1;

        // store back result
        //movss srcY, xmm5;
        // truncate
        cvttss2si ecx, xmm5;
        //mov imgy, ecx;
        //facts[1] := Frac(srcY);
        cvtsi2ss xmm4, ecx;
        subss xmm5, xmm4;
        // pSrc := pInit[imgy];
        // pSrc2 := pInit[imgy + 1];
        mov eax, pInit;
        mov edx, [eax + ecx*4];
        mov pSrc, edx;
        mov edx, [eax + ecx*4 + 4];
        mov pSrc2, edx;

        // if (imgX < w) and (imgY < h) and (imgX >= 0) and (imgY >= 0) then
        //cmp imgy, 0;
        cmp ecx, 0;
        jl @NoBilinFactAvail;
        cmp imgx, 0;
        jl @NoBilinFactAvail;
        mov edx, w;
        cmp imgx, edx;
        jge @NoBilinFactAvail;
        mov edx, h;
        //cmp imgy, edx;
        cmp ecx, edx
        jge @NoBilinFactAvail;

             // ############################################################
             // #### bilinear interpolation:

             // move both values to xmm0
             movlhps xmm0, xmm5;

             shufps xmm0, xmm5, 8;
//                pRes^ := (1 - factx)*(1 - facty)*pSrc^[imgx] +
//                         factx*(1 - facty)*pSrc^[imgx + 1] +
//                         facty*(1 - factx)*pSrc2^[imgx] +
//                         factx*facty*pSrc2^[imgx + 1];

             // calculate 1 - factors
             movaps xmm1, xmm2;
             subps xmm1, xmm0;
             movlhps xmm0, xmm1;

             // fill interpolation values
             pshufd xmm6, xmm0, $6F;
             pshufd xmm7, xmm0, $12;


             // fill image information
             mov edx, imgx;
             mov eax, pSrc;

             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             mov eax, pSrc2;
             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm6, xmm5;
             mov ecx, pRes;
             movlpd [ecx], xmm6;

             jmp @NextXValue;

        @NoBilinFactAvail:
             // pSrc := pInit[Max(0, Min(h, imgY))];
             xor edi, edi;
        //     mov ebx, imgy;
             mov ebx, ecx;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, h;
             cmp ebx, edi;
             cmovae ebx, edi;

             mov eax, pInit;
             mov edx, [eax + ebx*4];

             // pRes^ := pSrc^[Max(0, Min(w, imgX))];
             xor edi, edi;
             mov ebx, imgx;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, w;
             cmp ebx, edi;
             cmovae ebx, edi;

             movlpd xmm0, [edx + 8*ebx];
             mov edx, pRes;
             movlpd [edx], xmm0;

             jmp @NextXValue;

        // ################################################
        // ##### no triangle in range -> assign zero
        @assignZero:
             mov edx, pRes;
             movhpd [edx], xmm2;  // note the upper half of this register is already cleared!

        @NextXValue:

        // inc(actMap);
        // inc(pRes);
        // inc(actTri);
        add pRes, 8;    // sizeof(double)
        add actTri, 2;  // sizeof(smallint)
        add actMap, $10; // 16 = sizeof(TTriangleMapRec);

     // end for y loop
     dec iter;
     jnz @@forylabel;
     @exit:
     pop edi;
     pop esi;
     pop ebx;
end;


procedure TLinearTriangulationMapping.MapImageSSE3(
  const pInit: array of PConstDoubleArr; w, h: integer; pRes: PDouble;
  actMap: PTriangleMapRec; actTri: PSmallInt; pTriangulation: PTriangleDef;
  pPts: PPointf2Ds; pSSEPt: PSmallSingleArr; Width, Height: integer);
var iter : integer;
    imgX, imgY : integer;
    pSrc01, pSrc02 : PConstDoubleArr;
    pSrc11, pSrc12 : PConstDoubleArr;
    pSrc21, pSrc22 : PConstDoubleArr;
    initColorIncr : integer;
    resIncr : integer;
    colorPlaneIncr : integer;
asm
   // #################################################
   // #### Prologue: Init some variables
   push ebx;
   push esi;
   push edi;

   xorpd xmm3, xmm3;
   movq xmm2, cOnes;

   mov edx, h;
   mov initColorIncr, edx;

   mov ebx, pInit;
   mov edi, [ebx];
   mov ebx, [ebx + 4*edx];
   sub ebx, edi;
   mov colorPlaneIncr, ebx;

   dec w;
   dec h;

   mov edx, Height;
   imul edx, Width;
   jz @exit;

   mov iter, edx;
   shl edx, 3;  // (Height)*8 -> size of a color plane
   mov resIncr, edx;

   // #################################################
   // #### Go through all pixels
   // for y := 0 to iter - 1 do
   @@forylabel:
        //if actTri^ < 0
        mov eax, actTri;
        movsx eax, smallint ptr [eax];
        cmp eax, 0;
        jl @assignZero;

        // standard triangle procedure
        // calculate source coordinate
//           fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
//           fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
//           fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
//           fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
//           fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
//           fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;

        // init pointers
        mov esi, pPts;
        mov edi, pTriangulation;
        mov ebx, pSSEPt;

        mov eax, actTri;
        movsx eax, smallint ptr [eax];
        lea eax, [eax + 2*eax];         // 3*eax

        mov edx, [edi + 4*eax];         // 4*(3*eax) = index of 3*sizeof(integer)
        mov ecx, [esi + 8*edx];
        mov [ebx], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 16], ecx;

        mov edx, [edi + 4*eax + 4];
        mov ecx, [esi + 8*edx];
        mov [ebx + 4], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 20], ecx;

        mov edx, [edi + 4*eax + 8];
        mov ecx, [esi + 8*edx];
        mov [ebx + 8], ecx;
        mov ecx, [esi + 8*edx + 4];
        mov [ebx + 24], ecx;

        //srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].x +
//                     actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].x +
//                     actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].x;
//             srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].y +
//                     actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].y +
//                     actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].y;

        // note this works since the points are int the same range...
        mov ecx, actMap;
        movaps xmm1, [ecx];

        //mov eax, pSSEPt;   // already loaded in ebx
        movaps xmm0, [ebx];
        movaps xmm5, [ebx + 16];

        // multiply 4 values at once - x and y
        mulps xmm0, xmm1;
        mulps xmm5, xmm1;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;

        // store back result
        //movss srcX, xmm0;
        // truncate
        cvttss2si ecx, xmm0;
        mov imgx, ecx;
        //facts[0] := Frac(srcX);   = float(srcX) - Int(src)
        cvtsi2ss xmm1, ecx;
        subss xmm0, xmm1;

        // store back result
        //movss srcY, xmm5;
        // truncate
        cvttss2si ecx, xmm5;
        mov imgy, ecx;
        //facts[1] := Frac(srcY);
        cvtsi2ss xmm4, ecx;
        subss xmm5, xmm4;

        // pSrc01 := pInit[imgy];
        // pSrc02 := pInit[imgy + 1];
        // pSrc11 := pInit[imgy + h1];
        // pSrc12 := pInit[imgy + h1 + 1];
        // pSrc21 := pInit[imgy + 2*h1];
        // pSrc22 := pInit[imgy + 2*h1 + 1];
        mov eax, pInit;

        mov edx, [eax + ecx*4];
        mov pSrc01, edx;
        mov edx, [eax + ecx*4 + 4];
        mov pSrc02, edx;

        add ecx, initColorIncr;
        mov edx, [eax + ecx*4];
        mov pSrc11, edx;
        mov edx, [eax + ecx*4 + 4];
        mov pSrc12, edx;

        add ecx, initColorIncr;
        mov edx, [eax + ecx*4];
        mov pSrc21, edx;
        mov edx, [eax + ecx*4 + 4];
        mov pSrc22, edx;

        // if (imgX < w) and (imgY < h) and (imgX >= 0) and (imgY >= 0) then
        cmp imgy, 0;
        jl @NoBilinFactAvail;
        cmp imgx, 0;
        jl @NoBilinFactAvail;
        mov edx, w;
        cmp imgx, edx;
        jge @NoBilinFactAvail;
        mov edx, h;
        cmp imgy, edx;
        jge @NoBilinFactAvail;

             // ############################################################
             // #### bilinear interpolation:

             // move both values to xmm0
             movlhps xmm0, xmm5;
             shufps xmm0, xmm0, 8;

//                pRes^ := (1 - factx)*(1 - facty)*pSrc^[imgx] +
//                         factx*(1 - facty)*pSrc^[imgx + 1] +
//                         facty*(1 - factx)*pSrc2^[imgx] +
//                         factx*facty*pSrc2^[imgx + 1];

             // calculate 1 - factors
             movaps xmm1, xmm2;
             subps xmm1, xmm0;
             movlhps xmm0, xmm1;

             // fill interpolation values
             pshufd xmm6, xmm0, $6F;
             pshufd xmm7, xmm0, $12;

             // fill image information (3 times)
             mov edx, imgx;
             mov ecx, pRes;

             // R
             mov eax, pSrc01;

             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             mov eax, pSrc02;
             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             movlpd [ecx], xmm1;

             // G
             mov eax, pSrc11;

             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             mov eax, pSrc12;
             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             add ecx, resIncr;
             movlpd [ecx], xmm1;

             // B
             mov eax, pSrc21;

             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             mov eax, pSrc22;
             movupd xmm0, [eax + edx*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             add ecx, resIncr;
             movlpd [ecx], xmm1;

             jmp @NextXValue;

        @NoBilinFactAvail:
             // pSrc := pInit[Max(0, Min(h, imgY))];
             xor edi, edi;
             mov ebx, imgy;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, h;
             cmp ebx, edi;
             cmovae ebx, edi;

             mov eax, pInit;
             mov edx, [eax + ebx*4];

             // pRes^ := pSrc^[Max(0, Min(w, imgX))];
             xor edi, edi;
             mov ebx, imgx;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, w;
             cmp ebx, edi;
             cmovae ebx, edi;

             // init 3 values (RGB:
             movlpd xmm0, [edx + 8*ebx];
             mov ecx, pRes;
             movlpd [ecx], xmm0;

             add edx, colorPlaneIncr;
             add ecx, resIncr;
             movlpd xmm0, [edx + 8*ebx];
             movlpd [ecx], xmm0;

             add edx, colorPlaneIncr;
             add ecx, resIncr;
             movlpd xmm0, [edx + 8*ebx];
             movlpd [ecx], xmm0;

             jmp @NextXValue;

        // ################################################
        // ##### no triangle in range -> assign zero
        @assignZero:
             // assign 3 zeros (in each color plane)
             mov edx, pRes;
             movhpd [edx], xmm2;  // note the upper half of this register is already cleared!
             add edx, resIncr;
             movhpd [edx], xmm2;
             add edx, resIncr;
             movhpd [edx], xmm2;

        @NextXValue:

        // inc(actMap);
        // inc(pRes);
        // inc(actTri);
        add pRes, 8;     // sizeof(double)
        add actTri, 2;   // sizeof(smallint)
        add actMap, $10; // 16 = sizeof(TTriangleMapRec);

     // end for y loop
     dec iter;
     jnz @@forylabel;

     @exit:
     pop edi;
     pop esi;
     pop ebx;
end;

{$ELSE}

procedure TLinearTriangulationMapping.MapImageSSE(
  const pInit: array of PConstDoubleArr; w, h : integer; pRes: PDouble; actMap : PTriangleMapRec;
  actTri : PSmallInt; pTriangulation : PTriangleDef; pPts : PPointf2Ds; pSSEPt : PSmallSingleArr;
  Width, Height : integer);
var iter : integer;
	  imgX, imgY : integer;
    wloc, hloc : integer;
asm
	  // rcx :?? rdx = pInit, h = r9
   // the rest is on the stack

   // r10: r10  -> y*x iteration

   // r8 -> imgx
   // r11 -> imgy

   // #################################################
   // #### Prologue: Init some variables
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r11;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;
   .savenv xmm5;

   mov r14, pInit;
   mov r15, actMap;

   mov edx, w;
   dec edx;
   mov wloc, edx;
   mov edx, h;
   dec edx;
   mov hloc, edx;

   mov r9, pRes;
   mov r10, actTri;

   xorpd xmm3, xmm3;
   movq xmm2, cOnes;

   // #################################################
   // #### Go through all pixels
   mov eax, Height;
   imul eax, Width;
   jz @exit;

   mov iter, eax;
   // for iter := 0 to Width*Height - 1 do
   @@forylabel:

        //if actTri^ < 0
        movsx eax, smallint ptr [r10];
        cmp eax, 0;
        jl @assignZero;

        // standard triangle procedure
        // calculate source coordinate
        //  fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
//          fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
//          fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
//          fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
//          fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
//          fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;
        // init pointers
        mov rsi, pPts;
        mov rdi, pTriangulation;
        mov rbx, pSSEPt;

        movsx rax, smallint ptr [r10];
        lea rax, [rax + 2*rax];         // 3*eax

        mov edx, [rdi + 4*rax];         // 4*(3*eax) = index of 3*sizeof(integer)
        mov ecx, [rsi + 8*rdx];
        mov [rbx], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 16], ecx;

        mov edx, [rdi + 4*rax + 4];
        mov ecx, [rsi + 8*rdx];
        mov [rbx + 4], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 20], ecx;

        mov edx, [rdi + 4*rax + 8];
        mov ecx, [rsi + 8*rdx];
        mov [rbx + 8], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 24], ecx;

        //srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].x +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].x +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].x;
//                  srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].y +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].y +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].y;

        // note this works since the points are int the same range...
        movaps xmm1, [r15];

        //mov eax, pSSEPt;   // already loaded in ebx
        movaps xmm0, [rbx];
        movaps xmm5, [rbx + 16];

        // multiply 4 values at once - x and y
        mulps xmm0, xmm1;
        mulps xmm5, xmm1;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;

        // store back result
        //movss srcX, xmm0;
        // truncate
        cvttss2si r8d, xmm0;
        //facts[0] := Frac(srcX);   = float(srcX) - Int(src)
        cvtsi2ss xmm1, r8d;
        subss xmm0, xmm1;

        // store back result
        //movss srcY, xmm5;
        // truncate
        cvttss2si r11d, xmm5;
        //facts[1] := Frac(srcY);
        cvtsi2ss xmm4, r11d;
        subss xmm5, xmm4;

        // pSrc := pInit[r11];
        // pSrc2 := pInit[r11 + 1];
        mov r12, [r14 + r11*8];
        mov r13, [r14 + r11*8 + 8];

        // if (r8 < w) and (r11 < h) and (r8 >= 0) and (r11 >= 0) then
        cmp r11, 0;
        jl @NoBilinFactAvail;
        cmp r8, 0;
        jl @NoBilinFactAvail;
        cmp r8d, wloc;
        jge @NoBilinFactAvail;
        cmp r11d, hloc;
        jge @NoBilinFactAvail;

             // ############################################################
             // #### bilinear interpolation:

             // move both values to xmm0
             movlhps xmm0, xmm5;
             shufps xmm0, xmm0, 8;

//                pRes^ := (1 - factx)*(1 - facty)*pSrc^[r8] +
//                         factx*(1 - facty)*pSrc^[r8 + 1] +
//                         facty*(1 - factx)*pSrc2^[r8] +
//                         factx*facty*pSrc2^[r8 + 1];

             // calculate 1 - factors
             movaps xmm1, xmm2;
             subps xmm1, xmm0;
             movlhps xmm0, xmm1;

             // fill interpolation values
             pshufd xmm6, xmm0, $6F;
             pshufd xmm7, xmm0, $12;

             // fill image information
             movupd xmm0, [r12 + r8*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             movupd xmm0, [r13 + r8*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm6, xmm5;
             movlpd [r9], xmm6;

             jmp @NextXValue;

        @NoBilinFactAvail:
             // pSrc := pInit[Max(0, Min(h, r11))];
             xor edi, edi;
             mov rbx, r11;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, hloc;
             cmp ebx, edi;
             cmovae ebx, edi;

             mov rdx, [r14 + rbx*8];

             // pRes^ := pSrc^[Max(0, Min(w, r8))];
             xor edi, edi;
             mov rbx, r8;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, wloc;
             cmp ebx, edi;
             cmovae ebx, edi;

             movlpd xmm0, [rdx + 8*rbx];
             movlpd [r9], xmm0;

             jmp @NextXValue;

        // ################################################
        // ##### no triangle in range -> assign zero
        @assignZero:

             movhpd [r9], xmm2;  // note the upper half of this register is already cleared!

        @NextXValue:

        // inc(pRes);
        // inc(actTri);
        // inc(actMap);
        add r9, 8;    // inc(pRes) sizeof(double)
        add r10, 2;  // sizeof(smallint)
        add r15, 16; // inc actmap, 16:  16 = sizeof(TTriangleMapRec);

     // end for y loop
     dec iter;
     jnz @@forylabel;

     @exit:
end;


procedure TLinearTriangulationMapping.MapImageSSE3(
  const pInit: array of PConstDoubleArr; w, h: integer; pRes: PDouble;
  actMap: PTriangleMapRec; actTri: PSmallInt; pTriangulation: PTriangleDef;
  pPts: PPointf2Ds; pSSEPt: PSmallSingleArr; Width, Height: integer);
var iter : integer;
    imgX : integer;
    pSrc01, pSrc02 : PDouble;
    pSrc11, pSrc12 : PDouble;
    pSrc21, pSrc22 : PDouble;
    initColorIncr : integer;
    colPlaneIncr : int64;
    resIncr : int64;
    wloc, hloc : integer;
asm
	  // rcx :(pherhaps self??) rdx = pInit, h = r9
   // the rest is on the stack

   // r10: r10  -> y*x iteration

   // r8 -> imgx
   // r11 -> imgy

   // #################################################
   // #### Prologue: Init some variables
   .pushnv rbx;
   .pushnv rsi;
   .pushnv rdi;
   .pushnv r11;
   .pushnv r12;
   .pushnv r13;
   .pushnv r14;
   .pushnv r15;
   .savenv xmm5;

   mov r14, pInit;
   mov r15, actMap;

   mov edx, h;
   mov initColorIncr, edx;

   mov rax, [r14];
   mov rdx, [r14 + rdx*8];
   sub rdx, rax;
   mov colPlaneIncr, rdx;

   mov edx, w;
   dec edx;
   mov wloc, edx;
   mov edx, h;
   dec edx;
   mov hloc, edx;

   mov r9, pRes;
   mov r10, actTri;

   xorpd xmm3, xmm3;
   movq xmm2, cOnes;

   // #################################################
   // #### Go through all pixels
   mov eax, Height;
   imul eax, Width;
   jz @exit;

   mov iter, eax;

   shl rax, 3;  // (Height)*8 -> size of a color plane
   mov resIncr, rax;

   // for iter := 0 to Width*Height - 1 do
   @@forylabel:

        //if actTri^ < 0
        movsx eax, smallint ptr [r10];
        cmp eax, 0;
        jl @assignZero;

        // standard triangle procedure
        // calculate source coordinate
        //  fSSEPts1^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].x;
//          fSSEPts1^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].x;
//          fSSEPts1^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].x;
//          fSSEPts2^[0] := fFromPts[fTriangulation.Triangles[actTri^][0]].y;
//          fSSEPts2^[1] := fFromPts[fTriangulation.Triangles[actTri^][1]].y;
//          fSSEPts2^[2] := fFromPts[fTriangulation.Triangles[actTri^][2]].y;
        // init pointers
        mov rsi, pPts;
        mov rdi, pTriangulation;
        mov rbx, pSSEPt;

        movsx rax, smallint ptr [r10];
        lea rax, [rax + 2*rax];         // 3*eax

        mov edx, [rdi + 4*rax];         // 4*(3*eax) = index of 3*sizeof(integer)
        mov ecx, [rsi + 8*rdx];
        mov [rbx], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 16], ecx;

        mov edx, [rdi + 4*rax + 4];
        mov ecx, [rsi + 8*rdx];
        mov [rbx + 4], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 20], ecx;

        mov edx, [rdi + 4*rax + 8];
        mov ecx, [rsi + 8*rdx];
        mov [rbx + 8], ecx;
        mov ecx, [rsi + 8*rdx + 4];
        mov [rbx + 24], ecx;

        //srcX := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].x +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].x +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].x;
//                  srcY := actMap^.alpha*fFromPts[fTriangulation.Triangles[actMap^.TriNum][0]].y +
//                          actMap^.beta*fFromPts[fTriangulation.Triangles[actMap^.TriNum][1]].y +
//                          actMap^.gamma*fFromPts[fTriangulation.Triangles[actMap^.TriNum][2]].y;

        // note this works since the points are int the same range...
        movaps xmm1, [r15];

        //mov eax, pSSEPt;   // already loaded in ebx
        movaps xmm0, [rbx];
        movaps xmm5, [rbx + 16];

        // multiply 4 values at once - x and y
        mulps xmm0, xmm1;
        mulps xmm5, xmm1;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;
        haddps xmm0, xmm3;
        haddps xmm5, xmm3;

        // store back result
        //movss srcX, xmm0;
        // truncate
        cvttss2si r8d, xmm0;
        //facts[0] := Frac(srcX);   = float(srcX) - Int(src)
        cvtsi2ss xmm1, r8d;
        subss xmm0, xmm1;

        // store back result
        //movss srcY, xmm5;
        // truncate
        cvttss2si r11d, xmm5;
        //facts[1] := Frac(srcY);
        cvtsi2ss xmm4, r11d;
        subss xmm5, xmm4;

        // if (r8 < w) and (r11 < h) and (r8 >= 0) and (r11 >= 0) then
        cmp r11, 0;
        jl @NoBilinFactAvail;
        cmp r8, 0;
        jl @NoBilinFactAvail;
        cmp r8d, wloc;
        jge @NoBilinFactAvail;
        cmp r11d, hloc;
        jge @NoBilinFactAvail;

             // ############################################################
             // #### bilinear interpolation:

             // move both values to xmm0
             movlhps xmm0, xmm5;
             shufps xmm0, xmm0, 8;

//                pRes^ := (1 - factx)*(1 - facty)*pSrc^[r8] +
//                         factx*(1 - facty)*pSrc^[r8 + 1] +
//                         facty*(1 - factx)*pSrc2^[r8] +
//                         factx*facty*pSrc2^[r8 + 1];

             // calculate 1 - factors
             movaps xmm1, xmm2;
             subps xmm1, xmm0;
             movlhps xmm0, xmm1;

             // fill interpolation values
             pshufd xmm6, xmm0, $6F;
             pshufd xmm7, xmm0, $12;

             // fill image information (3 times)
             mov rdi, resIncr;
             mov rbx, r11;

             // R
             mov rax, [r14 + rbx*8];
             //mov rax, pSrc01;
             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;

             mov rax, [r14 + rbx*8 + 8];
             //mov rax, pSrc02;
             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             movlpd [r9], xmm1;

             // G
             //mov rax, pSrc11;
             add ebx, initColorIncr;
             mov rax, [r14 + rbx*8];

             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             //mov rax, pSrc12;
             mov rax, [r14 + rbx*8 + 8];

             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             movlpd [r9 + rdi], xmm1;

             // B
             // mov rax, pSrc21;
             add ebx, initColorIncr;
             mov rax, [r14 + rbx*8];

             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movdqa xmm5, xmm1;
             //mov rax, pSrc22;
             mov rax, [r14 + rbx*8 + 8];
             movupd xmm0, [rax + r8*8];
             cvtpd2ps xmm1, xmm0;
             movlhps xmm5, xmm1;

             // multiply and add the values for the bilinear interpolation
             mulps xmm5, xmm6;
             mulps xmm5, xmm7;
             haddps xmm5, xmm3;
             haddps xmm5, xmm3;

             // convert back result (to double)
             cvtss2sd xmm1, xmm5;
             movlpd [r9 + 2*rdi], xmm1;

             jmp @NextXValue;

			     @NoBilinFactAvail:
            // pSrc := pInit[Max(0, Min(h, r11))];
             xor edi, edi;
             mov rbx, r11;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, hloc;
             cmp ebx, edi;
             cmovae ebx, edi;

             mov rdx, [r14 + rbx*8];

             // pRes^ := pSrc^[Max(0, Min(w, r8))];
             xor edi, edi;
             mov rbx, r8;
             cmp ebx, edi;
             cmovb ebx, edi;
             mov edi, wloc;
             cmp ebx, edi;
             cmovae ebx, edi;

             // init 3 values (RGB:
             mov rdi, resIncr;
             mov rax, colPlaneIncr;

             movlpd xmm0, [rdx + 8*rbx];
             movlpd [r9], xmm0;

             add rdx, rax;
             movlpd xmm0, [rdx + 8*rbx];
             movlpd [r9 + rdi], xmm0;

             add rdx, rax;
             movlpd xmm0, [rdx + 8*rbx];
             movlpd [r9 + 2*rdi], xmm0;

             jmp @NextXValue;

        // ################################################
        // ##### no triangle in range -> assign zero
        @assignZero:

             // assign 3 zeros (in each color plane)
             mov rdi, resIncr;
             movhpd [r9], xmm2;  // note the upper half of this register is already cleared!
             movhpd [r9 + rdi], xmm2;
             movhpd [r9 + 2*rdi], xmm2;
        @NextXValue:

        // inc(pRes);
        // inc(actTri);
        // inc(actMap);
        add r9, 8;    // inc(pRes) sizeof(double)
        add r10, 2;  // sizeof(smallint)
        add r15, 16; // inc actmap, 16:  16 = sizeof(TTriangleMapRec);

     // end for y loop
     dec iter;
     jnz @@forylabel;

     @exit:
end;

{$ENDIF}

function TLinearTriangulationMapping.MapPoints(
  const pts: TPtsMappingObj): TPtsMappingObj;
var ptsMtx : TDoubleMatrix;
    y : Integer;
    destPts : TDoubleMatrix;
    actPt : TPointf2D;
    triNum : integer;
    alpha, beta, gamma : single;
begin
     assert(pts.Dim = 2, 'Error only two dimensions are supported');

     fTriangleFacts := nil;
     fTriangleFactsInv := nil;
     fPtToTriangleMap := nil;
     fPrevTriangle := -1;

     fToPts := fPts;

     ptsMtx := pts.PtsAsMtx;
     try
        destPts := TDoubleMatrix.Create(2, ptsMtx.Height);

        for y := 0 to ptsMtx.Height - 1 do
        begin
             // first check if the point is in the mesh:
             actpt.x := ptsMtx[0, y];
             actPt.y := ptsMtx[1, y];

             if IsInTriangles(actPt, triNum, alpha, beta, gamma) then
             begin
                  destPts[0, y] := alpha*fFromPts[fTriangulation.Triangles[TriNum][0]].x +
                                   beta*fFromPts[fTriangulation.Triangles[TriNum][1]].x +
                                   gamma*fFromPts[fTriangulation.Triangles[TriNum][2]].x;
                  destPts[1, y] := alpha*fFromPts[fTriangulation.Triangles[TriNum][0]].y +
                                   beta*fFromPts[fTriangulation.Triangles[TriNum][1]].y +
                                   gamma*fFromPts[fTriangulation.Triangles[TriNum][2]].y;
             end
             else
             begin
                  destPts[0, y] := ptsMtx[0, y];
                  destPts[1, y] := ptsMtx[1, y];
             end;

        end;
     finally
            ptsMtx.Free;
     end;

     Result := TPtsMappingObj.Create(destPts, True);
end;

procedure TLinearTriangulationMapping.OnLoadBinaryProperty(
  const Name: String; const Value; size: integer);
begin
     if CompareText(cMapLinPts, Name) = 0 then
     begin
          assert(size mod sizeof(TPointf2D) = 0, 'Error unknown size');
          SetLength(fPts, size div sizeof(TPointf2D));
          Move(Value, fPts[0], size);
     end
     else if CompareText(cMapLinFromPts, Name) = 0 then
     begin
          assert(size mod sizeof(TPointf2Ds) = 0, 'Error unknown size');
          SetLength(fFromPts, size div sizeof(TPointf2Ds));
          Move(Value, fFromPts[0], size);
     end
     else if CompareText(cMapLinToPts, Name) = 0 then
     begin
          assert(size mod sizeof(TPointf2D) = 0, 'Error unknown size');
          SetLength(fToPts, size div sizeof(TPointf2D));
          Move(Value, fToPts[0], size);
     end
     else
         inherited;
end;

procedure TLinearTriangulationMapping.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, cMapLinWidth) = 0
     then
         fImgWidth := Value
     else if CompareText(Name, cMapLinHeight) = 0
     then
         fImgHeight := Value
     else
         inherited;
end;

function TLinearTriangulationMapping.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, cMapLinTriangulation) = 0
     then
         fTriangulation := obj as TBaseTriangulation
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

initialization
   RegisterMathIO(TLinearTriangulationMapping);

end.
