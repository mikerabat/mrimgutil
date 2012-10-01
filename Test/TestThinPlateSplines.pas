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

unit TestThinPlateSplines;

interface

uses Windows, TestFramework, SysUtils, Classes, BaseTransformationTest, Types,
     BaseMathPersistence;

type
  TThinPlateTest = class(TBaseTransformationCase)
  private
    function CheckMtx(const m1 : Array of double; m2 : TDoubleDynArray) : boolean;
  published
    procedure TestSplines;
    procedure TestSplinesImg;
    procedure TestSplinesImgBMP;
  end;

implementation

uses Graphics, Matrix, ThinPlatesplines, Registration, ImageMatrixConv,
     jpeg, PtsDefinitions, Math, Dialogs, BinaryReaderWriter;

type
  TTPSHack = class(TTPSRegistration);
{ TThinPlateTest }

function TThinPlateTest.CheckMtx(const m1: array of double;
  m2: TDoubleDynArray): boolean;
var
  i: Integer;
begin
     Result := Length(m1) = Length(m2);

     if Result then
     begin
          for i := 0 to Length(m1) - 1 do
              Result := Result and (CompareValue(m1[i], m2[i], 1e-4) = 0);
     end;
end;

procedure TThinPlateTest.TestSplines;
const pts1 : Array[0..3] of TPointf2D = ( (x: 0; y : 1), (x : -1; y : 0), (x : 0; y : -1), (x : 1; y : 0));
      pts2 : Array[0..3] of TPointf2D = ( (x : 0; y : 0.75), (x : -1; y : 0.25), (x : 0; y : -1.25), (x : 1; y : 0.25));

      splineMtx : Array[0..13] of double = (0, -0.0902, 0, 0.0902, 0, -0.0902, 0, 0.0902, 0, 0, 1, 0, 0, 1); 
var fromPt, toPt : TPtsMappingObj;
    mapping : TTPSRegistration;
    hackObj : TTPSHack;
begin
     // example taken from the paper - Bookstein: Principal Warps: Thin-plate splines and the decomposition
     // of deformation.
     fromPt := TPtsMappingObj.Create(pts1);
     toPt := TPtsMappingObj.Create(pts2);
     try
        mapping := TTPSRegistration.Create;
        try
           mapping.CreatePointMapping(fromPt, toPt);

           // check resulting tps vectors
           hackObj := TTPSHack(mapping);
           CheckTrue((hackObj.PWidth = 2) and (hackObj.PHeight = 4), 'Points dimension error');
           CheckTrue((hackObj.SplineWidth = 2) and (hackObj.SplineHeight = 7), 'Spline dimension error');
           CheckTrue(CheckMtx(splineMtx, hackObj.Splines), 'Spline matrix is wrong');
        finally
               mapping.Free;
        end;
     finally
            fromPt.Free;
            toPt.Free;
     end;
end;

procedure TThinPlateTest.TestSplinesImg;
var pts1, pts2 : TDoubleMatrix;
    mapping : TTPSRegistration;
    fromPts, toPts : TPtsMappingObj;
    fromImg, DestImg : TDoubleMatrix;
    bmp : TBitmap;
    start, stop : Int64;
    freq : Int64;
begin
     pts1 := ReadObjFromFile('.\WarpTests\affe.txt') as TDoubleMatrix;
     pts2 := ReadObjFromFile('.\WarpTests\irdrew.1.txt') as TDoubleMatrix;
     fromPts := TPtsMappingObj.Create(pts1, True);
     toPts := TPtsMappingObj.Create(pts2, True);
     mapping := TTPSRegistration.Create;
     fromImg := nil;
     destImg := nil;
     try
        QueryPerformanceFrequency(freq);
        QueryPerformanceCounter(start);
        mapping.CreatePointMapping(fromPts, toPts);

        fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\irdrew.1.jpg', ctGrayScaleNorm);
        destImg := mapping.MapImage(fromImg);

        bmp := TMatrixImageConverter.ConvertImage(destImg, ctGrayScaleNorm);
        QueryPerformanceCounter(stop);

        Status(Format('TPS Matrix Warp: %.5f', [(stop - start)/freq]));
        try
           bmp.SaveToFile('tpswarp1.bmp');
        finally
               bmp.free;
        end;

        FreeAndNil(fromImg);
        FreeAndNil(destImg);

        // and vice versa ;)
        mapping.CreatePointMapping(toPts, fromPts);
        fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\affe.jpg', ctGrayScaleNorm);
        destImg := mapping.MapImage(fromImg);

        bmp := TMatrixImageConverter.ConvertImage(destImg, ctGrayScaleNorm);
        try
           bmp.SaveToFile('tpswarp2.bmp');
        finally
               bmp.free;
        end;
     finally
            mapping.Free;
            fromPts.Free;
            toPts.Free;
            fromImg.Free;
            destImg.Free;
     end;
end;

procedure TThinPlateTest.TestSplinesImgBMP;
var pts1, pts2 : TDoubleMatrix;
    mapping : TTPSRegistration;
    fromPts, toPts : TPtsMappingObj;
    bmp : TBitmap;
    destImg : TBitmap;
    jpg : TJPEGImage;
    start, stop : Int64;
    freq : Int64;
begin
     pts1 := ReadObjFromFile('.\WarpTests\affe.txt') as TDoubleMatrix;
     pts2 := ReadObjFromFile('.\WarpTests\irdrew.1.txt') as TDoubleMatrix;
     fromPts := TPtsMappingObj.Create(pts1, True);
     toPts := TPtsMappingObj.Create(pts2, True);
     mapping := TTPSRegistration.Create;
     bmp := TBitmap.Create;
     try
        jpg := TJPEGImage.Create;
        jpg.LoadFromFile('.\WarpTests\irdrew.1.jpg');
        bmp.Assign(jpg);
        jpg.Free;
        
        QueryPerformanceFrequency(freq);
        QueryPerformanceCounter(start);
        mapping.CreatePointMapping(fromPts, toPts);
        destImg := mapping.MapImage(bmp);
        QueryPerformanceCounter(stop);
        Status(Format('Splines BMP: %.5f', [(stop - start)/freq]));
        try
           destImg.SaveToFile('tpswarp3.bmp');
        finally
               destImg.Free;
        end;

        // and vice versa ;)
        mapping.CreatePointMapping(toPts, fromPts);

        jpg := TJPEGImage.Create;
        try
           jpg.LoadFromFile('.\WarpTests\affe.jpg');
           bmp.Assign(jpg);
        finally
               jpg.Free;
        end;

        destImg := mapping.MapImage(bmp);
        try
           destImg.SaveToFile('tpswarp4.bmp');
        finally
               destImg.Free;
        end;
     finally
            bmp.Free;
            mapping.Free;
            fromPts.Free;
            toPts.Free;
     end;
end;

initialization
   // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TThinPlateTest.Suite);

end.
