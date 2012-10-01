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

unit TestDelaunyTriangulation;

interface

uses
  Windows, SysUtils, TestFramework, Registration, Procrustes, RigidTransformation, Matrix,
  BaseTransformationTest;

// ##################################################
// #### Test the procrustres method
type
 TTestDelaunyTriangulation = class(TTestCase)
 published
   procedure TestSimpleTriangulation;
   procedure TestOnPicture;
   procedure TestTriangulationImgWarp;
   procedure TestTriangulationMatrixWarp;
 end;


implementation

uses DelaunyTriangulation, PtsDefinitions, graphics, jpeg,
     DelaunyMapping, ImageMatrixConv, BaseMathPersistence, Math;

{ TestDelaunyTriangulation }

procedure TTestDelaunyTriangulation.TestOnPicture;
var pts : TDoubleMatrix;
    triPts : TDynPointf2DArray;
    bmp : TBitmap;
    jpegImg : TJPEGImage;
    i : integer;
    colIdx : integer;
const cColors : Array[0..6] of TColor = (clYellow, clBlack, clBlue, clRed, clLime, clDkGray, clOlive);
begin
     pts := ReadObjFromFile('.\WarpTests\irdrew.1.txt') as TDoubleMatrix;
     triPts := MatrixToPoints(pts);
     pts.Free;

     with TDelaunyTriangulation.Create do
     try
        CreateTriangulation(triPts);

        // paint the triangulation on a picture
        jpegImg := TJPEGImage.Create;
        try
           jpegImg.LoadFromFile('.\warpTests\irdrew.1.jpg');
           bmp := TBitmap.Create;
           bmp.Assign(jpegImg);
        finally
               jpegImg.Free;
        end;

        try
           colIdx := 0;
           for i := 0 to Length(Triangles) - 1 do
           begin
                bmp.Canvas.Pen.Color := cColors[colIdx];
                inc(colIdx);
                if colidx > High(cColors) then
                   colIdx := 0;

                bmp.Canvas.MoveTo(Round(triPts[Triangles[i][0]].x), Round(triPts[Triangles[i][0]].y));
                bmp.Canvas.LineTo(Round(triPts[Triangles[i][1]].x), Round(triPts[Triangles[i][1]].y));
                bmp.Canvas.LineTo(Round(triPts[Triangles[i][2]].x), Round(triPts[Triangles[i][2]].y));
                bmp.Canvas.LineTo(Round(triPts[Triangles[i][0]].x), Round(triPts[Triangles[i][0]].y));
           end;

           bmp.SaveToFile('irdrew_tris.bmp');
        finally
               bmp.Free;
        end;
     finally
            Free;
     end;
end;

procedure TTestDelaunyTriangulation.TestSimpleTriangulation;
var pts : TDynPointf2DArray;
const expTriangles : Array[0..1] of TTriangleDef = ( (3, 0, 1), (3, 1, 2) );
begin
     // #####################################################
     // #### Create a simple point cloud with 4 points (2 triangles)
     SetLength(pts, 4);
     pts[0].x := 0;
     pts[0].y := 0;
     pts[1].x := 1;
     pts[1].y := 0;
     pts[2].x := 1;
     pts[2].y := 1;
     pts[3].x := 0.5;
     pts[3].y := 0.5;

     with TDelaunyTriangulation.Create do
     try
        CreateTriangulation(pts);
        CheckEqualsMem(@expTriangles, @Triangles[0][0], sizeof(expTriangles), 'Error Simple Delauny failed');
     finally
            Free;
     end;
end;

procedure TTestDelaunyTriangulation.TestTriangulationMatrixWarp;
var pts1, pts2 : TDoubleMatrix;
    mapping : TDelaunyTriangulationMapping;
    fromPts, toPts : TPtsMappingObj;
    fromImg, DestImg, destImg2 : TDoubleMatrix;
    bmp, bmp2 : TBitmap;
    start, stop : Int64;
    freq : Int64;
    maxDiff : double;
begin
     pts1 := ReadObjFromFile('.\WarpTests\affe.txt') as TDoubleMatrix;
     pts2 := ReadObjFromFile('.\WarpTests\irdrew.1.txt') as TDoubleMatrix;

     fromPts := TPtsMappingObj.Create(pts1, True);
     toPts := TPtsMappingObj.Create(pts2, True);
     mapping := TDelaunyTriangulationMapping.Create;
     fromImg := nil;
     destImg := nil;
     try
        mapping.InitTriangulation(toPts);

        QueryPerformanceFrequency(freq);
        QueryPerformanceCounter(start);

        //fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\irdrew.1.jpg', ctGrayScaleNorm);
        fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\irdrew.1.jpg', ctRGBNorm);
        mapping.InitPtsForMapping(fromPts);
        mapping.InitMappingFuncs(False);
        destImg := mapping.MapImage(fromImg, 3);

        //destImg := mapping.MapImage(fromImg, 1);
        mapping.InitMappingFuncs(True);
        destImg2 := mapping.MapImage(fromImg, 3);

        bmp := TMatrixImageConverter.ConvertImage(destImg, ctRGBNorm);
        bmp2 := TMatrixImageConverter.ConvertImage(destImg2, ctRGBNorm);
        QueryPerformanceCounter(stop);

        Status(Format('Delauny Matrix Warp1: %.5f', [(stop - start)/freq]));
        try
           bmp.SaveToFile('delaunyWarp1.bmp');
           bmp2.SaveToFile('delaunyWarp11.bmp');
        finally
               bmp.free;
               bmp2.Free;
        end;

        destImg2.Free;
        FreeAndNil(fromImg);
        FreeAndNil(destImg);

        // and vice versa ;)
        QueryPerformanceFrequency(freq);
        QueryPerformanceCounter(start);

        fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\affe.jpg', ctRGB);
        mapping.InitPtsForMapping(toPts, fromPts);
        mapping.InitMappingFuncs(False);
        destImg := mapping.MapImage(fromImg, 3);
        mapping.InitMappingFuncs(True);
        destImg2 := mapping.MapImage(fromImg, 3);
        bmp := TMatrixImageConverter.ConvertImage(destImg, ctRGB);
        QueryPerformanceCounter(stop);

        // check the max error:
        fromImg.Free;
        fromImg := destImg.Sub(destImg2);
        maxDiff := Max(abs(fromImg.Min), abs(fromImg.Max));

        Status(Format('MaxDiff: %.17f', [maxDiff]));
        try
           bmp.SaveToFile('delaunywarp2.bmp');
        finally
               bmp.free;
        end;
        Status(Format('Delauny Matrix Warp2: %.5f', [(stop - start)/freq]));
     finally
            mapping.Free;
            fromPts.Free;
            toPts.Free;
            fromImg.Free;
            destImg.Free;
     end;
end;

procedure TTestDelaunyTriangulation.TestTriangulationImgWarp;
var pts : TDoubleMatrix;
    toPts : TPtsMappingObj;
    fromPts : TPtsMappingObj;
    bmp : TBitmap;
    jpegImg : TJPEGImage;
    destImg : TBitmap;
    freq : Int64;
    start, stop : int64;
begin
     pts := ReadObjFromFile('.\WarpTests\affe.txt') as TDoubleMatrix;
     toPts := TPtsMappingObj.Create(pts);
     pts.Free;
     pts := ReadObjFromFile('.\WarpTests\irdrew.1.txt') as TDoubleMatrix;
     fromPts := TPtsMappingObj.Create(pts);
     pts.Free;

     try
        // paint the triangulation on a picture
        jpegImg := TJPEGImage.Create;
        try
           jpegImg.LoadFromFile('.\warpTests\irdrew.1.jpg');
           bmp := TBitmap.Create;
           bmp.Assign(jpegImg);
           bmp.Canvas.Draw(0, 0, jpegImg);
        finally
               jpegImg.Free;
        end;

        destImg := nil;
        try
           with TDelaunyTriangulationMapping.Create do
           try
              InitTriangulation(fromPts, bmp.Width, bmp.Height);

              QueryPerformanceFrequency(freq);
              QueryPerformanceCounter(start);
              InitPtsForMapping(toPts);
              destImg := MapImage(bmp);
              QueryPerformanceCounter(stop);

              Status(Format('Delauny warp: %.5f', [(stop - start)/freq]));
              destImg.SaveToFile('delaunyWarp.bmp');
           finally
                  Free;
           end;
        finally
               bmp.Free;
               destImg.Free;
        end;
     finally
            toPts.Free;
            fromPts.Free;
     end;
end;

initialization
  RegisterTest(TTestDelaunyTriangulation.Suite);
  Randomize;

end.
