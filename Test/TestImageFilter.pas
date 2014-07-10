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

unit TestImageFilter;

interface

uses Windows, TestFramework, SysUtils, Classes, BaseTransformationTest, Types,
     BaseMathPersistence;

type
  TImageFilterTest = class(TBaseTransformationCase)
  published
    procedure TestMeanFilter;
  end;

implementation

uses Graphics, Matrix, ImageMatrixConv,
     jpeg, Math, BinaryReaderWriter, ImageFilter;


procedure TImageFilterTest.TestMeanFilter;
var fromImg : TDoubleMatrix;
    destImg : TDoubleMatrix;
    bmp : TBitmap;
    freq, start, stop : Int64;
begin
     fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\irdrew.1.jpg', ctGrayScaleNorm);

     QueryPerformanceFrequency(freq);
     QueryPerformanceCounter(start);
     
     with TMeanFilter.Create(15) do
     try
        destImg := Filter(fromImg, 1);
     finally
            Free;
     end;

     QueryPerformanceCounter(stop);
     Status(Format('mean Filt: %.5f', [(stop - start)/freq]));
     
     bmp := TMatrixImageConverter.ConvertImage(destImg, ctGrayScaleNorm);
     bmp.SaveToFile('MeanFilt_1.bmp');
     bmp.Free;
     destImg.Free;
     fromImg.Free;

     fromImg := TMatrixImageConverter.ConvertImage('.\WarpTests\irdrew.1.jpg', ctRGB);

     QueryPerformanceFrequency(freq);
     QueryPerformanceCounter(start);
     
     with TMeanFilter.Create(15) do
     try
        destImg := Filter(fromImg, 3);
     finally
            Free;
     end;

     QueryPerformanceCounter(stop);
     Status(Format('mean Filt rgb: %.5f', [(stop - start)/freq]));
     
     bmp := TMatrixImageConverter.ConvertImage(destImg, ctRGB);
     bmp.SaveToFile('MeanFilt_2.bmp');
     bmp.Free;
     destImg.Free;
     fromImg.Free;
end;


initialization
   // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TImageFilterTest.Suite);

end.
