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

unit AAMVisuals;

// ###############################################################
// #### Some outputting functions for debug purposes
// ###############################################################

interface

uses SysUtils, Classes, Graphics, Matrix, AAMShapeBuilder, AAMAppearanceBuilder;

function AAMShapePoints(ShapeModel : TAAMShapeBuilder; width, height : integer) : TBitmap;
function AAMOrigPoints(ShapeModel: TAAMShapeBuilder; width, height : integer) : TBitmap;
function AAMTexture(AppearanceModel : TAAMAppearanceBuilder; p : TDoubleMatrix) : TBitmap;

implementation

uses Math, imageMatrixConv;

procedure PaintShape(bmp : TBitmap; const offsetx, offsety, scale : double; mtx : TDoubleMatrix);
var counter : integer;
    x, y : integer;
begin
     for counter := 0 to mtx.Height - 1 do
     begin
          x := Round(offsetx + scale*mtx[0, counter]);
          y := Round(offsety + scale*mtx[1, counter]);

          bmp.Canvas.FillRect(Rect(x - 2, y - 2, x + 2, y + 2));
     end;
end;

function AAMOrigPoints(ShapeModel: TAAMShapeBuilder; width, height : integer) : TBitmap;
var scale, offsetx, offsety : double;
    shpCnt : integer;
const col : Array[0..9] of TColor = (clBlack, clBlue, clGreen, clYellow, clMaroon, clRed, clCream, clLtGray, clDkGray, clOlive);
begin
     Result := TBitmap.Create;
     try
        Result.SetSize(width, height);
        Result.Canvas.Brush.Color := clWhite;
        Result.Canvas.FillRect(Rect(0, 0, width, height));

        offsetx := 0;
        offsety := 0;
        scale := 1;

        Result.Canvas.Brush.Color := clBlack;
        for shpCnt := 0 to ShapeModel.OrigShapesCount - 1 do
        begin
             Result.Canvas.Brush.Color := col[shpCnt mod Length(col)];
             PaintShape(Result, offsetx, offsety, scale, ShapeModel.OrigShapesMtx[shpCnt]);
        end;
     except
           Result.Free;

           raise;
     end;
end;

function AAMShapePoints(ShapeModel : TAAMShapeBuilder; width, height : integer) : TBitmap;
var meanShape : TDoubleMatrix;
    scale, offsetx, offsety : double;
    shpCnt : integer;
begin
     Result := TBitmap.Create;
     try
        Result.SetSize(width, height);
        Result.Canvas.Brush.Color := clWhite;
        Result.Canvas.FillRect(Rect(0, 0, width, height));

        offsetx := width/2;
        offsety := height/2;
        scale := min(offsetx, offsetx);

        Result.Canvas.Brush.Color := clBlack;
        for shpCnt := 0 to Length(ShapeModel.AlignedShapes) - 1 do
            PaintShape(Result, offsetx, offsety, scale, ShapeModel.AlignedShapes[shpCnt]);

        Result.Canvas.Brush.Color := clRed;
        meanShape := ShapeModel.MeanShape;
        try
           PaintShape(Result, offsetx, offsety, scale, MeanShape);
        finally
               meanShape.Free;
        end;
     except
           Result.Free;

           raise;
     end;
end;

function AAMTexture(AppearanceModel : TAAMAppearanceBuilder; p : TDoubleMatrix) : TBitmap;
var mtx : TDoubleMatrix;
begin
     mtx := AppearanceModel.TextureFromP(p);
     try
        Result := TMatrixImageConverter.ConvertImage(mtx, ctGrayScale);
     finally
            mtx.Free;
     end;
end;

end.
