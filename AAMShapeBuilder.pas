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

unit AAMShapeBuilder;

// ###############################################
// #### Shape definitions for Active Appearance Models
// ###############################################

interface

uses SysUtils, PtsDefinitions, Matrix, PCA, AAMMatrixExt, AAMConst;

// ###############################################
// #### building of shape models according to
// the original paper of Cootes et. al.
// Statistical Models of Appearance for Computer Vision
type
  TCustomAAMShapeBuilderParams = class(TObject)
  public
    ModelEnergy : double;
    KeepAlignedShapes : boolean;
    KeepOriginalShapes : boolean;
    PCAProgress : TPCAProgress;

    procedure PCAOnShapes(const Shapes : TDoubleMatrix; var PCA : TMatrixPCA); virtual; abstract;

    constructor Create;
  end;
type
  TAAMShapeBuilder = class(TAAMProgressObj)
  private
    fParams : TCustomAAMShapeBuilderParams;
    fShapeModel : TMatrixPCA;
    fAlignedShapes : TDoubleMatrixDynArr;
    fNumShapeCoords : integer;
    fNumDimensions : integer;
    fOrigShapes : TDoubleMatrixDynArr;

    function CalcMeanShape(const Shapes : Array of TAAMMatrix) : TDoubleMatrix;
    procedure InternalBuildShapeModel(const Shapes : Array of TAAMMatrix);
    procedure NormalizeShape(Shape : TDoubleMatrix);
    procedure AlignShape(fromShape, ToShape : TDoubleMatrix; var destShape : TAAMMatrix);
    procedure InternalBuildShapePCAModel(const Shapes : TAAMMatrixDynArr);
    function GetOrigMtx(index: integer): TDoubleMatrix;
    function GetOrigShapesCount: integer;
    procedure SetAlignedShapes(const Value: TDoubleMatrixDynArr);
    function GetNunModes: integer;

    procedure pcaProgress(Sender : TObject; progress : integer);
  public
    // ########################################################
    // #### Shape constructing methods
    property OrigShapesMtx[index : integer] : TDoubleMatrix read GetOrigMtx;
    property OrigShapesCount : integer read GetOrigShapesCount;
    property AlignedShapes : TDoubleMatrixDynArr read FAlignedShapes write SetAlignedShapes;

    procedure BuildShapeModel(Params : TCustomAAMShapeBuilderParams; const Shapes : Array of TDynPointf2dArray); overload;
    procedure BuildShapeModel(Params : TCustomAAMShapeBuilderParams; const Shapes : Array of TDoubleMatrix); overload;

    // ########################################################
    // #### Shape handling methods
    property NumModes : integer read GetNunModes;
    function MeanShape : TDoubleMatrix;
    function PFromShape(const Shape : TDoubleMatrix) : TDoubleMatrix;

    // helper functions
    function Model : TMatrixPCA;
    function EigValSum : double;

    destructor Destroy; override;
  end;

implementation

uses Procrustes, Registration, BinaryReaderWriter;

{ TAAMShape }

procedure TAAMShapeBuilder.BuildShapeModel(Params : TCustomAAMShapeBuilderParams; const Shapes: array of TDynPointf2dArray);
var mtx : Array of TDoubleMatrix;
    i : integer;
begin
     assert(Length(Shapes) > 0, 'Error empty shape references given');

     // build matrix from shape points
     SetLength(mtx, Length(Shapes));
     try
        for i := 0 to Length(Shapes) - 1 do
            mtx[i] := PointsToMatrix(shapes[i]);

        BuildShapeModel(Params, mtx);
     finally
            for i := 0 to Length(mtx) - 1 do
                mtx[i].Free;
     end;
end;

procedure TAAMShapeBuilder.AlignShape(fromShape, ToShape: TDoubleMatrix;
  var destShape: TAAMMatrix);
var fromPts : TPtsMappingObj;
    toPts : TPtsMappingObj;
    mappedPoints : TPtsMappingObj;
begin
     // ##################################################
     // #### Calculate transformation, transform the
     // shape and store the result in destShape
     fromPts := nil;
     toPts := nil;
     mappedPoints := nil;

     with TProcrustes.Create do
     try
        fromPts := TPtsMappingObj.Create(fromShape);
        toPts  := TPtsMappingObj.Create(toShape);

        CreatePointMapping(fromPts, toPts);
        mappedPoints := MapPoints(fromPts);
        destShape.Assign(mappedPoints.PtsAsMtxRef, True);
     finally
            fromPts.Free;
            toPts.Free;
            mappedPoints.Free;
            
            Free;
     end;
end;

procedure TAAMShapeBuilder.BuildShapeModel(Params : TCustomAAMShapeBuilderParams; const Shapes: Array of TDoubleMatrix);
var mtx : Array of TAAMMatrix;
    i : integer;
begin
     assert(Length(shapes) > 0, 'Error empty model given');
     fParams := params;

     for i := 0 to Length(fOrigShapes) - 1 do
         fOrigShapes[i].Free;

     fOrigShapes := nil;

     // build matrix from shape points
     SetLength(mtx, Length(Shapes));
     try
        fNumDimensions := Shapes[0].Width;
        fNumShapeCoords := Shapes[0].Height;

        for i := 0 to Length(Shapes) - 1 do
        begin
             mtx[i] := TAAMMatrix.Create;
             mtx[i].Assign(Shapes[i]);
        end;

        if fParams.KeepOriginalShapes then
        begin
             SetLength(fOrigShapes, Length(shapes));

             for i := 0 to Length(shapes) - 1 do
             begin
                  fOrigShapes[i] := TDoubleMatrix.Create;
                  fOrigShapes[i].Assign(shapes[i]);
             end;
        end;

        // ############################################################
        // #### Build Model
        InternalBuildShapeModel(mtx);
     finally
            for i := 0 to Length(Shapes) - 1 do
                mtx[i].Free;
     end;
end;

destructor TAAMShapeBuilder.Destroy;
var i : integer;
begin
     fShapeModel.Free;

     for i := 0 to Length(fAlignedShapes) - 1 do
         fAlignedShapes[i].Free;

     for i := 0 to Length(fOrigShapes) - 1 do
         fOrigShapes[i].Free;

     inherited;
end;

function TAAMShapeBuilder.EigValSum: double;
var i : integer;
begin
     Result := 0;
     for i := 0 to fShapeModel.EigVals.Height - 1 do
         Result := Result + fShapeModel.EigVals[0, i];
end;

function TAAMShapeBuilder.GetNunModes: integer;
begin
     Result := fShapeModel.NumModes;
end;

function TAAMShapeBuilder.GetOrigMtx(index: integer): TDoubleMatrix;
begin
     assert((index >= 0) and (index < Length(fOrigShapes)), 'Error index out of bounds');

     Result := fOrigShapes[index]; 
end;

function TAAMShapeBuilder.GetOrigShapesCount: integer;
begin
     Result := Length(fOrigShapes);
end;

procedure TAAMShapeBuilder.InternalBuildShapeModel(
  const Shapes: array of TAAMMatrix);
var meanShape : TDoubleMatrix;
    mean : TDoubleMatrix;
    i : Integer;
    alignedShapes : TAAMMatrixDynArr;
    converged : boolean;
    newMeanShape : TDoubleMatrix;
    numIter : integer;
const cMaxNumAlignSteps = 5;
      cMaxAlignChange = 1e-6;
begin
     DoProgress(0);

     for i := 0 to Length(fAlignedShapes) - 1 do
         fAlignedShapes[i].Free;
     fAlignedShapes := nil;
      
     // #########################################################
     // #### Algorithm from Cootes et. al.: Statistical Models of Appearance
     // page 14
     if Length(Shapes) = 0 then
        exit;

     // remove the mean from all elements (center of gravity)
     for i := 0 to Length(Shapes) - 1 do
     begin
          mean := Shapes[i].Mean(False);
          try
             Shapes[i].SubVectorInPlace(mean, True);
          finally
                 mean.Free;
          end;
     end;

     // elect the first shape to be the mean shape
     // and normalize it
     meanShape := nil;
     alignedShapes := nil;
     newMeanShape := nil;
     try
        SetLength(alignedShapes, Length(shapes));
        for i := 0 to Length(alignedShapes) - 1 do
            alignedShapes[i] := TAAMMatrix.Create;

        meanShape := CalcMeanShape(Shapes);

        // normalize: Translate to center of gravity (mean) an divide by the eukledian norm
        NormalizeShape(meanShape);

        converged := False;
        numIter := 0;
        while not Converged do
        begin
             // step 4 align all shapes to the mean shape
             for i := 0 to Length(shapes) - 1 do
                 AlignShape(Shapes[i], meanShape, alignedShapes[i]);

             // step 5: new mean shape is the mean of all shapes:
             newMeanShape.Free;
             newMeanShape := CalcMeanShape(alignedShapes);

             // step 6: scale to |newMeanShape|=1
             NormalizeShape(newMeanShape);

             // calculate convergence
             inc(numIter);
             meanShape.SubInPlace(newMeanShape);

             converged := (numIter >= cMaxNumAlignSteps) or (meanShape.ElementwiseNorm2 <= cMaxAlignChange);

             meanShape.Free;
             meanShape := newMeanShape;
             newMeanShape := nil;
        end;

        DoProgress(10);

        // step 7: align all shapes a last time to the new mean shape ->
        for i := 0 to Length(shapes) - 1 do
            AlignShape(Shapes[i], meanShape, alignedShapes[i]);

        // ##############################################################
        // #### Now we have  a set of aligned shapes -> calculate the shape model
        InternalBuildShapePCAModel(AlignedShapes);

        // ##############################################################
        // #### free shape occupied memory
        if fParams.KeepAlignedShapes then
        begin
             SetLength(fAlignedShapes, Length(Shapes));

             for i := 0 to Length(shapes) - 1 do
             begin
                  fAlignedShapes[i] := alignedShapes[i];
                  alignedShapes[i] := nil;
             end;
        end;

        meanShape.Free;
        if not fParams.KeepAlignedShapes then
           for i := 0 to Length(alignedShapes) - 1 do
               alignedShapes[i].Free;


        DoProgress(100);
     except
           meanShape.Free;
           newMeanShape.Free;

           for i := 0 to Length(alignedShapes) - 1 do
               alignedShapes[i].Free;
     end;
end;

procedure TAAMShapeBuilder.InternalBuildShapePCAModel(
  const Shapes: TAAMMatrixDynArr);
var mtx : TDoubleMatrix;
    counter : integer;
    rowShape : TDoubleMatrix;
begin
     // #######################################################
     // #### create a single matrix from the set of shapes
     mtx := TDoubleMatrix.Create(Length(Shapes), shapes[0].Width*shapes[0].height);
     try
        for counter := 0 to Length(Shapes) - 1 do
        begin
             rowShape := Shapes[counter].Reshape(1, mtx.Height);
             try
                mtx.AssignSubMatrix(rowShape, counter, 0);
             finally
                    rowShape.Free;
             end;
        end;

        // ########################################################
        // #### Create model
        fShapeModel.Free;

        // note: an external procedure is used so different pca procedure can be
        // attached
        fParams.PCAProgress := pcaProgress;
        fParams.PCAOnShapes(mtx, fShapeModel);
     finally
            mtx.Free;
     end;
end;

function TAAMShapeBuilder.MeanShape: TDoubleMatrix;
begin
     Result := TDoubleMatrix.Create;
     Result.Assign(fShapeModel.Mean);
     Result.ReshapeInPlace(fNumDimensions, fNumShapeCoords);
end;

function TAAMShapeBuilder.Model: TMatrixPCA;
begin
     Result := fShapeModel;
end;

function TAAMShapeBuilder.CalcMeanShape(const Shapes: array of TAAMMatrix): TDoubleMatrix;
var counter: Integer;
begin
     assert(Length(Shapes) > 0, 'Error at least one shape must be assigned');

     Result := TDoubleMatrix.Create;
     Result.Assign(Shapes[0]);

     for counter := 1 to Length(shapes) - 1 do
         Result.AddInplace(Shapes[counter]);

     Result.ScaleInPlace(1/Length(shapes));
end;

procedure TAAMShapeBuilder.NormalizeShape(Shape: TDoubleMatrix);
var norm : double;
begin
     norm := shape.ElementwiseNorm2;
     shape.ScaleInPlace(1/norm);
end;

procedure TAAMShapeBuilder.pcaProgress(Sender : TObject; progress: integer);
begin
     DoProgress(10 + progress div 90);
end;

function TAAMShapeBuilder.PFromShape(const Shape: TDoubleMatrix): TDoubleMatrix;
var mtx : TDoubleMatrix;
begin
     mtx := TDoubleMatrix.Create;
     try
        mtx.Assign(Shape);
        mtx.ReshapeInPlace(1, fNumShapeCoords*fNumDimensions);

        Result := fShapeModel.ProjectToFeatureSpace(mtx);
     finally
            mtx.Free;
     end;
end;

procedure TAAMShapeBuilder.SetAlignedShapes(const Value: TDoubleMatrixDynArr);
begin
     FAlignedShapes := Value;
end;

{ TCustomAAMShapeBuilderParams }

constructor TCustomAAMShapeBuilderParams.Create;
begin
     KeepAlignedShapes := True;
     KeepOriginalShapes := True;

     ModelEnergy := cDefShapeModelEnergy;
end;

end.
