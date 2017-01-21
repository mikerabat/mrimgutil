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

unit Procrustes;

// #####################################################
// #### Procrustes points registration
// #####################################################

interface

uses RigidTransformation, Registration, Matrix;

// base procrustes analysis -> creates an affine transformation
// with params scale, rotation and translation in respect to the lowest
// square root error
type
  TProcrustes = class(TBaseRigidPtsRegistration)
  protected
    class function ClassIdentifier : String; override;
  public
    procedure CreatePointMapping(fromPts, toPts : TPtsMappingObj); override;

    class function CreateMappingMatrix(fromPts, ToPts : TPtsMappingObj) : TDoubleMatrix; overload;
    class function CreateMappingMatrix(fromPts, ToPts : TDoubleMatrix) : TDoubleMatrix; overload; 
  end;

implementation

uses SysUtils, Math, MathUtilFunc, BaseMathPersistence;

{ TProcrustes }

class function TProcrustes.CreateMappingMatrix(fromPts,
  ToPts: TPtsMappingObj): TDoubleMatrix;
begin
     with TProcrustes.Create do
     try
        CreatePointMapping(fromPts, ToPts);

        Result := TDoubleMatrix.Create;
        Result.Assign(Mapping);
     finally
            Free;
     end;
end;

class function TProcrustes.ClassIdentifier: String;
begin
     Result := 'Procrustes';
end;

class function TProcrustes.CreateMappingMatrix(fromPts,
  ToPts: TDoubleMatrix): TDoubleMatrix;
var fromPtsObj, toPtsObj : TPtsMappingObj;
begin
     fromPtsObj := TPtsMappingObj.Create(fromPts, False);
     ToPtsObj := TPtsMappingObj.Create(ToPts, False);
     try
        Result := CreateMappingMatrix(fromPtsObj, toPtsObj);
     finally
            fromPtsObj.Free;
            ToPtsObj.Free;
     end;
end;

procedure TProcrustes.CreatePointMapping(fromPts, toPts: TPtsMappingObj);
var scalePts1 : TDoubleMatrix;
    scalePts2 : TDoubleMatrix;
    mean1 : TDoubleMatrix;
    mean2 : TDoubleMatrix;
    x, y : integer;
    dim : integer;
    H : TDoubleMatrix;
    U, S, V : TDoubleMatrix;
    detVU : double;
    R : TDoubleMatrix;
    t : TDoubleMatrix;
    temp : TDoubleMatrix;
    sx, sy : double;
    scaleFact : double;
    minVal : double;

//function ScaleCoordinates(mtx : TDoubleMatrix) : double;
//var x : integer;
//    minVal : double;
//    maxVal : double;
//    dim : integer;
//begin
//     Result := 0;
//     dim := mtx.Width - 1;
//     for x := 0 to dim - 1 do
//     begin
//          mtx.SetSubMatrix(x, 0, 1, mtx.Height);
//          minVal := mtx.Min;
//          maxVal := mtx.Max;
//
//          Result := Max(Result, maxVal - minVal);
//     end;
//
//     assert(Result > 0, 'Error, coordinates are all the same');
//
//     mtx.SetSubMatrix(0, 0, dim - 1, mtx.Height);
//     mtx.ScaleInPlace(1/Result);
//     mtx.UseFullMatrix;
//end;
begin
     scalePts1 := nil;
     scalePts2 := nil;
     mean1 := nil;
     mean2 := nil;
     H := nil;
     U := nil;
     S := nil;
     V := nil;
     t := nil;
     temp := nil;

     try
        // ########################################################
        // #### Normalize coordinates such that the longer side spans
        // a space from 0 to 1
        scalePts1 := fromPts.HmgPtsAsMtx;
        scalePts2 := toPts.HmgPtsAsMtx;

        assert(scalePts1.Width = scalePts2.Width, 'Dimension error');
        assert(scalePts1.Height = scalePts1.Height, 'Dimension error');

        dim := scalePts1.Width - 1;

       // sx := ScaleCoordinates(scalePts1);
       // sy := ScaleCoordinates(scalePts2);
       // scalePts2.ScaleInPlace(sx/sy);

        // ########################################################
        // #### Remove mean from both sets
        //scalePts1.SetSubMatrix(0, 0, scalePts1.Width - 1, scalePts1.Height);
        //scalePts2.SetSubMatrix(0, 0, scalePts2.Width - 1, scalePts1.Height);

        mean1 := scalePts1.Mean(False);
        mean2 := scalePts2.Mean(False);

        mean1[dim, 0] := 1;
        mean2[dim, 0] := 1;
        mean1.SetSubMatrix(0, 0, dim, 1);
        mean2.SetSubMatrix(0, 0, dim, 1);

        for y := 0 to scalePts1.Height - 1 do
        begin
             scalePts1.SetSubMatrix(0, y, dim, 1);
             scalePts1.SubInPlace(mean1);
             scalePts2.SetSubMatrix(0, y, dim, 1);
             scalePts2.SubInPlace(mean2);
        end;

        // ########################################################
        // #### Covariance matrix + svd
        scalePts1.UseFullMatrix;
        scalePts2.UseFullMatrix;

        scalePts1.TransposeInPlace;
        H := scalePts1.Mult(scalePts2);
        H.SVD(U, V, S, True);
        V.TransposeInPlace;
        FreeAndNil(H);

        // zero out all very small components:
        minVal := eps(Max(Abs(U.Max), Abs(U.min)));
        for y := 0 to U.Height - 1 do
            for x := 0 to U.Width - 1 do
                if Abs(U[x, y]) <= minVal then
                   U[x, y] := 0;

        // ########################################################
        // #### Calculate Rotation matrix
        H := V.Mult(U);
        detVU := H.Determinant;
        FreeAndNil(H);

        for y := 0 to V.Height - 1 do
            V[dim - 1, y] := V[dim - 1, y]*detVU;

        U.TransposeInPlace;
        R := V.Mult(U);
        FreeAndNil(U);
        FreeAndNil(V);
        FreeAndNil(S);

        // ########################################################
        // #### Calculate scale factor

        // transform means to homogen coordinates
        mean1.UseFullMatrix;
        mean2.UseFullMatrix;

        // translation
        mean1.TransposeInPlace;
        mean2.TransposeInPlace;
        temp := R.Mult(mean1);
        t := mean2.Sub(temp);
        FreeAndNil(temp);

        // set translation
        for y := 0 to dim - 1 do
            R[dim, y] := t[0, y];
        FreeAndNil(t);

        //FreeAndNil(scalePts1);
        //scalePts1 := fromPts.HmgPtsAsMtx;
        //scalePts1.TransposeInPlace;

        // rotate and translate (pts already transposed!)
        temp := R.Mult(scalePts1);
        temp.TransposeInPlace;
        scalePts1.TransposeInPlace;

        // note: the points are already translated!
        // this is more or less cosmetic to remove rounding errors
        for y := 0 to dim - 1 do
        begin
             R[dim, y] := 0;
             R[y, dim] := 0;
        end;


        sx := 0;
        sy := 0;
        for y := 0 to scalePts1.Height - 1 do
        begin
             sx := sx + temp[0, y]*scalePts2[0, y] + temp[1, y]*scalePts2[1, y];
             sy := sy + sqr(scalePts1[0, y]) + sqr(scalePts1[1, y]);
        end;

        scaleFact := sx/sy;
        FreeAndNil(temp);


        // scale rotation part
        R.SetSubMatrix(0, 0, dim, dim);
        R.ScaleInPlace(scaleFact);
        R.UseFullMatrix;

        // clear old translation
        for y := 0 to dim - 1 do
            R[dim, y] := 0;

        // new translation:
        temp := R.Mult(mean1);
        t := mean2.Sub(temp);
        FreeAndNil(temp);

        for y := 0 to dim - 1 do
            R[dim, y] := t[0, y];

        FreeAndNil(t);

        FreeAndNil(scalePts1);
        FreeAndNil(scalePts2);
        FreeAndNil(mean1);
        FreeAndNil(mean2);

        // #########################################
        // #### store result
        Mapping.Assign(R);
        FreeAndNil(R);
     except
           scalePts1.Free;
           scalePts2.Free;

           H.Free;
           U.Free;
           S.Free;
           V.Free;
           t.Free;
           temp.Free;
           mean1.Free;
           mean2.Free;

           raise;
     end;
end;

initialization
   RegisterMathIO(TProcrustes);

end.
