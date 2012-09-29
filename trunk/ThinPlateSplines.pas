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

unit ThinPlateSplines;

// #######################################################
// #### Thin plate splines - Non rigid transformation
// #######################################################

interface

uses SysUtils, Classes, Matrix, Registration, Types;

// based on - Bookstein: "Principal Warps: Thin-Plate Splines and the Decomposition of Deformations"
type
  TTPSRegistration = class(TBasePtsFromToMapping)
  private
    // note the matrices are stored with rows first so the first
    // column of a 4x4 matrix occupies the first 4 elements and so on
    fPWidth, fPHeight : integer;
    fP : TDoubleDynArray;
    fDimSplinesWidth, fDimSplinesHeight : integer;
    fDimSplines : TDoubleDynArray;
    fDim : integer;
    // according to the used dimension we need to use a different length function
    function TPSLenFunc(const len : double) : double; inline;
  protected
    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    procedure OnLoadDoubleArr(const Name : String; const Value : TDoubleDynArray); override;
    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
  protected
    property PWidth : integer read fPWidth;
    property PHeight : integer read fPHeight;
    property P : TDoubleDynArray read fP;
    property SplineWidth : integer read fDimSplinesWidth;
    property SplineHeight : integer read fDimSplinesHeight;
    property Splines : TDoubleDynArray read fDimSplines;
    property Dim : integer read fDim;
  public
    procedure CreatePointMapping(fromPts, toPts : TPtsMappingObj); override;
    function MapPoints(const pts : TPtsMappingObj) : TPtsMappingObj; override;
  end;

implementation

uses LinearAlgebraicEquations, PtsDefinitions, MatrixConst, BaseMathPersistence;

{ TTPSRegistration }

const cTPSPWidth = 'TPSPWidth';
      cTPSPHeight = 'TPSPHeight';
      cTPSP = 'TPSP';
      cTPSDimSplinesWidth = 'TPSDimSplinesWidth';
      cTPSDimSplinesHeight = 'TPSDimSplinesHeight';
      cTPSDim = 'TPSDim';
      cTPSDimSplines = 'TPSDimSplines';

class function TTPSRegistration.ClassIdentifier: String;
begin
     Result := 'ThinPlateSpline';
end;

procedure TTPSRegistration.CreatePointMapping(fromPts, toPts: TPtsMappingObj);
var P, K, L : TDoubleMatrix;
    pts1 : TDoubleMatrix;
    pts2 : TDoubleMatrix;
    tmp : TDoubleMatrix;
    x : Integer;
    y, i : Integer;
    eukDist : double;
begin
     pts1 := nil;
     pts2 := nil;
     P := nil;
     K := nil;
     L := nil;
     tmp := nil;
     try
        pts1 := fromPts.PtsAsMtx;
        pts2 := toPts.PtsAsMtx;

        assert((pts1.Width = pts2.Width) and (pts2.Height = pts1.Height), 'Dimension error - number of points and dimension must match');

        fDim := pts2.Width;

        // matrix P (1, pts1x, pts1y)
        P := TDoubleMatrix.Create(pts1.Width + 1, pts1.Height);
        P.AssignSubMatrix(pts1, 1, 0);
        for y := 0 to P.Height - 1 do
            P[0, y] := 1;

        // matrix K (nxn) as (
        //  0    U(r12)   U(r13) ...
        //  U(r21) 0      U(r23) ...
        //  U(r31)  U(r32)  0 ...          where r12 is eukledian distance between points 1 and 2  and U(r12) is r12^2*log(r12^2)
        // store extra space for the matrix P! Note: this is the 2D case - for 3D U(r12) is r12
        K := TDoubleMatrix.Create(pts1.Height + P.Width, pts1.Height + P.Width);
        for y := 0 to pts1.Height - 1 do
        begin
             for i := 0 to pts1.Height - 1 do
             begin
                  eukDist := 0;
                  for x := 0 to pts1.Width - 1 do
                      eukDist := eukDist + sqr(pts1[x, y] - pts1[x, i]);

                  // note: this is a "crude" check taking the floating point
                  // errors into account (double ca 1e-16) - so this works best with "well behaving" (normalized)
                  // coordinates thus the procedure should not be used with either very large nor
                  // very small coordinates
                  if eukDist > 1e-10
                  then
                      eukDist := TPSLenFunc(eukDist)
                  else
                      eukDist := 0;

                  K[y, i] := eukDist;
             end;
        end;

        // define matrix L as :
        //     K    |    P
        //     ------------
        //     P'   |    0
        // note: the matrix K can already store all the data:
        K.AssignSubMatrix(P, K.Width - P.Width, 0);
        P.TransposeInPlace;
        K.AssignSubMatrix(P, 0, K.Height - P.Height);
        FreeAndNil(P);

        if K.InvertInPlace <> leOk then
           raise Exception.Create('Error calculating the thin-plate-splines coefficient matrix. Matrix is not invertable');

        fPWidth := 0;
        fPHeight := 0;
        fP := nil;
        fP := pts1.SubMatrix;
        fPWidth := pts1.Width;
        fPHeight := pts1.Height;

        // define spline functions for all dimensions (note they are defined as double arrays to speed up access without
        // function calls
        fDimSplines := nil;
        fDimSplinesWidth := 0;
        fDimSplinesHeight := 0;

        tmp := TDoubleMatrix.Create(pts2.Width, pts2.Height + pts2.Width + 1);
        tmp.AssignSubMatrix(pts2);
        K.MultInPlace(tmp);
        fDimSplines := K.SubMatrix;
        fDimSplinesWidth := K.Width;
        fDimSplinesHeight := K.Height;

        FreeAndNil(pts1);
        FreeAndNil(pts2);
        FreeAndNil(tmp);
        FreeAndNil(K);
     except
           P.Free;
           pts1.Free;
           pts2.Free;
           K.Free;
           L.Free;
           tmp.Free;

           raise;
     end;
end;

procedure TTPSRegistration.DefineProps;
begin
     inherited;

     AddIntProperty(cTPSPWidth, fPWidth);
     AddIntProperty(cTPSPHeight, fPHeight);
     if Length(fP) > 0 then
        AddDoubleArr(cTPSP, fP);
     AddIntProperty(cTPSDimSplinesWidth, fDimSplinesWidth);
     AddIntProperty(cTPSDimSplinesHeight, fDimSplinesHeight);
     AddIntProperty(cTPSDim, fDim);
     if Length(fDimSplines) > 0 then
        AddDoubleArr(cTPSDimSplines, fDimSplines);
end;

function TTPSRegistration.MapPoints(const pts: TPtsMappingObj): TPtsMappingObj;
var mtx : TDoubleMatrix;
    counter : integer;
    t : TDoubleDynArray;
    i : integer;
    len : double;
    dim : integer;
    pidxX : integer;
    spIdx  : integer;
    ptsOut : TDoubleMatrix;
begin
     mtx := pts.PtsAsMtx;
     try
        assert(mtx.Width = fDim, 'Dimension error');
        assert(fDim > 0, 'Error 0 dimension');
        SetLength(t, fDim);

        ptsOut := TDoubleMatrix.Create(mtx.Width, mtx.Height);
        try
           for counter := 0 to mtx.Height - 1 do
           begin
                FillChar(t[0], sizeof(double)*Length(t), 0);

                // spline function
                pidxX := 0;
                spIdx := 0;
                for i := 0 to fPHeight - 1 do
                begin
                     len := 0;
                     for dim := 0 to fDim - 1 do
                     begin
                          len := len + sqr(fP[pidxX] - mtx[dim, counter]);
                          inc(pidxX);
                     end;

                     // see note in CreatePointMapping
                     if len > 1e-10 then
                     begin
                          for dim := 0 to fDim - 1 do
                          begin
                               t[dim] := t[dim] + TPSLenFunc(len)*fDimSplines[spIdx];
                               Inc(spIdx);
                          end;
                     end
                     else
                         inc(spIdx, fDim);
                end;

                // affine transformation
                for i := 0 to fDim - 1 do
                begin
                     spIdx := fPHeight*fDim + i;
                     ptsOut[i, counter] := t[i] + fDimSplines[spIdx];
                     inc(spIdx, fDim);

                     for dim := 0 to fDim - 1 do
                     begin
                          ptsOut[i, counter] := ptsOut[i, counter] + fDimSplines[spIdx]*mtx[dim, counter];
                          inc(spIdx, fDim);
                     end;
                end;
           end;

           // resulting object:
           Result := TPtsMappingObj.Create(ptsOut);
        finally
               ptsOut.Free;
        end;
     finally
            mtx.Free;
     end;
end;

procedure TTPSRegistration.OnLoadDoubleArr(const Name: String;
  const Value: TDoubleDynArray);
begin
     if CompareText(Name, cTPSP) = 0
     then
         fP := Value
     else if CompareText(Name, cTPSDimSplines) = 0
     then
         fDimSplines := Value
     else
         inherited;
end;

procedure TTPSRegistration.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, cTPSPWidth) = 0
     then
         fPWidth := Value
     else if CompareText(Name, cTPSPHeight) = 0
     then
         fPHeight := Value
     else if CompareText(Name, cTPSDimSplinesWidth) = 0
     then
         fDimSplinesWidth := Value
     else if CompareText(Name, cTPSDimSplinesHeight) = 0
     then
         fDimSplinesHeight := Value
     else if CompareText(Name, cTPSDim) = 0
     then
         fDim := Value
     else
         inherited;
end;

// note the len variable is already squared (r^2)
function TTPSRegistration.TPSLenFunc(const len: double): double;
begin
     case fdim of
       1: Result := len*len;  // r^3
       2: Result := len*ln(len);  // r^2*ln(r)   r = eukledian distance - note that the missing 0.5 factor does not influence the result
       3: Result := len;
     else
         raise EDimensionError.Create('Error thin plate splines only defined for 1,2,3 dimensions');
     end;
end;

initialization
   RegisterMathIO(TTPSRegistration);

end.
