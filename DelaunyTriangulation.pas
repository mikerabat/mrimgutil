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

unit DelaunyTriangulation;

// ##################################################
// #### Implementation of a simple Delauny triangulation algorithm
// ##################################################

interface

uses SysUtils, Types, Triangulation, PtsDefinitions;

type
  EDelaunyError = class(Exception);
// ##################################################
// #### Implemenatation bases on pascal implementation of GEOMPACK:
// http://people.sc.fsu.edu/~burkardt/f_src/geompack/geompack.html
type
  TDelaunyTriangulation = class(TBaseTriangulation)
  private
    function CheckPermutation(const p: TIntegerDynArray): Boolean;
    function IntWrap(ival: Integer; ilo: Integer; ihi: Integer): Integer;
    function DiagonalEdge(const x0, y0, x1, y1, x2, y2, x3, y3: double): Integer;
    function SwapEdges(i: Integer; var top: Integer; var btri: Integer; var bedg: Integer;
                       const pts : TDynPointf2DArray; var Triangulation : TTriangles; numTriangles : integer;
                       var TriNeighbours : TTriNeighbours; var stack: TIntegerDynArray) : Integer;
    procedure Permute(var Pts : TDynPointf2DArray; var idx : TIntegerDynArray);
    procedure InvertPermute(var idx : TIntegerDynArray);
    function IndirectHeapPtsSort(const pts : TDynPointf2DArray) : TIntegerDynArray;
    function LineOrientation(const xu, yu, xv1, yv1, xv2, yv2, dv : double): Integer;
    procedure VisibleEdge(x : Double; y: Double; const pts : TDynPointf2DArray; const Triangulation : TTriangles; const TriNeighbours : TTriNeighbours;
                          var ltri: Integer; var ledg: Integer; var rtri: Integer; var redg: Integer);
    procedure InternalCreateTriangulation(var pts : TDynPointf2DArray; var Triangulation : TTriangles; var TriNeighbours : TTriNeighbours);
  public
    procedure CreateTriangulation(var pts : TDynPointf2DArray); override;
  end;

implementation

uses Contnrs, MathUtilFunc, Math;

var cEps : double = 0.0;

procedure InitEps;
begin
     cEps := 1.0;
     while (1.0 < 1.0 + cEps) do
       cEps := cEps / 2.0;

     cEps := 2.0*cEps;
end;

{ TDelaunyTriangulation }

// based on geompack dtris2 implementation
procedure TDelaunyTriangulation.CreateTriangulation(var pts: TDynPointf2DArray);
var neighbours : TTriNeighbours;
begin
     InternalCreateTriangulation(pts, fTriangulation, neighbours);
end;

procedure TDelaunyTriangulation.InternalCreateTriangulation(
  var pts: TDynPointf2DArray; var Triangulation: TTriangles;
  var TriNeighbours: TTriNeighbours);
var cmaxx : double;
    cmaxy : double;
    e : Integer;
    error : Integer;
    i : Integer;
    indx : TIntegerDynArray;
    j : Integer;
    l : Integer;
    ledg : Integer;
    lr : Integer;
    ltri : Integer;
    m : Integer;
    m1 : Integer;
    m2 : Integer;
    n : Integer;
    redg : Integer;
    rtri : Integer;
    stack : TIntegerDynArray;
    t : Integer;
    tol : Double;
    top : Integer;
    inTolerance : boolean;
    numTriangles : integer;
begin
     SetLength(stack, Length(pts));

     tol := 100.0*cEps;
     // #######################################################
     // #### Sort the vertices by increasing(x,y).
     indx := IndirectHeapPtsSort(pts);
     Permute(Pts, indx);

     // #######################################################
     // #### Make sure that the data points are "reasonably" distinct.
     m1 := 1;
     for i := 2 to Length(pts) do
     begin
          m := m1;
          m1 := i;

          cmaxx := Max(Abs(pts[m - 1].x), Abs(pts[m1 - 1].x));
          cmaxy := Max(Abs(pts[m - 1].y), Abs(pts[m1 - 1].y));
          inTolerance := (tol*(cmaxx + 1.0) < Abs(pts[m - 1].x - pts[m1 - 1].x)) or
                         (tol*(cmaxy + 1.0) < Abs(pts[m - 1].y - pts[m1 - 1].y));

          if not inTolerance then
             raise EDelaunyError.CreateFmt('Dealauny failed - Fatal error #%d! Fails for point number I=%d. M=%d, M1=%d, X,Y(M)=(%f,%f), X,Y(M1)=(%f,%f).',
                                           [224, i, m, m1, pts[m - 1].x, pts[m - 1].y, pts[m1 - 1].x, pts[m1 - 1].y]);
     end;

     // #########################################################
     // #### Note: the original implementation uses a starting index of 1 - at the end
     // we have to take that into account!!

     // #########################################################
     // #### Starting from points M1 and M2, search for a third point M that
     //  makes a "healthy" triangle(M1,M2,M)
     m1 := 1;
     m2 := 2;
     j := 3;
     {$IFNDEF CPUX64}
     // seems the 64bit compiler is much more clever than the 32 bit
     m := j;
     lr := -1;
     {$ENDIF}

     while True do
     begin
          if (Length(pts) < j) then
             raise EDelaunyError.CreateFmt('Delauny - Fatal error #%d!',[225]);

          m := j;

          lr := LineOrientation(pts[m - 1].x, pts[m - 1].y, pts[m1 - 1].x, pts[m1 - 1].y, pts[m2 - 1].x, pts[m2 - 1].y, 0.0);

          if (lr <> 0) then
            break;

          inc(j);
     end;

     // ############################################################
     // #### Set up the triangle information for(M1,M2,M), and for any other
     // triangles you created because points were collinear with M1, M2.
     numTriangles := j - 2;
     SetLength(Triangulation, Length(pts)*2);
     SetLength(TriNeighbours, Length(pts)*2);

     if lr = -1 then
     begin
          Triangulation[0][0] := m1;
          Triangulation[0][1] := m2;
          Triangulation[0][2] := m;

          TriNeighbours[0][2] := -3;

          for i := 2 to numTriangles do
          begin
               m1 := m2;
               m2 := i + 1;
               Triangulation[i - 1][0] := m1;
               Triangulation[i - 1][1] := m2;
               Triangulation[i - 1][2] := m;
               TriNeighbours[i - 1][0] := -3*i;
               TriNeighbours[i - 1][1] := i;
               TriNeighbours[i - 1][2] := i - 1;
          end;

          TriNeighbours[numTriangles - 1][0] := -3*numTriangles - 1;
          TriNeighbours[numTriangles - 1][1] := -5;
          ledg := 2;
          ltri := numTriangles;
     end
     else
     begin
          Triangulation[0][0] := m2;
          Triangulation[0][1] := m1;
          Triangulation[0][2] := m;

          TriNeighbours[0][0] := -4;

          for i := 2 to numTriangles do
          begin
               m1:= m2;
               m2:= i + 1;

               Triangulation[i - 1][0] := m2;
               Triangulation[i - 1][1] := m1;
               Triangulation[i - 1][2] := m;
               TriNeighbours[i - 2][2] := i;
               TriNeighbours[i - 1][0] := -3*i - 3;
               TriNeighbours[i - 1][1] := i - 1;
          end;

          TriNeighbours[numTriangles - 1][2] := -3*numTriangles;
          TriNeighbours[0][1] := -3*numTriangles - 2;
          ledg := 2;
          ltri := 1;
     end;

     // ########################################################
     // #### Insert the vertices one at a time from outside the convex hull,
     //  determine visible boundary edges, and apply diagonal edge swaps until
     //  Delaunay triangulation of vertices(so far) is obtained.
     top := 0;

     for i := j + 1 to Length(pts) do
     begin
          m := i;
          m1 := Triangulation[ltri - 1][ledg - 1];

          if ledg <= 2
          then
              m2 := Triangulation[ltri - 1][ledg]
          else
              m2 := Triangulation[ltri - 1][0];


          lr := LineOrientation(pts[m - 1].x, pts[m - 1].y, pts[m1 - 1].x, pts[m1 - 1].y, pts[m2 - 1].x, pts[m2 - 1].y, 0.0);

          if 0 < lr then
          begin
               rtri := ltri;
               redg := ledg;
               ltri:= 0;
          end
          else
          begin
               l := -TriNeighbours[ltri - 1][ledg - 1];
               rtri := l div 3;
               redg := (l mod 3) + 1;
          end;

          VisibleEdge(pts[m - 1].x, pts[m - 1].y, pts, Triangulation, TriNeighbours, ltri, ledg, rtri, redg);

          n := numTriangles + 1;
          l := -TriNeighbours[ltri - 1][ledg - 1];

          if Length(Triangulation) <= n then
          begin
               SetLength(Triangulation, Max(10, Min(2*Length(Triangulation), Length(Triangulation) + 1000)));
               SetLength(TriNeighbours, Max(10, Min(2*Length(TriNeighbours), Length(TriNeighbours) + 1000)));
          end;

          while (true) do
          begin
               t := l div 3;
               e := (l mod 3) + 1;
               l := -TriNeighbours[t - 1][e - 1];
               m2 := Triangulation[t - 1][e - 1];

               if (e <= 2)
               then
                   m1 := Triangulation[t - 1][e]
               else
                   m1 := Triangulation[t - 1][0];

               inc(numTriangles);

               if Length(Triangulation) <= numTriangles then
               begin
                    SetLength(Triangulation, Max(10, Min(2*Length(Triangulation), Length(Triangulation) + 1000)));
                    SetLength(TriNeighbours, Max(10, Min(2*Length(TriNeighbours), Length(TriNeighbours) + 1000)));
               end;

               TriNeighbours[t - 1][e - 1] := numTriangles;
               Triangulation[numTriangles - 1][0] := m1;
               Triangulation[numTriangles - 1][1] := m2;
               Triangulation[numTriangles - 1][2] := m;
               TriNeighbours[numTriangles - 1][0] := t;
               TriNeighbours[numTriangles - 1][1] := numTriangles - 1;
               TriNeighbours[numTriangles - 1][2] := numTriangles + 1;

               inc(top);

               if (Length(pts) < top) then
                  raise EDelaunyError.CreateFmt('CreateDelauny - Fatal error #%d! Stack overflow.',[8]);

               stack[top - 1] := numTriangles;

               if (t = rtri) and (e = redg) then
                  break;
          end;

          TriNeighbours[ltri - 1][ledg - 1] := -3*n - 1;
          TriNeighbours[n - 1][1] := -3*numTriangles - 2;
          TriNeighbours[numTriangles - 1][2] := -l;
          ltri := n;
          ledg := 2;

          error := SwapEdges(m, top, ltri, ledg, pts, Triangulation, numTriangles, TriNeighbours, stack);

          if (error <> 0) then
             raise EDelaunyError.CreateFmt('CreateDelauny - Fatal error #%d! Error return from SWAPEC',[error]);
     end;

     // #######################################################
     // #### Now account for the sorting that we did.
     for i := 0 to 2 do
         for j := 0 to numTriangles - 1 do
             Triangulation[j][i] := indx[Triangulation[j][i] - 1];

     InvertPermute(indx);
     Permute(pts, indx);

     // #######################################################
     // #### remove offset of 1 from the triangulation data structs
     for i := 0 to numTriangles - 1 do
     begin
          for j := 0 to 2 do
          begin
               dec(Triangulation[i][j]);
               dec(TriNeighbours[i][j]);
          end;
     end;

     SetLength(Triangulation, numTriangles);
     SetLength(TriNeighbours, numTriangles);
end;

function TDelaunyTriangulation.IndirectHeapPtsSort(const pts: TDynPointf2DArray): TIntegerDynArray;
var aval : TPointf2D;
    i : Integer;
    indx : TIntegerDynArray;
    indxt : Integer;
    ir : Integer;
    j : Integer;
    l : Integer;
    n : integer;
begin
     n := Length(pts);
     if (n < 1) then
        Result :=  nil;

     if (n = 1) then
     begin
          SetLength(indx, 1);
          indx[0] := 1;

          Result := indx;
          exit;
     end;

     SetLength(indx, Length(pts));
     for i := 0 to n - 1 do
         indx[i] := i + 1;

     l := (n div 2) + 1;
     ir := n;

     while True do
     begin
          if (1 < l) then
          begin
               dec(l);
               indxt := indx[l - 1];
               aval := pts[indxt - 1];
          end
          else
          begin
               indxt := indx[ir - 1];
               aval := pts[indxt - 1];
               indx[ir - 1] := indx[0];
               dec(ir);

               if (ir = 0) then
               begin
                    indx[0] := indxt;
                    break;
               end;
          end;

          i := l;
          j := l + l;

          while(j <= ir) do
          begin
               if (j < ir) then
               begin
                    if (pts[indx[j - 1] - 1].x < pts[indx[j] - 1].x) or
                       ((pts[indx[j - 1] - 1].x = pts[indx[j] - 1].x) and (pts[indx[j - 1] - 1].y < pts[indx[j] - 1].y))
                    then
                        inc(j);
               end;

               if ((aval.x < pts[indx[j - 1] - 1].x) or
                  ((aval.x = pts[indx[j - 1] - 1].x) and (aval.y < pts[indx[j - 1] - 1].y))) then
               begin
                    indx[i - 1] := indx[j - 1];
                    i := j;
                    inc(j, j);
               end
               else
                   j := ir + 1;
          end;

          indx[i - 1] := indxt;
      end;

      Result := indx;
end;

// Originally perm_inv from geompack: inverts a permutation "in place".
procedure TDelaunyTriangulation.InvertPermute(var idx : TIntegerDynArray);
var i: Integer;
    i0: Integer;
    i1: Integer;
    i2: Integer;
function Sign(const num : integer) : integer;
begin
     if num < 0
     then
         Result := -1
     else
         Result := 1;
end;
begin
     if Length(idx) = 0 then
        raise EDelaunyError.Create('InvertPermute - Fatal error! Input value of nil.');

     if not CheckPermutation(idx) then
        raise Exception.Create('InvertPermute - Fatal error! The input array does not represent a proper permutation.');

     for i := 1 to Length(idx) do
     begin
          i1 := idx[i - 1];

          while i < i1 do
          begin
               i2 := idx[i1 - 1];
               idx[i1 - 1]:= -i2;
               i1 := i2;
          end;

          //iss := -Sign(idx[i - 1]);
          //idx[i - 1] := Sign(iss)*Abs(idx[i - 1]);
          idx[i - 1] := -idx[i - 1];
     end;

     for i := 1 to Length(idx) do
     begin
          i1 := -idx[i - 1];

          if (0 <= i1) then
          begin
               i0 := i;

               while True do
               begin
                    i2 := idx[i1 - 1];
                    idx[i1 - 1] := i0;

                    if (i2 < 0) then
                       break;

                    i0 := i1;
                    i1 := i2;
               end;
          end;
     end;
end;

// originally r82vec_permute from geompack.
procedure TDelaunyTriangulation.Permute(var Pts: TDynPointf2DArray; var idx: TIntegerDynArray);
var tmp : TPointf2D;
    i : Integer;
    iget : Integer;
    iput : Integer;
    istart : Integer;
begin
     if not CheckPermutation(idx) then
        raise EDelaunyError.Create('Permute - Fatal error! The input array does not represent a proper permutation.');

     // ##############################################################
     // #### Search for the next element of the permutation that has not been used.
     for istart := 1 to Length(idx) do
     begin
          if (idx[istart - 1] < 0)
          then
              continue
          else if (idx[istart - 1] = istart) then
          begin
               idx[istart - 1] := -idx[istart - 1];
               continue;
          end
          else
          begin
               tmp := pts[istart - 1];
               iget := istart;

               // ##########################################################
               // #### Copy the new value into the vacated entry.
               while true do
               begin
                    iput := iget;
                    iget := idx[iget - 1];

                    idx[iput - 1] := -idx[iput - 1];

                    if ((iget < 1) or (Length(pts) < iget)) then
                       raise EDelaunyError.Create('PERMUTE - Fatal error!');

                    if (iget = istart) then
                    begin
                         pts[iput - 1] := tmp;
                         break;
                    end;

                    pts[iput - 1] := pts[iget - 1];
               end;
          end;
     end;

     // #####################################################
     // #### Restore the signs of the entries.
     for i := 0 to Length(pts) - 1 do
         idx[i] := -idx[i];
end;

// originally perm_check from geompack - checks that a vector represents a (full) permutation.
function TDelaunyTriangulation.CheckPermutation(const p: TIntegerDynArray): Boolean;
var found: Boolean;
    i : Integer;
    seek : Integer;
begin
     for seek := 1 to Length(p) do
     begin
          found := false;

          for i := 0 to Length(p) - 1 do
          begin
               if (p[i] = seek) then
               begin
                    found := true;
                    break;
               end;
          end;

          if (not found) then
          begin
               Result := false;
               exit;
          end;
     end;

     Result := true;
end;

// originally lrline from geompack - determines whether a point is to the left of, right of,
//    or on a directed line parallel to a line through given points.
function TDelaunyTriangulation.LineOrientation(const xu, yu, xv1, yv1, xv2, yv2, dv : double): Integer;
var dx : double;
    dxu : double;
    dy : double;
    dyu : double;
    t : double;
    tolabs : double;
    value : Integer;
const tol : double = 0.0000001;
begin
     dx := xv2 - xv1;
     dy := yv2 - yv1;
     dxu := xu - xv1;
     dyu := yu - yv1;

     tolabs := tol*Max(Abs(dx), Max(Abs(dy), Max(Abs(dxu), Max(Abs(dyu), Abs(dv)))));

     t := dy*dxu - dx*dyu + dv*sqrt(dx*dx + dy*dy);

     if tolabs < t
     then
         value := 1
     else if -tolabs <= t
     then
         value := 0
     else if t < -tolabs
     then
         value := -1
     else
         raise EDelaunyError.Create('LineOrientation - Fatal error! Point position could not be calculated.');

     Result := value;
end;

// originaly i4_wrap from geompack, an integer to lie between given limits by wrapping.
function TDelaunyTriangulation.IntWrap(ival: Integer; ilo: Integer; ihi: Integer): Integer;
var jhi: Integer;
     jlo: Integer;
     value: Integer;
     wide: Integer;
function AbsMod(i: Integer; j: Integer): Integer;
var value: Integer;
begin
     if (j = 0) then
        raise EDivByZero.Create('AbsMod - Fatal error! AbsMod(I, J) called with J = 0');

     value:= i mod j;

     if value < 0 then
        value:= value + Abs(j);

     Result:= value;
end;
begin
     jlo := Min(ilo, ihi);
     jhi := Max(ilo, ihi);

     wide := jhi + 1 - jlo;

     if wide = 1
     then
         value:= jlo
     else
         value:= jlo + AbsMod(ival - jlo, wide);

     Result:= value;
end;

// originaly vbedg from geompack: determines which boundary edges are visible to a point.
//    The point(X,Y) is assumed to be outside the convex hull of the
//    region covered by the 2D triangulation.
procedure TDelaunyTriangulation.VisibleEdge(x : Double; y: Double; const pts : TDynPointf2DArray;
 const Triangulation : TTriangles; const TriNeighbours : TTriNeighbours;
 var ltri: Integer; var ledg: Integer; var rtri: Integer; var redg: Integer);
var a: Integer;
    ax: Double;
    ay: Double;
    b: Integer;
    bx: Double;
    by: Double;
    done: Boolean;
    e: Integer;
    l: Integer;
    lr: Integer;
    t: Integer;
begin
     // ###############################################################
     // #### Find the rightmost visible boundary edge using links, then possibly
     // leftmost visible boundary edge using triangle neighbor information.
     if ltri = 0 then
     begin
          done := false;
          ltri := rtri;
          ledg := redg;
     end
     else
         done := true;

     while True do
     begin
          l := -TriNeighbours[rtri - 1][redg - 1];
          t := l div 3;
          e := l mod 3 + 1;
          a := Triangulation[t - 1][e - 1];

          if e <= 2
          then
              b := Triangulation[t - 1][e]
          else
              b := Triangulation[t - 1][0];

          ax := pts[a - 1].x;
          ay := pts[a - 1].y;

          bx := pts[b - 1].x;
          by := pts[b - 1].y;

          lr := LineOrientation(x, y, ax, ay, bx, by, 0.0);

          if lr <= 0 then
             break;

          rtri:= t;
          redg:= e;
     end;

     if (done) then
        exit;

     t := ltri;
     e := ledg;

     while True do
     begin
          b := Triangulation[t - 1][e - 1];
          e := IntWrap(e - 1, 1, 3);

          while 0 < TriNeighbours[t - 1][e - 1] do
          begin
               t := TriNeighbours[t - 1][e - 1];

               if Triangulation[t - 1][0] = b
               then
                   e := 3
               else if Triangulation[t - 1][1] = b
               then
                   e:= 1
               else
                   e := 2;
          end;

          a := Triangulation[t - 1][e - 1];
          ax := pts[a - 1].x;
          ay := pts[a - 1].y;

          bx := pts[b - 1].x;
          by := pts[b - 1].y;

          lr := LineOrientation(x, y, ax, ay, bx, by, 0.0);

          if (lr <= 0) then
             break;
     end;

     ltri := t;
     ledg := e;
end;

// originally swapc from geompack - swaps diagonal edges until all triangles are Delaunay.
function TDelaunyTriangulation.SwapEdges(i: Integer; var top: Integer; var btri: Integer; var bedg: Integer;
  const pts : TDynPointf2DArray; var Triangulation : TTriangles; numTriangles : integer;
  var TriNeighbours : TTriNeighbours; var stack: TIntegerDynArray) : Integer;
var a : Integer;
    b : Integer;
    c : Integer;
    e : Integer;
    ee : Integer;
    em1 : Integer;
    ep1 : Integer;
    f : Integer;
    fm1 : Integer;
    fp1 : Integer;
    l : Integer;
    r : Integer;
    s : Integer;
    swap : Integer;
    t : Integer;
    tt : Integer;
    u : Integer;
    x : Double;
    y : Double;
begin
     // #################################################################
     // #### Determine whether triangles in stack are Delaunay, and swap
     // diagonal edge of convex quadrilateral if not.
     x := pts[i - 1].x;
     y := pts[i - 1].y;

     while top > 0 do
     begin
          t := stack[top - 1];
          dec(top);

          if Triangulation[t - 1][0] = i then
          begin
               e := 2;
               b := Triangulation[t - 1][2];
          end
          else if Triangulation[t - 1][1] = i then
          begin
               e := 3;
               b := Triangulation[t - 1][0];
          end
          else
          begin
               e := 1;
               b := Triangulation[t - 1][1];
          end;

          a := Triangulation[t - 1][e - 1];
          u := TriNeighbours[t - 1][e - 1];

          if TriNeighbours[u - 1][0] = t then
          begin
               f := 1;
               c := Triangulation[u - 1][2];
          end
          else if TriNeighbours[u - 1][1] = t then
          begin
               f := 2;
               c := Triangulation[u - 1][0];
          end
          else
          begin
               f := 3;
               c := Triangulation[u - 1][1];
          end;

          swap := DiagonalEdge(x, y, pts[a - 1].x, pts[a - 1].y, pts[c - 1].x, pts[c - 1].y, pts[b - 1].x, pts[b - 1].y);

          if swap = 1 then
          begin
               em1 := IntWrap(e - 1, 1, 3);
               ep1 := IntWrap(e + 1, 1, 3);
               fm1 := IntWrap(f - 1, 1, 3);
               fp1 := IntWrap(f + 1, 1, 3);

               Triangulation[t - 1][ep1 - 1] := c;
               Triangulation[u - 1][fp1 - 1] := i;
               r := TriNeighbours[t - 1][ep1 - 1];
               s := TriNeighbours[u - 1][fp1 - 1];
               TriNeighbours[t - 1][ep1 - 1] := u;
               TriNeighbours[u - 1][fp1 - 1] := t;
               TriNeighbours[t - 1][e - 1] := s;
               TriNeighbours[u - 1][f - 1] := r;

               if (0 < TriNeighbours[u - 1][fm1 - 1]) then
               begin
                    inc(top);
                    stack[top - 1] := u;
               end;

               if 0 < s then
               begin
                    if TriNeighbours[s - 1][0] = u
                    then
                        TriNeighbours[s - 1][0] := t
                    else if TriNeighbours[s - 1][1] = u
                    then
                        TriNeighbours[s - 1][1] := t
                    else
                        TriNeighbours[s - 1][2] := t;

                    inc(top);

                    if (Length(pts) < top) then
                    begin
                         Result := 8;
                         exit;
                    end;

                    stack[top - 1] := t;
               end
               else
               begin
                    if (u = btri) and (fp1 = bedg) then
                    begin
                         btri := t;
                         bedg := e;
                    end;

                    l := -(3*t + e - 1);
                    tt := t;
                    ee := em1;

                    while 0 < TriNeighbours[tt - 1][ee - 1] do
                    begin
                         tt := TriNeighbours[tt - 1][ee - 1];

                         if Triangulation[tt - 1][0] = a
                         then
                             ee := 3
                         else if Triangulation[tt - 1][1] = a
                         then
                             ee := 1
                         else
                             ee := 2;
                    end;

                    TriNeighbours[tt - 1][ee - 1] := l;
               end;

               if (0 < r) then
               begin
                    if TriNeighbours[r - 1][0] = t
                    then
                        TriNeighbours[r - 1][0] := u
                    else if TriNeighbours[r - 1][1] = t
                    then
                        TriNeighbours[r - 1][1] := u
                    else
                        TriNeighbours[r - 1][2] := u;
               end
               else
               begin
                    if (t = btri) and (ep1 = bedg) then
                    begin
                         btri := u;
                         bedg := f;
                    end;

                    l := -(3*u + f - 1);
                    tt := u;
                    ee := fm1;

                    while 0 < TriNeighbours[tt - 1][ee - 1] do
                    begin
                         tt := TriNeighbours[tt - 1][ee - 1];

                         if Triangulation[tt - 1][0] = b
                         then
                             ee := 3
                         else if Triangulation[tt - 1][1] = b
                         then
                             ee := 1
                         else
                             ee := 2;
                    end;

                    TriNeighbours[tt - 1][ee - 1] := l;
               end;
          end;
     end;

     Result := 0;
end;

// originally DiaEdg from GeomPack - chooses a diagonal edge.
function TDelaunyTriangulation.DiagonalEdge(const x0, y0, x1, y1, x2, y2, x3, y3: double): Integer;
var ca : Double;
    cb : Double;
    dx10 : Double;
    dx12 : Double;
    dx30 : Double;
    dx32 : Double;
    dy10 : Double;
    dy12 : Double;
    dy30 : Double;
    dy32 : Double;
    s : Double;
    tol : Double;
    tola : Double;
    tolb : Double;
    value: Integer;
begin
     tol := 100.0*cEps;

     dx10 := x1 - x0;
     dy10 := y1 - y0;
     dx12 := x1 - x2;
     dy12 := y1 - y2;
     dx30 := x3 - x0;
     dy30 := y3 - y0;
     dx32 := x3 - x2;
     dy32 := y3 - y2;

     tola := tol*Max(Abs(dx10), Max(Abs(dy10), Max(Abs(dx30), Abs(dy30))));
     tolb := tol*Max(Abs(dx12), Max(Abs(dy12), Max(Abs(dx32), Abs(dy32))));

     ca := dx10*dx30 + dy10*dy30;
     cb := dx12*dx32 + dy12*dy32;

     if ((tola < ca) and(tolb < cb))
     then
         value := -1
     else if ((ca < -tola) and(cb < -tolb))
     then
         value := 1
     else
     begin
          tola := Max(tola, tolb);
          s := (dx10*dy30 - dx30*dy10)*cb +(dx32*dy12 - dx12*dy32)*ca;

          if (tola < s)
          then
              value := -1
          else if (s < -tola)
          then
              value := 1
          else
              value := 0;
     end;

     Result := value;
end;

initialization
   InitEps;

end.
