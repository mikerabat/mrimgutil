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

unit AAMLinearWarp;

// ##############################################################
// ##### Linear warping using a triangulation and linear interpolation
// ##############################################################

interface

uses SysUtils, Classes, AAMWarp, Matrix, LinearTriangulationTransformation,
     BaseMathPersistence, PtsDefinitions, Triangulation;

// ################################################################
// #### Texture warper which uses a triangulation and linear interpolation
// as base
type
  TLinearAAMWarper = class(TCustomAAMWarper)
  protected
    fLinearWarp : TLinearTriangulationMapping;

    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override; //

    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
    class function ClassIdentifier : String; override;
  public
    function MapTexture(FromPts : TDoubleMatrix; img : TDoubleMatrix) : TDoubleMatrix; override;

    constructor Create(ToPts : TDoubleMatrix; DestImgWidth, DestImgHeight : integer; NumColorPlanes : integer); override;
    destructor Destroy; override;
  end;

// ################################################################
// #### Texture warper which uses a triangulation and linear interpolation
// extended for the use in the inverse compositional aam's
type
  TICLinearAAMWarper = class(TLinearAAMWarper)
  private
    function GetTriangles: TBaseTriangulation;
  protected
    class function ClassIdentifier : String; override;
  public
    property Triangulation : TBaseTriangulation read GetTriangles;
    function LinearInterpolatorFactors(x, y : integer; var ptIdx : TTriangleDef; var alpha, beta, gamma : single) : boolean;

    class procedure MapPoint(const xFrom, yFrom : double; const TriFrom : TTrianglePts; var xTo, yTo : double; const TriTo : TTrianglePts);
  end;
  TICLinearAAMWarperClass = class of TICLinearAAMWarper;

implementation

uses Registration, DelaunyMapping;

{ TLinearAAMWarper }

const cAAMLinWarper = 'AAMLinearWarpObj';

class function TLinearAAMWarper.ClassIdentifier: String;
begin
     Result := 'AAMLinearWarp';
end;

constructor TLinearAAMWarper.Create(ToPts: TDoubleMatrix; DestImgWidth,
  DestImgHeight: integer; NumColorPlanes : integer);
var ptsObj : TPtsMappingObj;
begin
     inherited Create(ToPts, DestImgWidth, DestImgHeight, NumColorPlanes);

     fLinearWarp := TDelaunyTriangulationMapping.Create;

     ptsObj := TPtsMappingObj.Create(ToPts, False);
     try
        fLinearWarp.InitTriangulation(ptsObj, DestImgWidth, DestImgHeight);
        fLinearWarp.InitPtsForMapping(ptsObj);
     finally
            ptsObj.Free;
     end;
end;

procedure TLinearAAMWarper.DefineProps;
begin
     // init base class properties
     inherited;

     if Assigned(fLinearWarp) then
        AddObject(cAAMLinWarper, fLinearWarp);
end;

function TLinearAAMWarper.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, cAAMLinWarper) = 0
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;


destructor TLinearAAMWarper.Destroy;
begin
     fLinearWarp.Free;
     
     inherited;
end;

function TLinearAAMWarper.MapTexture(FromPts, img: TDoubleMatrix): TDoubleMatrix;
begin
     // note the warping mechanism only needs references no fixed objects
     fLinearWarp.InitPtsForMapping(FromPts);
     Result := fLinearWarp.MapImage(img, NumColorPlanes);
end;

function TLinearAAMWarper.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, cAAMLinWarper) = 0
     then
         fLinearWarp := obj as TLinearTriangulationMapping
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

{ TICLinearAAMWarper }

class function TICLinearAAMWarper.ClassIdentifier: String;
begin
     Result := 'ICAAMLinearWarp';
end;

function TICLinearAAMWarper.GetTriangles: TBaseTriangulation;
begin
     Result := fLinearWarp.Triangulation;
end;

function TICLinearAAMWarper.LinearInterpolatorFactors(x, y: integer; var ptIdx : TTriangleDef; var alpha,
  beta, gamma: single) : boolean;
var pt : TPointf2D;
begin
     pt.x := x;
     pt.y := y;
     Result := fLinearWarp.CalcFactorsForPoints(pt, ptIdx, alpha, beta, gamma);
end;

class procedure TICLinearAAMWarper.MapPoint(const xFrom, yFrom: double;
  const TriFrom: TTrianglePts; var xTo, yTo: double; const TriTo: TTrianglePts);
var c, alpha, beta, gamma : double;
begin
     c := TriFrom[1].x*TriFrom[2].y - TriFrom[1].x*TriFrom[0].y - TriFrom[0].x*TriFrom[2].y -
          Trifrom[2].x*TriFrom[2].y + TriFrom[2].x*TriFrom[0].y + TriFrom[0].x*TriFrom[1].y;
     c := 1/c;
     alpha := (yFrom*TriFrom[2].x - xFrom*TriFrom[2].y + xFrom*Trifrom[1].y - yFrom*TriFrom[1].y +
               TriFrom[1].x*TriFrom[2].y - TriFrom[2].x*TriFrom[1].y)*c;
     beta := (-yFrom*TriFrom[2].x + yFrom*TriFrom[0].x + TriFrom[2].x*TriFrom[0].y + xFrom*TriFrom[2].y -
              TriFrom[0].x*TriFrom[2].y - TriFrom[2].x*TriFrom[1].y)*c;
     gamma := 1 - alpha - beta;

     xTo := alpha*TriTo[0].x + beta*TriTo[1].x + gamma*TriTo[2].x;
     yTo := alpha*TriTo[0].x + beta*TriTo[1].y + gamma*TriTo[2].y;
end;

initialization
   RegisterMathIO(TLinearAAMWarper);

end.
