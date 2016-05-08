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

unit Triangulation;

// ########################################################
// #### Base triangulation defintions
// ########################################################

interface

uses SysUtils, Classes, PtsDefinitions, BaseMathPersistence;

// ########################################################
// ##### Loading and storing of triangulation objects
type
  TBaseTriangulation = class(TBaseMathPersistence)
  private
    function GetVertextToTriangle: TVertexToTriangles;
  protected
    fTriangulation : TTriangles;
    fVertToTriangle : TVertexToTriangles;

    class function ClassIdentifier : String; override;
    procedure DefineProps; override;
    function PropTypeOfName(const Name : string) : TPropType; override;
    procedure OnLoadBinaryProperty(const Name : String; const Value; size : integer); override;
    procedure InitVertToTriangle;
  public
    property Triangles : TTriangles read fTriangulation;
    property VertextToTriangle : TVertexToTriangles read GetVertextToTriangle;

    procedure CreateTriangulation(var pts : TDynPointf2DArray); virtual; abstract;
  end;
  TBaseTriangulationClass = class of TBaseTriangulation;

implementation

uses math;

{ TBaseTriangulation }
const cBaseTriangulationDescr = 'Triangulation';
      cTriangles = 'Triangles';

class function TBaseTriangulation.ClassIdentifier: String;
begin
     Result := cBaseTriangulationDescr;
end;

procedure TBaseTriangulation.DefineProps;
begin
     if Length(fTriangulation) > 0 then
        AddBinaryProperty(cTriangles, fTriangulation[0], Length(fTriangulation)*sizeof(fTriangulation[0]));
end;

function TBaseTriangulation.PropTypeOfName(const Name: string): TPropType;
begin
     if CompareText(Name, cTriangles) = 0
     then
         Result := ptBinary
     else
         Result := inherited PropTypeOfName(Name);
end;

function TBaseTriangulation.GetVertextToTriangle: TVertexToTriangles;
begin
     InitVertToTriangle;

     Result := fVertToTriangle;
end;

procedure TBaseTriangulation.InitVertToTriangle;
var i : Integer;
    numPoints : integer;
    j : Integer;
    numTriPts : Array of integer;
begin
     if Length(fVertToTriangle) = 0 then
     begin
          // find max point
          numPoints := 0;
          for i := 0 to Length(fTriangulation) - 1 do
              numPoints := Max(fTriangulation[0][0], Max(fTriangulation[0][1], fTriangulation[0][2]));

          // build associative array
          SetLength(fVertToTriangle, numPoints);
          SetLength(numTriPts, numPoints);
          for i := 0 to numPoints - 1 do
          begin
               for j := 0 to Length(fTriangulation) - 1 do
               begin
                    if (fTriangulation[0][0] = i) or (fTriangulation[0][1] = i) or (fTriangulation[0][2] = i) then
                    begin
                         inc(numTriPts[i]);

                         if numTriPts[i] >= Length(fVertToTriangle[i]) then
                            SetLength(fVertToTriangle[i], Min(Max(10, Length(fVertToTriangle[i])*2), Length(fVertToTriangle[i]) + 100));

                         fVertToTriangle[i][numTriPts[i] - 1] := j;
                    end;
               end;
          end;

          // cleanup memory consumption
          for i := 0 to numPoints - 1 do
              SetLength(fVertToTriangle[i], numTriPts[i]);
     end;
end;

procedure TBaseTriangulation.OnLoadBinaryProperty(const Name: String;
  const Value; size: integer);
begin
     if CompareText(Name, cTriangles) = 0 then
     begin
          assert(size mod sizeof(TTriangleDef) = 0, 'Error unsupported size');
          SetLength(fTriangulation, size div sizeof(TTriangleDef));
          Move(Value, fTriangulation[0], size);
     end;
end;

initialization
   RegisterMathIO(TBaseTriangulation);

end.
