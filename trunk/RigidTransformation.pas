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

unit RigidTransformation;

// #######################################################
// #### Base rigid object transformations
// #######################################################

interface

uses Classes, Registration, Matrix, BaseMathPersistence;

// #######################################################
// #### Common properties for rigid mappings
// a rigid mapping is defined by an afine transformation:
type
  TBaseRigidPtsRegistration = class(TBasePtsFromToMapping)
  private
    fMapping : TDoubleMatrix;
  protected
    property Mapping : TDoubleMatrix read fMapping;

    procedure DefineProps; override;
    function OnLoadObject(const Name : String; obj : TBaseMathPersistence) : boolean; override;
  public
    function MapPoints(const pts : TPtsMappingObj) : TPtsMappingObj; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Types;

{ TBaseRigidPtsRegistration }

const cRigidRegMapping = 'RigidMapping';

constructor TBaseRigidPtsRegistration.Create;
begin
     inherited Create;

     fMapping := TDoubleMatrix.Create;
end;

procedure TBaseRigidPtsRegistration.DefineProps;
begin
     if Assigned(fMapping) then
        AddObject(cRigidRegMapping, fMapping);
end;

destructor TBaseRigidPtsRegistration.Destroy;
begin
     fMapping.Free;
     
     inherited;
end;

function TBaseRigidPtsRegistration.MapPoints(
  const pts: TPtsMappingObj): TPtsMappingObj;
var ptsMtx : TDoubleMatrix;
    resMtx : TDoubleMatrix;
begin
     Result := nil;
     if not Assigned(fMapping) then
        exit;

     // #######################################################
     // #### Map points in two steps
     ptsMtx := pts.HmgPtsAsMtx;
     try
        ptsMtx.TransposeInPlace;
        resMtx := fMapping.Mult(ptsMtx);
        resMtx.TransposeInPlace;
        resMtx.SetSubMatrix(0, 0, ptsMtx.Height - 1, resMtx.Height);

        Result := TPtsMappingObj.Create(resMtx, True);
     finally
            fMapping.UseFullMatrix;
            ptsMtx.Free;
     end;
end;

function TBaseRigidPtsRegistration.OnLoadObject(const Name: String;
  obj: TBaseMathPersistence): boolean;
begin
     Result := True;

     if CompareText(Name, cRigidRegMapping) = 0
     then
         fMapping := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;

end.
