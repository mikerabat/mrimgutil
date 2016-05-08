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

unit AAMWarp;

// ##############################################################
// #### AAM warping routines
// ##############################################################

interface

uses SysUtils, Classes, Matrix, Registration, BaseMathPersistence;

// ##############################################################
// #### Base class for all warping functions - Triangles, TPS and (evt) hardware
type
  TCustomAAMWarper = class(TBaseMathPersistence)
  private
    fTextureWidth : integer;
    fTextureHeight : integer;
    fNumColorPlanes : integer;
    fToPts : TDoubleMatrix;
  protected
    property NumColorPlanes : integer read fNumColorPlanes;
    property TextureWidth : integer read fTextureWidth;
    property TextureHeight : integer read fTextureHeight;
    property ToPts : TDoubleMatrix read fToPts;

    procedure OnLoadIntProperty(const Name : String; Value : integer); override;
    function OnLoadObject(const Name : String; Obj : TBaseMathPersistence) : boolean; override;

    procedure DefineProps; override;
    function PropTypeOfName(const Name: string): TPropType; override;

  public
    function MapTexture(FromPts : TDoubleMatrix; img : TDoubleMatrix) : TDoubleMatrix; virtual; abstract;

    constructor Create(ToPts : TDoubleMatrix; DestImgWidth, DestImgHeight : integer; NumColorPlanes : integer); virtual;
    destructor Destroy; override;
  end;
  TCustomAAMWarperClass = class of TCustomAAMWarper;

implementation

{ TCustomAAMWarper }

const cWarpWidth = 'WarpTextureWidth';
      cWarpHeight = 'WarpTextureHeight';
      cWarpToPts = 'WarpToPts';
      cWarpNumColorPts = 'WarpNumColorPlanes';

constructor TCustomAAMWarper.Create(ToPts: TDoubleMatrix; DestImgWidth,
  DestImgHeight: integer; NumColorPlanes : integer);
begin
     inherited Create;

     fToPts := TDoubleMatrix.Create;
     fToPts.Assign(ToPts);

     fTextureWidth := DestImgWidth;
     fTextureHeight := DestImgHeight;
     fNumColorPlanes := NumColorPlanes;
end;

procedure TCustomAAMWarper.DefineProps;
begin
     AddIntProperty(cWarpWidth, fTextureWidth);
     AddIntProperty(cWarpHeight, fTextureHeight);
     AddIntProperty(cWarpNumColorPts, fNumColorPlanes);
     AddObject(cWarpToPts, fToPts);
end;

function TCustomAAMWarper.PropTypeOfName(const Name: string): TPropType;
begin
     if (CompareText(Name, cWarpWidth) = 0) or (CompareText(Name, cWarpHeight) = 0) or
        (CompareText(Name, cWarpNumColorPts) = 0)
     then
         Result := ptInteger
     else if CompareText(Name, cWarpToPts) = 0
     then
         Result := ptObject
     else
         Result := inherited PropTypeOfName(Name);
end;

destructor TCustomAAMWarper.Destroy;
begin
     fToPts.Free;

     inherited;
end;

procedure TCustomAAMWarper.OnLoadIntProperty(const Name: String;
  Value: integer);
begin
     if CompareText(Name, cWarpWidth) = 0
     then
         fTextureWidth := Value
     else if CompareText(Name, cWarpHeight) = 0
     then
         fTextureHeight := Value
     else if CompareText(Name, cWarpNumColorPts) = 0
     then
         fNumColorPlanes := Value
     else
         inherited;
end;

function TCustomAAMWarper.OnLoadObject(const Name: String;
  Obj: TBaseMathPersistence): boolean;
begin
     Result := True;
     if CompareText(Name, cWarpToPts) = 0
     then
         fToPts := obj as TDoubleMatrix
     else
         Result := inherited OnLoadObject(Name, Obj);
end;


end.
