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

unit DelaunyMapping;

// ###################################################
// ##### Mini class which implements the lienar mapping for delauny triangulations
// ###################################################

interface

uses LinearTriangulationTransformation, Triangulation;

type
  TDelaunyTriangulationMapping = class(TLinearTriangulationMapping)
  protected
    function GetTriangulatorClass : TBaseTriangulationClass; override;
  end;

implementation

uses DelaunyTriangulation;

{ TDelaunyTriangulationMapping }

function TDelaunyTriangulationMapping.GetTriangulatorClass: TBaseTriangulationClass;
begin
     Result := TDelaunyTriangulation;
end;

end.
