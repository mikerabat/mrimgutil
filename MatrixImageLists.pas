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

unit MatrixImageLists;

// #########################################################
// #### Loading and converting of image lists
// #########################################################

interface

uses SysUtils, Classes, contnrs, Matrix, ImageMatrixConv;

// #########################################################
// #### Base class
type
  TCustomMtxImageList = class(TObject)
  private
    fNumImages : integer;
    fImgWidth : integer;
    fImgHeight : integer;

    fImgResize: TOnImgResizeEvent;
    fRecursive: boolean;
  protected
    property NumImages : integer read fNumImages;
    procedure OnImageLoad(mtx : TDoubleMatrix; actNum : integer; const fileName : string); virtual; abstract;
  public
    property OnImgResizeEvent : TOnImgResizeEvent read fImgResize write fImgResize;
    property Recursive : boolean read fRecursive write fRecursive;

    procedure ReadListFromDirectory(const Directory : string; convType : TImageConvType);
    procedure ReadListFromDirectoryRaw(const Directory : string; convType : TImageConvType);
  end;

// #########################################################
// #### complete list as a whole matrix
type
  TOneMtxImageList = class(TCustomMtxImageList)
  private
    fImages : TDoubleMatrix;
  protected
    procedure OnImageLoad(mtx : TDoubleMatrix; actNum : integer; const fileName : string); override;
  public
    property Images : TDoubleMatrix read fImages;

    constructor Create;
    destructor Destroy; override;
  end;

// #########################################################
// #### Complete list
type
  TMtxImageList = class(TCustomMtxImageList)
  private
    fImages : TObjectList;
    function GetImage(index: integer): TDoubleMatrix;
    function GetImageCount: integer;
  protected
    procedure OnImageLoad(mtx : TDoubleMatrix; actNum : integer; const FileName : string); override;
  public
    property Images[index : integer] : TDoubleMatrix read GetImage;
    property ImageCount : integer read GetImageCount;

    constructor Create;
    destructor Destroy; override;
  end;

// #########################################################
// #### list for incremental building - note the procedure does not store
// the images
type
  TImageLoadEvent = procedure(Sender : TObject; mtx : TDoubleMatrix; actNum, NumImags : integer; const FileName : string) of Object;
  TIncrementalImageList = class(TCustomMtxImageList)
  private
    fOnImageLoad : TImageLoadEvent;
  protected
    procedure OnImageLoad(mtx : TDoubleMatrix; actNum : integer; const FileName : string); override;
  public
    property OnImageStep : TImageLoadEvent read fOnImageLoad write fOnImageLoad;
  end;

implementation

uses Graphics;

{ TMtxImageList }

constructor TMtxImageList.Create;
begin
     inherited Create;

     fImages := TObjectList.Create(True);
end;

destructor TMtxImageList.Destroy;
begin
     fImages.Free;

     inherited;
end;

function TMtxImageList.GetImage(index: integer): TDoubleMatrix;
begin
     Result := TDoubleMatrix(fImages[index]);
end;

function TMtxImageList.GetImageCount: integer;
begin
     Result := fImages.Count;
end;

procedure TMtxImageList.OnImageLoad(mtx: TDoubleMatrix; actNum : integer; const fileName : string);
var img : TDoubleMatrix;
begin
     if actNum = 0 then
     begin
          fImages.Clear;
          fImages.Capacity := NumImages;
     end;

     // ############################################
     // #### copy image
     img := TDoubleMatrix.Create;
     img.Assign(mtx);
     fImages.Add(img);
end;

{ TCustomMtxImageList }

procedure TCustomMtxImageList.ReadListFromDirectory(const Directory: string;
  convType: TImageConvType);
var registeredExtensions : TStringList;
    nonRegExtensions : TStringList;
    fileNames : TStringList;
    i : Integer;
    pict : TPicture;
    bmp : TBitmap;
    converter : TMatrixImageConverter;
    mtx : TDoubleMatrix;

procedure EnumFilesInDirRecursive(const dir : string; ResList : TStringList);
var imgFiles : TSearchRec;
    ext : string;
    pict : TPicture;
begin
     // note it would be easier if we would have access to the function Graphics.GetFileFormats
     // function. Se I have to build it myself
     if FindFirst(IncludeTrailingPathDelimiter(dir) + '*.*', faAnyFile, imgFiles) = 0 then
     begin
          repeat
                if (imgFiles.Attr and faDirectory) = faDirectory then
                begin
                     // recursively check for new files
                     if (imgFiles.Name[1] <> '.') and fRecursive then
                        EnumFilesInDirRecursive(IncludeTrailingPathDelimiter(dir) + imgFiles.Name, ResList);

                     continue;
                end;

                ext := ExtractFileExt(imgFiles.Name);

                if nonRegExtensions.IndexOf(ext) >= 0
                then
                    continue
                else if registeredExtensions.IndexOf(ext) >= 0
                then
                    ResList.Add(IncludeTrailingPathDelimiter(dir) + imgFiles.Name)
                else
                begin
                     // test load the file -> if successfull then it's good
                     try
                        pict := TPicture.Create;
                        try
                           pict.LoadFromFile(IncludeTrailingPathDelimiter(dir) + imgFiles.Name);

                           if fImgWidth = -1 then
                           begin
                                fImgWidth := pict.Width;
                                fImgHeight := pict.Height;
                           end;

                           registeredExtensions.Add(ExtractFileExt(imgFiles.Name));
                           ResList.Add(IncludeTrailingPathDelimiter(dir) + imgFiles.Name)
                        finally
                               pict.Free;
                        end;
                     except
                           nonRegExtensions.Add(ExtractFileExt(imgFiles.Name));
                     end;
                end;
          until FindNext(imgFiles) <> 0;
     end;

     FindClose(imgFiles);
end;

function EnumFilesInDir(const dir : string) : TStringList;
begin
     Result := TStringList.Create;

     EnumFilesInDirRecursive(dir, Result);
end;

begin
     fImgWidth := -1;
     fImgHeight := -1;
     registeredExtensions := TStringList.Create;
     nonRegExtensions := TStringList.Create;
     try
        filenames := EnumFilesInDir(Directory);
        try
           fNumImages := fileNames.Count;
           if fNumImages = 0 then
              exit;

           mtx := TDoubleMatrix.Create(fImgWidth, fImgHeight);
           try
              converter := TMatrixImageConverter.Create(convType, False, True, fImgWidth, fImgHeight);
              try
                 // #####################################################
                 // #### read the images
                 i := 0;
                 while i < fNumImages do
                 begin
                      pict := TPicture.Create;
                      try
                         pict.LoadFromFile(filenames[i]);
                         bmp := TBitmap.Create;
                         try
                            bmp.SetSize(pict.Width, pict.Height);
                            bmp.Canvas.Draw(0, 0, pict.Graphic);

                            if (bmp.Width <> fImgWidth) or (bmp.Height <> fImgHeight) and Assigned(fImgResize) then
                               fImgResize(Self, bmp, fImgWidth, fImgHeight);

                            if (bmp.Width <> fImgWidth) or (bmp.Height <> fImgHeight) then
                               raise Exception.Create('Error image properties do not match the expected ones');

                            // ########################################################
                            // #### convert and store images
                            converter.ImageToMatrix(mtx, bmp);
                            OnImageLoad(mtx, i, fileNames[i]);
                         finally
                                bmp.Free;
                         end;
                      finally
                             pict.Free;
                      end;

                      inc(i);
                 end;
              finally
                     converter.Free;
              end;
           finally
                  mtx.Free;
           end;
        finally
               filenames.Free;
        end;
     finally
            registeredExtensions.Free;
            nonRegExtensions.Free;
     end;
end;

procedure TCustomMtxImageList.ReadListFromDirectoryRaw(const Directory: string;
  convType: TImageConvType);
var registeredExtensions : TStringList;
    nonRegExtensions : TStringList;
    fileNames : TStringList;
    i : Integer;
    pict : TPicture;
    bmp : TBitmap;
    mtx : TDoubleMatrix;

procedure EnumFilesInDirRecursive(const dir : string; ResList : TStringList);
var imgFiles : TSearchRec;
    ext : string;
    pict : TPicture;
begin
     // note it would be easier if we would have access to the function Graphics.GetFileFormats
     // function. Se I have to build it myself
     if FindFirst(IncludeTrailingPathDelimiter(dir) + '*.*', faAnyFile, imgFiles) = 0 then
     begin
          repeat
                if (imgFiles.Attr and faDirectory) = faDirectory then
                begin
                     // recursively check for new files
                     if (imgFiles.Name <> '') and (imgFiles.Name[1] <> '.') and fRecursive then
                        EnumFilesInDirRecursive(IncludeTrailingPathDelimiter(dir) + imgFiles.Name, ResList);

                     continue;
                end;

                ext := ExtractFileExt(imgFiles.Name);

                if nonRegExtensions.IndexOf(ext) >= 0
                then
                    continue
                else if registeredExtensions.IndexOf(ext) >= 0
                then
                    ResList.Add(IncludeTrailingPathDelimiter(dir) + imgFiles.Name)
                else
                begin
                     // test load the file -> if successfull then it's good
                     try
                        pict := TPicture.Create;
                        try
                           pict.LoadFromFile(IncludeTrailingPathDelimiter(dir) + imgFiles.Name);

                           if fImgWidth = -1 then
                           begin
                                fImgWidth := pict.Width;
                                fImgHeight := pict.Height;
                           end;

                           registeredExtensions.Add(ExtractFileExt(imgFiles.Name));
                           ResList.Add(IncludeTrailingPathDelimiter(dir) + imgFiles.Name)
                        finally
                               pict.Free;
                        end;
                     except
                           nonRegExtensions.Add(ExtractFileExt(imgFiles.Name));
                     end;
                end;
          until FindNext(imgFiles) <> 0;
     end;

     FindClose(imgFiles);
end;

function EnumFilesInDir(const dir : string) : TStringList;
begin
     Result := TStringList.Create;

     EnumFilesInDirRecursive(dir, Result);
end;

begin
     fImgWidth := -1;
     fImgHeight := -1;
     registeredExtensions := TStringList.Create;
     nonRegExtensions := TStringList.Create;
     try
        filenames := EnumFilesInDir(Directory);
        try
           fNumImages := fileNames.Count;
           if fNumImages = 0 then
              exit;

           // #####################################################
           // #### read the images
           i := 0;
           while i < fNumImages do
           begin
                pict := TPicture.Create;
                try
                   pict.LoadFromFile(filenames[i]);
                   bmp := TBitmap.Create;
                   try
                      bmp.SetSize(pict.Width, pict.Height);
                      bmp.Canvas.Draw(0, 0, pict.Graphic);

                      mtx := TMatrixImageConverter.ConvertImage(bmp, convType);
                      try
                         // ########################################################
                         // #### convert and store images
                         OnImageLoad(mtx, i, filenames[i]);
                      finally
                             mtx.Free
                      end;
                   finally
                          bmp.Free;
                   end;
                finally
                       pict.Free;
                end;

                inc(i);
           end;
        finally
               filenames.Free;
        end;
     finally
            registeredExtensions.Free;
            nonRegExtensions.Free;
     end;
end;

{ TOneMtxImageList }

constructor TOneMtxImageList.Create;
begin
     inherited Create;

     fImages := TDoubleMatrix.Create;
end;

destructor TOneMtxImageList.Destroy;
begin
     fImages.Free;
     
     inherited;
end;

procedure TOneMtxImageList.OnImageLoad(mtx: TDoubleMatrix; actNum : integer; const fileName : string);
var oldWidth, oldHeight : integer;
begin
     if actNum = 0 then
     begin
          fImages.Free;
          fImages := TDoubleMatrix.Create(NumImages, mtx.Width*mtx.Height);
     end;

     oldWidth := mtx.Width;
     oldHeight := mtx.Height;

     mtx.ReshapeInPlace(1, oldWidth*oldHeight);

     fImages.SetRow(actNum, mtx);
     mtx.ReshapeInPlace(oldWidth, oldHeight);
end;

{ TIncrementalImageList }

procedure TIncrementalImageList.OnImageLoad(mtx: TDoubleMatrix;
  actNum: integer; const FileName : string);
begin
     if Assigned(fOnImageLoad) then
        fOnImageLoad(Self, mtx, actNum, NumImages, FileName);
end;

end.
