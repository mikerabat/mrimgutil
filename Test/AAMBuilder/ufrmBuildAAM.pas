unit ufrmBuildAAM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Mask, ComCtrls, BaseAAMModel, Spin,
  Matrix, AAMModelBuilder;

type
  TfrmAAMBuilder = class(TForm)
    pnlOptions: TPanel;
    chkBuildColorAAM: TCheckBox;
    pnlDir: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblAAMDB: TLabel;
    pbOverall: TProgressBar;
    btnBuild: TButton;
    lblLevels: TLabel;
    cboWarpClass: TComboBox;
    lblModelEnergy: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    pbStep: TProgressBar;
    lblStep: TLabel;
    svModel: TSaveDialog;
    tbAAMModel: TTabControl;
    btnLoad: TButton;
    deOpenAAM: TOpenDialog;
    edAAMDir: TEdit;
    edLevels: TEdit;
    edShapeEnergy: TEdit;
    edTextureEnergy: TEdit;
    edCombinedEnergy: TEdit;
    edLearnSteps: TEdit;
    edScale: TEdit;
    procedure edAAMDirExit(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure tbAAMModelChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fAAMDir : string;
    fModel : TCustomAAMModel;
    fParams : Array of TTrackBar;

    procedure OnPChange(Sender : TObject);
    procedure InitImages;
    procedure InitDisturbanceParams;
    procedure InitDB(const dir : string);
    function LoadFaces : TDoubleMatrixDynArr;
    procedure BuildProgress(Sender : TObject; overallProgress : integer; stepProgress : integer; step : TAAMStep);
  public
    { Public-Deklarationen }
    property Model : TCustomAAMModel read fModel;
  end;

var
  frmAAMBuilder: TfrmAAMBuilder;

implementation

uses BaseMathPersistence, AAMPyramidModelBuilder, CombinedPCA, ShapePCA,
     TexturePCA, AAMTPSWarp, AAMLinearWarp, imageMatrixConv, Math,
     AAMPyramidModel, jpeg, BinaryReaderWriter, AAMConst,
     ufrmAAMParams, ufrmAAMOptimize, Registry;

{$R *.dfm}

function TfrmAAMBuilder.LoadFaces: TDoubleMatrixDynArr;
var i : integer;
    sr : TSearchRec;
begin
     i := 0;
     if FindFirst(edAAMDir.Text + '\*.mtx', faAnyFile, sr) = 0 then
     begin
          repeat
                inc(i);
          until FindNext(sr) <> 0;
     end;
     FindClose(sr);

     SetLength(Result, i);
     i := 0;
     if FindFirst(edAAMDir.Text + '\*.mtx', faAnyFile, sr) = 0 then
     begin
          repeat
                Result[i] := ReadObjFromFile(edAAMDir.Text + '\' + sr.Name) as TDoubleMatrix;
                inc(i);
          until FindNext(sr) <> 0;
     end;
     FindClose(sr);
end;

procedure TfrmAAMBuilder.OnPChange(Sender: TObject);
begin
     InitImages;
end;

procedure TfrmAAMBuilder.tbAAMModelChange(Sender: TObject);
begin
     frmModelParams.Visible := false;
     frmModelParams.Parent := nil;
     frmAAMOptimize.Visible := false;
     frmAAMOptimize.Parent := nil;

     if tbAAMModel.TabIndex = 0 then
     begin
          frmModelParams.Parent := tbAAMModel;
          frmModelParams.Visible := True;
     end
     else
     begin
          frmAAMOptimize.Parent := tbAAMModel;
          frmAAMOptimize.Visible := True;
     end;
end;

procedure TfrmAAMBuilder.btnBuildClick(Sender: TObject);
var shapes : TDoubleMatrixDynArr;
    pyramidParams : TPCAAAMPyramidCombinedParams;
    i : integer;
begin
     if not DirectoryExists(edAAMDir.Text) then
     begin
          MessageDlg('The given database directory (' + edAAMDir.Text + ') does not exist.', mtError, [mbOK], 0);
          exit;
     end;
     FreeAndNil(fModel);

     shapes := LoadFaces;
     try
        // build a pyramid according to the selected properties
        pyramidParams := TPCAAAMPyramidCombinedParams.Create;
        pyramidParams.ShapeParams := TPCAAAMShapeParams.Create;
        pyramidParams.LearnParams := TLearnParams.Create;
        
        pyramidParams.ShapeParams.ModelEnergy := StrToInt( edShapeEnergy.Text )/100;

        pyramidParams.AppearanceParams := TPCAAAMTextureParams.Create;

        if chkBuildColorAAM.Checked
        then
            pyramidParams.AppearanceParams.ImgLoadType := ctRGB
        else
            pyramidParams.AppearanceParams.ImgLoadType := ctGrayScale;

        pyramidParams.AppearanceParams.AppearanceScale := StrToInt( edScale.Text );

        if cboWarpClass.ItemIndex = 0
        then
            pyramidParams.AppearanceParams.WarpClass := TLinearAAMWarper
        else
            pyramidParams.AppearanceParams.WarpClass := TAAMTPSWarp;
        pyramidParams.AppearanceParams.ModelEnergy := StrToInt( edTextureEnergy.Text )/100;

        pyramidParams.learnParams.CombinedModelEnergy := StrToInt( edCombinedEnergy.Text )/100;
        pyramidParams.learnParams.NumIterSteps := StrToInt( edLearnSteps.Text );
        pyramidParams.NumLevels := StrToInt( edLevels.Text );

        // build the model
        with TPyramidAAMBuilder.Create(TAAMModelBuilder, TPyramidAAMModel) do
        try
           Progress := BuildProgress;
           SetParams(pyramidParams);
           fModel := BuildAAModel(shapes, edAAMDir.Text);
        finally
               Free;
        end;
     finally
            // cleanup
            for i := 0 to Length(shapes) - 1 do
                shapes[i].Free;

            shapes := nil;

            lblStep.Caption := '---';
     end;

     // now it's time to save the model
     if svModel.Execute then
        fModel.SaveToFile(svModel.FileName, TBinaryReaderWriter);

     InitDisturbanceParams;
     InitImages;

     frmAAMOptimize.Init;
end;

procedure TfrmAAMBuilder.btnLoadClick(Sender: TObject);
begin
     if deOpenAAM.Execute then
     begin
          FreeAndNil(fModel);
          fModel := ReadObjFromFile(deOpenAAM.FileName) as TCustomAAMModel;

          InitDisturbanceParams;
          InitImages;

          frmAAMOptimize.Init;
     end;
end;

procedure TfrmAAMBuilder.BuildProgress(Sender: TObject; overallProgress,
  stepProgress: integer; step: TAAMStep);
begin
     pbOverall.Position := overallProgress;
     pbStep.Position := stepProgress;
     case step of
       asBuildShape: lblStep.Caption := 'Shape';
       asBuildTexture: lblStep.Caption := 'Texture';
       asBuildCombined: lblStep.Caption := 'Combined';
       asLearn: lblStep.Caption := 'Learning';
       asFinalize: lblStep.Caption := 'Finalize';
     end;

     lblStep.Update;
end;

procedure TfrmAAMBuilder.edAAMDirExit(Sender: TObject);
begin
     if DirectoryExists(edAAMDir.Text) then
        InitDB(edAAMDir.Text);
end;

procedure TfrmAAMBuilder.FormCreate(Sender: TObject);
var directory : string;
begin
     // load last known recording directory from the registry
     with TRegistry.Create(KEY_READ) do
     try
        RootKey := HKEY_CURRENT_USER;
        OpenKeyReadOnly('Software\AAMBuilder');
        directory := ReadString('LastDBDir');
     finally
            Free;
     end;

     if (directory <> '') and DirectoryExists(directory) then
        edAAMDir.Text := directory;
end;

procedure TfrmAAMBuilder.FormDestroy(Sender: TObject);
var directory : string;
begin
     directory := edAAMDir.Text;

     if (directory <> '') and DirectoryExists(directory) then
     begin
          // save last known recording directory from the registry
          with TRegistry.Create(KEY_ALL_ACCESS) do
          try
             RootKey := HKEY_CURRENT_USER;
             OpenKey('Software\AAMBuilder', True);
             WriteString('LastDBDir', directory);
          finally
                 Free;
          end;
     end;
end;

procedure TfrmAAMBuilder.FormShow(Sender: TObject);
begin
     tbAAMModelChange(self);
end;

procedure TfrmAAMBuilder.InitDB(const dir: string);
var sr : TSearchRec;
    numFiles : integer;
    mtx : IMatrix;
    numPts : integer;
begin
     fAAMDir := IncludeTrailingPathDelimiter(dir);

     numFiles := 0;

     numPts := -1;
     // check out the number of files
     if FindFirst(fAAMDir + '*.mtx', faAnyFile, sr) = 0 then
     begin
          repeat
                mtx := ReadObjFromFile(fAAMDir + sr.Name) as IMatrix;

                if numPts = -1
                then
                    numPts := mtx.Height
                else
                begin
                     if numPts <> mtx.Height then
                        raise Exception.Create(Format('Error number of points do not match in %s, pts: %d, pts in file: %d ',
                                                      [sr.Name, numPts, mtx.Height]));
                end;
                inc(numFiles);
          until FindNext(sr) <> 0;
     end;

     FindClose(sr);

     lblAAMDB.Caption := 'AAM DB: ' + IntToStr(numFiles) + ' found';
end;

procedure TfrmAAMBuilder.InitDisturbanceParams;
var i : integer;
begin
     // remove old ones
     for i := 0 to Length(fParams) - 1 do
     begin
          fParams[i].Free;
     end;

     fParams := nil;

     if not Assigned(fModel) then
        exit;

     SetLength(fParams, fModel.NumEigVals);
     for i := 0 to fModel.NumEigVals - 1 do
     begin
          fParams[i] := TTrackBar.Create(self);
          fParams[i].Parent := frmModelParams.pnlParams;
          fParams[i].Align := alNone;
          if i = 0
          then
              fParams[i].Top := frmModelParams.lblDisturbanceParams.Height
          else
              fParams[i].Top := fParams[i-1].Top + fParams[i-1].Height;


          fParams[i].Frequency := 5;
          fParams[i].Min := -200;
          fParams[i].Max := 200;
          fParams[i].Position := 0;
          fParams[i].OnChange := OnPChange;
          fParams[i].ShowSelRange := False;
          fParams[i].TickStyle := tsNone;
     end;
end;

procedure TfrmAAMBuilder.InitImages;
var p : TDoubleMatrix;
    img : TDoubleMatrix;
    bmp : TBitmap;
    i, j : Integer;
    shape : IMatrix;
    bmp2 : TBitmap;
    idx : integer;
begin
     frmModelParams.imgMeanImg.Picture := nil;

     if not Assigned(fModel) then
        exit;

     // create a normal mean image at the highest level
     p := TDoubleMatrix.Create(1, fModel.NumEigVals + cNumPoseParams);
     p[0, p.Height - 1] := 1 + fModel.TextureContrast;
     p[0, p.Height - 2] := fModel.TextureBrightness;
     p[0, fModel.NumEigVals] := StrToInt( edScale.Text );
     p[0, fmodel.NumEigVals + 1] := 0;
     p[0, fmodel.NumEigVals + 2] := frmModelParams.imgMeanImg.Width div 2;
     p[0, fmodel.NumEigVals + 3] := frmModelParams.imgMeanImg.Height div 2;

     img := fModel.TextureFromP(p);
     if fModel.NumColorPlanes = 3
     then
         bmp := TMatrixImageConverter.ConvertImage(img, ctRGB)
     else
         bmp := TMatrixImageConverter.ConvertImage(img, ctGrayScale);


     bmp2 := TBitmap.Create;
     bmp2.SetSize(bmp.Width*4, 2*(bmp.Height*3 div 2));
     bmp2.Canvas.Draw(3*bmp2.Width div 4 - bmp.Width div 2, bmp2.Height div 2 - bmp.height div 2, bmp);
     bmp.Canvas.Brush.Style := bsSolid;

     p := TDoubleMatrix.Create(1, fModel.NumEigVals + cNumPoseParams);
     p[0, p.Height - 1] := 1 + fModel.TextureContrast;
     p[0, p.Height - 2] := fModel.TextureBrightness;
     p[0, fModel.NumEigVals] := StrToInt( edScale.Text );
     p[0, fmodel.NumEigVals + 1] := 0;
     p[0, fmodel.NumEigVals + 2] := bmp2.width div 4;
     p[0, fmodel.NumEigVals + 3] := bmp2.Height div 2;

     // add some disturbed items
     bmp2.Canvas.Brush.Color := clLtGray;
     bmp2.Canvas.pen.Color := clLtGray;

     for i := 0 to 5 do
     begin
          idx := random(fModel.NumEigVals);

          p[0, idx] := ifthen(random(2) = 1, -1, 1)*random(10)/2*sqrt(fModel.EigVals[idx]);

          shape := fModel.ShapeFromP(P);

          for j := 0 to shape.Height - 1 do
              bmp2.Canvas.Ellipse(Round(shape[0, j]) - 2, Round(shape[1, j]) - 2, Round(shape[0, j]) + 2, Round(shape[1, j]) + 2);
     end;

     for i := 0 to fModel.NumEigVals - 1 do
         p[0, i] := 0;

     p[0, fmodel.NumEigVals + 2] := bmp2.width div 4;
     p[0, fmodel.NumEigVals + 3] := bmp2.Height div 2;

     // show the shapes on the left side of that image
     shape := fModel.ShapeFromP(P);

     bmp2.Canvas.Brush.Color := clRed;
     bmp2.Canvas.pen.Color := clRed;
     for i := 0 to shape.Height - 1 do
         bmp2.Canvas.Ellipse(Round(shape[0, i]) - 2, Round(shape[1, i]) - 2, Round(shape[0, i]) + 2, Round(shape[1, i]) + 2);

     frmModelParams.imgMeanimg.Picture.Assign(bmp2);

     bmp.Free;
     img.Free;
     bmp2.Free;

     // init disturbed image
     for i := 0 to fModel.NumEigVals - 1 do
         p[0, i] := fParams[i].Position/100*sqrt(fModel.EigVals[i]);

     img := fModel.TextureFromP(p);
     if fModel.NumColorPlanes = 3
     then
         bmp := TMatrixImageConverter.ConvertImage(img, ctRGB)
     else
         bmp := TMatrixImageConverter.ConvertImage(img, ctGrayScale);

     frmModelParams.imgDisturbed.Picture.Assign(bmp);

     bmp.Free;
     img.Free;
end;

end.
