unit ufrmAAMOptimize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls, Spin;

type
  TfrmAAMOptimize = class(TForm)
    Panel1: TPanel;
    btnLoadTestImage: TButton;
    dlTestImage: TOpenPictureDialog;
    pbTestImage: TPaintBox;
    pbOptimized: TPaintBox;
    Label1: TLabel;
    lblOptTime: TLabel;
    edScale: TEdit;
    procedure btnLoadTestImageClick(Sender: TObject);
    procedure pbTestImagePaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbTestImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure pbOptimizedPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbTestImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private-Deklarationen }
    fBmp : TBitmap;
    fBmpInit : TBitmap;
    fBmpOpt : TBitmap;
    fx, fy : integer;
    fFreq : Int64;

    procedure OptimizeImage;
  public
    { Public-Deklarationen }
    procedure Init;
  end;

var
  frmAAMOptimize: TfrmAAMOptimize;

implementation

uses ufrmBuildAAM, Math, matrix, AAMConst, ImageMatrixConv, AAMPyramidModel,
     AAMModel;

{$R *.dfm}

procedure TfrmAAMOptimize.btnLoadTestImageClick(Sender: TObject);
var pic : TPicture;
begin
     if dlTestImage.Execute then
     begin
          pic := TPicture.Create;
          try
             pic.LoadFromFile(dlTestImage.FileName);

             fBmp.Free;
             fBmp := TBitmap.Create;
             fBmp.PixelFormat := pf24bit;
             fBmp.SetSize(pic.Width, pic.Height);
             fBmp.Canvas.Draw(0, 0, pic.Graphic);
          finally
                 pic.Free;
          end;
     end;

     pbTestImage.Invalidate;
     pbOptimized.Invalidate;
end;

procedure TfrmAAMOptimize.FormCreate(Sender: TObject);
begin
     QueryPerformanceFrequency(fFreq);
end;

procedure TfrmAAMOptimize.FormDestroy(Sender: TObject);
begin
     fBmp.Free;
     fBmpInit.Free;
     fBmpOpt.Free;
end;

procedure TfrmAAMOptimize.FormResize(Sender: TObject);
begin
     pbTestImage.Width := width div 2 - 5;
     pbOptimized.Left := width div 2 + 5;
     pbOptimized.Width := width div 2 - 5;
end;

procedure TfrmAAMOptimize.Init;
var i : integer;
begin
     edScale.Text := IntToStr( Round(frmAAMBuilder.Model.LearnInitScale) );

     if frmAAMBuilder.Model is TCustomPyramidAAMModel then
     begin
          for i := 0 to TCustomPyramidAAMModel(frmAAMBuilder.Model).NumLevels - 1 do
          begin
               if TCustomPyramidAAMModel(frmAAMBuilder.Model).Models[i] is TAAMModel then
                    TAAMModel(TCustomPyramidAAMModel(frmAAMBuilder.Model).Models[i]).SetGlobalConstraints(10, 10, 10, 100);
          end;
     end;
end;

procedure TfrmAAMOptimize.OptimizeImage;
var p, optP : TDoubleMatrix;
    img : TDoubleMatrix;
    cnvType : TImageConvType;
    start, stop : int64;
begin
     p := TDoubleMatrix.Create(1, frmAAMBuilder.Model.NumEigVals + cNumPoseParams);
     p[0, p.Height - 1] := 1 + frmAAMBuilder.model.TextureContrast;
     p[0, p.Height - 2] := frmAAMBuilder.model.TextureBrightness;
     p[0, frmAAMBuilder.model.NumEigVals] := StrToInt( edScale.Text );
     p[0, frmAAMBuilder.model.NumEigVals + 1] := 0;
     p[0, frmAAMBuilder.model.NumEigVals + 2] := fX;
     p[0, frmAAMBuilder.model.NumEigVals + 3] := fY;

     FreeAndNil(fBmpInit);
     FreeAndNil(fBmpOpt);
     // build the initial image
     if frmAAMBuilder.Model.NumColorPlanes = 3
     then
         cnvType := ctRGB
     else
         cnvType := ctGrayScale;

     img := TMatrixImageConverter.ConvertImage(fBmp, cnvType);
     try
        frmAAMBuilder.Model.TextureFromPInImg(img, p);
        fBmpInit := TMatrixImageConverter.ConvertImage(img, cnvType);
     finally
            img.Free;
     end;

     // optimize image
     QueryPerformanceCounter(start);
     img := TMatrixImageConverter.ConvertImage(fBmp, cnvType);
     try
        optP := frmAAMBuilder.model.Optimize(img, p);
        QueryPerformanceCounter(stop);
        
        frmAAMBuilder.Model.TextureFromPInImg(img, optP);
        fBmpOpt := TMatrixImageConverter.ConvertImage(img, cnvType);
        optP.Free;
     finally
            img.Free;
     end;

     lblOptTime.Caption := Format('%.2fms', [(stop - start)*1000/fFreq]);
end;

procedure TfrmAAMOptimize.pbOptimizedPaint(Sender: TObject);
var offsetX, offsetY : integer;
begin
     if Assigned(fBmpOpt) then
     begin
          offsetX := Max(0, pbTestImage.Width div 2 - fBmp.Width div 2);
          offsetY := Max(0, pbTestImage.Height div 2 - fBmp.Height div 2);

          pbOptimized.Canvas.Draw(offsetX, offsetY, fBmpOpt);
     end;
end;

procedure TfrmAAMOptimize.pbTestImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var offsetX, offsetY : integer;
begin
     if Assigned(fBmp) and (ssLeft in shift) then
     begin
          offsetX := Max(0, pbTestImage.Width div 2 - fBmp.Width div 2);
          offsetY := Max(0, pbTestImage.Height div 2 - fBmp.Height div 2);

          fx := x - offsetX;
          fY := y - offsetY;

          OptimizeImage;

          pbTestImage.Invalidate;
          pbOptimized.Invalidate;
     end;
end;

procedure TfrmAAMOptimize.pbTestImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var offsetX, offsetY : integer;
begin
     if Assigned(fBmp) then
     begin
          offsetX := Max(0, pbTestImage.Width div 2 - fBmp.Width div 2);
          offsetY := Max(0, pbTestImage.Height div 2 - fBmp.Height div 2);

          fx := x - offsetX;
          fY := y - offsetY;

          OptimizeImage;

          pbTestImage.Invalidate;
          pbOptimized.Invalidate;
     end;
end;

procedure TfrmAAMOptimize.pbTestImagePaint(Sender: TObject);
var offsetX, offsetY : integer;
begin
     if not Assigned(fBmp) or not Assigned(frmAAMBuilder.Model) then
        exit;

     //
     offsetX := Max(0, pbTestImage.Width div 2 - fBmp.Width div 2);
     offsetY := Max(0, pbTestImage.Height div 2 - fBmp.Height div 2);

     if not Assigned(fBmpInit)
     then
         pbTestImage.Canvas.Draw(offsetX, offsetY, fBmp)
     else
         pbTestImage.Canvas.Draw(offsetX, offsetY, fBmpInit);
end;

end.
