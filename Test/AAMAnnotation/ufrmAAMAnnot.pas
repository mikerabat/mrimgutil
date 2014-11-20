unit ufrmAAMAnnot;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, Contnrs;

type
  TfrmAAMAnnotator = class(TForm)
    Panel1: TPanel;
    pbAAMAnnot: TPaintBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    openImgDialog: TOpenPictureDialog;
    Label1: TLabel;
    cboZoom: TComboBox;
    Label2: TLabel;
    lblNumPoints: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure pbAAMAnnotPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure pbAAMAnnotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure cboZoomExit(Sender: TObject);
    procedure cboZoomSelect(Sender: TObject);
    procedure pbAAMAnnotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbAAMAnnotMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private-Deklarationen }
    fPtidx : integer;
    fImg : TPicture;
    fFileName : string;
    fPts : TList;
    fZoomFact : integer;
    fDrawImg : TBitmap;

    procedure UpdateNumPts;
    procedure InitDrawImg;
    procedure ClearPoints;
  public
    { Public-Deklarationen }
  end;

var
  frmAAMAnnotator: TfrmAAMAnnotator;

implementation

uses Math, Jpeg, matrix, BinaryReaderWriter, BaseMathPersistence, ImageMatrixConv, ImageResize;

const cPtRadius = 3;

{$R *.dfm}

procedure TfrmAAMAnnotator.Button1Click(Sender: TObject);
var mtx : IMatrix;
    i : Integer;
    pt : PPoint;
begin
     if openImgDialog.Execute then
     begin
          FreeAndNil(fImg);
          fFileName := openImgDialog.FileName;
          fImg := TPicture.Create;
          fImg.LoadFromFile(fFileName);
          ClearPoints;
          
          InitDrawImg;

          // load eventual attached mtx file
          if FileExists(ChangeFileExt(fFileName, '.mtx')) then
          begin
               mtx := ReadObjFromFile(ChangeFileExt(fFileName, '.mtx')) as IMatrix;

               for i := 0 to mtx.Height - 1 do
               begin
                    new(pt);

                    pt^.X := round(mtx[0, i]);
                    pt^.Y := round(mtx[1, i]);
                    fPts.Add(pt);
               end;
          end;

          pbAAMAnnot.Invalidate;
     end;

     UpdateNumPts;
end;

procedure TfrmAAMAnnotator.Button2Click(Sender: TObject);
var mtx : IMatrix;
    fileName : string;
    i : Integer;
begin
     fileName := ChangeFileExt(fFileName, '.mtx');

     if FileExists(fileName) then
     begin
          if MessageDlg('File ' + fileName + ' already exitsts!'+#13+#10+'Overwrite?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
             exit;
     end;

     // create a matrix from the annotated points.
     mtx := TDoubleMatrix.Create(2, fPts.Count);
     for i := 0 to fPts.Count - 1 do
     begin
          mtx[0, i] := PPoint(fPts[i])^.X;
          mtx[1, i] := PPoint(fPts[i])^.Y;
     end;

     mtx.SaveToFile(fileName, TBinaryReaderWriter);
end;

procedure TfrmAAMAnnotator.Button3Click(Sender: TObject);
begin
     ClearPoints;
     pbAAMAnnot.Invalidate;
end;

procedure TfrmAAMAnnotator.Button4Click(Sender: TObject);
begin
     if fPts.Count > 0 then
     begin
          Dispose(PPoint(fPts[fPts.count - 1]));
          fPts.Delete(fPts.Count - 1);
          
          pbAAMAnnot.Invalidate;
     end;

     UpdateNumPts;
end;

procedure TfrmAAMAnnotator.cboZoomExit(Sender: TObject);
var zoomfact : integer;
begin
     if not TryStrToInt(cboZoom.Text, zoomFact) then
     begin
          cboZoom.Text := IntToStr(fZoomFact);
          exit;
     end;

     zoomFact := Min(500, Max(50, zoomFact));
     cboZoom.Text := IntToStr(fZoomFact);

     if not Assigned(fDrawImg) or (zoomFact <> fZoomFact) then
     begin
          fZoomFact := zoomfact;
          InitDrawImg;

          pbAAMAnnot.Invalidate;
     end;
end;

procedure TfrmAAMAnnotator.cboZoomSelect(Sender: TObject);
begin
     cboZoomExit(Sender);
     SetFocus;
end;

procedure TfrmAAMAnnotator.ClearPoints;
var
  counter: Integer;
begin
     for counter := 0 to fPts.Count - 1 do
         Dispose(PPoint(fPts[counter]));

     fPts.Clear;
     UpdateNumPts;
end;

procedure TfrmAAMAnnotator.FormCreate(Sender: TObject);
begin
     fZoomFact := 100;
     cboZoom.Text := '100';
     fImg := nil;
     fPts := TList.Create;
     fPtidx := -1;
end;

procedure TfrmAAMAnnotator.FormDestroy(Sender: TObject);
begin
     fImg.Free;
     fDrawImg.Free;
     fPts.Free;
end;

procedure TfrmAAMAnnotator.InitDrawImg;
var mtx1, mtx2 : TDoubleMatrix;
    bmp : TBitmap;
begin
     FreeAndNil(fDrawImg);

     mtx1 := nil;
     mtx2 := nil;
     try
        bmp := TBitmap.Create;
        try
           bmp.PixelFormat := pf24bit;
           bmp.SetSize(fImg.Width, fImg.Height);
           bmp.Canvas.Draw(0, 0, fImg.Graphic);

           mtx1 := TMatrixImageConverter.ConvertImage(bmp, ctRGB)
        finally
               bmp.Free;
        end;

        with TBilinearImageResize.Create(mtx1.Height div fImg.Height) do
        try
           mtx2 := Resize(mtx1, Round(fImg.Width*fZoomFact/100), Round(fImg.Height*fZoomFact/100));
        finally
               Free;
        end;

        fDrawImg := TMatrixImageConverter.ConvertImage(mtx2, ctRGB);
     finally
            mtx1.Free;
            mtx2.Free;
     end;
end;

procedure TfrmAAMAnnotator.pbAAMAnnotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pt : TPoint;
    offsetX, offsetY : integer;
    i : integer;
begin
     fPtIdx := -1;

     // check if a point is near x,y
     if not Assigned(fImg) or not Assigned(fDrawImg) then
        exit;

     offsetX := (pbAAMAnnot.Width - fDrawImg.Width) div 2;
     offsetY := (pbAAMAnnot.Height - fDrawImg.Height) div 2;
     if offsetX < 0 then
        offsetX := 0;
     if offsetY < 0 then
        offsetY := 0;


     pt.X := (X - offsetX)*100 div fZoomFact;
     pt.Y := (y - offsetY)*100 div fZoomFact;

     for i := 0 to fPts.Count - 1 do
     begin
          if PtInRect(Rect(PPoint(fPts[i])^.X - cPtRadius, PPoint(fPts[i])^.Y - cPtRadius,
                      PPoint(fPts[i])^.X + cPtRadius, PPoint(fPts[i])^.Y + cPtRadius), pt) then
          begin
               fPtidx := i;
               break;
          end;
     end;
end;

procedure TfrmAAMAnnotator.pbAAMAnnotMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var offsetX : integer;
    offsetY : integer;
    pPt : PPoint;
    rc : TRect;
    i : integer;
    pt : TPoint;
begin
     if not Assigned(fDrawImg) then
        exit;
     offsetX := (pbAAMAnnot.Width - fDrawImg.Width) div 2;
     offsetY := (pbAAMAnnot.Height - fDrawImg.Height) div 2;
     if offsetX < 0 then
        offsetX := 0;
     if offsetY < 0 then
        offsetY := 0;

     if fPtidx >= 0 then
     begin
          Cursor := crHandPoint;
          pPt := PPoint(fPts[fPtidx]);

          rc.Left := (pPt^.X*fZoomFact) div 100 + offsetX - cPtRadius - 5;
          rc.Top := (pPt^.Y*fZoomFact) div 100 + offsetY - cPtRadius - 5 + pbAAMAnnot.Top;
          rc.Right := rc.Left + 2*cPtRadius + 5 + pbAAMAnnot.Canvas.TextWidth('AAAA');
          rc.Bottom := rc.Top + 2*cPtRadius + 5 + 2*pbAAMAnnot.Canvas.TextHeight('AAAA');

          InvalidateRect(Handle, @rc, True);

          pPt^.X := (X - offsetX)*100 div fZoomFact;
          ppt^.Y := (Y - offsetY)*100 div fZoomFact;

          rc.Left := X - cPtRadius - 5;
          rc.Top := y - cPtRadius - 5 + pbAAMAnnot.Top;
          rc.Right := rc.Left + 2*cPtRadius + 5;
          rc.Bottom := rc.Top + 2*cPtRadius + 5;
          InvalidateRect(Handle, @rc, True);
     end
     else if shift = [] then
     begin

          // check if a point is within the rect
          pt.X := (X - offsetX)*100 div fZoomFact;
          pt.Y := (y - offsetY)*100 div fZoomFact;

          for i := 0 to fPts.Count - 1 do
          begin
               if PtInRect(Rect(PPoint(fPts[i])^.X - cPtRadius, PPoint(fPts[i])^.Y - cPtRadius,
                           PPoint(fPts[i])^.X + cPtRadius, PPoint(fPts[i])^.Y + cPtRadius), pt) then
               begin
                    Cursor := crHandPoint;
                    exit;
               end;
          end;

          Cursor := crDefault;
     end;
end;

procedure TfrmAAMAnnotator.pbAAMAnnotMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var offsetX : integer;
    offsetY : integer;
    pt : PPoint;
    rc : TRect;
begin
     if not Assigned(fImg) or not Assigned(fDrawImg) then
        exit;

     offsetX := (pbAAMAnnot.Width - fDrawImg.Width) div 2;
     offsetY := (pbAAMAnnot.Height - fDrawImg.Height) div 2;
     if offsetX < 0 then
        offsetX := 0;
     if offsetY < 0 then
        offsetY := 0;

     if fPtidx >= 0
     then
         pt := PPoint(fPts[fPtidx])
     else
         new(pt);
     pt^.X := (X - offsetX)*100 div fZoomFact;
     pt^.Y := (Y - offsetY)*100 div fZoomFact;

     if fPtidx = -1 then
        fPts.Add(pt);

     rc.Left := X - cPtRadius - 1;
     rc.Top := y - cPtRadius - 1 + pbAAMAnnot.Top;
     rc.Right := rc.Left + 2*cPtRadius + 2 + pbAAMAnnot.Canvas.TextWidth('AAAA');;
     rc.Bottom := rc.Top + 2*cPtRadius + 2 + 2*pbAAMAnnot.Canvas.TextHeight('AAAA');;
     InvalidateRect(Handle, @rc, False);
     fPtidx := -1;

     UpdateNumPts;
end;

procedure TfrmAAMAnnotator.pbAAMAnnotPaint(Sender: TObject);
var offsetx, offsetY : integer;
    counter : integer;
    pt : PPoint;
    drawPt : TPoint;
begin
     if not Assigned(fImg) or not Assigned(fDrawImg) then
        exit;

     offsetX := (pbAAMAnnot.Width - fDrawImg.Width) div 2;
     offsetY := (pbAAMAnnot.Height - fDrawImg.Height) div 2;
     if offsetX < 0 then
        offsetX := 0;
     if offsetY < 0 then
        offsetY := 0;
     pbAAMAnnot.Canvas.Draw(offsetX, offsetY, fDrawImg);

     pbAAMAnnot.Canvas.Brush.Color := clLtGray;
     pbAAMAnnot.Canvas.Pen.Color := clDkGray;

     pbAAMAnnot.Canvas.Font.Size := 7;

     for counter := 0 to fPts.Count - 1 do
     begin
          pt := PPoint(fPts[counter]);
          drawPt.X := (pt^.X*fZoomFact div 100) + offsetX;
          drawPt.Y := (pt^.Y*fZoomFact div 100) + offsetY;
          pbAAMAnnot.Canvas.Ellipse(drawPt.X - cPtRadius, drawPt.Y - cPtRadius,
                                    drawPt.X + cPtRadius, drawPt.Y + cPtRadius);

          pbAAMAnnot.Canvas.Brush.Style := bsClear;
          pbAAMAnnot.Canvas.TextOut(drawPt.x + cPtRadius + 1, drawPt.y + pbAAMAnnot.Canvas.Font.Height div 2, IntToStr(counter + 1));
          pbAAMAnnot.Canvas.Brush.Style := bsSolid;
     end;
end;

procedure TfrmAAMAnnotator.UpdateNumPts;
begin
     lblNumPoints.Caption := IntToStr(fPts.Count);
end;

end.
