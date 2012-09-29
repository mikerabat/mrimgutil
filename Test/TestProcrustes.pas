unit TestProcrustes;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Registration, Procrustes, RigidTransformation, Matrix,
  BaseTransformationTest;

// ##################################################
// #### Test the procrustres method
type
 TestTProcrustes = class(TBaseTransformationCase)
 published
   procedure TestCreatePointMapping;
 end;

implementation

uses SysUtils, Classes, PtsDefinitions;

procedure TestTProcrustes.TestCreatePointMapping;
var toPts: TPtsMappingObj;
    fromPts: TPtsMappingObj;
    pts : TDoubleMatrix;
    procrustes : TProcrustes;
    mappedPts : TPtsMappingObj;
    i : integer;
begin
     pts := CreatePts(1, 0, 0, 0);

     with TStringList.Create do
     try
        decimalseparator := '.';
        for i := 0 to pts.Height - 1 do
            Add(Format('%.3f,%.3f', [pts[0,i], pts[1,i]]));

        SaveToFile('D:\pts1.txt');//,TEncoding.ASCII);
     finally
            Free;
     end;
     toPts := TPtsMappingObj.Create(pts);
     pts.Free;
     pts := CreatePts(2.5, 0.3, 10, 10);
     fromPts := TPtsMappingObj.Create(pts);


     with TStringList.Create do
     try
        decimalseparator := '.';
        for i := 0 to pts.Height - 1 do
            Add(Format('%.3f,%.3f', [pts[0,i], pts[1,i]]));

        SaveToFile('D:\pts2.txt');//,TEncoding.ASCII);
     finally
            Free;
     end;
     pts.Free;



     mappedPts := nil;
     procrustes := TProcrustes.Create;
     try
        procrustes.CreatePointMapping(fromPts, toPts);
        mappedPts := procrustes.MapPoints(fromPts);

        Check(ComparePts(toPts, mappedPts), 'Procrustes failed.');
     finally
            procrustes.Free;
            mappedPts.Free;
            toPts.Free;
            fromPts.Free;
     end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTProcrustes.Suite);
end.

