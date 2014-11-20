program AAMAnnotator;

uses
  Forms,
  ufrmAAMAnnot in 'ufrmAAMAnnot.pas' {frmAAMAnnotator};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAAMAnnotator, frmAAMAnnotator);
  Application.Run;
end.
