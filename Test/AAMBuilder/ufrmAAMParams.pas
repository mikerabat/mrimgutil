unit ufrmAAMParams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmModelParams = class(TForm)
    pnlBaseModelParams: TPanel;
    Panel1: TPanel;
    imgMeanImg: TImage;
    Label9: TLabel;
    Panel2: TPanel;
    imgDisturbed: TImage;
    Panel3: TPanel;
    pnlParams: TPanel;
    lblDisturbanceParams: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmModelParams: TfrmModelParams;

implementation

{$R *.dfm}

end.
