program wsm;

uses
  Vcl.Forms,
  main in 'main.pas' {Form2},
  WSHtmlCommon in 'HTMLtools\WSHtmlCommon.pas',
  WSCommon in 'WScommon\WSCommon.pas',
  Vcl.Themes,
  Vcl.Styles,
  WSDBcommon in 'WSDB\WSDBcommon.pas',
  WSHelpers in 'WScommon\WSHelpers.pas',
  WSDBProductQueries in 'WSDB\WSDBProductQueries.pas',
  WSOCProduct in 'WSOC\WSOCProduct.pas',
  WSProductCommon in 'WScommon\WSProductCommon.pas',
  WSPCUnionMain in 'WSPCUnion\WSPCUnionMain.pas',
  WSShopCommon in 'WScommon\WSShopCommon.pas',
  WCRestApi in 'WCRestApi.pas',
  frm_pcu_list in 'frm_pcu_list.pas' {Form1},
  WSMMain in 'WSMMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
