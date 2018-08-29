program wsm;

uses
  Vcl.Forms,
  main in 'main.pas' {Form2},
  WSHtmlCommon in 'HTMLtools\WSHtmlCommon.pas',
  WSCommon in 'WScommon\WSCommon.pas',
  Vcl.Themes,
  Vcl.Styles,
  WSDBcommon in 'WSDB\WSDBcommon.pas',
  WSHelpers in 'WScommon\WSHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Auric');
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
