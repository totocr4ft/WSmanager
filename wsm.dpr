program wsm;

uses
  Vcl.Forms,
  main in 'main.pas' {Form2},
  WSHtmlCommon in 'HTMLtools\WSHtmlCommon.pas',
  WSCommon in 'WScommon\WSCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
