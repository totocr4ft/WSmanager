unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WSDBCommon, Vcl.StdCtrls, WSHtmlCommon, WSHelpers,
  Vcl.Grids;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
  ht: TWSHtml;
  db : Tdatabase;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var Res: Tresultset;
begin
  ht := TWSHtml.create;
  Res := ht.GetCSV('https://shop.pcunion.hu/arlista/arlista2.php?email=garamszegit@gmail.com&pass=c2c0b999d197ee4afefe29ee62b2e7d9db31e658&groupid=1');
  ResultsetToStringgrid(StringGrid1, Res);
end;

end.
