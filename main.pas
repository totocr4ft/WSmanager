unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WSDBCommon, Vcl.StdCtrls, WSHtmlCommon, WSHelpers,
  Vcl.Grids, WSCommon, WSPCUnionMain, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    Button2: TButton;
    Image1: TImage;
    Edit1: TEdit;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
  ht: TWSHtml;
  db : Tdatabase;
  pcu: TWSPCUnion;
  procedure GetLog(Amsg:string; Atype: TWSmsgtype);
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var Res: Tresultset;
begin
  pcu.ReadCSV;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  pcu.FillProductsSpec(0);
  pcu.FillProductsImage(0);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  Image1.Picture.Assign(pcu.GetProductByIndex(0).ProductImage);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  ht.PostTest;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  pcu := TWSPCUnion.Create;
  pcu.OnLog := GetLog;
  ht := TWSHtml.create;
  ht.OnLog := GetLog;
end;

procedure TForm2.GetLog(Amsg: string; Atype: TWSmsgtype);
begin
  Memo1.Lines.Add(Amsg);
end;

end.
