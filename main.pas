unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WSDBCommon, Vcl.StdCtrls, WSHtmlCommon, WSHelpers,
  Vcl.Grids, WSCommon, WSPCUnionMain, Vcl.ExtCtrls, WCRestApi, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, Data.Bind.Components,
  Data.Bind.ObjectScope, WSMMain;

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
    e_cons_key: TEdit;
    e_secret: TEdit;
    e_base_url: TEdit;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
  ht: TWSHtml;
  db : Tdatabase;
  pcu: TWSPCUnion;
  rest: TWCProductManager;
  FWSMain: TWSMMain;
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
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
 // Image1.Picture.Assign(pcu.GetProductByIndex(0).ProductImage);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  ht.PostTest;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  Manager: TWCProductManager;
  prod_in, prod_out: TWCProduct;
begin
  Manager := TWCProductManager.Create(e_cons_key.Text, e_secret.Text, e_base_url.Text);
  try

  finally
    Memo1.Text := Manager.LastResponse;
  end;
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  Manager: TWCProductManager;
  prod_in, prod_out: TWCProduct;
begin
  Manager := TWCProductManager.Create(e_cons_key.Text, e_secret.Text, e_base_url.Text);
  try
 //   Manager.CreateProduct;
   // Manager.DownloadCategories;
  finally
    Memo1.Lines.Add(Manager.LastResponse);
  end;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  FWSMain.UploadProdFormWSPCU;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FWSMain := TWSMMain.Create(e_cons_key.Text, e_secret.Text, e_base_url.Text);
  pcu := TWSPCUnion.Create;
  pcu.OnLog := GetLog;
  ht := TWSHtml.create;
  ht.OnLog := GetLog;
  FWSMain.OnLog := GetLog;
end;

procedure TForm2.GetLog(Amsg: string; Atype: TWSmsgtype);
begin
  Memo1.Lines.Add(Amsg);
end;

end.
