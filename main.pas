unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WSDBCommon, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
  private
    { Private declarations }
  public
  db : Tdatabase;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
