unit WSOCProduct;

interface
uses System.SysUtils ,Classes, WSProductCommon;

  type

  TWSOCProduct = class(TInterfacedObject, IWSProduct)
  private
    FProdData: TWSProductData;
    FLanguageID: integer;
    FID: integer;
    function GetID: Integer;
    function GetData: TWSProductData;
  public
    constructor create();


  end;



implementation

{ TWSOCProduct }

function TWSOCProduct.GetData: TWSProductData;
begin

end;

function TWSOCProduct.GetID: Integer;
begin

end;


end.
