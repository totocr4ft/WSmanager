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
    function GetLangID: integer;
  public
    constructor create(AID: integer = 0; ALangID: integer = 0; AData: TWSProductData = nil);

    property ID: integer read GetID;
    property LanguageID: integer read GetLangID;
    property ProductData: TWSProductData read FProdData;
  end;



implementation

{ TWSOCProduct }

constructor TWSOCProduct.create(AID, ALangID: integer; AData: TWSProductData);
begin
  FID := AID;
  FLanguageID := ALangID;
  FProdData := AData;
end;

function TWSOCProduct.GetData: TWSProductData;
begin
  Result := FProdData;
end;

function TWSOCProduct.GetID: Integer;
begin
  Result := FID;
end;


function TWSOCProduct.GetLangID: integer;
begin
  Result := FLanguageID;
end;

end.
