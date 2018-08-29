unit WSShopCommon;

interface
uses Classes, System.SysUtils, System.Generics.Collections, WSOCPRoduct, WSProductCommon, WSCommon;

  type
   TWSProducts = TDictionary<integer, IWSProduct>;

   TWSShop = class(TWSCommon)
   private
     FShopName: string;
     FShopMainURL: string;
     FProducts: TWSProducts;
     function GetProducts: TWSProducts;
     function GetShopURL: string;
     function GetShopName: string;
   public
     property ShopName: string read GetShopName;
     property ShopURL: string read GetShopURL;
     property Products: TWSProducts read GetProducts;
   end;

implementation

{ TWSShop }

function TWSShop.GetProducts: TWSProducts;
begin
  Result := FProducts;
end;

function TWSShop.GetShopName: string;
begin
  Result := FShopName;
end;

function TWSShop.GetShopURL: string;
begin
  Result := FShopMainURL;
end;

end.
