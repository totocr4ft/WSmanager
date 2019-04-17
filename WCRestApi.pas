unit WCRestApi;

interface

uses
  System.SysUtils, System.JSON,  REST.Client, REST.Types, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Authenticator.OAuth, System.Generics.Collections, IdHTTP, System.Classes, REST.HttpClient, REST.Utils,
  WSCommon;

type
  TWCStringArray = array of string;
  TWCIntArray = array of Integer;

  TWCProductTag = record
    id: Integer;
    name: string;
    slug: string;
  end;



  TWCProductDownload = record
    id: string;
    name: string;
    file_: string;
  end;

  TWCProductImage = record
    id: Integer;
    date_created: TDateTime;
    date_created_gmt: TDateTime;
    date_modified: TDateTime;
    date_modified_gmt: TDateTime;
    src: string;
    name: string;
    alt: string;
  end;

  TWCProductDefaultAttribute = record
    id: Integer;
    name: string;
    option: string;
  end;

  TWCPRoductDimension = record
    length: string;
    width: string;
    height: string;
  end;

  TWCProductAttribute = record
    id: Integer;
    name: string;
    position: string;
    visible: Boolean;
    variation: Boolean;
    options: TWCStringArray;
  end;

  TWCProductMetadata = record
    id: Integer;
    key: string;
    value: string;
  end;

  TWCProductCategory = record
    id: Integer;
    name: string;
    slug: string;
    parent: Integer;
    description: string;
    display: string;
    image: TWCProductImage;
    menu_order: Integer;
    count: integer;
  end;

  TWCProductDimensions = TList<TWCPRoductDimension>;
  TWCProductDownloads = TDictionary<string , TWCProductDownload>;
  TWCProductMetadatas = TDictionary<Integer, TWCProductMetadata>;
  TWCProductAttributes = TDictionary<Integer, TWCProductAttribute>;
  TWCProductDefaultAttributes = TDictionary<Integer, TWCProductDefaultAttribute>;
  TWCProductTags = TDictionary<Integer, TWCProductTag>;
  TWCProductCategories = TDictionary<Integer, TWCProductCategory>;
  TWCProductImages = TDictionary<Integer, TWCProductImage>;
  TWCategories = TDictionary<integer , TWCProductCategory>;

  TWCProduct = record
    id: integer;
    name: string;
    slug: string;
    permalink: string;
    date_created: TDateTime;
    date_created_gmt: TDateTime;
    date_modified: TDateTime;
    date_modified_gmt: TDaTetime;
    type_: string;
    status: string;
    featured: string;
    catalog_visibility: string;
    description: string;
    short_description: string;
    sku: string;
    price: string;
    regular_price: string;
    sale_price: string;
    date_on_sale_from: string;
    date_on_sale_to: string;
    date_on_sale_to_gmt: string;
    price_html: string;
    on_sale: Boolean;
    purchasable: Boolean;
    total_sales: integer;
    virtual_: Boolean;
    downloadable: Boolean;
    downloads: TWCProductDownloads;
    download_limit: integer;
    download_expiry: integer;
    external_url: string;
    button_text: string;
    tax_status: string;
    tax_class: string;
    manage_stock: Boolean;
    stock_quantity: integer;
    stock_status: string;
    backorders: string;
    backorders_allowed: Boolean;
    backordered: Boolean;
    sold_individually: Boolean;
    weight: string;
    dimensions: TWCProductDimensions;
    shipping_required: Boolean;
    shipping_taxable: Boolean;
    shipping_class: string;
    shipping_class_id: Integer;
    reviews_allowed: Boolean;
    average_rating: string;
    rating_count: Integer;
    related_ids: TWCIntArray;
    upsell_ids: TWCIntArray;
    cross_sell_ids: TWCIntArray;
    parent_id: integer;
    purchase_note: string;
    categories: TWCProductCategories;
    tags: TWCProductTags;
    images: TWCProductImages;
    attributes: TWCProductAttributes;
    default_attributes: TWCProductDefaultAttributes;
    variations: TWCIntArray;
    grouped_products: TWCIntArray;
    menu_order: integer;
    meta_data: TWCProductMetadatas;
  end;
  TWCProducts = TDictionary<integer, TWCProduct>;

  TWCProductManager = class(TWSCommon)
  private
    FApiConsKey: string;
    FApiConsSecret: string;
    FApiBaseUrl: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FlastResponse: string;
    FOAuth1Authenticator: TOAuth1Authenticator;
    FProducts: TWCProducts;
    FCategories: TWCProductCategories;
    FLastRequest: string;
    FLastSignature: string;
    procedure AddProduct(AJson: TJSONObject); overload;
    procedure AddProduct(AProduct: TWCProduct); overload;
    procedure AddCategory(ACategory: TWCProductCategory); overload;
    procedure AddCategory(Ajson: TJSONObject); overload;
    function JsonToProduct(AJson: TJsonobject): TWCProduct;
    function JsonToCategory(AJson: TJsonobject): TWCProductCategory;
    // JSON READEREK
    function GetProductDownloads(AJson: TJsonValue): TWCProductDownloads;
    function GetProductDimensions(AJson: TJsonValue): TWCProductDimensions;
    function GetProductCategories(AJson: TJsonValue): TWCProductCategories;
    function GetProductTags(AJson: TJsonValue): TWCProductTags;
    function GetProductImages(AJson: TJsonValue): TWCProductImages;
    function GetProductDefAttributes(AJson: TJsonValue): TWCProductDefaultAttributes;
    function GetProductAttributes(AJson: TJsonValue): TWCProductAttributes;
    function GetProductMetaData(AJson: TJsonValue): TWCProductMetadatas;
    function GetIDArray(AJson: TJSONValue): TWCIntArray;
    function GetStringArray(AJson: TJSONValue): TWCStringArray;
    procedure GetCategories;
    procedure OAuth;
    procedure Prepare;
  public
    constructor Create(AApiConsKey, AApiConsSecret, ABaseUrl: string);
    function CreateProduct(AProduct: TWCProduct):TWCProduct;
    function ModProduct(Id: integer; AProduct: TWCProduct): TWCProduct;
    procedure DownloadCategories;
    procedure DownloadProducts;
    function LastResponse: string;
    function Products: TWCProducts;
    procedure DeleteAllProduct;
    procedure DeleteProduct(AProdId: Integer);
  end;

implementation
uses main;
{ TWCProductManager }

procedure TWCProductManager.AddProduct(AProduct: TWCProduct);
begin
  FProducts.Add(AProduct.id, AProduct);
end;

constructor TWCProductManager.Create(AApiConsKey, AApiConsSecret, ABaseUrl: string);
begin
  FApiConsKey    := AApiConsKey;
  FApiConsSecret := AApiConsSecret;
  FApiBaseUrl    := ABaseUrl;
  FRESTClient    := TRESTClient.Create(ABaseUrl);
  FRESTRequest   := TRESTRequest.Create(nil);
  FRESTResponse  := TRESTResponse.Create(nil);
  FCategories    := TWCProductCategories.Create();
  FOAuth1Authenticator := TOAuth1Authenticator.Create(nil);
  FProducts := TDictionary<Integer, TWCProduct>.Create();
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

procedure TWCProductManager.DeleteAllProduct;
var
  LProduct: TWCProduct;
  I: Integer;
begin
  for I := 0 to FProducts.Count - 1 do
  begin
    try
      DeleteProduct( FProducts[i].id );
    except

    end;
  end;
end;

procedure TWCProductManager.DeleteProduct(AProdId: Integer);
begin
  Prepare;
  FRESTClient.BaseURL := FApiBaseUrl + '/wp-json/wc/v3/Products/'+ IntToStr(AProdId);
  FRESTRequest.Method := REST.Types.rmDELETE;
  OAuth;
  FRESTRequest.Execute;
end;

procedure TWCProductManager.OAuth;
var
  LNonce: string;
  LTimestamp: string;
begin
  if(Assigned(FOAuth1Authenticator)) then
    FOAuth1Authenticator.Free;
  FOAuth1Authenticator := TOAuth1Authenticator.Create(nil);

  LNonce := FOAuth1Authenticator.Nonce;
  FOAuth1Authenticator.ConsumerSecret := FApiConsSecret;
  FRESTRequest.AddParameter('oauth_consumer_key', FApiConsKey);
  FRESTRequest.AddParameter('oauth_consumer_secret', FApiConsSecret);
  FLastSignature := FOAuth1Authenticator.SigningClass.BuildSignature(FRESTRequest, FOAuth1Authenticator);
  LTimestamp := FOAuth1Authenticator.timeStamp.DeQuotedString;
  FRESTRequest.AddParameter('oauth_timestamp', LTimestamp);
  FRESTRequest.AddParameter('oauth_nonce', LNonce);
  FRESTRequest.AddParameter('oauth_signature', FLastSignature );
  FRESTRequest.AddParameter('oauth_signature_method', 'HMAC-SHA1');
  FRESTRequest.AddParameter('oauth_version', '1.0');
end;

function TWCProductManager.LastResponse: string;
begin
  Result := FlastResponse;
end;

//**************  JSON OBJECT -> TWCPRODUCT *************  //

procedure TWCProductManager.AddProduct(AJson: TJSONObject);
var
  LProduct: TWCProduct;
begin
  LProduct := JsonToProduct(AJson);
  FProducts.Add(FProducts.Count, JsonToProduct(AJson));
end;

function TWCProductManager.JsonToProduct(AJson: TJsonobject): TWCProduct;
var I: Integer;
    LProduct: TWCProduct;
begin
  LProduct.id   := AJson.GetValue<Integer>('id');
  LProduct.name := AJson.GetValue<string>('name');
  LProduct.slug := AJson.GetValue<string>('slug');
  LProduct.permalink := AJson.GetValue<string>('permalink');
  LProduct.date_created := AJson.GetValue<TDateTime>('date_created');
  LProduct.date_created_gmt := AJson.GetValue<TDateTime>('date_created_gmt');
  LProduct.date_modified := AJson.GetValue<TDateTime>('date_modified');
  LProduct.date_modified_gmt := AJson.GetValue<TDateTime>('date_modified_gmt');
  LProduct.type_ := AJson.GetValue<string>('type');
  LProduct.status := AJson.GetValue<string>('status');
  LProduct.featured := AJson.GetValue<string>('featured');
  LProduct.catalog_visibility := AJson.GetValue<string>('catalog_visibility');
  LProduct.description := AJson.GetValue<string>('description');
  LProduct.sku := AJson.GetValue<string>('sku');
  LProduct.price := AJson.GetValue<string>('price');
  LProduct.regular_price := AJson.GetValue<string>('regular_price');
  LProduct.sale_price := AJson.GetValue<string>('sale_price');
  LProduct.date_on_sale_from := AJson.GetValue<string>('date_on_sale_from');
  LProduct.date_on_sale_to := AJson.GetValue<string>('date_on_sale_to');
  LProduct.date_on_sale_to_gmt := AJson.GetValue<string>('date_on_sale_to_gmt');
  LProduct.price_html := AJson.GetValue<string>('price_html');
  LProduct.on_sale := AJson.GetValue<Boolean>('on_sale');
  LProduct.purchasable := AJson.GetValue<Boolean>('purchasable');
  LProduct.total_sales := AJson.GetValue<Integer>('total_sales');
  LProduct.virtual_ := AJson.GetValue<Boolean>('virtual');
  LProduct.downloadable := AJson.GetValue<Boolean>('downloadable');
  LProduct.downloads := GetProductDownloads(AJson.GetValue('downloads'));
  LProduct.download_limit := AJson.GetValue<Integer>('download_limit');
  LProduct.download_expiry := AJson.GetValue<Integer>('download_expiry');
  LProduct.external_url := AJson.GetValue<string>('external_url');
  LProduct.button_text := AJson.GetValue<string>('button_text');
  LProduct.tax_status := AJson.GetValue<string>('tax_status');
  LProduct.tax_class := AJson.GetValue<string>('tax_class');
  LProduct.manage_stock := AJson.GetValue<Boolean>('manage_stock');
  if LProduct.manage_stock then
    LProduct.stock_quantity := AJson.GetValue<Integer>('stock_quantity')
  else
    LProduct.stock_quantity := 0;

  LProduct.stock_status := AJson.GetValue<string>('stock_status');
  LProduct.backorders := AJson.GetValue<string>('backorders');
  LProduct.backorders_allowed := AJson.GetValue<Boolean>('backorders_allowed');
  LProduct.backordered := AJson.GetValue<Boolean>('backordered');
  LProduct.sold_individually := AJson.GetValue<Boolean>('sold_individually');
  LProduct.weight := AJson.GetValue<string>('weight');
  LProduct.dimensions := GetProductDimensions(AJson.GetValue('dimensions'));
  LProduct.shipping_required := AJson.GetValue<Boolean>('shipping_required');
  LProduct.shipping_taxable := AJson.GetValue<Boolean>('shipping_taxable');
  LProduct.shipping_class := AJson.GetValue<string>('shipping_class');
  LProduct.shipping_class_id := AJson.GetValue<Integer>('shipping_class_id');
  LProduct.reviews_allowed := AJson.GetValue<Boolean>('reviews_allowed');
  LProduct.average_rating := AJson.GetValue<string>('average_rating');
  LProduct.related_ids := GetIDArray(AJson.GetValue('related_ids'));
  LProduct.upsell_ids := GetIDArray(AJson.GetValue('upsell_ids'));
  LProduct.cross_sell_ids := GetIDArray(AJson.GetValue('cross_sell_ids'));
  LProduct.parent_id := AJson.GetValue<Integer>('parent_id');
  LProduct.categories := GetProductCategories(AJson.GetValue('categories'));
  LProduct.tags := GetProductTags(AJson.GetValue('tags'));
  LProduct.images := GetProductImages(AJson.GetValue('images'));
  LProduct.attributes := GetProductAttributes(AJson.GetValue('attributes'));
  LProduct.default_attributes := GetProductDefAttributes(AJson.GetValue('default_attributes'));
  LProduct.variations := GetIDArray(AJson.GetValue('variations'));
  LProduct.grouped_products := GetIDArray(AJson.GetValue('grouped_products'));
  LProduct.menu_order := AJson.GetValue<Integer>('menu_order');
  LProduct.meta_data := GetProductMetaData(AJson.GetValue('meta_data'));
  Result := LProduct;
end;

procedure TWCProductManager.GetCategories;
begin

end;

function TWCProductManager.GetIDArray(AJson: TJSONValue): TWCIntArray;
var
  LObj: TJSONValue;
begin
   SetLength(Result, 0);
   for LObj in (AJson as TJSonArray) do
   begin
     SetLength(Result, Length(Result) + 1);
     Result[high(Result)] := StrToInt( LObj.Value );
   end;
end;

function TWCProductManager.GetStringArray(AJson: TJSONValue): TWCStringArray;
var
  LObj: TJSONValue;
begin
   SetLength(Result, 0);
   for LObj in (AJson as TJSonArray) do
   begin
     SetLength(Result, Length(Result) + 1);
     Result[high(Result)] := LObj.Value;
   end;
end;

function TWCProductManager.GetProductCategories(
  AJson: TJsonValue): TWCProductCategories;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductCategory;
begin
  Result := TWCProductCategories.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.slug := (Litem as TJSONObject).GetValue<string>('slug');
    Result.Add(LDl.id ,LDl);
  end;
end;

function TWCProductManager.GetProductDimensions( AJson: TJsonValue): TWCProductDimensions;
var
  LDl: TWCPRoductDimension;
begin
  Result := TWCPRoductDimensions.Create();
  LDl.length := (AJson as TJSONObject).GetValue<string>('length');
  LDl.width := (AJson as TJSONObject).GetValue<string>('width');
  LDl.height := (AJson as TJSONObject).GetValue<string>('height');
  Result.Add(LDl);
end;

function TWCProductManager.GetProductDownloads(AJson: TJsonValue): TWCProductDownloads;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductDownload;
begin
  Result := TWCProductDownloads.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<string>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.file_ := (Litem as TJSONObject).GetValue<string>('file');
    Result.Add(LDl.id, LDl);
  end;
end;

function TWCProductManager.GetProductImages(AJson: TJsonValue): TWCProductImages;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductImage;
begin
  Result := TWCProductImages.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.src := (Litem as TJSONObject).GetValue<string>('src');
    LDl.alt := (Litem as TJSONObject).GetValue<string>('src');
    Result.Add(LDl.id, LDl);
  end;
end;

function TWCProductManager.GetProductTags(AJson: TJsonValue): TWCProductTags;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductTag;
begin
  Result := TWCProductTags.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.slug := (Litem as TJSONObject).GetValue<string>('slug');
    Result.Add(LDl.id, LDl);
  end;
end;

function TWCProductManager.GetProductDefAttributes(AJson: TJsonValue): TWCProductDefaultAttributes;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductDefaultAttribute;
begin
  Result := TWCProductDefaultAttributes.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.option := (Litem as TJSONObject).GetValue<string>('option');
    Result.Add(LDl.id, LDl);
  end;
end;

function TWCProductManager.GetProductAttributes(AJson: TJsonValue): TWCProductAttributes;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductAttribute;
begin
  Result := TWCProductAttributes.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.name := (Litem as TJSONObject).GetValue<string>('name');
    LDl.position := (Litem as TJSONObject).GetValue<string>('position');
    LDl.visible := (Litem as TJSONObject).GetValue<Boolean>('visible');
    LDl.variation := (Litem as TJSONObject).GetValue<Boolean>('variation');
    LDl.options := GetStringArray((Litem as TJSONObject).GetValue('options'));
    Result.Add(LDl.id, LDl);
  end;
end;

function TWCProductManager.GetProductMetaData(AJson: TJsonValue): TWCProductMetadatas;
var
  LArray: TJSONArray;
  Litem: TJSONValue;
  LDl: TWCProductMetadata;
begin
  Result := TWCProductMetadatas.Create();
  for Litem in (AJson as TJSONArray) do
  begin
    LDl.id := (Litem as TJSONObject).GetValue<Integer>('id');
    LDl.key := (Litem as TJSONObject).GetValue<string>('key');
    LDl.value := (Litem as TJSONObject).GetValue<string>('value');
    Result.Add(LDl.id, LDl);
  end;
end;

procedure TWCProductManager.DownloadProducts;
var
 LMainArray: TJSONArray;
 LMainValue: TJSONValue;
 LItem     : TJSONValue;
 LJsonValue: TJSONValue;
 LJsonObj: TJSONObject;
 Ltimestamp, Lnonce: string;
begin
  FProducts.Clear;
  Prepare;
  FRESTClient.BaseURL := FApiBaseUrl + '/wp-json/wc/v3/products';
  FRESTRequest.Method := REST.Types.rmGET;
    if(Assigned(FOAuth1Authenticator)) then
    FOAuth1Authenticator.Free;
  FOAuth1Authenticator := TOAuth1Authenticator.Create(nil);

  LNonce := FOAuth1Authenticator.Nonce;
  FOAuth1Authenticator.ConsumerSecret := FApiConsSecret;
  FRESTRequest.AddParameter('oauth_consumer_key', FApiConsKey);
  FRESTRequest.AddParameter('oauth_consumer_secret', FApiConsSecret);
  FRESTRequest.AddParameter('per_page', '100');
  FLastSignature := FOAuth1Authenticator.SigningClass.BuildSignature(FRESTRequest, FOAuth1Authenticator);
  LTimestamp := FOAuth1Authenticator.timeStamp.DeQuotedString;
  FRESTRequest.AddParameter('oauth_timestamp', LTimestamp);
  FRESTRequest.AddParameter('oauth_nonce', LNonce);
  FRESTRequest.AddParameter('oauth_signature', FLastSignature );
  FRESTRequest.AddParameter('oauth_signature_method', 'HMAC-SHA1');
  FRESTRequest.AddParameter('oauth_version', '1.0');
  FRESTRequest.Execute;
  FlastResponse := FRESTResponse.JSONText;
  LMainValue := FRESTResponse.JSONValue;
  LMainArray := LMainValue as TJSONArray;
  for LJsonValue in LMainArray do
   begin
      AddProduct(LJsonValue as TJSONObject);
   end;
end;

//**************  JSON OBJECT -> TWCCategory *************  //

function TWCProductManager.JsonToCategory(AJson: TJSONObject): TWCProductCategory;
var I: Integer;
    LCategory: TWCProductCategory;
begin
  LCategory.id   := AJson.GetValue<Integer>('id');
  LCategory.name := AJson.GetValue<string>('name');
  LCategory.slug := AJson.GetValue<string>('slug');
  LCategory.parent := AJson.GetValue<integer>('parent');
  LCategory.description := AJson.GetValue<string>('description');
  LCategory.display := AJson.GetValue<string>('display');
//  LCategory.image := AJson.GetValue<string>('slug');
  LCategory.menu_order := AJson.GetValue<integer>('menu_order');
  LCategory.count := AJson.GetValue<integer>('count');
  Result := Lcategory;
end;


procedure TWCProductManager.AddCategory(ACategory: TWCProductCategory);
begin
  FCategories.Add(ACategory.id, ACategory);
end;

procedure TWCProductManager.AddCategory(Ajson: TJSONObject);
var
  LCategeroy: TWCProductCategory;
begin
  LCategeroy := JsonToCategory(AJson);
  FCategories.Add(LCategeroy.id, LCategeroy);
end;

procedure TWCProductManager.DownloadCategories;
var
 LMainArray: TJSONArray;
 LMainValue: TJSONValue;
 LItem     : TJSONValue;
 LJsonValue: TJSONValue;
 LJsonObj: TJSONObject;
begin
  FCategories.Clear;
  Prepare;
  FRESTClient.BaseURL := FApiBaseUrl + '/wp-json/wc/v3/Products/categories';
  FRESTRequest.Method := REST.Types.rmGET;
  OAuth;
  FRESTRequest.Execute;
  FlastResponse := FRESTResponse.JSONText;
  LMainValue := FRESTResponse.JSONValue;
  LMainArray := LMainValue as TJSONArray;
  for LJsonValue in LMainArray do
   begin
      AddCategory(LJsonValue as TJSONObject);
   end;
end;

//************************************************************

procedure TWCProductManager.Prepare;
begin
  if Assigned(FRESTRequest) then
    FRESTRequest.Free;

  if(Assigned(FOAuth1Authenticator)) then
    FOAuth1Authenticator.Free;

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest.Response := FRESTResponse;
  FOAuth1Authenticator := TOAuth1Authenticator.Create(nil);
end;

function TWCProductManager.CreateProduct(AProduct: TWCProduct): TWCProduct;
var
  LSignature: string;
  LNonce: string;
  LTimestamp: string;
  s: string;
begin
  Prepare;
  LNonce := FOAuth1Authenticator.Nonce;
  FRESTClient.BaseURL := FApiBaseUrl + '/wp-json/wc/v3/Products';
  FRESTRequest.Method := REST.Types.rmPost;
  FOAuth1Authenticator.ConsumerSecret := FApiConsSecret;
  FRESTRequest.AddParameter('oauth_consumer_key', FApiConsKey);
  FRESTRequest.AddParameter('oauth_consumer_secret', FApiConsSecret);
  FRESTRequest.AddParameter('name', AProduct.name);
  FRESTRequest.AddParameter('type', 'simple');
  FRESTRequest.AddParameter('regular_price', AProduct.regular_price);
  FRESTRequest.AddParameter('description', AProduct.description);
  FRESTRequest.AddParameter('short_description', '');
  if AProduct.images.Count = 1 then
  begin
   if Copy( AProduct.images[0].src, 1, 4) = 'http' then
      FRESTRequest.AddParameter('images[0][src]', AProduct.images[0].src);
  end;
  FLastSignature := FOAuth1Authenticator.SigningClass.BuildSignature(FRESTRequest, FOAuth1Authenticator);
  LTimestamp := FOAuth1Authenticator.timeStamp.DeQuotedString;
  FRESTRequest.AddParameter('oauth_timestamp', LTimestamp);
  FRESTRequest.AddParameter('oauth_nonce', LNonce);
  FRESTRequest.AddParameter('oauth_signature', FLastSignature );
  FRESTRequest.AddParameter('oauth_signature_method', 'HMAC-SHA1');
  FRESTRequest.AddParameter('oauth_version', '1.0');
  FRESTRequest.Execute;
  FLastResponse := FRESTResponse.Content;
end;

function TWCProductManager.ModProduct(Id: integer; AProduct: TWCProduct): TWCProduct;
var
  LSignature: string;
  LNonce: string;
  LTimestamp: string;
  s: string;
begin
  FRESTRequest.Params.Clear;
  LNonce := FOAuth1Authenticator.Nonce;
  FOAuth1Authenticator.ConsumerSecret := FApiConsSecret;

  FRESTClient.BaseURL := FApiBaseUrl + '/wp-json/wc/v3/Products/'+IntToStr(Id);
  FRESTRequest.Method := REST.Types.rmPost;


  FRESTRequest.AddParameter('oauth_consumer_key', FApiConsKey);
  FRESTRequest.AddParameter('oauth_consumer_secret', FApiConsSecret);
  FRESTRequest.AddParameter('regular_price', '2500');
  FRESTRequest.AddParameter('description', '<table><tr><td>asdasdasdasd</td><td>asdasdasdasdsa</td></tr></table>');
  FLastSignature := FOAuth1Authenticator.SigningClass.BuildSignature(FRESTRequest, FOAuth1Authenticator);
  LTimestamp := FOAuth1Authenticator.timeStamp.DeQuotedString;
  FRESTRequest.AddParameter('oauth_timestamp', LTimestamp);
  FRESTRequest.AddParameter('oauth_nonce', LNonce);
  FRESTRequest.AddParameter('oauth_signature', FLastSignature );
  FRESTRequest.AddParameter('oauth_signature_method', 'HMAC-SHA1');
  FRESTRequest.AddParameter('oauth_version', '1.0');
  FRESTRequest.Execute;
  FLastResponse := FRESTResponse.Content;
end;

  //******************  GETTER SETTER  *****************//

function TWCProductManager.Products: TWCProducts;
begin
  Result := FProducts;
end;

//procedure TWCProductManager.TestProdupdate;
//var
//  ht: TIdHTTP;
//  stream: TStringStream;
//  retht: TRESTHTTP;
//  sign, LSignStr , Ltimestamp: string;
//  Paramlist : TStringList;
//  LNonce, LParamsStr : string;
//  i : integer;
//  jso: TJSONObject;
//begin
//  retht := TRESTHTTP.Create;
//  LTimestamp := FOAuth1Authenticator.timeStamp.DeQuotedString;
//  LNonce := FOAuth1Authenticator.Nonce;
//
//  Paramlist := TStringList.Create;
//  Paramlist.Values['name'] := 'Teszt 2';
//  Paramlist.Values['type'] := 'simple';
//  Paramlist.Values['regular_price'] := '21.99';
//  Paramlist.Values['description'] := 'description';
//  Paramlist.Values['images[0][src]'] := 'https://shop.pcunion.hu/image/cache/catalog/pc_k/lenovo-m92p-desktop-800x800.jpg';
//  Paramlist.Values['short_description'] := 'short_description';
//  Paramlist.Values['oauth_consumer_secret'] := FApiConsSecret;
//  Paramlist.Values['oauth_consumer_key'] := FApiConsKey;
//  Paramlist.Values['oauth_timestamp'] := Ltimestamp;
//  Paramlist.Values['oauth_nonce'] := LNonce;
//  Paramlist.Values['oauth_signature_method'] := 'HMAC-SHA1';
//  Paramlist.Values['oauth_version'] := '1.0';
//
//  Paramlist.Sort;
//    /// Step #2 - build a single string from the params
//  LParamsStr := '';
//  for i := 0 to Paramlist.Count - 1 do
//  begin
//    LParamsStr := LParamsStr + URIEncode(Paramlist.Names[i]) + '=' + URIEncode(Paramlist.ValueFromIndex[i]);
//    if (i < Paramlist.Count - 1) then
//      LParamsStr := LParamsStr + '&';
//  end;
//
//  LSignStr := 'POST' + '&'+ URIEncode(FApiBaseUrl + '/wp-json/wc/v3/Products')+ '&' + URIEncode(LParamsStr);
//  sign := retht.HashHMACSHA1(LSignStr ,FApiConsSecret + '&');
//  LParamsStr := LParamsStr + '&oauth_signature=' + URIEncode(sign);
//
//  jso := TJSONObject.Create;
//  for i := 0 to Paramlist.Count - 1 do
//  begin
//    jso.AddPair(Paramlist.Names[i], Paramlist.ValueFromIndex[i] );
//  end;
//
//  stream :=  TStringStream.Create(LParamsStr, TEncoding.UTF8);
//
//  ht := TIdHTTP.Create();
//  ht.Request.Method := 'POST';
//  ht.Request.Connection := 'keep-alive';
//  ht.Request.ContentLength := stream.Size;
//  ht.Request.ContentType := 'application/x-www-form-urlencoded';
//  ht.Request.Host := '192.168.1.7';
//  ht.Request.Accept := 'application/json';
//  ht.Request.AcceptCharSet := 'UTF-8';
//  ht.Request.AcceptEncoding := 'identity';
//
//  FlastResponse := ht.Post(FApiBaseUrl + '/wp-json/wc/v3/Products', stream );
//
//
//end;


end.

