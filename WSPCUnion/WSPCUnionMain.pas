unit WSPCUnionMain;

interface
uses Classes, System.SysUtils, System.Generics.Collections,
     WSOCPRoduct,
     WSCommon,
     WSHtmlCommon,
     WSDBcommon,
     WCRestApi;

  type

  TWSPCUnion = class(TWSCommon)
  private
    FPRoducts: TWCProducts;
    FHTmlManager: TWSHtml;
    FLangID: Integer;
    procedure RecLog(Amsg:string; Atype: TWSmsgtype);
  public
    property Products: TWCProducts read FPRoducts;
    property LanguageID: integer read FLangID write FLangID;
    function ReadCSV: Boolean;
    function FillProductsSpec(AIndex: integer): Boolean;
    function GetProductByIndex(AIndex: integer):TWCProduct;
    function GetProductByID(AID: integer): TWCProduct;
    constructor Create;
  end;

  const

  PCU_MAIN_URL    = 'https://shop.pcunion.hu';
  PCU_GET_PRODUCT = PCU_MAIN_URL + '/index.php?route=product/product&product_id=';
  PCU_GT_PR_LIST  = PCU_MAIN_URL + '/arlista/arlista2.php?email=garamszegit@gmail.com'
                                 + '&pass=c2c0b999d197ee4afefe29ee62b2e7d9db31e658&groupid=1';

implementation

{ TWSPCUnion }

constructor TWSPCUnion.Create;
begin
  inherited;

  FLangID := 0;
  FHTmlManager := TWSHtml.create;
  FHTmlManager.OnLog := RecLog;
  FPRoducts := TWCProducts.Create;
end;

function TWSPCUnion.ReadCSV: Boolean;
var
  LRes : Tresultset;
  LProduct: TWCProduct;
  I, LProductID: Integer;
  LMemStream: TStream;
  LImage: TWCProductImage;
begin
  Result := False;
  LRes := FHTmlManager.GetCSV(PCU_GT_PR_LIST);
  if Length(LRes) = 0 then
  begin
    Log('CSV Error', LERROR);
    exit;
  end;
  FPRoducts.Clear;
  LMemStream := TStream.Create;
  for I := 0 to Length(Lres) - 1 do
  begin
    try
      if not TryStrToInt(LRES[i].Values['product_id'], LProductID) then
      begin
        Log('ID Error ' + LRES[i].Values['product_id'] , LERROR);
        Continue;
      end;
      LProduct.id := LProductID;
      LProduct.sku := LRES[i].Values['model'];
      LProduct.name := LRES[i].Values['name'];
      LProduct.regular_price := LRES[i].Values['your_price'+#$D];
      LProduct.regular_price := StringReplace(LProduct.regular_price, #$D, '', [rfReplaceAll, rfIgnoreCase]);
      LProduct.stock_quantity := StrToInt(LRES[i].Values['quantity']);
      LProduct.images := TDictionary<Integer, TWCProductImage>.Create();
      LImage.src := LRES[i].Values['image'];
      LProduct.images.Add(0 ,LImage);

      FPRoducts.Add(I, LProduct);
     // FillProductsSpec(FPRoducts.Count - 1);
      Log('Product Registered: ' + IntToStr( LProduct.ID ) , LDUMPINFO);
    Except
      on E: exception do
        Log('Product List creation error: ' + E.Message, LEXCEPT);
    end;
  end;
  Log('CSV received Count: ' + IntToStr(Length(LRes)));
  Result := True;
end;


function TWSPCUnion.FillProductsSpec(AIndex: integer): Boolean;
var
  LProd: TWCProduct;
  LTable: TWShtmlTable;
  LTabcontents: TResultset;
  LTabcontent: TStringlist;
  LHeader: string;
begin
  Result := False;
  if FPRoducts.Count = 0 then
  begin
    Log('No product!', LERROR);
    Exit;
  end;

    try
      LProd := FPRoducts[FPRoducts.ToArray[AIndex].Key];
      FHTmlManager.get(PCU_GET_PRODUCT + IntToStr(LProd.id));

      for LTable in FHTmlManager.Tables.ToArray do
      begin
        LTabcontents := TResultset( LTable.content.ToArray );
        LHeader := '';
        for LTabcontent in LTabcontents do
        begin
          case LTabcontent.Count of
            0: Continue;
            1:
            begin
              Log('Maybe header: ' + LTabcontent[0], LDUMPINFO);
              LHeader := LTabcontent[0];
            end;
            2:
            begin
              Log('Maybe a key-value pair: '
                  + StringReplace( LTabcontent[0], ':', '', [rfReplaceAll, rfIgnoreCase] )
                  + ' > ' + LTabcontent[1] , LDUMPINFO);
              if LHeader = 'Specifikáció' then
              begin
                try
                    LProd.description := LProd.description + '<table>';
                    LProd.description := LProd.description + '<tr><td>'+LTabcontent[0]+'</td><td>'+LTabcontent[1]+'</td></tr>';
                    LProd.description := LProd.description + '</table>';

                except
                  on E: exception do
                    Log('Secification fillup failed: ' + E.Message, LEXCEPT);
                end;
              end;
            end;
          end;
        end;
      end;
    except
      on E: exception do
      begin
        Log('Secification fillup failed: ' + E.Message, LEXCEPT);
        exit;
      end;
    end;
    Result := True;

end;

function TWSPCUnion.GetProductByID(AID: integer): TWCProduct;
begin
  Result := FPRoducts[AID];
end;

function TWSPCUnion.GetProductByIndex(AIndex: integer): TWCProduct;
begin
  Result := FPRoducts[FPRoducts.ToArray[AIndex].Key];
end;

procedure TWSPCUnion.RecLog(Amsg: string; Atype: TWSmsgtype);
begin
  if Assigned(_onLog) then
    _onLog(Amsg, Atype);
end;

end.
