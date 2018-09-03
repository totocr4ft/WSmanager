unit WSPCUnionMain;

interface
uses Classes, System.SysUtils, System.Generics.Collections,
     WSOCPRoduct,
     WSCommon,
     WSHtmlCommon,
     WSDBcommon;

  type

  TWSPCUnion = class(TWSCommon)
  private
    FPRoducts: TWSOCPRoducts;
    FHTmlManager: TWSHtml;
    FLangID: Integer;
    procedure RecLog(Amsg:string; Atype: TWSmsgtype);
  public
    property Products: TWSOCPRoducts read FPRoducts;
    property LanguageID: integer read FLangID write FLangID;
    function ReadCSV: Boolean;
    function FillProductsSpec(AIndex: integer): Boolean;
    function FillProductsImage(AIndex: integer): Boolean;
    function GetProductByIndex(AIndex: integer):TWSOCProduct;
    function GetProductByID(AID: integer): TWSOCProduct;
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
  FPRoducts := TDictionary<Integer, TWSOCProduct>.Create;
end;

function TWSPCUnion.FillProductsImage(AIndex: integer): Boolean;
var
  LProd: TWSOCProduct;
begin
  Result := False;
  if FPRoducts.Count = 0 then
  begin
    Log('No product!', LERROR);
    Exit;
  end;

  try
    LProd := FPRoducts[FPRoducts.ToArray[AIndex].Key];
    if LProd.ProductData.Values['image'] = '' then
      exit;

    LProd.ProductImage := FHTmlManager.GetJPG(LProd.ProductData.Values['image']);
    if not Assigned(LProd.ProductImage) then
      exit;
  except
    on E: exception do
    begin
      Log('Image fillup failed: ' + E.Message, LEXCEPT);
      exit;
    end;
  end;
  Result := True;
end;

function TWSPCUnion.FillProductsSpec(AIndex: integer): Boolean;
var
  LProd: TWSOCProduct;
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
      FHTmlManager.get(PCU_GET_PRODUCT + IntToStr(LProd.ID));

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
                  if not Assigned(LProd.ProductSpec) then
                    LProd.ProductSpec := TStringList.Create;
                  LProd.ProductSpec.AddPair(LTabcontent[0], LTabcontent[1]);
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

function TWSPCUnion.GetProductByID(AID: integer): TWSOCProduct;
begin
  Result := FPRoducts[AID];
end;

function TWSPCUnion.GetProductByIndex(AIndex: integer): TWSOCProduct;
begin
  Result := FPRoducts[FPRoducts.ToArray[AIndex].Key];
end;

function TWSPCUnion.ReadCSV: Boolean;
var
  LRes : Tresultset;
  LProduct: TWSOCProduct;
  I, LProductID: Integer;
  LMemStream: TStream;
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


      LProduct := TWSOCProduct.create(LProductID,
                                      LanguageID,
                                      LRes[i]
                                     );

      FPRoducts.Add(LProduct.ID, LProduct);
      Log('Product Registered: ' + IntToStr( LProduct.ID ) , LDUMPINFO);
    Except
      on E: exception do
        Log('Product List creation error: ' + E.Message, LEXCEPT);
    end;
  end;
  Log('CSV received Count: ' + IntToStr(Length(LRes)));
  Result := True;
end;

procedure TWSPCUnion.RecLog(Amsg: string; Atype: TWSmsgtype);
begin
  if Assigned(_onLog) then
    _onLog(Amsg, Atype);
end;

end.
