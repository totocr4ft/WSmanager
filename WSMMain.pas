unit WSMMain;

interface
uses Classes, System.SysUtils, System.Generics.Collections,
     WSOCPRoduct, WSProductCommon, WSCommon, WCRestApi, WSPCunionMain, Vcl.Forms;

  type
  TWSMMain = class(TWSCommon)
  private
    FWCProductManager: TWCProductManager;
    FWSPCunion: TWSPCUnion;
    procedure RecLog(Amsg:string; Atype: TWSmsgtype);
  public
    constructor Create(AApiConsKey, AApiConsSecret, ABaseUrl: string);
    procedure DownloadPCUproducts;
    procedure DownloadWCProducts;
    procedure WCFlushProducts;
    procedure UploadProdFormWSPCU;
  end;

implementation

{ TWSMMain }

constructor TWSMMain.Create(AApiConsKey, AApiConsSecret, ABaseUrl: string);
begin
  inherited Create;
  FWCProductManager := TWCProductManager.Create(AApiConsKey, AApiConsSecret, ABaseUrl);
  FWSPCunion := TWSPCUnion.Create;
  FWCProductManager.OnLog := RecLog;
  FWSPCunion.OnLog := Reclog;
end;

procedure TWSMMain.RecLog(Amsg: string; Atype: TWSmsgtype);
begin
  if Assigned(_onLog) then
    _onLog(Amsg, Atype);
end;

procedure TWSMMain.DownloadPCUproducts;
begin
  FWSPCunion.ReadCSV;
end;

procedure TWSMMain.DownloadWCProducts;
begin
  FWCProductManager.DownloadCategories;
  FWCProductManager.DownloadProducts;
end;

procedure TWSMMain.UploadProdFormWSPCU;
var
  I: Integer;
begin
  try
    FWCProductManager.DownloadProducts;
    FWCProductManager.DeleteAllProduct;
    FWSPCunion.ReadCSV;
  except

  end;
  for i := 0 to FWSPCunion.Products.Count - 1 do
  begin
    try
      FWCProductManager.CreateProduct(FWSPCunion.Products[I]);
      Application.ProcessMessages;
      Sleep(1000);
      Log('Termék feltöltve: '+FWSPCunion.Products[I].name );
    except
      Log('Termék Hiba: '+FWSPCunion.Products[I].name );
    end;

  end;

end;

procedure TWSMMain.WCFlushProducts;
begin
  FWCProductManager.DownloadProducts;
  FWCProductManager.DeleteAllProduct;
end;



end.
