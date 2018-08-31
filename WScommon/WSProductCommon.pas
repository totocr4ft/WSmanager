unit WSProductCommon;

interface
uses Classes, System.SysUtils, System.Generics.Collections;


  type
  TWSProductData = TStringList;

  IWSProduct = Interface(IInterface)
    ['{4C645696-D895-42C2-B6E0-9E6045E9CD7D}']
    function GetID: Integer;
    function GetData: TWSProductData;
    function GetLangID: integer;
  End;


implementation

end.
