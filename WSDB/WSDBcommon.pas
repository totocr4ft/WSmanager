unit WSDBcommon;

interface

uses System.Classes,forms,Vcl.Dialogs,System.SysUtils, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI, WSCommon;

 type
  Tresultset = array of TStringList;
  Tdatabase = Class(TWSCommon)

   private
    CONN : TFDConnection;
    QUERY: TFDQuery;
   public
    constructor create();
    destructor destroy;
    property con:TFDConnection Read CONN;
    function convert_result(Q:TFDQuery):Tresultset;
    function run_query(str:string;params:array of string):Tresultset;
    function run_query_not_conv(str:string;params:array of string; out con:Tdatabase):TFDquery;
    function exec_query(str:string;params:array of string):boolean;
    function exec_query_ret_id(str:string;params:array of string):integer;

    //*******  TRANSCTION FUNCTIONS *******//
    function begin_trans():Tdatabase;
    function commit_trans(db:Tdatabase):Boolean;
    function rollback_trans(db:Tdatabase):Boolean;
    function trans_exec_query(db:Tdatabase;str:string;params:array of string):boolean;
    function trans_run_query(db:Tdatabase;str:string;params:array of string):Tresultset;
   End;
    procedure free_result(res:Tresultset);

implementation

constructor Tdatabase.create();
var con:TFDConnection;
    con_str,text: String;
    inipath : string;
    f : TextFile;
    data    : TStringList;
begin
 inherited;
 inipath := ExtractFilePath(Application.ExeName)+'db.ini';
 if(FileExists(inipath)) then
  begin
    Assignfile(F,inipath);
    Reset(F);
    ReadLn(F, text);
    data := TStringList.Create;
    data.CommaText := text;

    CONN := TFDConnection.Create(nil);
    CONN.DriverName := 'MYSQL';
    CONN.Params.UserName := 'c2munkalap';//data.Values['USER'];
    CONN.Params.Password := '1nf0baz1s';//data.Values['PASS'];
    CONN.Params.Add('Server=78.131.88.108');//+data.Values['SERVER']);
    CONN.Params.Add('Database=c2munkalap');//+data.Values['DB']);
    CONN.Params.Add('CharacterSet=UTF8');
    CONN.Params.Add('VendorLib='+ExtractFilePath(Application.ExeName)+'libmysql.dll');
    CONN.LoginPrompt := False;
    CONN.FetchOptions.Mode := fmAll;
    CONN.ResourceOptions.SilentMode := False;
    CONN.ResourceOptions.DirectExecute := True;
    CONN.ResourceOptions.CmdExecTimeout := 10000;
    Log('MYSQL Connection established', LSQLCOMMON);
    CloseFile(f);
    try
     CONN.Connected := True;
    Except
     on E:Exception do
      begin
       Log('DB Error' + E.Message, LEXCEPT);
      end;
    end;
  end;
end;

destructor Tdatabase.destroy;
begin
 CONN.Close;
 CONN.Destroy;
 inherited;
end;

function Tdatabase.convert_result(Q:TFDquery):Tresultset;
var
 i,y : integer;
 a   :Tresultset;
begin
if Q = NIL then exit;
if Q.RecordCount <> 0 then
 begin
  try
   Q.First;
   SetLength(a,q.RecordCount);
   i := 0;
  while not Q.EOF do
   begin
    a[i] := TStringList.Create;
    for y := 0 to  q.FieldCount -1 do
     begin
      a[i].Values[q.Fields[y].FieldName] := q.FieldByName(q.Fields[y].FieldName).AsString;
     end;
    inc(i);
    Q.Next;
   end;
   result := a;
  except
  on E:Exception do
   begin
    Log('DB Error' + E.Message, LEXCEPT);
   end;
  end;
 end;
end;

function Tdatabase.trans_run_query(db:Tdatabase;str:string;params:array of string):Tresultset;
var d : TFDquery;
    res: Tresultset;
    i:integer;
begin
{try
 if not Assigned(db) then exit;
 d := TFDquery.Create(nil);
 d.Connection := CONN;
 d.SQL.Add(str);
 if length(params) > 0 then
  begin
   for i := 0 to length(params) - 1 do
    begin
     d.Params.Items[i].Value:= params[i];
    end;
  end;
 d.Open;
if d.RecordCount > 0 then
   begin
    result := convert_result(d);
   end
else result := nil;
finally
 d.free;
end;      }
end;

function Tdatabase.run_query(str:string;params:array of string):Tresultset;
var
    res: Tresultset;
    i:integer;
    y : Integer;
begin
  try
   QUERY := TFDquery.Create(nil);
   LOG('Q: '+str, LSQLDUMP);
   LOG('**** PARAMS: ****');
   for y := 0 to Length(params) - 1 do
     begin
      LOG('P'+IntToStr(y+1) +': '+params[y] , LSQLDUMP) ;
     end;
    LOG('**** END ****', LSQLDUMP);

   QUERY.ResourceOptions.CmdExecTimeout := 10000 ;
   QUERY.Connection := CONN;
   QUERY.SQL.Add(str);
   if length(params) > 0 then
    begin
     for i := 0 to length(params) - 1 do
      begin
       QUERY.Params.Items[i].Value:= params[i];
      end;
    end;
   QUERY.Active := True;
  if QUERY.RecordCount > 0 then
     begin
      result := convert_result(QUERY);
     end
  else result := nil;
   CONN.destroy;
  except
  on E:Exception do
   begin
    Log('DB Error' + E.Message, LEXCEPT);
    if Assigned(CONN) then CONN.destroy;
   end;
  end;
 QUERY.free;
end;

function Tdatabase.run_query_not_conv(str:string;params:array of string; out con:Tdatabase):TFDquery;
var d : TFDquery;
    res: Tresultset;
    i,y:integer;
    CONN : Tdatabase;
begin
{  try
   CONN := Tdatabase.create();
   d := TFDquery.Create(nil);
   d.Connection := CONN.con;
   d.SQL.Add(str);
  // MC.logger('DBQUERY', 'Q: '+str);
  // MC.logger('DBQUERY', '**** PARAMS: ****');
   for y := 0 to Length(params) - 1 do
     begin
  //    MC.logger('DBQUERY', 'P'+IntToStr(y+1) +': '+params[y]) ;
     end;
 //   MC.logger('DBQUERY', '**** END ****');
   if length(params) > 0 then
    begin
     for i := 0 to length(params) - 1 do
      begin
       d.Params.Items[i].Value:= params[i];
      end;
    end;
   d.Open;
  if d.RecordCount > 0 then
     begin
      result := d;
      con := CONN;
     end
  else result := nil;
  except
  on E:Exception do
   begin
 //   MC.logger('EX',E.ClassName+': '+ E.Message );
    if Assigned(CONN) then CONN.destroy;
   end;
  end;  }
end;

function Tdatabase.trans_exec_query(db:Tdatabase;str:string;params:array of string):boolean;
var d : TFDquery;
    res: Tresultset;
    i:integer;
    CONN : Tdatabase;
   // r : _Recordset;
begin
{try
  if not Assigned(db) then exit;
  CONN := db;
  d := TFDQuery.Create(nil);
  d.Connection := CONN.con;
  if length(params) > 0 then
   begin
    for i := 0 to length(params) - 1 do
     begin
      d.Params.Items[i].Value:= params[i];
     end;
   end;
  d.Execute;
  result := True;
finally
  d.free;
end;    }
end;

function Tdatabase.exec_query(str:string;params:array of string):boolean;
var d : TFDQuery;
    res: Tresultset;
    i,y:integer;
    CONN : Tdatabase;
begin
{  try
   CONN := Tdatabase.create();
   d := TFDQuery.Create(nil);
   d.Connection := CONN.con;
   d.SQL.Add(str);
 //  MC.logger('DBQUERY', 'Q: '+str);
 //  MC.logger('DBQUERY', '**** PARAMS: ****');
   for y := 0 to Length(params) - 1 do
     begin
 //     MC.logger('DBQUERY', 'P'+IntToStr(y+1) +': '+params[y]) ;
     end;
//   MC.logger('DBQUERY', '**** END ****');
   if length(params) > 0 then
    begin
     for i := 0 to length(params) - 1 do
      begin
       d.params.Items[i].Value:= params[i];
      end;
    end;
   d.ExecSQL;
   result := True;
   CONN.destroy;
  except
  on E:Exception do
   begin
//    MC.logger('EX',E.ClassName+': '+ E.Message );
    if Assigned(CONN) then CONN.destroy;
   end;
  end;
 d.free;    }
end;

function Tdatabase.exec_query_ret_id(str:string;params:array of string):integer;
var d : TFDQuery;
    res: Tresultset;
    i,y:integer;
    CONN : Tdatabase;
begin
 { try
   CONN := Tdatabase.create();
   d := TFDQuery.Create(nil);
   d.Connection := CONN.con;
   d.SQL.Add(str);
 //  MC.logger('DBQUERY', 'Q: '+str);
 //  MC.logger('DBQUERY', '**** PARAMS: ****');
   for y := 0 to Length(params) - 1 do
     begin
  //    MC.logger('DBQUERY', 'P'+IntToStr(y+1) +': '+params[y]) ;
     end;
 //  MC.logger('DBQUERY', '**** END ****');
   if length(params) > 0 then
    begin
     for i := 0 to length(params) - 1 do
      begin
       d.params.Items[i].Value:= params[i];
      end;
    end;
   d.ExecSQL;
   res := trans_run_query(CONN,' select LAST_INSERT_ID() as ID ',[]);
   result := StrToInt(res[0].Values['ID']);
   CONN.destroy;
  except
  on E:Exception do
   begin
  //  MC.logger('EX',E.ClassName+': '+ E.Message );
    if Assigned(CONN) then CONN.destroy;
   end;
  end;
 d.free;   }
end;

function Tdatabase.begin_trans():Tdatabase;
var db:Tdatabase;
begin
{ try
  db := Tdatabase.create();
  db.CONN.Transaction.StartTransaction;
  Result := db;
 finally
 end;  }
end;

function Tdatabase.commit_trans(db:Tdatabase):Boolean;
begin
{  try
   db.CONN.Transaction.Commit;
   Result := true;
  finally
   db.destroy;
  end;  }
end;

function Tdatabase.rollback_trans(db:Tdatabase):Boolean;
begin
 { try
   db.CONN.Transaction.Rollback;
   Result := true;
  finally
   db.destroy;
  end;  }
end;

procedure free_result(res:Tresultset);
var i:integer;
begin
for I := 0 to Length(res)-1 do
 begin
  FreeAndNil(res[i]);
 end;
 SetLength(res,0);
end;

end.


