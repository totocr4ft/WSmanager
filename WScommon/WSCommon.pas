unit WSCommon;

interface
uses System.classes, System.Sysutils;

    type
      TWSmsgtype = (LINFO, LWARN, LERROR ,LEXCEPT, LDUMPINFO );
      TWSmsgOptions = array of TWSmsgtype;
      TWSonLog = procedure(Amsg:string; Atype: TWSmsgtype) of object;

      TWSCommon = class(TObject)
       private
         _logOpt: TWSmsgOptions;
         _onlog : TWSonLog;
         function LogtypeToString(t:TWSmsgtype):string;
         function LogtypeAllowed(t:TWSmsgtype):boolean;
       public
         property OnLog : TWSonLog read _onlog write _onlog;
         property LogOptions : TWSmsgOptions read _logOpt write _logOpt;
         procedure Log(Amsg:string; Atype: TWSmsgtype = LINFO);
         constructor create;
      end;

implementation

{ TWSCommon }

constructor TWSCommon.create;
begin
 _logOpt := [LINFO, LWARN, LERROR ,LEXCEPT];
end;

procedure TWSCommon.Log(Amsg: string; Atype: TWSmsgtype);
begin
 if AMsg <> '' then
 begin
  if LogtypeAllowed(AType) then
  begin
    AMsg := FormatDateTime('YYYY-MM-DD h:n:s', now) + ' ( ' + LogtypeToString(Atype) + ' ) > ' + Amsg;
    if Assigned(_onlog) then
     _onlog(Amsg,Atype);
  end;
 end;
end;

function TWSCommon.LogtypeAllowed(t: TWSmsgtype): boolean;
 var i : integer;
begin
 Result := false;
 for I := 0 to Length(LogOptions) - 1 do
 begin
  if t = LogOptions[i] then
  begin
   Result := true;
   break;
  end;
 end;
end;

function TWSCommon.LogtypeToString(t: TWSmsgtype): string;
begin
 case t of
   LINFO:   Result := 'Information';
   LWARN:   Result := 'Warning';
   LERROR:  Result := 'Error';
   LEXCEPT: Result := 'Exception' ;
 end;
end;

end.

