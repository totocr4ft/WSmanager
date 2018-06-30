unit WSCommon;

interface
uses System.classes, System.Sysutils, VCL.Forms;

    type
      TWSmsgtype = (LINFO, LWARN, LERROR ,LEXCEPT, LDUMPINFO, LSQLCOMMON, LSQLDUMP);
      TWSmsgOptions = array of TWSmsgtype;
      TWSonLog = procedure(Amsg:string; Atype: TWSmsgtype) of object;

      TWSCommon = class(TObject)
       private
         _logOpt: TWSmsgOptions;
         _onlog : TWSonLog;
         function LogtypeToString(t:TWSmsgtype):string;
         function LogtypeAllowed(t:TWSmsgtype):boolean;
         function log_dir:string;
         procedure append_file(filename:string;txt:string);
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
 _logOpt := [LINFO, LWARN, LERROR ,LEXCEPT, LDUMPINFO, LSQLCOMMON, LSQLDUMP];
end;

procedure TWSCommon.Log(Amsg: string; Atype: TWSmsgtype);
var ActualDir : string;
begin
 if AMsg <> '' then
 begin
  if LogtypeAllowed(AType) then
  begin
    AMsg := FormatDateTime('YYYY-MM-DD h:n:s', now) + ' ( ' + LogtypeToString(Atype) + ' ) > ' + Amsg;
    ActualDir := log_dir;
    if Assigned(_onlog) then
      _onlog(Amsg,Atype);
    if actualdir <> '' then
      append_file(actualdir+'LOG.txt', AMsg);
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
   LINFO:      Result := 'Information';
   LWARN:      Result := 'Warning';
   LERROR:     Result := 'Error';
   LEXCEPT:    Result := 'Exception' ;
   LDUMPINFO:  Result := 'Data Dump' ;
   LSQLCOMMON: Result := 'SQL Info' ;
   LSQLDUMP:   Result := 'SQL DUMP' ;
 end;
end;

procedure TWSCommon.append_file(filename:string;txt:string);
var f   : TextFile;
begin
try
if FileExists(filename) then
 begin
  AssignFile(f, filename);
  Append(f);
  Writeln(f,txt);
 end
  else
   begin
    AssignFile(f, filename);
    Rewrite(f);
    Writeln(f,txt);
   end;
 CloseFile(f);
except
 CloseFile(f);
end;
end;

function TWSCommon.log_dir:string;
var f   : TextFile;
    path, new,year,month,day,tcp_string: string;
begin
path  := ExtractFilePath(Application.ExeName);
year  := FormatDateTime('YYYY', now);
month := FormatDateTime('MM', now);
day   := FormatDateTime('DD', now);
try
if not DirectoryExists(path + 'LOG') then MkDir(path + 'LOG');
if not DirectoryExists(path + 'LOG\' + year) then MkDir(path + 'LOG\'+year);
if not DirectoryExists(path + 'LOG\'+  year + '\' + month ) then MkDir(path + 'LOG\'+ year + '\' + month);
if not DirectoryExists(path + 'LOG\' + year + '\' + month + '\' + day ) then MkDir(path + 'LOG\'+ year + '\' + month + '\' + day);
Result := path + 'LOG\'+ year + '\' + month + '\' + day + '\';
except
 on e:Exception do
  begin
   Result := '';
  end;
end;
end;

end.

